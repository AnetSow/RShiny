# setwd("C:/Users/Aneta/Projects/RShinyApp")

library(shiny)
library(shinyalert)
library(serial)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)

library(gridExtra)
library(shinyjs)
library(waiter)

library(httr)
library(viridis)
library(shinycssloaders)


saveData <- function(res) {
  if (exists("responses")) {
    print(">>> Adding new measurements to data table...")
    responses2 <<- data.frame(res, stringsAsFactors = FALSE)
    responses2 <<- responses2 %>% separate(res, c("channel", "freq", "Z_re", "Z_im", "cycle"), extra='drop')
    responses <<- rbind(responses, responses2)
    print(responses)
  } else {
    print(">>> Creating data table with measurements...")
    responses <<- data.frame(res, stringsAsFactors = FALSE)
    print(responses)
    
    responses <<- responses %>% separate(res, c("channel", "freq", "Z_re", "Z_im", "cycle"), extra='drop')
    print(responses)
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

getDataValue <- function(i, j) {
  stopifnot(exists("responses"))
  responses[i, j]
}

setDataValue <- function(i, j, value) {
  stopifnot(exists("responses"))
  responses[i, j] <<- value
  responses
}


fields <- c("channelChoice", "cycleNumber", "cmdChoice")
NUM_PAGES <- 8


ui <- shinyUI(fluidPage(
  
  titlePanel("Mobi C bis"),
  
  sidebarLayout(position = "right",
            sidebarPanel(
              
              helpText("This app analyses efficiency of electrode's channels. Use buttons below in order to connect or disconnect with the device."),
        
              useShinyalert(),
              actionButton("conButton", label="Connect"),
              actionButton("disconButton", label="Disconnect"),
              
              hr(),
              helpText("You can either choose parameters to execute measurement or plot charts from uploaded log file."),
              checkboxGroupInput("channelChoice", 
                                 label = h5("Select channels to measure"), 
                                 choices = list("Channel 1" = 1, "Channel 2" = 2, 
                                                "Channel 3" = 3, "Channel 4" = 4, 
                                                "Channel 5" = 5, "Channel 6" = 6,
                                                "Channel 7" = 7, "Channel 8" = 8),
                                 selected = 1),

              textInput("cycleNumber", h5("Select number of measuring cycles"), "1"),
              
              selectInput("cmdChoice", h5("Choose measuring command"), 
                          choices = list("107" = 107, "110" = 110,
                                         "114" = 114), selected = 107),
              
              br(),
              actionButton("startButton", label="Start"),
              actionButton("plotButton", label="Plot"),
              
              hr(),
              fileInput('datafile', h5('Choose log file'), multiple = FALSE),
              
              br(),
              actionButton("plotLogButton", label="Plot")

            ),
            
            mainPanel(
              h3("Measurements"), DT::dataTableOutput("responses"),
              h3("Measurements from log"), DT::dataTableOutput("logOut"),
              
              br(),
              # use_waiter(),
              useShinyjs(),
              hidden(
                lapply(as.numeric(NUM_PAGES), function(i) {
                  div(class = "page",
                      id = paste0("step", i), "Step", i)
                  })
              ),

              br(),
              plotOutput("plot") %>% withSpinner(color = "blue"),

              br(),
              actionButton("prevButton", "< Previous"),
              actionButton("nextButton", "Next >")
            )
        )
  ))

server <- shinyServer(function(input, output, session) {

          # Connecting with the device
          observeEvent(input$conButton, {
            mobiConn <<- serialConnection("mobi", port="COM5", mode="115200,n,8,1")
            open(mobiConn)
            
            connChecking <- function(mobiConn){
              if (isOpen(mobiConn) == TRUE){ 
                print("Successfully connected!")
              } else {
                print("Connection failed!")}
            }
            
            shinyalert(connChecking(mobiConn))
          })
          
          # Disonnecting with the device
          observeEvent(input$disconButton, {
            close(mobiConn)
            
            disconnChecking <- function(mobiConn){
              if (isOpen(mobiConn) == FALSE){ 
                print("Successfully disconnected!")
              } else {
                print("Disconnection failed!")}
            }
            
            shinyalert(disconnChecking(mobiConn))
          })
         
          # Collecting parameters choosed by User 
          formData <- reactive({
            sapply(fields, function(x) input[[x]], simplify = FALSE)
          })
          
          # Measurement executing
          output$responses <- DT::renderDataTable({
            req(input$startButton)

            inputParameters <<- isolate(formData())

            frequencies <- c("7017", "10796", "16841", "26315", "42103", "60148", "105258")
            
            print(">>> Checking which command")
            
            ### CMD 107 ###
            
            if (inputParameters$cmdChoice == "107") {
              
              print(">>> Command 107")  
              
              for (c in 1:inputParameters$cycleNumber) {
                print(c)
                
                for (ch in inputParameters$channelChoice) {
                  print(ch)
                  
                  for (f in frequencies) {
                    print(f)
                  
                    request <- paste(inputParameters$cmdChoice, "3", ch, f, "0\r\n", sep=" ")
      
                    write.serialConnection(mobiConn, request)
                    
                    Sys.sleep(0.2)
                    
                    confirmation <- read.serialConnection(mobiConn, n = 0)
                    print(confirmation)
                    
                      if (grepl("016777200", confirmation) == TRUE) {
                        print(">>> Measurement confirmed")
                        Sys.sleep(5)
                        res <- read.serialConnection(mobiConn, n = 0)
                        print(res)

                        res <- paste(substr(res, 9, 25), as.character(c), sep=" ")
                        print(res)
                        
                        saveData(res)
                        datatable(loadData(), rownames = FALSE)
        
                      } else {
                        print(">>> Measurement not confirmed")
                      }
                  }
                }
              }
              print(">>> Measurement executed")  
              return(responses)
                
            } else if (inputParameters$cmdChoice == "110") { 
              
              ### CMD 110 ###
              
              print(">>> Command 110") 
              
              for (ch in inputParameters$channelChoice){
                print(ch)
                
                for (c in 1:inputParameters$cycleNumber) {
                  print(c)
                  
                  for (f in frequencies) {
                    print(f)
                    
                    write.serialConnection(mobiConn, "108 0\r\n") # otwarcie sesji
                    Sys.sleep(0.2)
                    read.serialConnection(mobiConn, n = 0)
                    print(">>> Session opened")
                    
                    Sys.sleep(5)
                    
                    request <- paste(inputParameters$cmdChoice, "3", ch, f, "0\r\n", sep=" ")
                    
                    write.serialConnection(mobiConn, request)
                    
                    Sys.sleep(10)
                    
                    confirmation <- read.serialConnection(mobiConn, n = 0)
                    print(confirmation)
                    
                    Sys.sleep(10)
                    
                    write.serialConnection(mobiConn, "109 0\r\n")
                    Sys.sleep(0.2)
                    read.serialConnection(mobiConn, n = 0)
                    print(">>> Session closed")
                    
                    if (grepl("016777200", confirmation) == TRUE) {
                      print(">>> Measurement confirmed")
                      Sys.sleep(5)
                      # res <- read.serialConnection(mobiConn, n = 0)
                      # print(res)
                      
                      res <- paste(substr(confirmation, 33, 49), as.character(c), sep=" ")
                      print(res)
                      
                      saveData(res)
                      datatable(loadData(), rownames = FALSE)
                      
                    } else {
                      print(">>> Measurement not confirmed")
                    }
                  }
                }
              }
              print(">>> Measurement executed")  
              return(responses)
              
            } else {
              
              ### CMD 114 ###
 
              print(">>> Command 114")
              
              channel_vector <- rep(0, 8)
              
              for (i in 1:length(channel_vector)){
                for (ch in inputParameters$channelChoice){
                  if (i == ch) {
                    channel_vector[i] = 1}
                }
              }
              
              print(">>> Selected channels")
              print(paste(channel_vector, collapse=" "))
              
              for (c in 1:inputParameters$cycleNumber) {
                print(c)
              
                write.serialConnection(mobiConn, "108 0\r\n")
                Sys.sleep(2)
                read.serialConnection(mobiConn, n = 0)
                print(">>> Session opened")
                
                Sys.sleep(0.2)
                
                request <- paste(inputParameters$cmdChoice, " 9 0 ", paste(channel_vector, collapse=" "), "\r\n", sep="")
                print(">>> request")
                print(request)
                
                write.serialConnection(mobiConn, request)
                
                Sys.sleep(0.5)
                
                confirmation <- read.serialConnection(mobiConn, n = 0)
                print(">>> confirmation")
                print(confirmation)
  
                if (grepl("016777200", confirmation) == TRUE) {
                  print(">>> Measurement confirmed")
                  Sys.sleep(3)
                  
                  for (i in 1:length(inputParameters$channelChoice)) {
                    res <- read.serialConnection(mobiConn, n = 0)
                    print(res)
                    
                    res_7017 <- paste(substr(res, 9, 9), substr(res, 16, 29), as.character(c), sep=" ")
                    res_10796 <- paste(substr(res, 9, 9), substr(res, 31, 45), as.character(c), sep=" ")
                    res_16841 <- paste(substr(res, 9, 9), substr(res, 47, 61), as.character(c), sep=" ")
                    res_26315 <- paste(substr(res, 9, 9), substr(res, 63, 77), as.character(c), sep=" ")
                    res_42103 <- paste(substr(res, 9, 9), substr(res, 79, 93), as.character(c), sep=" ")
                    res_60148 <- paste(substr(res, 9, 9), substr(res, 95, 109), as.character(c), sep=" ")
                    res_105258 <- paste(substr(res, 9, 9), substr(res, 111, 126), as.character(c), sep=" ")
                    
                    res_all <- matrix(c(res_7017, 
                                  res_10796, 
                                  res_16841, 
                                  res_26315, 
                                  res_42103, 
                                  res_60148, 
                                  res_105258), 
                                ncol=1, nrow=7)
  
                    saveData(res_all)
                    datatable(loadData(), rownames = FALSE)
                    
                    Sys.sleep(5)
                  }
                } else {
                  print(">>> Measurement not confirmed")
                }
                
                Sys.sleep(5)
                
                write.serialConnection(mobiConn, "109 0\r\n")
                read.serialConnection(mobiConn, n = 0)
                print(">>> Session closed")
              }
                
              print(">>> Measurement executed")  
              return(responses)
            }

            }, options = list(pageLength = 8)
          )

          # Transforming measurement data into datatable
          proxy <- dataTableProxy("responses")
          
          # Datatable updating
          observeEvent(input$responses_cell_edit, {
            info <- input$responses_cell_edit
            str(info)
            
            i <- info$row
            j <- info$col + 1
            v <- info$value
            
            newValue <- isolate(DT:::coerceValue(v, getDataValue(i, j)))
            setDataValue(i, j, newValue)
            DT::replaceData(proxy, loadData(), resetPaging = FALSE, rownames = FALSE)
          })
          
          
          # Uploading the selected log file and parsing data from log
          logData <- reactive({
            req(input$datafile)
            if(!exists("log_df", mode="function")) source("logs_parser.R")
            log_df()
          })
          
          output$logOut <- DT::renderDataTable({
            toPlot <- logData()
            return(toPlot)
          }, options = list(pageLength = 8)
          )
          
          # Navigating through plots
          rv <- reactiveValues(page = 1)

          observe({
            toggleState(id = "prevButton", condition = rv$page > 1)
            toggleState(id = "nextButton", condition = rv$page < NUM_PAGES)
            hide(selector = ".page")
            show(paste0("step", rv$page))
          })

          navPage <- function(direction) {
            rv$page <- rv$page + direction
          }

          observeEvent(input$prevButton, navPage(-1))
          observeEvent(input$nextButton, navPage(1))
          
          # Plotting measurement results
          observeEvent(input$plotButton, {

            if (isOpen(mobiConn) == TRUE) { 
              
            output$plot <- renderPlot({
              
              toPlot <- loadData()
              toPlot[] <- lapply(toPlot, as.numeric)

              print(">>> Data toPlot")
              print(toPlot)

              channel_list <- unique(toPlot$channel)
              plot_list <- list()


              for (i in channel_list) {

                df_channel = toPlot[toPlot$channel == i,]

                Z_im = - df_channel$Z_im

                p <- ggplot(df_channel, aes(x = Z_re, y = Z_im, group = cycle, color = factor(cycle))) +
                      geom_line() +
                      theme_minimal() +
                      theme(legend.position = "right") +
                      ggtitle(paste("Impedance curve for channel", as.character(i), sep = " ")) +
                      labs(x='Z re', y='-Z im') +
                      scale_color_brewer(palette = "RdYlBu")

                plot_list[[i]] <- p
              }
              plot_list[[rv$page]]
            })
            
            } else { shinyalert("You are not connected!") }
          })
          
          
          # Plotting measurement from log
          observeEvent(input$plotLogButton, {
            
          if(exists("log_df", mode="function")) {
                toPlot <- logData()
                
              output$plot <- renderPlot({
              
                print(">>> Data toPlot")
                print(toPlot)
                
                channel_list <- unique(toPlot$channel)
                plot_list <- list()
                
                
                for (i in channel_list) {
                  
                  df_channel = toPlot[toPlot$channel == i,]
                  
                  p <- ggplot(df_channel, aes(x = Z_re, y = Z_im, group = cycle, color = factor(cycle))) +
                    geom_line() +
                    theme_minimal() +
                    theme(legend.position = "right") +
                    ggtitle(paste("Impedance curve for channel", as.character(i), sep = " ")) +
                    labs(x='Z re', y='-Z im') +
                    scale_color_brewer(palette = "RdYlBu")
                  
                  plot_list[[i]] <- p
                }
                plot_list[[rv$page]]
              })
            
            } else { shinyalert("Try to upload a log file again.") } 
          })

})


# Creates Shiny app
shinyApp(ui = ui, server = server)
