# setwd("C:/Users/Aneta/Projects/RShinyApp")

library(shiny)
library(shinyalert)
library(serial)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list=ls())

saveData <- function(res) {
  if (exists("responses")) {
    print(">>> Adding new measurements to data table...")
    responses2 <<- data.frame(res, stringsAsFactors = FALSE)
    responses2 <<- responses2 %>% separate(res, c("channel", "freq", "Z_re", "Z_im"), extra='drop')
    responses <<- rbind(responses, responses2)
    print(responses)
    
  } else {
    print(">>> Creating data table with measurements...")
    responses <<- data.frame(res, stringsAsFactors = FALSE)
    print(responses)
    
    responses <<- responses %>% separate(res, c("channel", "freq", "Z_re", "Z_im"), extra='drop')
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



ui <- shinyUI(fluidPage(
  
  titlePanel("Mobi C bis v2"),
  
  sidebarLayout(position = "right",
            sidebarPanel(
              
              helpText("Use buttons below in order to connect or disconnect with the device."),
        
              useShinyalert(),
              actionButton("conButton", label="Connect"),
              actionButton("disconButton", label="Disconnect"),
              
              hr(),
              checkboxGroupInput("channelChoice", 
                                 label = h5("Select channels to measure"), 
                                 choices = list("Channel 1" = 1, "Channel 2" = 2, 
                                                "Channel 3" = 3, "Channel 4" = 4, 
                                                "Channel 5" = 5, "Channel 6" = 6,
                                                "Channel 7" = 7, "Channel 8" = 8),
                                 selected = 1),
              hr(),
              textInput("cycleNumber", h5("Select number of measuring cycles"), "1"),
              
              hr(),
              selectInput("cmdChoice", h5("Choose measuring command"), 
                          choices = list("107" = 107, "110" = 110,
                                         "114" = 114), selected = 107),
        
              actionButton("startButton", label="Start"),
              actionButton("plotButton", label="Plot")
              
            ),
            
            mainPanel(
              h3("Measurements"), DT::dataTableOutput("responses"),
              h3("Impedance curve for selected frequencies"), plotOutput("plot")
            )
  )
))

server <- shinyServer(function(input, output, session) {
  
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
          
          formData <- reactive({
            sapply(fields, function(x) input[[x]], simplify = FALSE)
          })
          
          output$responses <- DT::renderDataTable({
            req(input$startButton)

            inputParameters <- isolate(formData())

            frequencies <- c("7017", "10796", "16841", "26315", "42103", "60148", "105258")
            
            print(">>> Checking which command")
            
            ### CMD 107 ###
            
            if (inputParameters$cmdChoice == "107") {
              
              print(">>> Command 107")  
              
              for (ch in inputParameters$channelChoice){
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

                      res <- substr(res, 9, 25)
                      print(res)
                      
                      saveData(res)
                      datatable(loadData(), rownames = FALSE)
      
                    } else {
                      print(">>> Measurement not confirmed")
                    }
                }
              }
            } else if (inputParameters$cmdChoice == "110") { 
              
              ### CMD 110 ###
              
              print(">>> Command 110") 
              
              for (ch in inputParameters$channelChoice){
                print(ch)
                
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
                    res <- read.serialConnection(mobiConn, n = 0)
                    print(res)
                    
                    res <- substr(res, 9, 25)
                    print(res)
                    
                    saveData(res)
                    datatable(loadData(), rownames = FALSE)
                    
                  } else {
                    print(">>> Measurement not confirmed")
                  }
                }
              }
              
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
                  
                  # res <- substr(res, 9, 25)
                  # print(">>> res")
                  # print(res)
                  res_7017 <- paste(substr(res, 9, 9), substr(res, 16, 29), sep=" ")
                  res_10796 <- paste(substr(res, 9, 9), substr(res, 31, 45), sep=" ")
                  res_16841 <- paste(substr(res, 9, 9), substr(res, 47, 61), sep=" ")
                  res_26315 <- paste(substr(res, 9, 9), substr(res, 63, 77), sep=" ")
                  res_42103 <- paste(substr(res, 9, 9), substr(res, 79, 93), sep=" ")
                  res_60148 <- paste(substr(res, 9, 9), substr(res, 95, 109), sep=" ")
                  res_105258 <- paste(substr(res, 9, 9), substr(res, 111, 126), sep=" ")
                  
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
              
            }, options = list(pageLength = 5)
          )

          proxy <- dataTableProxy("responses")
          
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
          
          observeEvent(input$plotButton, {
            toPlot <- loadData()

            toPlot[] <- lapply(toPlot, as.numeric)
            print("Data toPlot") 
            print(toPlot)
            
            output$plot <- renderPlot({
              re = toPlot$Z_re
              im = toPlot$Z_im
              channel = toPlot$channel
              
              ggplot(toPlot,  aes(x=re, y=im, group = channel)) + 
                  geom_point() +
                  geom_line(color = channel)
            })
        })

})


# Creates Shiny app
shinyApp(ui = ui, server = server)
