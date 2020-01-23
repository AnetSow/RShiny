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
    responses2 <<- responses2 %>% separate(res, c("channel", "freq", "impedance_re", "impedance_im"), extra='drop')
    responses <<- rbind(responses, responses2)
    print(responses)
    
  } else {
    print(">>> Creating data table with measurements...")
    responses <<- data.frame(res, stringsAsFactors = FALSE)
    print(responses)
    
    responses <<- responses %>% separate(res, c("channel", "freq", "impedance_re", "impedance_im"), extra='drop')
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
              textInput("cycleNumber", "Number of measuring cycles", "1"),
              
              hr(),
              selectInput("cmdChoice", h5("Choose measuring command"), 
                          choices = list("107" = 107, "110" = 110,
                                         "114" = 114), selected = 107),
        
              actionButton("startButton", label="Start"),
              actionButton("plotButton", label="Plot")
              
            ),
            
            mainPanel(
              # h4("Output as text"), textOutput("mobiOut"),
              h4("Output as data table"), DT::dataTableOutput("responses"),
              h4("Plot"), plotOutput("plot")
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
            
            frequencies <- c("1000", "5000", "9000", "14000")
            
            for (f in frequencies) {
              print(f)
            
              request <- paste(inputParameters$cmdChoice, "3", inputParameters$channelChoice, f, "0\r\n", sep=" ")
              
              # write.serialConnection(mobiConn, "108 0\r\n") # otwarcie sesji
              # read.serialConnection(mobiConn, "108 0\r\n")
              
              write.serialConnection(mobiConn, request)
              
              Sys.sleep(0.2)
              
              confirmation <- read.serialConnection(mobiConn, n = 0)
              print(confirmation)
              
              if (grepl('016777200', confirmation) == TRUE) {
                print(">>> Measurement confirmed")
                Sys.sleep(5)
                res <- read.serialConnection(mobiConn, n = 0)
                print(res)
                res <- substr(res, 9, 25)
                print(res)
                
                saveData(res)
                datatable(loadData(), rownames = FALSE)
                
                # write.serialConnection(mobiConn, "109 0\r\n") # zamkniecie sesji
                # read.serialConnection(mobiConn, "109 0\r\n")
                
              } else {
                print(">>> Measurement not confirmed")
              }
            }
            responses
          },
          options = list(
            # dom = 't',
            # ordering = FALSE,
            # scroller = list(rowHeight = 100)
            pageLength = 5
          )
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
              re = toPlot$impedance_re
              im = toPlot$impedance_im
              channel = toPlot$channel
              
              ggplot(toPlot,  aes(x=re, y=im, group = channel)) + 
                  geom_point() +
                  geom_line(color = channel)
            })
        })

})


# Creates Shiny app
shinyApp(ui = ui, server = server)
