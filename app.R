# setwd("C:/Users/Aneta/Projects/RShinyApp")

library(shiny)
library(shinyalert)
library(serial)


# Defines UI for app
ui <- shinyUI(fluidPage(
  
  titlePanel("Mobi Dx C"),

  sidebarLayout(position = "right",
    sidebarPanel(
      
      helpText("Use below buttons to connect or disconnect with the device."),
      
      # creates buttons to connect or disconnect
      useShinyalert(),
      actionButton("conButton", "Connect"),
      actionButton("disconButton", "Disconnect"),

      hr(),
      # creates input for a command and executing button
      textInput("Command", "Enter a command:", " "), 
      actionButton("executeButton", "Execute")
    ),
    
    mainPanel(
      h4("Used command"),
      # shows last used command
      verbatimTextOutput("Command")
    )
  )
))

# Defines server for app
server <- shinyServer(function(input, output, session) {
  
  # creates, opens and checks (dis)connection with the device
  observeEvent(input$conButton, {
    mobiConn <- serialConnection("mobi", port="COM5", mode="115200,n,8,1")
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
  
  # executes the command entered by user
  request <- eventReactive(input$executeButton, {

    write.serialConnection(mobiConn, paste(input$Command, "\r\n", sep=""))
  })

  # displays last command entered by user
  output$Command <- renderText({
    request()
    read.serialConnection(mobiConn, n = 0)
    
    # history <-  vector(mode = "list", length = 10)
    # history <- c(history, read.serialConnection(mobiConn, n = 0))
    # return(history)
    
  })

})


# Creates Shiny app
shinyApp(ui = ui, server = server)