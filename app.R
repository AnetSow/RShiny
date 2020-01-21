# setwd("C:/Users/Aneta/Projects/RShinyApp")

library(shiny)
library(shinyalert)
library(serial)
library(DT)


# Defines UI for app
ui <- shinyUI(fluidPage(
  
  titlePanel("Mobi C bis"),

  sidebarLayout(position = "right",
    sidebarPanel(
      
      helpText("Use buttons below in order to connect or disconnect with the device."),
      
      # creates buttons to connect or disconnect
      useShinyalert(),
      actionButton("conButton", label="Connect"),
      actionButton("disconButton", label="Disconnect"),

      # hr(),
      # # creates input for a command and executing button
      # textInput("Command", "Enter a command:", " "), 
      # actionButton("executeButton", "Execute"),
      
      hr(),
      # creates checkbox for channel choice
      checkboxGroupInput("channelChoice", 
                         label = h5("Select channels to measure"), 
                         choices = list("Channel 1" = 1, "Channel 2" = 2, 
                                        "Channel 3" = 3, "Channel 4" = 4, 
                                        "Channel 5" = 5, "Channel 6" = 6,
                                        "Channel 7" = 7, "Channel 8" = 8),
                         selected = 1),
      
      hr(),
      # creates input for number of cycles to run (for each channel)
      textInput("cycleNumber", "Number of measuring cycles", ""),
      
      hr(),
      # creates input for measuring command choice
      selectInput("cmdChoice", h5("Choose measuring command"), 
                  choices = list("107" = 107, "110" = 110,
                                 "114" = 114), selected = 107),
      
      # creates executing button
      actionButton("startButton", label="Start")

    ),
    
    mainPanel(
      # h4("Used command"),
      # # shows last used command
      # verbatimTextOutput("Command"),
      
      # shows used parameters
      # h4("Used parameters"), textOutput("paramsOut"),
      # h4("Output as text"), textOutput("mobiOut"),
      h4("Output as data table"), dataTableOutput("dataTableOut")
    )
  )
))

# Defines server for app
server <- shinyServer(function(input, output, session) {
  
  # creates, opens and checks (dis)connection with the device
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

  # executes the command with parameters entered by user - as a text output
  request <- eventReactive(input$startButton, {
    perm.vector <- paste(input$channelChoice,input$cycleNumber,input$cmdChoice, sep = " ")
    print(perm.vector)
    write.serialConnection(mobiConn, paste(perm.vector, "\r\n", sep=""))
    # data manipulaton
    Sys.sleep(0.2)
    res <- read.serialConnection(mobiConn, n = 0)
    res2 <- unlist(strsplit(res, " "))
    m <- matrix(res2, ncol=length(res2))
    df <- as.data.frame(m)
    })

  # displays command 
  output$mobiOut <- renderText({
    request()
    Sys.sleep(0.2)
    read.serialConnection(mobiConn, n = 0)
  })
  

  # calls as a data table output
  output$dataTableOut <- DT::renderDataTable({
    request()}, options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
    pageLength = 15)
  )
  
})


# Creates Shiny app
shinyApp(ui = ui, server = server)