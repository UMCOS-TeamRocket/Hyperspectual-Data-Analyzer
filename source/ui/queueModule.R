library(magrittr)
source("source/processqueue.R")

queueModuleUI <- function(id) {
  #namespace for the module
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(2, p("Queue:", style = "color: white; size: 20pt; padding-left: 10px;")),
      column(1, actionButton(ns("runQueue"), "Run")),
      column(1, actionButton(ns("clearQueue"), "clear"))
    ),
    br(),
    
    HTML(paste0("#", ns("queue"), " {
                background-color: #383a40; 
                border-color: #383a40; 
                color: white; 
                font-size: 15px; 
                padding-left: 10px;}
                ")) %>% 
    tags$style() %>%
    tags$head(),
    
    verbatimTextOutput(ns("queue")),
  
    br(),
  )
}

queueModuleServer <- function(input, output, session, queueData) {
  #Clear all items from queue and output
  observeEvent(input$clearQueue, {
    queueData$text <- ""
    queueData$processes <- list()
    queueData$outputImageDirectories <- list()
    queueData$outputStatistics <- list()
    
    #log info
    flog.info("Queue Cleared", name = "logFile")
  })
  
  #Display what processes are in the queue
  output$queue <- renderText({
    if (queueData$text == "") {
      "queue is empty"
    } else {
      queueData$text
    }
  })
  
  #Run all processes in queue
  observeEvent(input$runQueue, {
    #log info
    flog.info(paste("Run Queue of length", length(queueData$processes)), name = "logFile")
    
    if (length(queueData$processes) > 0) {
      #clear output variables
      queueData$outputImageDirectories <- list()
      queueData$outputStatistics <- list()
      
      startTime <- Sys.time()
      
      print("Processing Queue...")
      
      #CALL TO BACKEND CODE. located here: source/processqueue.R
      #create a map for each process in the queue
      errors <- processQueue(queueData)
      
      endTime <- difftime(Sys.time(), startTime, units = "mins")
      print("Finished Processing Queue")
      print(endTime)
      
      flog.info("Finished Processing Queue", name = "logFile")
      flog.info(paste("Queue finished in", endTime, "minutes"), name = "logFile")
      
      #check if any errors occured
      if (length(errors) != 0) {
        #separate each error with a couple newlines
        outputString <- ""
        for(i in 1:length(errors)) {
          outputString <- paste(outputString, errors[[i]], "<br><br>")
        }
        
        #display an error dialog with all error messages
        showModal(modalDialog(
          fluidRow(
            h4(HTML(outputString))
          ),
          title = "Some Processes Ran Into An Error",
          easyClose = TRUE
        ))
      }
    } else {
      #display an error dialog if the queue is empty
      showModal(modalDialog(
        fluidRow(
          h4("Queue is empty")
        ),
        title = "Failed to run Queue:",
        easyClose = TRUE
      ))
    }
  })
}
