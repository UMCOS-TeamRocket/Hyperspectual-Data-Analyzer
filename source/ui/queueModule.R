library(magrittr)
source("source/processqueue.R")

queueModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(2, p("Queue:", style = "color: white; size: 20pt; padding-left: 10px;")),
      column(1, actionButton(ns("runQueue"), "Run")),
      column(1, actionButton(ns("clearQueue"), "clear"))
    ),
    br(),
    
    HTML(paste0("#", ns("queue"), " {background-color: #383a40; border-color: #383a40; color: white; font-size: 15px; padding-left: 10px;}")) %>% 
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
    if (length(queueData$processes) > 0) {
      #clear output section
      queueData$outputImageDirectories <- list()
      queueData$outputStatistics <- list()
      
      print("Processing Queue...")
      
      errors <- processQueue(queueData)
      
      print("Finished Processing Queue")
      
      if (length(errors) != 0) {
        outputString <- errors[[1]]
        for(i in 2:length(errors)) {
          outputString <- paste(outputString, errors[[i]], sep = "<br><br>")
        }
        
        showModal(modalDialog(
          fluidRow(
            h4(HTML(outputString))
          ),
          title = "Some Processes Ran Into An Error",
          easyClose = TRUE
        ))
      }
    } else {
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
