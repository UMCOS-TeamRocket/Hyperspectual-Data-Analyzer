library(magrittr)
source("source/processQueue.R")

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
    queueData$parameters <- list()
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
    if (length(queueData$parameters) > 0) {
      tryCatch({
        #clear output section
        queueData$outputImageDirectories <- list()
        queueData$outputStatistics <- list()
        
        #TODO: add to output queue data as it is processed
        #image output placeholder
        queueData$outputImageDirectories[[1]] <- normalizePath(file.path("./output/plots", "testImage.jpg"))
        queueData$outputStatistics[[1]] <- "*some revolutionary data*"
        
        print("Processing Queue...")
        
        processQueue(queueData$parameters)
        
        print("Finished Processing Queue")
      }, warning = function(warning) {
        showModal(modalDialog(
          fluidRow(
            h4(HTML(paste0(warning, collapse = "")))
          ),
          title = "Warning",
          easyClose = TRUE
        ))
      }, error = function(error) {
        showModal(modalDialog(
          fluidRow(
            h4(HTML(paste0(error, collapse = "")))
          ),
          title = "Error",
          easyClose = TRUE
        ))
      })
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