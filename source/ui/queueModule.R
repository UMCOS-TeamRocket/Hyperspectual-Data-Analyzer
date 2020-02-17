library(magrittr)
library(ipc)
library(future)
library(promises)
plan(multiprocess)
source("source/processqueue.R")

queueModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(2, p("Queue:", style = "color: white; size: 20pt; padding-left: 10px;")),
      column(1, actionButton(ns("runQueue"), "Run")),
      column(1, actionButton(ns("clearQueue"), "clear")),
      column(1, actionButton(ns("cancelQueue"),"cancel"))
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
  
  interruptor <- AsyncInterruptor$new()
  result_val <- reactiveVal()
  running <- reactiveVal(FALSE)
  queue <- shinyQueue()
  queue$consumer$start(100)
  message <- reactiveVal(character(0))
  
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
        
        if(running())
          return(NULL)
        running(TRUE)
        
        progress <- AsyncProgress$new(message="Processing Queue")
        
        message("HI")
        
        process <- queueData$processes[[1]]
        parameters <- process$parameters
              classifierParameters <- process$classifierParameters

              spectralLibraryDirectory <- parameters$libraryDirectory

              createNewClassifier <- parameters$newClassifier
              classifierDirectory <- parameters$classifierFile
              classifierName <- parameters$classifierName

               mtry <- classifierParameters$mtry()
               ntree <- classifierParameters$ntree()
               importance <- classifierParameters$importance()

              imageDirectory <- parameters$imageDirectory

              outputFileName <- parameters$outputFileName

              hdwDirectory <- parameters$libraryDirectory

        fut <- future({
          queue$producer$fireAssignReactive("message","HOLA")
          processQueue(classifierName, spectralLibraryDirectory, mtry, ntree, importance, imageDirectory, outputFileName)
        })#%...>% rbind(queueData)
        
        fut <- catch(fut, 
                     function(error) {
                       # showModal(modalDialog(
                       #   fluidRow(
                       #     h4(HTML(paste0(error, collapse = "")))
                       #   ),
                         # title = "Error",
                         # easyClose = TRUE
                       #))
                       print(error)
                     })
        fut <- finally(fut, function(){
          progress$close()
          running(FALSE)
          print("Finished Processing Queue")
        })
        NULL
        
    }
     else {
      showModal(modalDialog(
        fluidRow(
          h4("Queue is empty")
        ),
        title = "Failed to run Queue:",
        easyClose = TRUE
      ))
    }
  })
  observeEvent(input$cancelQueue, {
   if(running())
     interruptor$interrupt("User Interrupt")
  })
  observe({
    print(message())
  })
  
}
