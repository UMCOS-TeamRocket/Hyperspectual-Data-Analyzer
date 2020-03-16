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

queueModuleServer <- function(input, output, session, selectDataModule, imageOutput) {
  queueData <- reactiveValues()
  queueData$processes <- list()
  queueData$text <- ""
  
  #Clear all items from queue and output
  observeEvent(input$clearQueue, {
    queueData$text <- ""
    queueData$processes <- list()
    queueData$outputImageDirectories <- list()
    queueData$outputStatistics <- list()
    
    #log info
    flog.info("Queue Cleared", name = "logFile")
  })
  
  #execute when 'add to queue' is clicked in the select data module
  observeEvent(selectDataModule$addToQueue, {
    processParameters <- selectDataModule$processParameters
    
    #check if a map has already been created with the same name
    if (file.exists(paste("output/plots/", processParameters$outputFileName, ".jpg", sep = ""))) {
      showModal(modalDialog(
        fluidRow(
          h4("A map with that output name has already been created. Please pick another Output File Name.")
        ),
        title = "Warning",
        easyClose = TRUE
      ))

      #TODO: ask if they want to proceed anyway. find a dialog that blocks, or render text and a button at the bottom of the module
      return()
    } else {
      for (process in queueData$processes) {
        if (processParameters$outputFileName == process$outputFileName) {
          showModal(modalDialog(
            fluidRow(
              h4("A map with that output name has already been added to the queue. Please pick another Output File Name.")
            ),
            title = "Warning",
            easyClose = TRUE
          ))

          #TODO: ask if they want to proceed anyway. find a dialog that blocks, or render text and a button at the bottom of the module
          return()
        }
      }
    }
    
    
    #add all relevant parameters as a list to the list of processes
    queueData$processes[[length(queueData$processes) + 1]] <<- processParameters
    
    #build the string that represents each process to be displayed on the ui
    textVector <- c(paste("Process#:", length(queueData$processes)))
    textVector <- c(textVector, paste("Spectral Library:", processParameters$libraryDirectory))
    
    #get classifierParameters from processParameters
    classifierParameters <- processParameters$classifierParameters
    
    #display classifier parameters if a new classifier is being created, else just display the directory to the classifier
    if (classifierParameters$newClassifier == 1) {
      textVector <- c(textVector, paste("Classifier Name:", classifierParameters$classifierName))
      textVector <- c(textVector, paste("mtry:", classifierParameters$mtry))
      textVector <- c(textVector, paste("ntree:", classifierParameters$ntree))
      textVector <- c(textVector, paste("importance:", classifierParameters$importance))
    } else {
      textVector <- c(textVector, paste("Classifier Directory:", classifierParameters$classifierFile))
    }
    
    #add image directory and desired output file
    textVector <- c(textVector, paste("Image:", processParameters$imageDirectory))
    textVector <- c(textVector, paste("Output File Name:", processParameters$outputFileName))
    
    #seperate each parameter with a newline
    outputString <- ""
    for (string in textVector) {
      outputString <- paste(outputString, string, sep = "\n")
    }
    
    #append this string to the existing string of all processes
    queueData$text <<- paste(queueData$text, outputString, sep = "\n")
    
    flog.info(paste("Process Added to Queue:", outputString), name = "logFile")
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
      imageOutput$directories <- list()
      imageOutput$statistics <- list()
      
      startTime <- Sys.time()
      
      print("Processing Queue...")
      
      #CALL TO BACKEND CODE. located here: source/processqueue.R
      #create a map for each process in the queue
      errors <- processQueue(queueData, imageOutput)
      
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
