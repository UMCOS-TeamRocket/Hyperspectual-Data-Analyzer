#******************************************************************
# Hyperspectual Data Analyzer
# Created by Team Rocket w/ University of Maine Forestry Department
#
#
# Sam Beaudoin
# Isaac Fair
# Aiden Lammert
# Noah Monto
# Matthew Prescott
#
#
#******************************************************************

library(shiny)
library(shinyWidgets)
library(magrittr)
library(shinythemes)
library(shinyFiles)
library(futile.logger)

library(here)
setwd(here())

source("source/createOutputDirectories.R")
source("source/ui/selectDataModule.R")
source("source/ui/queueModule.R")
source("source/ui/spectralLibraryModule.R")
source("source/ui/imageOutputModule.R")

createOutputDirectories()

#get current date time and replace ":" with "-" so it can be used as a file name
currentDateTime <- str_replace_all(Sys.time(), ":", "-")
#replace the space between the date and tiem with "T"
currentDateTime <- str_replace(currentDateTime, " ", "T")
#add file extension and directory
currentDateTime <- paste("output/logs/", currentDateTime, ".log", sep = "")

#Set up logger
flog.logger("logFile")
flog.appender(appender.file(currentDateTime), "logFile")
#use flog.info, flog.warn, flog.error, flog.debug to write to log file

ui <- fluidPage( theme =shinytheme("slate"),
                 tags$head(tags$style(HTML('body, input, button, select {font-family: "Calibri"; background-color: #121212;}'))),
                           
                 titlePanel("Hyperspectral Data Analyzer"),
                 
                 br(), 
                 br(),
                 
                 fluidRow(style = "background-color: #383a40;",
                          br(),
                          column(5, spectralLibraryModuleUI("spectralLibrary"), style = "background-color: #383a40;"),
                          column(5, selectDataUI("selectData"), style = "background-color: #383a40;"),
                          br()
                          ),
                 
                 br(), 
                 br(),
                 
                 #QUEUE
                 fluidRow(style = "background-color: #383a40;", queueModuleUI("queue")),
                 
                 br(),
                 
                 #Output
                 fluidRow(tagList(br(), imageOutputModuleUI("imageOutput"), br()), style = "background-color: #383a40;"),
                 
                 img(src="logo.png", height="10%", width="10%", align="right")
             )

server <- function(input, output, session) {
  #INITIALIZE
  #VARIABLES
  root <- c(home = fs::path_home(), project = here())
  
  # #CLASSIFIER PARAMETERS MODULE
  # rfClassifierParameters <- callModule(rfClassifierParametersServer, "rfClassifierParameters")
  
  #SPECTRAL LIBRARY MODULE
  spectralLibraryModuleValues <- callModule(spectralLibraryModuleServer, "spectralLibrary")
  
  #SELECT DATA MODULE
  selectDataModule <- callModule(selectDataServer, "selectData", spectralLibraryModuleValues)
  
  #variable to 
  queueData <- reactiveValues()
  queueData$processes <- list()
  queueData$text <- ""
  queueData$outputImageDirectories <- list()
  queueData$outputStatistics <- list()
  
  #QUEUE MODULE
  queueModule <- callModule(queueModuleServer, "queue", queueData)
  
  #execute when 'add to queue' is clicked in the select data module
  observeEvent(selectDataModule$addToQueue, {
    processParameters <- selectDataModule$processParameters
    
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
  
  #IMAGE OUTPUT MODULE
  imageOutputModule <- callModule(imageOutputModuleServer, "imageOutput", queueData)
}

flog.info("Application Start", name = "logFile")

shinyApp(ui = ui, server = server)
