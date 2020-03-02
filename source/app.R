#******************************************************************
# Hyperspectual Data Analyzer
# Created by Team Rocket w/ University of Maine Forestry Department
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
source("source/ui/rfClassifierParametersModule.R")
source("source/ui/queueModule.R")
source("source/ui/spectralLibraryModule.R")
source("source/ui/imageOutputModule.R")
source("source/ui/viewDataModule.R")

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

ui <- 
  fluidPage( theme =shinytheme("slate"),
    HTML(".navbar .navbar-nav {float: right}
          .navbar .navbar-header {float: left;}
          .navbar-inner { background-color: #23262b }
          .navbar-static-top 
          {
            position: static;
            margin-bottom: 0px;
            background-color: #23262b
          }
          a{color:#D2403A}") %>%
    tags$style() %>%
    tags$head(),
 
    navbarPage("Hyperspectral Data Analyzer",
             tabPanel("Home",
                      br(), br(),
                      
                      tagList(
                        tabsetPanel(type = "tabs",
                                    #SELECT DATA TAB
                                    tabPanel("Select Data", style = "background-color: #383a40;", selectDataUI("selectData")),
                                    
                                    #SPECTRAL LIBRARY TAB
                                    tabPanel("Update/Create Spectral Library", style = "background-color:#383a40;", spectralLibraryModuleUI("spectralLibrary"))),
                        
                        tags$head(tags$style(HTML('body, input, button, select {
                                                                font-family: "Calibri";
                                                                background-color: #121212;}')))
                      ) %>%
                      sidebarPanel(style = "background-color: #383a40; border-color: #383a40;") %>%
                      sidebarLayout(
                        #CLASSIFIER PARAMETERS
                        mainPanel(style = "background-color: #383a40;", br(), rfClassifierParametersUI("rfClassifierParameters"))

                      ),
                      
                      br(), br(),

                      #QUEUE
                      fluidRow(style = "background-color: #383a40;", queueModuleUI("queue")),
                      
                      br(),
                      
                      #Output
                      fluidRow(tagList(br(), imageOutputModuleUI("imageOutput"), br()), style = "background-color: #383a40;"),

                      img(src="logo.png", height="10%", width="10%", align="right")
             ),
             #VIEW DATA TAB
             viewDataModuleUI("viewData") %>%
               sidebarPanel(style = "background-color: #383a40; border-color: #383a40;") %>%
               tabPanel(title = "View Data")
    )
)



server <- function(input, output, session) {
  #INITIALIZE
  #VARIABLES
  root <- c(home = fs::path_home(), project = here())
  
  #CLASSIFIER PARAMETERS MODULE
  rfClassifierParameters <- callModule(rfClassifierParametersServer, "rfClassifierParameters")
  
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
    #add all relevant parameters as a list to the list of processes
    queueData$processes[[length(queueData$processes) + 1]] <<- list(parameters = selectDataModule$processParameters, 
                                                                      classifierParameters = rfClassifierParameters)
    #build the string that represents each process to be displayed on the ui
    textVector <- c(paste("Process#:", length(queueData$processes)))
    textVector <- c(textVector, paste("Spectral Library:", selectDataModule$processParameters$libraryDirectory))
    
    if (selectDataModule$processParameters$newClassifier == 1) {
      textVector <- c(textVector, paste("Classifier Name:", selectDataModule$processParameters$classifierName))
    } else {
      textVector <- c(textVector, paste("Classifier Directory:", selectDataModule$processParameters$classifierFile))
    }
    
    classifierParameters <- paste(c(rfClassifierParameters$mtry(),
                                    rfClassifierParameters$ntree(),
                                    rfClassifierParameters$importance()))
    textVector <- c(textVector, paste("Classifier Parameters:", classifierParameters))
    textVector <- c(textVector, paste("Image:", selectDataModule$processParameters$imageDirectory))
    textVector <- c(textVector, paste("Output File Name:", selectDataModule$processParameters$outputFileName))
    
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
  
  #VIEW DATA MODULE
  viewDataModule <- callModule(viewDataModuleServer, "viewData")
}

flog.info("Application Start", name = "logFile")

shinyApp(ui = ui, server = server)
