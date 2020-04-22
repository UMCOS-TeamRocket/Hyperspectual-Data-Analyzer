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

closeAllConnections()
rm(list=ls())

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
  
  #SPECTRAL LIBRARY MODULE
  spectralLibraryModuleValues <- callModule(spectralLibraryModuleServer, "spectralLibrary")
  
  #SELECT DATA MODULE
  selectDataModule <- callModule(selectDataServer, "selectData", spectralLibraryModuleValues)
  
  #keep track of output images to be displayed
  imageOutput <- reactiveValues()
  imageOutput$directories <- list()
  imageOutput$statistics <- list()
  
  #QUEUE MODULE
  queueModule <- callModule(queueModuleServer, "queue", selectDataModule, imageOutput)
  
  #IMAGE OUTPUT MODULE
  imageOutputModule <- callModule(imageOutputModuleServer, "imageOutput", imageOutput)
}

flog.info("Application Start", name = "logFile")

shinyApp(ui = ui, server = server)
