library(shiny)
library(shinyWidgets)
library(magrittr)
library(shinythemes)
library(shinyFiles)

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
  
  classifierChoices <- c()
  fieldSpecDirectory <- ""
  imageDirectory <- ""
  
  #CLASSIFIER PARAMETERS MODULE
  rfClassifierParameters <- callModule(rfClassifierParametersServer, "rfClassifierParameters")
  
  #SPECTRAL LIBRARY MODULE
  spectralLibraryModule <- callModule(spectralLibraryModuleServer, "spectralLibrary")
  
  #SELECT DATA MODULE
  selectDataModule <- callModule(selectDataServer, "selectData", spectralLibraryModule)
  
  #QUEUE MODULE
  queueData <- reactiveValues()
  queueData$parameters <- list()
  queueData$text <- ""
  queueModule <- callModule(queueModuleServer, "queue", queueData)
  
  #COLLECT PROCESS PARAMETERS WHEN 'ADD TO QUEUE' IS CLICKED
  observeEvent(selectDataModule$addToQueue, {
    queueData$parameters[[length(queueData$parameters) + 1]] <<- list(parameters = selectDataModule$processParameters, 
                                                                      classifierParameters = rfClassifierParameters)
    
    #BUILD OUTPUT STRING
    textVector <- c(paste("Process#:", length(queueData$parameters)))
    textVector <- c(textVector, paste("Spectral Library:", selectDataModule$processParameters$libraryDirectory))
    textVector <- c(textVector, paste("Classifier Name:", selectDataModule$processParameters$classifierName))
    classifierParameters <- paste(c(rfClassifierParameters$mtry(),
                                    rfClassifierParameters$ntree(),
                                    rfClassifierParameters$importance()))
    textVector <- c(textVector, paste("Classifier Parameters:", classifierParameters))
    textVector <- c(textVector, paste("Image:", selectDataModule$processParameters$imageDirectory))
    textVector <- c(textVector, paste("Output File Name:", selectDataModule$processParameters$outputFileName))
    
    outputString <- ""
    for (string in textVector) {
      outputString <- paste(outputString, string, sep = "\n")
    }
    
    queueData$text <<- paste(queueData$text, outputString, sep = "\n")
  })
  
  #IMAGE OUTPUT MODULE
  imageOutputModule <- callModule(imageOutputModuleServer, "imageOutput")
  
  #VIEW DATA MODULE
  viewDataModule <- callModule(viewDataModuleServer, "viewData")
}

shinyApp(ui = ui, server = server)
