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
                      br(),
                      br(),
                          sidebarLayout(

                            sidebarPanel(style = "background-color: #383a40; border-color: #383a40;",
                              tabsetPanel(type = "tabs",
                                        #SELECT DATA TAB
                                        tabPanel("Select Data", style = "background-color: #383a40;", selectDataUI("selectData")),
                                        
                                        #Spectral Library Tab
                                        tabPanel("Update/Create Spectral Library", style = "background-color:#383a40;", spectralLibraryModuleUI("spectralLibrary"))),
                              
                              tags$head(tags$style(HTML('body, input, button, select {
                                                                font-family: "Calibri";
                                                                background-color: #121212;}')
                              )),

                            ),

                            #CLASSIFIER PARAMETERS
                            mainPanel(style = "background-color: #383a40;", br(), rfClassifierParametersUI("rfClassifierParameters")
                            )

                      ),
                      br(),
                      
                      br(),

                      #QUEUE
                      fluidRow(style = "background-color: #383a40;", queueModuleUI("queue")),
                      
                      br(),
                      
                      #Output
                      fluidRow(
                        style = "background-color: #383a40;",
                        br(),
                        fluidRow(
                          column(2, p("Output:", style = "color: white; size: 20pt; padding-left: 10px;")),
                          
                        ),
                        br(),
                        
                        
                        img(src="CopyOfLight lichen2.jpg", height="100%", width="100%", align="center"),
                        
                        imageOutput("Output"),
                        tags$head(tags$style(HTML("#output {background-color: #383a40; border-color: #383a40; color: white; font-size: 15px; padding-left: 10px;}"))),
                        br(),
                      ),

                      img(src="logo.png", height="10%", width="10%", align="right")
             ),
             #VIEW DATA TAB
             tabPanel("View Data",
                      sidebarPanel(style = "background-color: #383a40; border-color: #383a40;",
                                   #Change the background color of the Select Data/Generate Class tab buttons to black when selected and gray otherwise
                                   tags$style(HTML("
                                            .tabbable > .nav > li > a {background-color: #383a40;  color:white}
                                            .tabbable > .nav > li > a[data-value='Classifiers'] {border-color: #2b2b2b; background-color: #2b2b2b;  color:white}
                                            .tabbable > .nav > li > a[data-value='Output'] {border-color: #2b2b2b; background-color: #2b2b2b;   color:white}
                                            .tabbable > .nav > li > a[data-value='Config'] {border-color: #2b2b2b; background-color: #2b2b2b;   color:white}
                                            .tabbable > .nav > li[class=active] > a {border-color: #383a40; background-color: #383a40; color:white}
                              ")),
                                   tabsetPanel(type = "tabs",
                                               #CLASSIFIER DATA TAB
                                               tabPanel("Classifiers", style = "background-color: #383a40;",
                                                  
                                               ),

                                               #OUTPUT TAB
                                               tabPanel("Output", style = "background-color:#383a40;",
                                               ),
                                               #CONFIGURATION TAB  
                                               tabPanel("Config", style = "background-color: #383a40;",
                                                        
                                               )


                                   ),
                      ),


             )
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
    queueData$parameters[[length(queueData$parameters) + 1]] <<- list(selectDataModule$processParameters, rfClassifierParameters)
    
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
  
}

shinyApp(ui = ui, server = server)
