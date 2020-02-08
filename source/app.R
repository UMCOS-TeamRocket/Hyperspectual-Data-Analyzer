library(shiny)
library(shinyWidgets)
library(magrittr)
library(shinythemes)
library(shinyFiles)

library(here)
setwd(here())

source("source/fieldSpecProcessing/bySite.R")
source("source/generateSpectralLibraryFiles.R")
source("source/processQueue.R")
source("source/createOutputDirectories.R")
source("source/ui/selectDataModule.R")
source("source/ui/rfClassifierParametersModule.R")
source("source/ui/queueModule.R")

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
                                        tabPanel("Update/Create Spectral Library", style = "background-color:#383a40;",
                                                 fluidRow(
                                                   column(4, shinyDirButton("fieldSpecDirInput", "Browse", "Select Directory With Field Spec Data")),
                                                   column(8, verbatimTextOutput("fieldSpecDirOutput", placeholder = TRUE))
                                                 ),
                                                 
                                                 #TODO: change label from update to something else?
                                                 actionButton("updateSpectralBySite", "Update"),
                                                 
                                                 br(),
                                                 br(),
                                                 
                                                 multiInput(
                                                   inputId = "spectralList", label =span("List of Spectral Objects By Site", style="color:white"),
                                                   choices = list.files(path = "output/fieldSpec", full.names = FALSE),
                                                   options = list(
                                                     enable_search = TRUE
                                                   )
                                                 ),
                                                 
                                                 textInput("spectralLibraryName",label = div(style="color: white;", "Spectral Library Name")),
                                                 
                                                 actionButton("createSpectralLibrary", "Create Spectral Library")
                                        )

                              ),
                              
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
  
  #SELECT DATA MODULE
  selectDataModule <- callModule(selectDataServer, "selectData")
  
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
  
  #Update Spectra Objects By Site
  observeEvent(input$updateSpectralBySite, {
    if (fieldSpecDirectory == "") {
      showModal(modalDialog(
        fluidRow(
          h3("Please select a directory")
        ),
        title = "Missing Information",
        easyClose = TRUE
      ))
      return()
    }
    
    print("Processing Spectra By Field...")
    
    errors <- processFieldSpec(fieldSpecDirectory)
    
    print("Finished Processing Spectra By Field")
    
    spectraList <- list.files(path = "output/fieldSpec", full.names = FALSE)
    updateMultiInput(
      session = session,
      inputId = "spectralList",
      selected = c(),
      choices = spectraList
    )
    
    if (length(errors) > 0) {
      showModal(modalDialog(
        fluidRow(
          h3(paste(length(errors), "Error(s) Occured While Processing Spectra By Field:")),
          h4(paste(errors, collapse = " "))
        ),
        title = "Error",
        easyClose = TRUE
      ))
    }
  })
  
  #CREATE SPECTRAL LIBRARY
  observeEvent(input$createSpectralLibrary, {
    spectralLibraryName <- input$spectralLibraryName
    
    if (spectralLibraryName == "") {
      showModal(modalDialog(
        fluidRow(
          h3("Please enter a name for the Spectral Library")
        ),
        title = "Missing Information",
        easyClose = TRUE
      ))
      
      return()
    } else if (is.null(input$spectralList)) {
      showModal(modalDialog(
        fluidRow(
          h3("Please select at least 1 spectral object")
        ),
        title = "Missing Information",
        easyClose = TRUE
      ))
      
      return()
    }
    
    
    listOfSpectraObjects <- c()
    index <- 1
    
    for(fileName in input$spectralList) {
      listOfSpectraObjects[index] <- paste("output/fieldSpec", fileName, sep = "/")
      index <- index + 1
    }
    
    tryCatch({
      print("Generating Spectral Library Files...")
      
      generateSpectralLibraryFiles(listOfSpectraObjects, spectralLibraryName)
      
      print("Generated Spectral Library Files")
    }, warning = function(warning) {
      showModal(modalDialog(
        fluidRow(
          h4(paste0(warning))
        ),
        title = "Warning",
        easyClose = TRUE
      ))
    }, error = function(error) {
      showModal(modalDialog(
        fluidRow(
          h4(paste0(error))
        ),
        title = "Error",
        easyClose = TRUE
      ))
    })
    
    spectralLibraryFiles <- list.files(path = "output/hdwSpectralLibraries", full.names = FALSE)
    allLibraryFiles <- c(spectralLibraryFiles)
    #updateSelectInput(session, inputId = "librarySelect", label = div(style="color: white;", "Spectral Library:"), allLibraryFiles)
  })
  
  #SELECT FIELD SPEC DIRECTORY
  observe({
    shinyDirChoose(
      input,
      'fieldSpecDirInput',
      roots = root,
      session = session
    )
    
    output$fieldSpecDirOutput <- renderPrint({
      if (is.integer(input$fieldSpecDirInput)) {
        fieldSpecDirectory <<- ""
        cat("No directory has been selected")
      } else {
        fieldSpecDirectory <<- parseDirPath(root, input$fieldSpecDirInput)
        parseDirPath(root, input$fieldSpecDirInput)
      }
    })
  })
  
}

shinyApp(ui = ui, server = server)
