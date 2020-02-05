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

createOutputDirectories()

ui <- 
  fluidPage( theme =shinytheme("slate"),
  tags$head(
   tags$style(HTML("
          .navbar .navbar-nav {float: right}
          .navbar .navbar-header {float: left;}
          .navbar-inner { background-color: #23262b }
          .navbar-static-top 
          {
            position: static;
            margin-bottom: 0px;
            background-color: #23262b
          }
          a{color:#D2403A}
        "))
  ),
 
  navbarPage("Hyperspectral Data Analyzer",
             tabPanel("Home",
                      br(),
                      br(),
                          sidebarLayout(

                            sidebarPanel(style = "background-color: #383a40; border-color: #383a40;",
                              
                              tags$style(HTML(".tabbable > .nav > li > a {background-color: #383a40;  color:white}")),
                              
                              tabsetPanel(type = "tabs",
                                          #SELECT DATA TAB
                                          tabPanel("Select Data", style = "background-color: #383a40;",
                                                   #Drop down for Spectral Library selection
                                                   selectInput("librarySelect", label = div(style="color: white;", "Spectral Library:"), 
                                                               c(list.files(path = "output/hdwSpectralLibraries", full.names = FALSE), 
                                                                 list.files(path = "output/spectralLibraries", full.names = FALSE))),
                                                   
                                                   #label to be used as the filename for the classifier
                                                   textInput("classifierName",label = div(style="color: white;", "Classifier File Name:")),
                                                   
                                                   #TODO: limit file types
                                                   fluidRow(
                                                     column(4, shinyFilesButton("imageHdwInput", "Browse", title = "Select HDW Image", multiple = FALSE)),
                                                     column(8, verbatimTextOutput("imageHdwOutput", placeholder = TRUE))
                                                   ),
                                                   
                                                   fluidRow(
                                                     column(4, shinyFilesButton("imageInput", "Browse", title = "Select Image", multiple = FALSE)),
                                                     column(8, verbatimTextOutput("imageOutput", placeholder = TRUE))
                                                   ),
                                                   
                                                   #label to be used as the filename for the output
                                                   textInput("outputFileName",label = div(style="color: white;", "Output File Name:")),

                                                   #Button to generate a classifier
                                                   actionButton("addToQueueButton", "Add to Queue")
                                          ),
                                        
                                        #Spectral Library Tab
                                        tabPanel("Update/Create Spectral Library", style = "background-color:#383a40;",
                                                 fluidRow(
                                                   column(4, shinyDirButton("fieldSpecDirInput", "Browse", "Select Directory With Field Spec Data")),
                                                   column(8, verbatimTextOutput("fieldSpecDirOutput", placeholder = TRUE))
                                                 ),
                                                 
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
                            mainPanel(
                              br(),
                              
                              style = "background-color: #383a40;",

                              #change the color of the min and max values on the slider to white
                              tags$style(HTML(".irs-max {color: white;}
                                              .irs-min {color: white;}")),

                              setSliderColor(c("#D2403A", "#D2403A"), c(1, 2)),
                              sliderInput("mtry", label = div(style="color: white;", "Number Of Sampled Variables:"),
                                          min = 0, max = 10,
                                          value = 3,
                                          ),
                              sliderInput("ntree", label = div(style="color: white;", "Number of trees to grow:"),
                                          min = 0, max = 1000,
                                          value = 500),
                              checkboxInput("importance", label = div(style="color: white;", "Importance"), value = TRUE)
                            )

                      ),
                      br(),
                      br(),

                      #QUEUE
                      fluidRow(
                        style = "background-color: #383a40;",
                        br(),
                        fluidRow(
                          column(2, p("Queue:", style = "color: white; size: 20pt; padding-left: 10px;")),
                          column(1, actionButton("runQueue", "Run")),
                          column(1, actionButton("clearQueue", "clear"))
                        ),
                        br(),

                        verbatimTextOutput("queue"),
                        tags$head(tags$style(HTML("#queue {background-color: #383a40; border-color: #383a40; color: white; font-size: 15px; padding-left: 10px;}"))),
                        br(),
                      ),
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
  #VARIABLES
  dataRoot <- c(home = fs::path_home(), data = paste(here(), "data", sep = "/"))
  outputRoot <- c(home = fs::path_home(), output = paste(here(), "output", sep = "/"))
  root <- c(home = fs::path_home(), project = here())
  
  queueText <- c() #vector of strings to be displayed in the ui queue
  queue <- list() #vector of string vectors. each element = (data cube file directory, classifier name)
  classifierChoices <- c()
  fieldSpecDirectory <- ""
  imageHDWDirectory <- ""
  imageDirectory <- ""
  
  
  
  #INITIALIZE
  #initialize queue
  output$queue <- renderText({"queue is empty"})
  updateSelectInput(session, "classifierSelect", label = div(style="color: white;", "Classifier:"), classifierChoices)
  
  #add to queue button is pressed
  observeEvent(input$addToQueueButton, {
    displayMessage <- FALSE
    message <- ""
    #check if a file has been selected
    if (input$classifierName == "") {
      message <- "Please enter a Classifier Name"
      displayMessage <- TRUE
    } else if (imageHDWDirectory == "") {
      message <- "Please select an HDW image"
      displayMessage <- TRUE
    } else if (imageDirectory == "") {
      message <- "Please select an image"
      displayMessage <- TRUE
    } else if (input$outputFileName == "") {
      message <- "Please enter an Output File Name"
      displayMessage <- TRUE
    }
    
    if (displayMessage) {
      showModal(modalDialog(
        fluidRow(
          h4(message)
        ),
        title = "Missing Information",
        easyClose = TRUE
      ))
      
      return()
    }
    
    #gather process parameters
    ##temporary paths
    
    # The HDW naming is now wrong, didn't change cause would break everything
    libraryDirectory <- paste("output/hdwSpectralLibraries/", input$librarySelect, sep = "")   
    
    classifier <- c(libraryDirectory, input$mtry, input$ntree, input$importance, input$classifierName)
    images <- c(imageHDWDirectory, imageDirectory)
    newProcess <- list(classifier, images, input$outputFileName)
    
    #add process to queue
    queue[[length(queue) + 1]] <<- newProcess
    
    #TODO: create a better string to add to queue
    queueText <<- c(queueText, newProcess)
    
    if (length(queueText) == 0) {
      output$queue <- renderText({"queue is empty"})
    } else {
      #create one string seperated by new lines
      string <- queueText[1]
      
      if (length(queueText) > 1) {
        for (i in 2:length(queueText)) {
          string <- paste(string, queueText[i], sep = "\n")
        }
      }
      
      output$queue <- renderText({string})
    }
  })
  
  #Clear all items from queue
  observeEvent(input$clearQueue, {
    queueText <<- c()
    queue <<- c()
    
    output$queue <- renderText({"queue is empty"})
  })
  
  #Run all processes in queue
  observeEvent(input$runQueue, {
    if (length(queue) > 0) {
      tryCatch({
        print("Processing Queue...")
        
        processQueue(queue)
     
        
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
      print("queue is empty")
    }
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
  
  observeEvent(input$createSpectralLibrary, {
    spectralLibraryName <- input$spectralLibraryName
    
    if (is.null(spectralLibraryName)) {
      print("no name entered")
    } else {
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
      
      hdwSpectralLibraryFiles <- list.files(path = "output/hdwSpectralLibraries", full.names = FALSE)
      spectralLibraryFiles <- list.files(path = "output/spectralLibraries", full.names = FALSE)
      allLibraryFiles <- c(hdwSpectralLibraryFiles, spectralLibraryFiles)
      updateSelectInput(session, inputId = "librarySelect", label = div(style="color: white;", "Spectral Library:"), allLibraryFiles)
    }
  })
  
  #SELECT FIELD SPEC DIRECTORY
  observe({
    shinyDirChoose(
      input,
      'fieldSpecDirInput',
      roots = dataRoot,
      session = session
    )
    
    output$fieldSpecDirOutput <- renderPrint({
      if (is.integer(input$fieldSpecDirInput)) {
        fieldSpecDirectory <<- ""
        cat("No directory has been selected")
      } else {
        fieldSpecDirectory <<- parseDirPath(dataRoot, input$fieldSpecDirInput)
        parseDirPath(dataRoot, input$fieldSpecDirInput)
      }
    })
  })
  
  #SELECT HDW IMAGE FILE
  observe({
    shinyFileChoose(
      input,
      'imageHdwInput',
      roots = dataRoot,
      session = session
    )
    
    output$imageHdwOutput <- renderPrint({
      if (is.integer(input$imageHdwInput)) {
        imageHDWDirectory <<- ""
        cat("No file has been selected")
      } else {
        imageHDWDirectory <<- parseFilePaths(dataRoot, input$imageHdwInput)[[1,4]][1]
        parseFilePaths(dataRoot, input$imageHdwInput)[[1,4]][1]
      }
    })
  })
  
  #SELECT IMAGE FILE
  observe({
    shinyFileChoose(
      input,
      'imageInput',
      roots = dataRoot,
      session = session
    )
    
    output$imageOutput <- renderPrint({
      if (is.integer(input$imageInput)) {
        imageDirectory <<- ""
        cat("No directory has been selected")
      } else {
        imageDirectory <<- parseFilePaths(dataRoot, input$imageInput)[[1,4]][1]
        parseFilePaths(dataRoot, input$imageInput)[[1,4]][1]
      }
    })
  })
  
  
}

shinyApp(ui = ui, server = server)
