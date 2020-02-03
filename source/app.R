library(shiny)
library(shinyWidgets)
library(magrittr)
library(shinythemes)

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

                                                   #images that have been uploaded to the server
                                                   #temporarily looking in this directory
                                                   selectInput("imageDirectory", label = div(style="color: white;", "Image:"), list.files(path = "data/Test_imagery_AVIRIS", full.names = FALSE)),
                                                   selectInput("imageHdwDirectory", label = div(style="color: white;", "Image Hdw:"), list.files(path = "data/Test_imagery_HDW", full.names = FALSE)),
                                                   #filenames<-list.files(pattern="\\.csv$")
                                                   
                                                   #label to be used as the filename for the output
                                                   textInput("outputFileName",label = div(style="color: white;", "Output File Name:")),

                                                   #Button to generate a classifier
                                                   actionButton("addToQueueButton", "Add to Queue")
                                          ),
                                        
                                        #Spectral Library Tab
                                        tabPanel("Update/Create Spectral Library", style = "background-color:#383a40;",
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

                            #RANDOM PARAMETERS
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

                      img(src="logo.png", height="10%", width="10%", align="right")
             ),
             #UPLOAD DATA TAB
             tabPanel("Upload Data",
                      fluidPage(

                        br(),
                        #UPLOAD CLASSIFIER
                        sidebarPanel(
                          width = 12,
                          style = "background-color: #383a40;",
                          fileInput(
                          inputId = "files",
                          label = div(style="color: white;", "Drag and Drop or Browse:"),
                          multiple = TRUE,
                          buttonLabel = "Browse",
                          placeholder = "No file selected"
                          ),
                          textInput("uploadClassifierNameInput", label = div(style="color: white;", "Classifier Name:"), value = ""),
                          
                          #Button to upload a classifier
                          actionButton("uploadClassifierButton", "Upload Classifier"),

                        ),
                        #UPLOAD DATA CUBE
                        sidebarPanel(
                          width = 12,
                          style = "background-color: #383a40;",
                          fileInput(
                            inputId = "files",
                            label = div(style="color: white;", "Drag and Drop or Browse:"),
                            multiple = TRUE,
                            buttonLabel = "Browse",
                            placeholder = "No file selected"
                          ),
                          textInput("uploadDataCubeInput", label = div(style="color: white;", "Data Cube Name:"), value = ""),
                          
                          #Button to upload a dataCube
                          actionButton("uploadDataCube", "Upload Data Cube"),
                          
                        ),
                        
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
  queueText <- c() #vector of strings to be displayed in the ui queue
  queue <- list() #vector of string vectors. each element = (data cube file directory, classifier name)
  classifierChoices <- c("Classifier 1", "Classifier 2")
  
  
  #INITIALIZE
  #initialize queue
  output$queue <- renderText({"queue is empty"})
  updateSelectInput(session, "classifierSelect", label = div(style="color: white;", "Classifier:"), classifierChoices)
  
  #add to queue button is pressed
  observeEvent(input$addToQueueButton, {
    #chck if a file has been selected
    if (is.null(input$classifierName)) {
      print("Please enter a Classifier Name")
    } else if (is.null(input$imageDirectory)) {
      print("Please select an AV image")
    } else if (is.null(input$imageHdwDirectory)) {
      print("Please select an AV VI image")
    } else if (is.null(input$outputFileName)) {
      print("Please enter an Output File Name")
    } else {
      #gather process parameters
      ##temporary paths
      libraryDirectory <- paste("output/hdwImagery/", input$librarySelect, sep = "")
      imageDirecotry <- paste("data/Test_imagery_AVIRIS/", input$imageDirectory, sep = "")
      imageHdwDirectory <- paste("data/Test_imagery_HDW", input$imageHdwDirectory, sep = "")
      
      classifier <- c(libraryDirectory, input$mtry, input$ntree, input$importance, input$classifierName)
      images <- c(imageDirecotry, imageHdwDirectory)
      newProcess <- list(classifier, images, input$outputFileName)
      
      #add process to queue
      queue[[length(queue) + 1]] <<- newProcess
      
      #TODO: create a better string to add to queue
      
      queueText <<- c(queueText, newProcess)
    }
    
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
    print("Processing Spectra By Field...")
    
    errors <- processFieldSpec("data/Field_spec/Alaska")
    
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
  
  #Upload classifier
  observeEvent(input$uploadClassifierButton, {
    if (is.null(input$uploadClassifierNameInput)) {
      print("no classifier name entered")
    } else {
      
      #add to the list of classifier names
      classifierChoices <<- c(classifierChoices, input$uploadClassifierNameInput)
      
      #add the new classifier name to the drop down in the select data tab
      updateSelectInput(session, "classifierSelect", label = div(style="color: white;", "Classifier:"), classifierChoices)
      
      print("classifier uploaded")
    }
  })
}

shinyApp(ui = ui, server = server)
