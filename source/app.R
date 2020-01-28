library(shiny)
library(shinyWidgets)
library(magrittr)

library(here)
setwd(here())

source("source/fieldSpecProcessing/bySite.R")
source("source/generateSpectralLibraryFiles.R")
source("source/processQueue.R")

ui <- 
  fluidPage(
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
        "))
  ),
 
  navbarPage("Hyperspectral Data Analyzer",
             tabPanel("Home",
                      br(),
                      br(),
                          sidebarLayout(

                            sidebarPanel(style = "background-color: #383a40; border-color: #383a40;",
                              #Change the background color of the Select Data/Generate Class tab buttons to black when selected and gray otherwise
                              tags$style(HTML("
                                            .tabbable > .nav > li > a {background-color: #383a40;  color:white}
                                            .tabbable > .nav > li > a[data-value='Select Data'] {border-color: #2b2b2b; background-color: #2b2b2b;  color:white}
                                            
                              ")), #.tabbable > .nav > li[class=active] > a {border-color: #383a40; background-color: #383a40; color:white}
                              tabsetPanel(type = "tabs",
                                          #SELECT DATA TAB
                                          tabPanel("Select Data", style = "background-color: #383a40;",
                                                   #Drop down for Spectral Library selection
                                                   selectInput("librarySelect", label = div(style="color: white;", "Spectral Library:"), list.files(path = "output/hdwImagery", full.names = FALSE)),
                                                   
                                                   #label to be used as the filename for the classifier
                                                   textInput("classifierName", "Classifier File Name:"),

                                                   #images that have been uploaded to the server
                                                   #temporarily looking in this directory
                                                   selectInput("imageDirectory", label = div(style="color: white;", "Image:"), list.files(path = "data/Test_imagery_AVIRIS", full.names = FALSE)),
                                                   selectInput("imageHdwDirectory", label = div(style="color: white;", "Image Hdw:"), list.files(path = "data/Test_imagery_HDW", full.names = FALSE)),
                                                   #filenames<-list.files(pattern="\\.csv$")
                                                   
                                                   #label to be used as the filename for the output
                                                   textInput("outputFileName", "Output File Name:"),

                                                   #Button to generate a classifier
                                                   actionButton("addToQueueButton", "Add to Queue")
                                          ),
                                        
                                        #Spectral Library Tab
                                        tabPanel("Update/Create Spectral Library", style = "background-color:#383a40;",
                                                 actionButton("updateSpectralBySite", "Update"),
                                                 
                                                 br(),
                                                 br(),
                                                 
                                                 multiInput(
                                                   inputId = "spectralList", label = "List of Spectral Objects By Site",
                                                   choices = list.files(path = "output/fieldSpec", full.names = FALSE),
                                                   options = list(
                                                     enable_search = TRUE
                                                   )
                                                 ),
                                                 
                                                 textInput("spectralLibraryName", "Spectral Library Name"),
                                                 
                                                 actionButton("createSpectralLibrary", "Create Spectral Library")
                                        )

                              ),
                              
                              tags$head(tags$style(
                                HTML('
                                                            body, input, button, select {
                                                                font-family: "Calibri";
                                                                background-color: #121212;
                                                            }')
                              )),

                            ),

                            #RANDOM PARAMETERS
                            mainPanel(
                              style = "background-color: #383a40;",

                              #change the color of the min and max values on the slider to white
                              tags$style(HTML(".irs-max {color: white;}
                                              .irs-min {color: white;}")),

                              setSliderColor(c("red", "red"), c(1, 2)),
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
    } else if (is.null(input$imageAvDirectory$name)) {
      print("Please select an AV image")
    } else if (is.null(input$imageHdwDirectory$name)) {
      print("Please select an HDW image")
    } else if (is.null(input$imageAvViDirectory$name)) {
      print("Please select an AV VI image")
    } else if (is.null(input$outputFileName)) {
      print("Please enter an Output File Name")
    } else {
      #gather process parameters
      classifier <- c(input$librarySelect, input$mtry, input$ntree, input$importance, input$classifierName)
      images <- c(input$imageDirectory, input$imageHdwDirectory)
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
      print("Processing Queue...")
      
      processQueue(queue)
      
      print("Finished Processing Queue")
    } else {
      print("queue is empty")
    }
  })
  
  #Update Spectra Objects By Site
  observeEvent(input$updateSpectralBySite, {
    tryCatch({
      print("Processing Spectra By Field...")
      processFieldSpec("data/Field_spec/Alaska")
      print("Finished Processing Spectra By Field")
      
      spectraList <- list.files(path = "output/fieldSpec", full.names = FALSE)
      updateMultiInput(
        session = session,
        inputId = "spectralList",
        selected = c(),
        choices = spectraList
      )
      
    }, warning = function(warning) {
      print(warning)
    }, error = function(error) {
      print(error)
    })
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
    
      print("Generating Spectral Library Files...")
      
      generateSpectralLibraryFiles(listOfSpectraObjects, spectralLibraryName)
      
      print("Generated Spectral Library Files")
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