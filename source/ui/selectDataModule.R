selectDataUI <- function(id) {
  #namespace for the module
  ns <- NS(id)
  
  tagList(
    #Drop down for Spectral Library selection
    selectInput(ns("librarySelect"), label = div(style="color: white;", "Spectral Library:"), 
                c(list.files(path = "output/outputSpectralLibraries", full.names = FALSE))),
    
    radioButtons(inputId = ns("classifierRadioButtons"),
                 label = div(style="color: white;", "Classifier:"),
                 inline = FALSE, 
                 width = NULL, 
                 choiceNames = list("Use Previously Generated Classifier", "Create New Classifier"),
                 choiceValues = list(0, 1)),
    
    uiOutput(ns("classifierSection")),
    
    #TODO: limit file types
    fluidRow(
      column(4, shinyFilesButton(ns("imageInput"), "Browse", title = "Select Image", multiple = FALSE)),
      column(8, verbatimTextOutput(ns("imageOutput"), placeholder = TRUE))
    ),
    
    #label to be used as the filename for the output
    textInput(ns("outputFileName"),label = div(style="color: white;", "Output File Name:")),
    
    #Button to generate a classifier
    actionButton(ns("addToQueueButton"), "Add to Queue")
  )
}



selectDataServer <- function(input, output, session, spectralLibraryModuleValues) {
  #root directory to be used by the file dialog when selecting a data cube
  root <- c(home = fs::path_home(), project = here())
  
  #update the spectral library dropdown with the currently available spectral library files
  observe({
    updateSelectInput(session = session,
                      inputId = "librarySelect",
                      choices = spectralLibraryModuleValues$spectralLibraryFiles)
  })
  
  #RENDER CLASSIFIER UI SECTION
  output$classifierSection <- renderUI({
    if (input$classifierRadioButtons == 1) {
      #create new classifier
      textInput(inputId = session$ns("classifierName"), 
                label = div(style="color: white;", "Classifier File Name:"))
    } else {
      #use previously generated classifiers
      selectInput(inputId = session$ns("classifierSelect"),
                  label = div(style="color: white;", "Select Classifier:"),
                  choices = list.files(path = "output/classifiers", full.names = FALSE))
    }
  })
  
  #SELECT IMAGE FILE
  observe({
    #display the file select dialog
    shinyFileChoose(
      input,
      'imageInput',
      roots = root,
      session = session
    )
    
    #display the chosen directory
    output$imageOutput <- renderPrint({
      if (is.integer(input$imageInput)) {
        imageDirectory <<- ""
      } else {
        imageDirectory <<- parseFilePaths(root, input$imageInput)[[1,4]][1]
      }
    })
  })
  
  
  data <- reactiveValues()
  
  #add to queue button is pressed
  observeEvent(input$addToQueueButton, {
    displayMessage <- FALSE
    message <- ""
    
    #check if all fields contain informations
    if (imageDirectory == "") {
      message <- "Please select a Data Cube"
      displayMessage <- TRUE
    } else if (input$outputFileName == "") {
      message <- "Please enter an Output File Name"
      displayMessage <- TRUE
    } else if (input$classifierRadioButtons == 1) {
      if (input$classifierName == "") {
        message <- "Please enter a Classifier Name"
        displayMessage <- TRUE
      }
    } else if (input$classifierRadioButtons == 0) {
      if (input$classifierSelect == "") {
        message <- "No available classifiers. A new classifier must be created first."
        displayMessage <- TRUE
      }
    }
    
    #if a piece of information is missing, display an error dialog
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
    libraryDirectory <- paste("output/outputSpectralLibraries/", input$librarySelect, sep = "")
    
    newProcess <- list(libraryDirectory = libraryDirectory,
                       newClassifier = input$classifierRadioButtons,
                       classifierFile = paste("output/classifiers/", input$classifierSelect, sep = ""),
                       classifierName = input$classifierName, 
                       imageDirectory = imageDirectory, 
                       outputFileName = input$outputFileName)
    
    #add selected parameters to data variable
    data$processParameters <- newProcess
    
    #random number to indicate that the add to queue button has been pressed
    data$addToQueue <- rnorm(1)
  })
  
  return(data)
}