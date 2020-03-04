selectDataUI <- function(id) {
  #namespace for the module
  ns <- NS(id)
  
  tagList(
    #Drop down for Spectral Library selection
    selectInput(ns("librarySelect"), label = div(style="color: white;", "Spectral Library:"), 
                c(list.files(path = "output/hdwSpectralLibraries", full.names = FALSE))),
    
    radioButtons(inputId = ns("classifierRadioButtons"),
                 label = div(style="color: white;", "Classifier:"),
                 inline = FALSE, 
                 width = NULL, 
                 choiceNames = list("Use Previously Generated Classifier", "Create New Classifier"),
                 choiceValues = list(0, 1)),
    
    uiOutput(ns("classifierSection")),
    
    #TODO: limit file types
    fluidRow(
      column(2, shinyFilesButton(ns("imageInput"), "Browse", title = "Select Image", multiple = FALSE)),
      column(5, verbatimTextOutput(ns("imageOutput"), placeholder = TRUE))
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
  
  observeEvent(input$classifierRadioButtons, {
    #RENDER CLASSIFIER UI SECTION
    output$classifierSection <- renderUI({
      if (input$classifierRadioButtons == 1) {
        #create new classifier
        tagList (
          #mtry random forest parameter
          sliderInput(session$ns("mtry"), label = div(style="color: white;", "Number Of Sampled Variables:"),
                      min = 0, max = 10,
                      value = 3),
          
          #ntree random forest parameter
          sliderInput(session$ns("ntree"), label = div(style="color: white;", "Number of trees to grow:"),
                      min = 0, max = 1000,
                      value = 500),
          
          #importance random forest parameter
          #from https://cran.r-project.org/web/packages/ranger/ranger.pdf page 16
          #Variable importance mode, one of 'none', 'impurity', 'impurity_corrected', 'permutation'.
          selectInput(inputId = session$ns("importance"),
                      label = div(style="color: white;", "Importance:"),
                      choices = c("none", "impurity", "impurity_corrected", "permutation")),
          
          #name for new classifier
          textInput(inputId = session$ns("classifierName"), label = div(style="color: white;", "Classifier File Name:")),
          
          #change the color of the min and max values on the slider to white
          tags$style(HTML(".irs-max {color: white;}.irs-min {color: white;}")),
          
          #TODO: only works once, then they turn blue
          #make sliders red
          setSliderColor(c("#D2403A", "#D2403A"), c(1, 2))
        )
      } else {
        #use previously generated classifiers
        selectInput(inputId = session$ns("classifierSelect"),
                    label = div(style="color: white;", "Select Classifier:"),
                    choices = list.files(path = "output/classifiers", full.names = FALSE))
      }
    })
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
        #get directory from imageInput
        imageDirectory <<- parseFilePaths(root, input$imageInput)[[1,4]][1]
        flog.info(paste("Selected Image Direcotry", imageDirectory), name = "logFile")
        
        #this needs to be the last line in order for it to be returned to renderPrint()
        parseFilePaths(root, input$imageInput)[[1,4]][1]
      }
    })
  })
  
  
  data <- reactiveValues()
  
  #add to queue button is pressed
  observeEvent(input$addToQueueButton, {
    flog.info("Add to Queue", name = "logFile")
    
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
      
      flog.warn(paste("Missing Information:", message), name = "logFile")
      
      return()
    }
    
    classifierParameters <- list(newClassifier = input$classifierRadioButtons,
                                 classifierFile = paste("output/classifiers/", input$classifierSelect, sep = ""),
                                 mtry = input$mtry,
                                 ntree = input$ntree,
                                 importance = input$importance,
                                 classifierName = input$classifierName)
    
    newProcess <- list(libraryDirectory = paste("output/hdwSpectralLibraries/", input$librarySelect, sep = ""),
                       classifierParameters = classifierParameters,
                       imageDirectory = imageDirectory, 
                       outputFileName = input$outputFileName)
    
    #add selected parameters to data variable
    data$processParameters <- newProcess
    
    #random number to indicate that the add to queue button has been pressed
    data$addToQueue <- rnorm(1)
  })
  
  return(data)
}