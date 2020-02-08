selectDataUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    #Drop down for Spectral Library selection
    selectInput(ns("librarySelect"), label = div(style="color: white;", "Spectral Library:"), 
                c(list.files(path = "output/hdwSpectralLibraries", full.names = FALSE))),
    
    #label to be used as the filename for the classifier
    textInput(ns("classifierName"),label = div(style="color: white;", "Classifier File Name:")),
    
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



selectDataServer <- function(input, output, session) {
  root <- c(home = fs::path_home(), project = here())
  
  #SELECT IMAGE FILE
  observe({
    shinyFileChoose(
      input,
      'imageInput',
      roots = root,
      session = session
    )
    
    output$imageOutput <- renderPrint({
      if (is.integer(input$imageInput)) {
        imageDirectory <<- ""
        cat("No image has been selected")
      } else {
        imageDirectory <<- parseFilePaths(root, input$imageInput)[[1,4]][1]
        parseFilePaths(root, input$imageInput)[[1,4]][1]
      }
    })
  })
  
  
  data <- reactiveValues()
  
  #add to queue button is pressed
  observeEvent(input$addToQueueButton, {
    displayMessage <- FALSE
    message <- ""
    
    #check if all fields contain informations
    if (input$classifierName == "") {
      message <- "Please enter a Classifier Name"
      displayMessage <- TRUE
    } else if (imageDirectory == "") {
      message <- "Please select a Data Cube"
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
    # The HDW naming is now wrong, didn't change cause would break everything
    libraryDirectory <- paste("output/hdwSpectralLibraries/", input$librarySelect, sep = "")
    
    newProcess <- list(libraryDirectory = libraryDirectory, 
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