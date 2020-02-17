source("source/fieldSpecProcessing/bySite.R")
source("source/generateSpectralLibraryFiles.R")

spectralLibraryModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4, shinyDirButton(ns("fieldSpecDirInput"), "Browse", "Select Directory With Field Spec Data")),
      column(8, verbatimTextOutput(ns("fieldSpecDirOutput"), placeholder = TRUE))
    ),
    
    actionButton(ns("updateSpectralBySite"), "Update Spectra By Site"),
    
    br(),
    br(),
    
    multiInput(
      inputId = ns("spectralList"), label =span("List of Spectral Objects By Site", style="color:white"),
      choices = list.files(path = "output/fieldSpec", full.names = FALSE),
      options = list(
        enable_search = TRUE
      )
    ),
    
    actionButton(ns("selectAllSpectra"), "Select All Spectra"),
    
    br(),
    br(),
    
    textInput(ns("spectralLibraryName"),label = div(style="color: white;", "Spectral Library Name")),
    
    actionButton(ns("createSpectralLibrary"), "Create Spectral Library")
  )
}

spectralLibraryModuleServer <- function(input, output, session) {
  #INITIALIZE VARIABLES
  root <- c(home = fs::path_home(), project = here())
  spectraList <- list.files(path = "output/fieldSpec", full.names = FALSE)
  returnValues <- reactiveValues()
  returnValues$spectralLibraryFiles <- list.files(path = "output/hdwSpectralLibraries", full.names = FALSE)
  
  #SELECT FIELD SPEC DIRECTORY
  observe({
    shinyDirChoose(
      input,
      'fieldSpecDirInput',
      roots = root,
      session = session
    )
    
    #display chosen directory
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
    
    spectraList <<- list.files(path = "output/fieldSpec", full.names = FALSE)
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
  
  #SELECT ALL SPECTRA
  observeEvent(input$selectAllSpectra, {
    updateMultiInput(
      session = session,
      inputId = "spectralList",
      choices = spectraList,
      selected = spectraList
    )
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
    
    returnValues$spectralLibraryFiles <- list.files(path = "output/hdwSpectralLibraries", full.names = FALSE)
  })
  
  return(returnValues)
}