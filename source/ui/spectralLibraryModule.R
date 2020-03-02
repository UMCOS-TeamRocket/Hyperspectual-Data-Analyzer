source("source/fieldSpecProcessing/bySite.R")
source("source/generateSpectralLibraryFiles.R")

spectralLibraryModuleUI <- function(id) {
  #namespace for the module
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4, shinyDirButton(ns("fieldSpecDirInput"), "Browse", "Select Directory With Field Spec Data")),
      column(8, verbatimTextOutput(ns("fieldSpecDirOutput"), placeholder = TRUE))
    ),
    
    actionButton(ns("updateSpectralBySite"), "Update Spectra By Site"),
    
    br(),
    br(),
    
    HTML(paste0("#", ns("spectralList"), "+div div a {color: red;}")) %>% 
      tags$style() %>%
      tags$head(),
    
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
  #variable used as root directory when selecting a field spec directory
  root <- c(home = fs::path_home(), project = here())
  
  #variable to hold the current list of spectra that have already been generated
  spectraList <- list.files(path = "output/fieldSpec", full.names = FALSE)
  
  #reactive values that are to be returned from this function
  returnValues <- reactiveValues()
  #list of spectral library files that have already been generated
  returnValues$spectralLibraryFiles <- list.files(path = "output/hdwSpectralLibraries", full.names = FALSE)
  
  #open a directory select dialog when 'browse' button is clicked for the user to select a directory with field spec data
  observe({
    shinyDirChoose(
      input,
      'fieldSpecDirInput',
      roots = root,
      session = session
    )
    
    #display chosen directory
    output$fieldSpecDirOutput <- renderPrint({
      #check if a directory was selected
      if (is.integer(input$fieldSpecDirInput)) {
        fieldSpecDirectory <<- ""
      } else {
        fieldSpecDirectory <<- parseDirPath(root, input$fieldSpecDirInput)
        flog.info(paste("Field Spec Directory Chosen:", fieldSpecDirectory), name = "logFile")
        
        #this needs to be the last line in order to return it to renderPrint()
        parseDirPath(root, input$fieldSpecDirInput)
      }
    })
  })
  
  #execute when 'update spectra by site' button is clicked
  observeEvent(input$updateSpectralBySite, {
    flog.info("Update Spectra By Site", name = "logFile")
    
    #check if a directory has been selected
    if (fieldSpecDirectory == "") {
      #display error dialog
      showModal(modalDialog(
        fluidRow(
          h3("Please select a directory")
        ),
        title = "Missing Information",
        easyClose = TRUE
      ))
      
      flog.warn("Directory containing field spec data was not chosen", name = "logFile")
      
      return()
    }
    
    print("Processing Spectra By Site...")
    
    #create a specra object for each directory under fieldSpecDirectory recursively
    #return any errors that occur during this process and store in 'errors' variable
    errors <- processFieldSpec(fieldSpecDirectory)
    
    print("Finished Processing Spectra By Site")
    flog.info("FInished Processing Spectra By Site", name = "logFile")
    
    #update the spectraList with any new spectra that were created
    spectraList <<- list.files(path = "output/fieldSpec", full.names = FALSE)
    
    flog.info("Update List of Spectra Objects", name = "logFile")
    
    #reflect the updated list in the UI
    updateMultiInput(
      session = session,
      inputId = "spectralList",
      selected = c(),
      choices = spectraList
    )
    
    #check if any errors occured while processing field spec data
    if (length(errors) > 0) {
      #if so, display an error dialog with the directory where the error occured and the error message
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
  
  #when this button is clicked, select all available spectra objects to be used in a spectral library
  observeEvent(input$selectAllSpectra, {
    updateMultiInput(
      session = session,
      inputId = "spectralList",
      choices = spectraList,
      selected = spectraList
    )
    
    flog.info("Select all Spectra Objects", name = "logFile")
  })
  
  #execute when 'create spectral library' is clicked
  observeEvent(input$createSpectralLibrary, {
    flog.info("Create Spectral Library Button Clicked", name = "logFile")
    
    #get the name that was typed in by the user for the spectral library
    spectralLibraryName <- input$spectralLibraryName
    
    #if the spectral library name field was left blank, display an error dialog
    if (spectralLibraryName == "") {
      showModal(modalDialog(
        fluidRow(
          h3("Please enter a name for the Spectral Library")
        ),
        title = "Missing Information",
        easyClose = TRUE
      ))
      
      flog.warn("No Spectral Library Name was entered", name = "logFile")
      
      return()
    } else if (is.null(input$spectralList)) {
      #if no spectra were selected to be used, display an error dialog
      showModal(modalDialog(
        fluidRow(
          h3("Please select at least 1 spectral object")
        ),
        title = "Missing Information",
        easyClose = TRUE
      ))
      
      flog.warn("No Spectral Objects were selected", name = "logFile")
      
      return()
    }
    
    
    listOfSpectraDirectories <- c()
    index <- 1
    
    #rebuild the directories for each spectra object file so the generateSpectralLibraryFiles() function knows where to look for them
    for(fileName in input$spectralList) {
      listOfSpectraDirectories[index] <- paste("output/fieldSpec", fileName, sep = "/")
      index <- index + 1
    }
    
    tryCatch({
      print("Generating Spectral Library Files...")
      flog.info("Generating Spectral Library Files", name = "logFile")
      
      #create a spectral library
      generateSpectralLibraryFiles(listOfSpectraDirectories, spectralLibraryName)
      
      print("Generated Spectral Library Files")
      flog.info("Generated Spectral Library Files", name = "logFile")
  
    }, warning = function(warning) {
      flog.warn(warning, name = "logFile")
      #display an error dialog if a warning or error occured during the generateSpectralLibraryFiles() function
      showModal(modalDialog(
        fluidRow(
          h4(paste0(warning))
        ),
        title = "Warning",
        easyClose = TRUE
      ))
    }, error = function(error) {
      flog.error(error, name = "logFile")
      
      showModal(modalDialog(
        fluidRow(
          h4(paste0(error))
        ),
        title = "Error",
        easyClose = TRUE
      ))
    })
    
    #update the list of available spectral library files
    returnValues$spectralLibraryFiles <- list.files(path = "output/hdwSpectralLibraries", full.names = FALSE)
  })
  
  return(returnValues)
}