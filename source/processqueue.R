library(shiny)
library(shinyWidgets)

source("source/imageModels/generateRFClassifier.R")
source("source/imageModels/predict.R")
source("source/imageProcessing/processHDWImage.R")

processQueue <- function(queue) {
  withProgress(message = 'Processing Queue', min = 0, max = length(queue), value = 0, {
    index <- 0
    for (process in queue) {
      tryCatch({
        parameters <- process$parameters
        classifierParameters <- process$classifierParameters
        
        classifierName <- parameters$classifierName
        spectralLibraryDirectory <- parameters$libraryDirectory
        mtry <- classifierParameters$mtry()
        ntree <- classifierParameters$ntree()
        importance <- classifierParameters$importance()
        
        imageDirectory <- parameters$imageDirectory
        
        outputFileName <- parameters$outputFileName
        
        print(paste("Current Process:", outputFileName))
        
        #increase progress bar and change detail text
        setProgress(index, detail = outputFileName)
        
        print("Generating RF Classifier")
        classifierDirectory <- generateRFClassifier(classifierName, spectralLibraryDirectory, mtry, ntree, importance)
        
        print("Processing HDW Image")
        hdwViDirectory <- processHDWImage(imageDirectory)
        
        print("Predicting")
        outputDirectory <- predictFunction(classifierDirectory, image, hdwViDirectory, outputFileName)
        
        print("Process Finished")
        
      }, warning = function(warning) {
        warning(warning)
        message <- paste ("WARNING - While process")
      }, error = function(error) {
        message <- paste ("WARNING - While process")
        stop(error)
      }, finally = {
        index <- index + 1
      })
    }
    
    setProgress(length(queue))
  })
  
}
