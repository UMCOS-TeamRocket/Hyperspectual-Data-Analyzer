library(shiny)
library(shinyWidgets)

source("source/imageModels/generateRFClassifier.R")
source("source/imageModels/predict.R")
source("source/imageProcessing/processHDWImage.R")

processQueue <- function(queueData) {
  withProgress(message = 'Processing Queue', min = 0, max = length(queueData$processes), value = 0, {
    index <- 0
    for (process in queueData$processes) {
      tryCatch({
        #timer
        startTime <- proc.time()
        
        #separate process parameters
        parameters <- process$parameters
        classifierParameters <- process$classifierParameters
        
        spectralLibraryDirectory <- parameters$libraryDirectory
        
        createNewClassifier <- parameters$newClassifier
        classifierDirectory <- parameters$classifierFile
        classifierName <- parameters$classifierName
        
        mtry <- classifierParameters$mtry()
        ntree <- classifierParameters$ntree()
        importance <- classifierParameters$importance()
        
        imageDirectory <- parameters$imageDirectory

        outputFileName <- parameters$outputFileName
        
        hdwDirectory <- parameters$libraryDirectory
        
        print(paste("Current Process:", outputFileName))
        
        #increase progress bar and change detail text
        setProgress(index, detail = outputFileName)
        
        withProgress(message = paste("Processing:", outputFileName), min = 0, max = 1, value = 0, {
          if (createNewClassifier == 1) {
            setProgress(0, detail = "Generating Classifier")
            
            print("Generating RF Classifier")
            classifierDirectory <- generateRFClassifier(classifierName, spectralLibraryDirectory, mtry, ntree, importance)
          }
          
          setProgress(0.3, detail = "Processing HDW Image")
          
          print("Processing HDW Image")
          hdwDirectory <- processHDWImage(imageDirectory)
          setProgress(0.6, detail = "Predicting")
          
          #I hard coded this because some file somewhere is broken
          #R is so confusing that I can not for the life of me find it
          #classifierDirectory<-"output/classifiers/test.rds"
          #hdwDirectory <- "output/test.csv"
          print("Predicting")
          outputDirectory <- predictFunction(classifierDirectory, imageDirectory, hdwDirectory, outputFileName)
          
          endTime <- proc.time() - startTime
          
          #save output image directory
          queueData$outputImageDirectories[[length(queueData$outputImageDirectories) + 1]] <- outputDirectory
          
          #create output text
          #TODO: separate with new line (somehow... why isnt it easy)
          textString <- c(paste("Process#:", index + 1), 
                          paste("Output File Name:", outputFileName),
                          endtime)
          
          #add output text to list of outputStatistics
          queueData$outputStatistics[[length(queueData$outputStatistics) + 1]] <- textString
          
          setProgress(1)
          
          print("Process Finished")
        })
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
    
    setProgress(length(queueData$processes))
  })
  
}
