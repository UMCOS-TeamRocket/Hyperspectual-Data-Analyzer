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
          
          #get file name from directory
          fileName <- basename(imageDirectory)
          #remove file extension
          fileName <- substr(fileName, 1, nchar(fileName) - 4)
          #add "_dataHDW" to end of file name and reconstruct directory
          hdwDirectory <- paste("output/hdwImagery/", fileName, "_dataHDW.csv", sep = "")
          
          print(paste("directory:", hdwDirectory))
          
          if(!file.exists(hdwDirectory)) {
            print("file did not exist")
            print("Processing HDW Image")
            hdwDirectory <- processHDWImage(imageDirectory)
          } else {
            print("file existed")
          }
          
          print(paste("directory:", hdwDirectory))
          
          setProgress(0.6, detail = "Predicting")
          
          #Testing code
          #classifierDirectory<-"output/classifiers/test.rds"
          #hdwDirectory <- "output/test.csv"
          
          print("Predicting")
          outputDirectory <- predictFunction(classifierDirectory, imageDirectory, hdwDirectory, outputFileName)
          
          endTime <- proc.time() - startTime
          
          #save output image directory
          queueData$outputImageDirectories[[length(queueData$outputImageDirectories) + 1]] <- outputDirectory
          
          #create output text
          #TODO: separate with new line
          textString <- c(paste("Process#:", index + 1, "\n"), 
                          paste("Output File Name:", outputFileName, "\n"),
                          paste("Run Time:", endTime[[1]], "\n"))
          
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
