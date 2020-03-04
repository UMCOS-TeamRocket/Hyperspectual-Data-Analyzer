library(shiny)
library(shinyWidgets)

source("source/imageModels/generateRFClassifier.R")
source("source/imageModels/predict.R")
source("source/imageProcessing/processImage.R")

processQueue <- function(queueData) {
  #progress bar that displays what processes the queue is currently on
  withProgress(message = 'Processing Queue', min = 0, max = length(queueData$processes), value = 0, {
    errors <- list()
    index <- 0
    for (process in queueData$processes) {
      tryCatch({
        #time the process
        startTime <- proc.time()
        
        classifierParameters <- process$classifierParameters
        
        print(paste("Current Process:", process$outputFileName))
        flog.info(paste("Current Process:", process$outputFileName), name = "logFile")
        
        #increase progress bar and change detail text
        setProgress(index, detail = process$outputFileName)
        
        #sup-progress bar that displays what stage the individual process is in
        withProgress(message = paste("Processing:", process$outputFileName), min = 0, max = 1, value = 0, {
          
          #if reusing a classifier, this will store the directory to that file. Else, this will hold a incomplete directory temporarily
          classifierDirectory <- classifierParameters$classifierFile
          
          #check if a new classifier needs to be created
          if (classifierParameters$newClassifier == 1) {
            setProgress(0, detail = "Generating Classifier")
            
            print("Generating RF Classifier")
            flog.info(paste("Generating New Classifier:", classifierParameters$classifierName), name = "logFile")
            
            classifierDirectory <- generateRFClassifier(process$libraryDirectory, classifierParameters)
          }
          
          setProgress(0.3, detail = "Processing Image")
          
          #do a check to see if this data cube has already gone through resampling and VI
          #get file name of the data cube from it's directory
          fileName <- basename(process$imageDirectory)
          #remove file extension
          fileName <- substr(fileName, 1, nchar(fileName) - 4)
          
          #add "_data" to end of file name and reconstruct directory
          #this is what the output file of the function processImage() would look like
          #we can check if this file exists. 
          #If it does, then there is no need to call processImage() and we can just use this file that's already been created
          directory <- paste("output/imagery/", fileName, "_data.csv", sep = "")
          
          #check if the file exists
          if(!file.exists(directory)) {
            #if it does not, pass the image into the processImage() function
            print("Processing Image")
            flog.info(paste("Processing Image:", process$imageDirectory), name = "logFile")
            
            directory <- processImage(process$imageDirectory)
          }
          
          setProgress(0.6, detail = "Predicting")
          
          print("Predicting")
          flog.info("Predicting", name = "logFile")
          
          #pass the directory of the classifier, the original image, the processed image, and the desired name of the output file
          outputDirectory <- predictFunction(classifierDirectory, process$imageDirectory, directory, process$outputFileName)
          
          #endTime is the amount of time the process took to complete
          endTime <- proc.time() - startTime
          
          #save output image directory
          queueData$outputImageDirectories[[length(queueData$outputImageDirectories) + 1]] <- outputDirectory
          flog.info(paste("Prediction Output Directory:", outputDirectory), name = "logFile")
          
          #create output text
          statistics <- c(paste("Process#:", index + 1), 
                          paste("Output File Name:", process$outputFileName),
                          paste("Run Time:", endTime[[1]], "seconds"))
          
          #add output text to list of outputStatistics
          queueData$outputStatistics[[length(queueData$outputStatistics) + 1]] <- statistics
          
          flog.info(paste("Prediction Output Statistics:", statistics), name = "logFile")
          
          setProgress(1)
          
          print("Process Finished")
          flog.info("Process Finished", name = "logFile")
        })
      }, warning = function(warning) {
        errorMessage <- paste("Process#:", index + 1, "<br>",
                              "Output File Name:", process$outputFileName, "<br>",
                              "Error Message:", warning)
        
        errors[[length(errors) + 1]] <<- HTML(errorMessage)
        
        flog.warn(HTML(errorMessage), name = "logFile")
      }, error = function(error) {
        errorMessage <- paste("Process#:", index + 1, "<br>",
                              "Output File Name:", process$outputFileName, "<br>",
                              "Error Message:", error)
        
        errors[[length(errors) + 1]] <<- HTML(errorMessage)
        
        flog.error(HTML(errorMessage), name = "logFile")
      }, finally = {
        index <- index + 1
      })
    }
    
    setProgress(length(queueData$processes))
    
    return(errors)
  })
  
}
