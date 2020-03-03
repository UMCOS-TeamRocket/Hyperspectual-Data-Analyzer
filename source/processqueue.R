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
        
        #separate process parameters into separate variables
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
        
        #sup-progress bar that displays what stage the individual process is in
        withProgress(message = paste("Processing:", outputFileName), min = 0, max = 1, value = 0, {
          #check if a new classifier needs to be created
          if (createNewClassifier == 1) {
            setProgress(0, detail = "Generating Classifier")
            
            print("Generating RF Classifier")
            classifierDirectory <- generateRFClassifier(classifierName, spectralLibraryDirectory, mtry, ntree, importance)
          }
          
          setProgress(0.3, detail = "Processing Image")
          
          #do a check to see if this data cube has already gone through resampling and VI
          #get file name of the data cube from it's directory
          fileName <- basename(imageDirectory)
          #remove file extension
          fileName <- substr(fileName, 1, nchar(fileName) - 4)
          
          #add "_data" to end of file name and reconstruct directory
          #this is what the output file of the function processImage() would look like
          #we can check if this file exists. 
          #If it does, then there is no need to call processImage() and we can just use this file that's already been created
          directory <- paste("output/imagery/", fileName, "_data.csv", sep = "")
          
          #check if the file exists
          
          #TODO
          #REMOVE COMMENTS
          #STRICTLY FOR TESTING
          #if(!file.exists(directory)) {
            #if it does not, pass the image into the processImage() function
            print("Processing Image")
            directory <- processImage(imageDirectory)
          #}
          
          setProgress(0.6, detail = "Predicting")
          
          print("Predicting")
          #pass the directory of the classifier, the original image, the processed image, and the desired name of the output file
          outputDirectory <- predictFunction(classifierDirectory, imageDirectory, directory, outputFileName)
          
          #endTime is the amount of time the process took to complete
          endTime <- proc.time() - startTime
          
          #save output image directory
          queueData$outputImageDirectories[[length(queueData$outputImageDirectories) + 1]] <- outputDirectory
          
          #create output text
          textString <- c(paste("Process#:", index + 1), 
                          paste("Output File Name:", outputFileName),
                          paste("Run Time:", endTime[[1]], "seconds"))
          
          #add output text to list of outputStatistics
          queueData$outputStatistics[[length(queueData$outputStatistics) + 1]] <- textString
          
          setProgress(1)
          
          print("Process Finished")
        })
      }, warning = function(warning) {
        errorMessage <- paste("Process#:", index + 1, "<br>",
                              "Output File Name:", outputFileName, "<br>",
                              "Error Message:", warning)
        
        errors[[length(errors) + 1]] <<- HTML(errorMessage)
      }, error = function(error) {
        errorMessage <- paste("Process#:", index + 1, "<br>",
                              "Output File Name:", outputFileName, "<br>",
                              "Error Message:", error)
        
        errors[[length(errors) + 1]] <<- HTML(errorMessage)
      }, finally = {
        index <- index + 1
      })
    }
    
    setProgress(length(queueData$processes))
    
    return(errors)
  })
  
}
