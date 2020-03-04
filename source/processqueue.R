library(shiny)
library(shinyWidgets)

source("source/imageModels/generateRFClassifier.R")
source("source/imageModels/predict.R")
source("source/imageProcessing/processHDWImage.R")

processQueue <- function(queueData) {
  #progress bar that displays what processes the queue is currently on
  withProgress(message = 'Processing Queue', min = 0, max = length(queueData$processes), value = 0, {
    errors <- list()
    index <- 0
    for (process in queueData$processes) {
      tryCatch({
        #time the process
        startTime <- proc.time()
        
        print(paste("Current Process:", process$outputFileName))
        flog.info(paste("Current Process:", process$outputFileName), name = "logFile")
        
        #increase progress bar and change detail text
        setProgress(index, detail = process$outputFileName)
        
        #sup-progress bar that displays what stage the individual process is in
        withProgress(message = paste("Processing:", process$outputFileName), min = 0, max = 1, value = 0, {
          
          classifierParameters <- process$classifierParameters
          
          #if a new classifier needs to be created, this will be null. Otherwise, this will be a string
          classifierDirectory <- classifierParameters$classifierFile
          
          #check if a new classifier needs to be created
          if (classifierParameters$newClassifier == 1) {
            #a new classifier needs to be generated
            setProgress(0, detail = "Generating Classifier")
            
            print("Generating RF Classifier")
            flog.info(paste("Generating New Classifier:", classifierParameters$classifierName), name = "logFile")
            
            classifierDirectory <- generateRFClassifier(process$libraryDirectory, classifierParameters)
          }
          
          setProgress(0.3, detail = "Processing HDW Image")
          
          #do a check to see if this data cube has already gone through resampling and VI
          #get file name of the data cube from it's directory
          fileName <- basename(process$imageDirectory)
          #remove file extension
          fileName <- substr(fileName, 1, nchar(fileName) - 4)
          
          #add "_dataHDW" to end of file name and reconstruct directory
          #this is what the output file of the function processHDWImage() would look like
          #we can check if this file exists. 
          #If it does, then there is no need to call processHDWImage() and we can just use this file that's already been created
          hdwDirectory <- paste("output/hdwImagery/", fileName, "_dataHDW.csv", sep = "")
          
          #check if the file exists
          if(!file.exists(hdwDirectory)) {
            #if it does not, pass the image into the processHDWImage() function
            print("Processing Image")
            flog.info(paste("Processing Image:", process$imageDirectory), name = "logFile")
            
            hdwDirectory <- processHDWImage(process$imageDirectory)
          } else {
            flog.info(paste("Re-using previously generated image:", hdwDirectory), name = "logFile")
          }
          
          setProgress(0.6, detail = "Predicting")
          
          print("Predicting")
          flog.info("Predicting", name = "logFile")
          #pass the directory of the classifier, the original image, the processed image, and the desired name of the output file
          outputDirectory <- predictFunction(classifierDirectory, process$imageDirectory, hdwDirectory, process$outputFileName)
          
          flog.info(paste("Prediction output saved in", outputDirectory), name = "logFile")
          #endTime is the amount of time the process took to complete
          endTime <- proc.time() - startTime
          
          #save output image directory
          queueData$outputImageDirectories[[length(queueData$outputImageDirectories) + 1]] <- outputDirectory
          
          #create output text
          textString <- c(paste("Process#:", index + 1), 
                          paste("Output File Name:", process$outputFileName),
                          paste("Run Time:", endTime[[1]], "seconds"))
          
          #add output text to list of outputStatistics
          queueData$outputStatistics[[length(queueData$outputStatistics) + 1]] <- textString
          
          setProgress(1)
          
          print("Process Finished")
          flog.info(paste("Process #", index + 1, " Finished", sep = ""), name = "logFile")
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
