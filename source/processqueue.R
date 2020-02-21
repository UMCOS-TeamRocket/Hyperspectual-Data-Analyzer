library(shiny)
library(shinyWidgets)

source("source/imageModels/generateRFClassifier.R")
source("source/imageModels/predict.R")
source("source/imageProcessing/processHDWImage.R")

processQueue <- function(queue,progressBar, interruptor, classifierName, spectralLibraryDirectory, mtry, ntree, importance,imageDirectory, outputFileName, createNewClassifier, classifierDirectory, outputImageDirectories, outputStatistics) {
  index <- 0
  #for (process in queueData$processes) {
       interruptor$execInterrupts()
    tryCatch({
      queue$producer$fireAssignReactive("message", "Predicting")
      startTime <- proc.time()
      
      
      queue$producer$fireAssignReactive("message",paste("Current Process:", outputFileName))
      progressBar$set(index, detail = outputFileName)
      
      if (createNewClassifier == 1) {
                #setProgress(0, detail = "Generating Classifier")

                queue$producer$fireAssignReactive("message","Generating RF Classifier")
                #message("Generating RF Classifier")
                interruptor$execInterrupts()
                classifierDirectory <- generateRFClassifier(classifierName, spectralLibraryDirectory, mtry, ntree, importance)
                interruptor$execInterrupts()
      }
      
      #classifierDirectory <- generateRFClassifier(classifierName, spectralLibraryDirectory, mtry, ntree, importance)
      queue$producer$fireAssignReactive("message","Processing HDW Image")
      interruptor$execInterrupts()
      hdwDirectory <- processHDWImage(imageDirectory)
      interruptor$execInterrupts()
      
      queue$producer$fireAssignReactive("message", "Predicting")
      interruptor$execInterrupts()
      outputDirectory <- predictFunction(classifierDirectory, imageDirectory, hdwDirectory, outputFileName)
      interruptor$execInterrupts()
      
      endTime <- proc.time() - startTime
      
      outputImageDirectories[[length(outputImageDirectories) + 1]] <- outputDirectory

      #create output text
      #TODO: separate with new line (somehow... why isnt it easy)
      textString <- c(paste("Process#:", index + 1, "\n"),
         paste("Output File Name:", outputFileName, "\n"),
         paste("Run Time:", endTime[[1]], "\n"))

      #add output text to list of outputStatistics
      outputStatistics[[length(outputStatistics) + 1]] <- textString
      
      queue$producer$fireAssignReactive("message","Process Finished")
      
    }, warning = function(warning) {
            warning(warning)
            message <- paste ("WARNING - While process")
    }, error = function(error) {
            message <- paste ("WARNING - While process")
            stop(error)
    }, finally = {
            index <- index + 1
   })
  #}
  #progressBar$set(length(queueData$processes))
}
#queue$producer$fireAssignReactive("message", "Predicting2")

#   index <- 0
#   for (process in queueData) {
#     #interruptor$execInterrupts()
#     tryCatch({
#       #timer
#       #queue$producer$fireAssignReactive("message", "Predicting")
#       startTime <- proc.time()
#       
#       #separate process parameters
#       parameters <- process$parameters
#       classifierParameters <- process$classifierParameters
#       
#       spectralLibraryDirectory <- parameters$libraryDirectory
#       
#       createNewClassifier <- parameters$newClassifier
#       classifierDirectory <- parameters$classifierFile
#       classifierName <- parameters$classifierName
#       
#       # mtry <- classifierParameters$mtry()
#       # ntree <- classifierParameters$ntree()
#       # importance <- classifierParameters$importance()
#       
#       imageDirectory <- parameters$imageDirectory
#       
#       outputFileName <- parameters$outputFileName
#       
#       hdwDirectory <- parameters$libraryDirectory
#       
#       #queue$producer$fireAssignReactive("message",paste("Current Process:", outputFileName))
#       #message(paste("Current Process:", outputFileName))
#       
#       #increase progress bar and change detail text
#       #progressBar$set(index, detail = outputFileName)
#       #setProgress(index, detail = outputFileName)
#       
#       if (createNewClassifier == 1) {
#         #setProgress(0, detail = "Generating Classifier")
#         
#         #queue$producer$fireAssignReactive("message","Generating RF Classifier")
#         #message("Generating RF Classifier")
#         classifierDirectory <- generateRFClassifier(classifierName, spectralLibraryDirectory, mtry, ntree, importance)
#       }
#       
#       #setProgress(0.3, detail = "Processing HDW Image")
#       
#       #queue$producer$fireAssignReactive("message","Processing HDW Image")
#       #message("Processing HDW Image")
#       hdwDirectory <- processHDWImage(imageDirectory)
#       #setProgress(0.6, detail = "Predicting")
#       
#       #I hard coded this because some file somewhere is broken
#       #R is so confusing that I can not for the life of me find it
#       #classifierDirectory<-"output/classifiers/test.rds"
#       #hdwDirectory <- "output/test.csv"
#       #queue$producer$fireAssignReactive("message", "Predicting")
#       #message("Predicting")
#       outputDirectory <- predictFunction(classifierDirectory, imageDirectory, hdwDirectory, outputFileName)
#       
#       endTime <- proc.time() - startTime
#       
#       #save output image directory
#       # queueData$outputImageDirectories[[length(queueData$outputImageDirectories) + 1]] <- outputDirectory
#       # 
#       # #create output text
#       # #TODO: separate with new line (somehow... why isnt it easy)
#       # textString <- c(paste("Process#:", index + 1, "\n"), 
#       #                 paste("Output File Name:", outputFileName, "\n"),
#       #                 paste("Run Time:", endTime[[1]], "\n"))
#       # 
#       # #add output text to list of outputStatistics
#       # queueData$outputStatistics[[length(queueData$outputStatistics) + 1]] <- textString
#       
#       #setProgress(1)
#       #queue$producer$fireAssignReactive("message","Process Finished")
#       #message("Process Finished")
#     }, warning = function(warning) {
#       warning(warning)
#       message <- paste ("WARNING - While process")
#     }, error = function(error) {
#       message <- paste ("WARNING - While process")
#       stop(error)
#     }, finally = {
#       index <- index + 1
#     })
#   }
#   
#   #progressBar$set(length(queueData$processes))
#   #setProgress(length(queueData$processes))
#     
