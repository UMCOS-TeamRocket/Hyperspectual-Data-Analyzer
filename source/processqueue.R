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
        classifier <- process[[1]]
        spectralLibrary <- classifier[1]
        mtry <- classifier[2]
        ntree <- classifier[3]
        importance <- classifier[4]
        classifierFileName <- classifier[5]
        
        images <- process[[2]]
        image <- images[1]
        hdwImage <- images[2]
        
        outputFileName <- process[[3]]
        
        print(paste("Current Process:", outputFileName))
        
        #increase progress bar and change detail text
        setProgress(index, detail = outputFileName)
        
        print("Generating RF Classifier")
        classifierDirectory <- generateRFClassifier(classifierFileName, spectralLibrary, mtry, ntree, importance)
        
        print("Processing HDW Image")
        hdwViDirectory <- processHDWImage(image)
        
        print("Predicting")
        outputDirectory <- predict(classifierDirectory, image, hdwImage, hdwViDirectory, outputFileName)
        
        print("Process Finished")
        
      }, warning = function(warning) {
        print(warning)
      }, error = function(error) {
        print(error)
      }, finally = {
        index <- index + 1
      })
    }
    
    setProgress(length(queue))
  })
  
}
