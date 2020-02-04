library(randomForest)
library(ranger)

generateRFClassifier <- function(classifierName, spectralLibraryDirectory, numOfSampledVariables = 3, treeNum = 500, importance = TRUE) {
  tryCatch({
    ##Reads in VIs for specctral library each functional group has a total of 25 scans and imagery
    spectralLibrary_VIs_equal25<-read.csv(file(spectralLibraryDirectory), fileEncoding = 'UTF16LE')
   
    ##Remove unwanted metadata from spectral library
    spectralLibrary_VIs_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL
    
    ##We can build randomforest model
    #rf_AV_VIs <- randomForest(PFT_3~., data=spectralLibrary_VIs_equal25, mtry = strtoi(numOfSampledVariables), ntree = strtoi(treeNum), importance = importance)
    rf_AV_VIs <- ranger(PFT_3~., data=spectralLibrary_VIs_equal25, mtry = strtoi(numOfSampledVariables), importance = 'impurity')
    ##Now lets save the random forest classifier that was created
  
    saveRDS(rf_AV_VIs, paste(paste("output/classifiers/", classifierName, sep = ""), ".rds", sep = ""))
    
    
    return(paste(paste("output/classifiers/", classifierName, sep = ""), ".rds", sep = ""))
  }, warning = function(warning) {
    message <- paste0("WARNING - While Generating RF Classifier ", spectralLibraryDirectory)
    message <- paste0(message, warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste0("ERROR - While Generating RF Classifier ", spectralLibraryDirectory)
    message <- paste0(message, error, sep = " : ")
    stop(message)
  })

}