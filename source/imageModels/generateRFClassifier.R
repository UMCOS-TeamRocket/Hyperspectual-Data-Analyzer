library(randomForest)
library(foreach)
library(doParallel)
library(parallel)

generateRFClassifier <- function(classifierName, spectralLibraryDirectory, numOfSampledVariables = 3, treeNum = 500, importance = TRUE) {
  tryCatch({
    ##Reads in VIs for specctral library each functional group has a total of 25 scans and imagery 
    spectralLibrary_VIs_equal25<-read.csv(spectralLibraryDirectory)
    
    ##Remove unwanted metadata from spectral library
    spectralLibrary_VIs_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL
    
    core_count <- detectCores()
    cl <- makeCluster(core_count-2)
    registerDoParallel(cl)
    
    ##We can build randomforest model
    rf_AV_VIs <- randomForest(PFT_3~., data=alaskaSpeclib_VIs_equal25, mtry = numOfSampledVariables, ntree = treeNum, importance = importance)
    
    stopCluster(cl)
    closeAllConnections()
    ##Now lets save the random forest classifier that was created
    saveRDS(rf_AV_VIs, paste(paste("output/classifiers/", classifierName, sep = ""), ".rds", sep = ""))
    
    return(paste(paste("output/classifiers/", classifierName, sep = ""), ".rds", sep = ""))
  }, warning = function(warning) {
    message <- paste("WARNING - While Generating RF Classifier", spectralLibraryDirectory)
    message <- paste(message, warning, sep = " : ")
    print(message)
  }, error = function(error) {
    message <- paste("ERROR - While Generating RF Classifier", spectralLibraryDirectory)
    message <- paste(message, error, sep = " : ")
    print(message)
  })

}
