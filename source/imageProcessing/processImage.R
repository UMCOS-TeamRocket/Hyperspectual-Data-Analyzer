source("source/imageProcessing/resampleImgBands.R")
source("source/imageProcessing/createImgVIs.R")

processImage <- (imageDirectory) {
  tryCatch({
    #get file name from directory
    fileName <- baseName(imageDirectory)
    #remove file extension
    fileName <- substr(fileName, 1, nchar(fileName) - 4)
    
    resampleIMGBands(imageDirectory, fileName)
    
    #directory where resampleIMGBands() saves the df
    dfDirectory <- paste(paste("output/hdwImagery/images/", fileName, sep = ""), "_HDW_df.csv", sep = "")
    
    createImgVi(dfDirectory, fileName)
  }, warning = function(warning) {
    print(warning)
  }, error = function(error) {
    print(error)
  })
}