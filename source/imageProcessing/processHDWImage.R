source("source/imageProcessing/resampleImgBands.R")
source("source/imageProcessing/createImgVIs.R")

processHDWImage <- function (imageDirectory) {
  tryCatch({
    #get file name from directory
    fileName <- baseName(imageDirectory)
    #remove file extension
    fileName <- substr(fileName, 1, nchar(fileName) - 4)
    
    resampeldDirectories <- resampleBandsHDW(imageDirectory, fileName)
    
    dfDirectory <- resampledDirectories[1]
    hdwViDirectory <- createImgHDWVi(dfDirectory, fileName)
    
    return(hdwViDirectory)
  }, warning = function(warning) {
    print(warning)
  }, error = function(error) {
    print(error)
  })
}