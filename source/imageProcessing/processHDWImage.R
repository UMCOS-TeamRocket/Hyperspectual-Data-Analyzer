source("source/imageProcessing/resampleBandsHDW.R")
source("source/imageProcessing/imageHDWVIs.R")

processHDWImage <- function (imageDirectory) {
  tryCatch({
    #get file name from directory
    fileName <- basename(imageDirectory)
    #remove file extension
    fileName <- substr(fileName, 1, nchar(fileName) - 4)
    
    resampeldDirectories <- resampleBandsHDW(imageDirectory, fileName)
    
    dfDirectory <- resampledDirectories[1]
    hdwViDirectory <- createImgHDWVi(dfDirectory, fileName)
    
    return(hdwViDirectory)
  }, warning = function(warning) {
    warning(warning)
  }, error = function(error) {
    stop(error)
  })
}