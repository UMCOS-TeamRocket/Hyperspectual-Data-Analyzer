source("source/imageProcessing/resampleBandsHDW.R")
source("source/imageProcessing/imageHDWVIs.R")

processHDWImage <- function (imageDirectory) {
  tryCatch({
    #get file name from directory
    fileName <- basename(imageDirectory)
    #remove file extension
    fileName <- substr(fileName, 1, nchar(fileName) - 4)

    resampledDirectories <- resampleBandsHDW(imageDirectory, fileName)

    dfDirectory <- resampledDirectories[1]

    hdwViDirectory <- createImgHDWVi(dfDirectory, fileName)
    
    return(hdwViDirectory)
  }, warning = function(warning) {
    warning(warning)
    message <- paste ("WARNING - While process")
  }, error = function(error) {
    message <- paste ("WARNING - While process")
    stop(error)
  })
}