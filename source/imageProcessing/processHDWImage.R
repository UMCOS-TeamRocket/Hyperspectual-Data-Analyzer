source("source/imageProcessing/resampleBandsHDW.R")
source("source/imageProcessing/imageHDWVIs.R")

processHDWImage <- function (imageDirectory) {
  tryCatch({
    #get file name from directory
    fileName <- basename(imageDirectory)
    #remove file extension
    fileName <- substr(fileName, 1, nchar(fileName) - 4)
  
    print("Resampling")
    resampledDirectories <- resampleBandsHDW(imageDirectory, fileName)

    dfDirectory <- resampledDirectories[1]

    print("Generating VIs")
    hdwViDirectory <- createImgHDWVi(resampledDirectories[["df"]], fileName)
    
    hdw_010nm<-read.csv(resampledDirectories[["nm10"]])
    hdw_050nm<-read.csv(resampledDirectories[["nm50"]])
    hdw_100nm<-read.csv(resampledDirectories[["nm100"]])
    VI<-read.csv(hdwViDirectory)
    
    colnames(hdw_010nm)[-1:-2]<-paste0(colnames(hdw_010nm)[-1:-2],"_010nm")
    colnames(hdw_050nm)[-1:-2]<-paste0(colnames(hdw_050nm)[-1:-2],"_050nm")
    colnames(hdw_100nm)[-1:-2]<-paste0(colnames(hdw_100nm)[-1:-2],"_100nm")
    colnames(VI)[-1:-2]<-paste0(colnames(VI)[-1:-2],"_VIs")

    dataHDW<-Reduce(cbind,list(hdw_010nm,hdw_050nm[-1:-2],hdw_100nm[-1:-2],VI[-1:-2]))
    write.csv(dataHDW, paste("output/hdwImagery/", fileName, "_dataHDW.csv", sep = ""), row.names = FALSE)
  
    return(paste("output/hdwImagery/", fileName, "_dataHDW.csv", sep = ""))
  }, warning = function(warning) {
    warning(warning)
    message <- paste ("WARNING - While process")
  }, error = function(error) {
    message <- paste ("WARNING - While process")
    stop(error)
  })
}