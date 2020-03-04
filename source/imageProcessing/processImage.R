source("source/imageProcessing/resampleBands.R")
source("source/imageProcessing/imageVIs.R")

processImage <- function (imageDirectory) {
  tryCatch({
    #get file name from directory
    fileName <- basename(imageDirectory)
    #remove file extension
    fileName <- substr(fileName, 1, nchar(fileName) - 4)
  
    print("Resampling")
    resampledDirectories <- resampleBands(imageDirectory, fileName)

    dfDirectory <- resampledDirectories[1]

    print("Generating VIs")
    ViDirectory <- createImgVi(resampledDirectories[["df"]], fileName)
    
    #Read in resampled files
    band_010nm<-read.csv(resampledDirectories[["nm10"]])
    band_050nm<-read.csv(resampledDirectories[["nm50"]])
    band_100nm<-read.csv(resampledDirectories[["nm100"]])
    VI<-read.csv(ViDirectory)
    
    #Change coloumn names
    colnames(band_010nm)[-1:-2]<-paste0(colnames(band_010nm)[-1:-2],"_010nm")
    colnames(band_050nm)[-1:-2]<-paste0(colnames(band_050nm)[-1:-2],"_050nm")
    colnames(band_100nm)[-1:-2]<-paste0(colnames(band_100nm)[-1:-2],"_100nm")
    colnames(VI)[-1:-2]<-paste0(colnames(VI)[-1:-2],"_VIs")

    #Combine all of the files
    data<-Reduce(cbind,list(band_010nm,band_050nm[-1:-2],band_100nm[-1:-2],VI[-1:-2]))
    write.csv(data, paste("output/imagery/", fileName, "_data.csv", sep = ""), row.names = FALSE)
  
    #Send to predict
    return(paste("output/imagery/", fileName, "_data.csv", sep = ""))
  }, warning = function(warning) {
    warning(warning)
    message <- paste ("WARNING - While process")
  }, error = function(error) {
    message <- paste ("WARNING - While process")
    stop(error)
  })
}