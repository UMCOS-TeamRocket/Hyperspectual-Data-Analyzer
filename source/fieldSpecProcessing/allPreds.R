#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

#directories: a character vector containing the directories to HDW 010nm, 050nm, 100nm, and VIs .csv files
fieldSpecAllPreds <- function(directories, outputName = "spectralLibrary") {
  tryCatch({
    #cannot assign these variables a 1-dimensional
    #alaskaSpeclib_HDW_010nm_equal25 <- ""
    #alaskaSpeclib_HDW_050nm_equal25 <- ""
    #alaskaSpeclib_HDW_100nm_equal25 <- ""
    #alaskaSpeclib_HDW_VIs_equal25 <- ""
    
    ###Reads in all predictors for scans
    for (directory in directories) {
      if (grepl("010nm", directory)) {
        alaskaSpeclib_HDW_010nm_equal25 <<- read.csv(directory)
      } else if (grepl("050nm", directory)) {
        alaskaSpeclib_HDW_050nm_equal25 <<- read.csv(directory)
      } else if (grepl("100nm", directory)) {
        alaskaSpeclib_HDW_100nm_equal25 <<- read.csv(directory)
      } else if (grepl("VIs", directory)) {
        alaskaSpeclib_HDW_VIs_equal25 <<- read.csv(directory)
      } 
    }
    
    ##Make names for colnames in each df unique
    colnames(alaskaSpeclib_HDW_010nm_equal25)[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_010nm_equal25)[-1:-7],"_010nm")
    colnames(alaskaSpeclib_HDW_050nm_equal25)[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_050nm_equal25)[-1:-7],"_050nm")
    colnames(alaskaSpeclib_HDW_100nm_equal25)[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_100nm_equal25)[-1:-7],"_100nm")
    colnames(alaskaSpeclib_HDW_VIs_equal25  )[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_VIs_equal25  )[-1:-7],"_VIs"  )
    
    ##Let's merge these dataframes
    alaskaSpecLib_data_HDW<-Reduce(cbind,list(alaskaSpeclib_HDW_010nm_equal25
                                              ,alaskaSpeclib_HDW_050nm_equal25[-1:-7]
                                              ,alaskaSpeclib_HDW_100nm_equal25[-1:-7]
                                              ,alaskaSpeclib_HDW_VIs_equal25  [-1:-7]))
    ##Lets save this dataframe
    write.csv(alaskaSpecLib_data_HDW, paste(paste("output/hdwSpectralLibraries/", outputName, sep = ""), "_data_HDW.csv", sep = ""),row.names = FALSE)
    
  }, warning = function(warning) {
    message <- paste("WARNING - While processing all preds", warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste("ERROR - While processing all preds", error, sep = " : ")
    stop(message)
  })
}
