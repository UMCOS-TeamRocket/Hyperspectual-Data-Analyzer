#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

#directories: a character vector containing the directories to HDW 010nm, 050nm, 100nm, and VIs .csv files
fieldSpecAllPreds <- function(directories, outputName = "spectralLibrary") {
  tryCatch({
  ###Reads in all predictors for scans
    for (directory in directories) {
      if (grepl("010nm", directory)) {
        HDW_010nm_equal25 <<- read.csv(directory)
      } else if (grepl("050nm", directory)) {
        HDW_050nm_equal25 <<- read.csv(directory)
      } else if (grepl("100nm", directory)) {
        HDW_100nm_equal25 <<- read.csv(directory)
      } else if (grepl("VIs", directory)) {
        HDW_VIs_equal25 <<- read.csv(directory)
      } 
    }
    
    ##Make names for colnames in each df unique
    colnames(HDW_010nm_equal25)[-1:-7]<-paste0(colnames(HDW_010nm_equal25)[-1:-7],"_010nm")
    colnames(HDW_050nm_equal25)[-1:-7]<-paste0(colnames(HDW_050nm_equal25)[-1:-7],"_050nm")
    colnames(HDW_100nm_equal25)[-1:-7]<-paste0(colnames(HDW_100nm_equal25)[-1:-7],"_100nm")
    colnames(HDW_VIs_equal25)[-1:-7]<-paste0(colnames(HDW_VIs_equal25)[-1:-7],"_VIs"  )
    
    ##Let's merge these dataframes
    dataHDW<-Reduce(cbind,list(HDW_010nm_equal25
                                              ,HDW_050nm_equal25[-1:-7]
                                              ,HDW_100nm_equal25[-1:-7]
                                              ,HDW_VIs_equal25[-1:-7]))
    ##Lets save this dataframe
    write.csv(dataHDW, paste(paste("output/hdwSpectralLibraries/", outputName, sep = ""), "_data_HDW.csv", sep = ""),row.names = FALSE)
    
  }, warning = function(warning) {
    message <- paste("WARNING - While processing all preds", warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste("ERROR - While processing all preds", error, sep = " : ")
    stop(message)
  })
}
