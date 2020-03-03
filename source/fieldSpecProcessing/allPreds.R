#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

#directories: a character vector containing the directories to 010nm, 050nm, 100nm, and VIs .csv files
fieldSpecAllPreds <- function(directories, outputName = "spectralLibrary") {
  tryCatch({
  ###Reads in all predictors for scans
    for (directory in directories) {
      if (grepl("010nm", directory)) {
        equal25_010nm <<- read.csv(directory)
      } else if (grepl("050nm", directory)) {
        equal25_050nm <<- read.csv(directory)
      } else if (grepl("100nm", directory)) {
        equal25_100nm <<- read.csv(directory)
      } else if (grepl("VIs", directory)) {
        equal25_VIs <<- read.csv(directory)
      } 
    }
    
    ##Make names for colnames in each df unique
    colnames(equal25_010nm)[-1:-7]<-paste0(colnames(equal25_010nm)[-1:-7],"_010nm")
    colnames(equal25_050nm)[-1:-7]<-paste0(colnames(equal25_050nm)[-1:-7],"_050nm")
    colnames(equal25_100nm)[-1:-7]<-paste0(colnames(equal25_100nm)[-1:-7],"_100nm")
    colnames(equal25_VIs)[-1:-7]<-paste0(colnames(equal25_VIs)[-1:-7],"_VIs"  )
    
    ##Let's merge these dataframes
    data<-Reduce(cbind,list(equal25_010nm
                                              ,equal25_050nm[-1:-7]
                                              ,equal25_100nm[-1:-7]
                                              ,equal25_VIs[-1:-7]))
    ##Lets save this dataframe
    write.csv(data, paste(paste("output/outputSpectralLibraries/", outputName, sep = ""), "_data.csv", sep = ""),row.names = FALSE)
    
  }, warning = function(warning) {
    message <- paste("WARNING - While processing all preds", warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste("ERROR - While processing all preds", error, sep = " : ")
    stop(message)
  })
}
