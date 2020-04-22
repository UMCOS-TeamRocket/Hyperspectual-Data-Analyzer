#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

#params:
##resampled: list of length 3. contains R objects for 10nm, 50nm, and 100nm
##vi: R Object
##outputname: string. used to name the output file
#
#output: function does not return a value. writes spectralLibrary to a .csv file in output/spectralLibraries/
##output/spectralLibraries/ is a directory we look in for any created spectral libraries on the front end

fieldSpecAllPreds <- function(resampled, vi, outputName = "spectralLibrary") {
  tryCatch({
    #Set variables to the Spectal Lists
    equal25_010nm <- resampled[[1]]
    equal25_050nm <- resampled[[2]]
    equal25_100nm <- resampled[[3]]
    equal25_VIs <- vi
    
    ##Make names for colnames in each df unique
    colnames(equal25_010nm)[-1:-7]<-paste0(colnames(equal25_010nm)[-1:-7],"_010nm")
    colnames(equal25_050nm)[-1:-7]<-paste0(colnames(equal25_050nm)[-1:-7],"_050nm")
    colnames(equal25_100nm)[-1:-7]<-paste0(colnames(equal25_100nm)[-1:-7],"_100nm")
    colnames(equal25_VIs)[-1:-7]<-paste0(colnames(equal25_VIs)[-1:-7],"_VIs"  )

    ##Let's merge these dataframes
    data<-Reduce(cbind,list(equal25_010nm, equal25_050nm[-1:-7], equal25_100nm[-1:-7], equal25_VIs[-1:-7]))
    
    ##Lets save this dataframe
    write.csv(data, paste(paste("output/spectralLibraries/", outputName, sep = ""), "_data.csv", sep = ""),row.names = FALSE)
  }, warning = function(warning) {
    message <- paste("WARNING - While processing all preds", warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste("ERROR - While processing all preds", error, sep = " : ")
    stop(message)
  })
}
