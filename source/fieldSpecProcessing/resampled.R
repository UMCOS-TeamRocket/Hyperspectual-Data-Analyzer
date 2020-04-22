####################Calculates the resampled bands for the spectral library developed from bandpases####
library(spectrolab)
library(tidyverse)

#params:
##spectralLibraryDfDirectory: string
#
#output: list of spectralLibrary objects. length of 3. list(nm10, nm50, nm100)

fieldSpecResampled <- function(spectralLibraryDfDirectory) {
  tryCatch({
    ##Reads in spectral library as a dataframe
    ##this is the spectral library that had all uncalibrated bands removed
    spectralLibrary_df<-read.csv(spectralLibraryDfDirectory, check.names = F)
    
    #spectralLibrary_df<-directory
    
    spectralLibrary<-spectralLibrary_df[-1:-7]%>%as.spectra()
    
    ##Now lets resample every 5nm and 10nm
    ##we need to do this for the image as well
    spectralLibrary_010nm<-spectralLibrary%>%spectrolab::resample(seq(399.444,899.424,10 )) %>%as.data.frame()%>%dplyr::select(-sample_name)
    spectralLibrary_050nm<-spectralLibrary%>%spectrolab::resample(seq(399.444,899.424,50 )) %>%as.data.frame()%>%dplyr::select(-sample_name)
    spectralLibrary_100nm<-spectralLibrary%>%spectrolab::resample(seq(399.444,899.424,100))%>%as.data.frame()%>%dplyr::select(-sample_name)
    
    spectralLibrary_010nm<-cbind(spectralLibrary_df[1:7],spectralLibrary_010nm)
    spectralLibrary_050nm<-cbind(spectralLibrary_df[1:7],spectralLibrary_050nm)
    spectralLibrary_100nm<-cbind(spectralLibrary_df[1:7],spectralLibrary_100nm)

    specList<-list(nm10 = spectralLibrary_010nm,
                   nm50 = spectralLibrary_050nm,
                   nm100 = spectralLibrary_100nm)
    
    return(specList)
    
  }, warning = function(warning) {
    message <- paste("WARNING - While resampling", directory)
    message <- paste(message, warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste("ERROR - While resampling", directory)
    message <- paste(message, error, sep = " : ")
    stop(message)
  })
}
