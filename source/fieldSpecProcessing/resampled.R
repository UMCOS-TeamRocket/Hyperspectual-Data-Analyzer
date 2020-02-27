####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)

fieldSpecResampled <- function(directory, outputName = "spectralLibrary") {
  tryCatch({
    ##Reads in spectral library as a dataframe
    ##this is the spectral library that had all uncalibrated bands removed
    spectralLibrary_HDW_df<-read.csv(directory, check.names = F)
    
    spectralLibrary_HDW<-spectralLibrary_HDW_df[-1:-7]%>%as.spectra()
    
    ##Now lets resample every 5nm and 10nm
    ##we need to do this for the image as well
    spectralLibrary_HDW_010nm<-spectralLibrary_HDW%>%spectrolab::resample(seq(399.444,899.424,10 )) %>%as.data.frame()%>%dplyr::select(-sample_name)
    spectralLibrary_HDW_050nm<-spectralLibrary_HDW%>%spectrolab::resample(seq(399.444,899.424,50 )) %>%as.data.frame()%>%dplyr::select(-sample_name)
    spectralLibrary_HDW_100nm<-spectralLibrary_HDW%>%spectrolab::resample(seq(399.444,899.424,100))%>%as.data.frame()%>%dplyr::select(-sample_name)
    
    spectralLibrary_HDW_010nm<-cbind(spectralLibrary_HDW_df[1:7],spectralLibrary_HDW_010nm)
    spectralLibrary_HDW_050nm<-cbind(spectralLibrary_HDW_df[1:7],spectralLibrary_HDW_050nm)
    spectralLibrary_HDW_100nm<-cbind(spectralLibrary_HDW_df[1:7],spectralLibrary_HDW_100nm)
    
    ###Lets save our new dfs
    write.csv(spectralLibrary_HDW_010nm, paste(paste("output/hdwSpectralLibraries/", outputName, sep = ""), "_HDW_010nm_equal25.csv", sep = ""), row.names = FALSE)
    write.csv(spectralLibrary_HDW_050nm, paste(paste("output/hdwSpectralLibraries/", outputName, sep = ""), "_HDW_050nm_equal25.csv", sep = ""), row.names = FALSE)
    write.csv(spectralLibrary_HDW_100nm, paste(paste("output/hdwSpectralLibraries/", outputName, sep = ""), "_HDW_100nm_equal25.csv", sep = ""), row.names = FALSE)
    
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
