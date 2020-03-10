####################Calculates the resampled bands for the spectral library developed from bandpases####
library(spectrolab)
library(tidyverse)

fieldSpecResampled <- function(outputName = "spectralLibrary") {
  tryCatch({
    ##Reads in spectral library as a dataframe
    ##this is the spectral library that had all uncalibrated bands removed
    spectralLibrary_df<-read.csv("output/intermediateFiles/temp.csv", check.names = F)
    
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
    
    # directories <- list(equal25_010nm = paste(paste("output/intermediateFiles/spectralLibraries/", outputName, sep = ""), "_010nm_equal25.csv", sep = ""),
    #                     equal25_050nm = paste(paste("output/intermediateFiles/spectralLibraries/", outputName, sep = ""), "_050nm_equal25.csv", sep = ""),
    #                     equal25_100nm = paste(paste("output/intermediateFiles/spectralLibraries/", outputName, sep = ""), "_100nm_equal25.csv", sep = ""))
    # 
    # ###Lets save our new dfs
    # write.csv(spectralLibrary_010nm, paste(paste("output/intermediateFiles/spectralLibraries/", outputName, sep = ""), "_010nm_equal25.csv", sep = ""), row.names = FALSE)
    # write.csv(spectralLibrary_050nm, directories$equal25_050nm, row.names = FALSE)
    # write.csv(spectralLibrary_100nm, directories$equal25_100nm, row.names = FALSE)
    # 

    specList<-list(spectralLibrary_010nm,spectralLibrary_050nm,spectralLibrary_100nm)
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
