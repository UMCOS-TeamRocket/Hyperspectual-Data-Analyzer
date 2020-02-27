####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

resampleBandsHDW <- function(imageDirectory, fileName = "image") {
  tryCatch({
    ##Reads in image as dataframe 
    IMG_HDW<-brick(imageDirectory)%>%rasterToPoints()%>%as.data.frame()
  
    ##Reads in bandpasses for imagery to be used later
    HDW_ng_wv<-scan("output/Headwall_wv", numeric())
  
    ##lets remove all those bads that had noise
    IMG_HDW[275:328]<-NULL
  
    ##change colnames to correct band names
    colnames(IMG_HDW)[-1:-2]<-HDW_ng_wv
  
    ##Remove all pixels with NA values
    IMG_HDW<-na.omit(IMG_HDW)
  
    ##create a datframe with the coordinates for imagery to be used later
    cords<-IMG_HDW%>%dplyr::select(1,2)
  
    #TODO
    #Make these lines more efficient
    ##Do the same steps above for imagery
    IMG_HDW_010nm<-IMG_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,10 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
    IMG_HDW_050nm<-IMG_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,50 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
    IMG_HDW_100nm<-IMG_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,100))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
  
    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_HDW_010nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values

    ##Lets remove these rows
    IMG_HDW_010nm<-IMG_HDW_010nm%>%subset(`399.444`>0)

    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_HDW_050nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values

    ##Lets remove these rows
    IMG_HDW_050nm<-IMG_HDW_050nm%>%subset(`399.444`>0)

    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_HDW_100nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values
  
    ##Remove these rows
    IMG_HDW_100nm<-IMG_HDW_100nm%>%subset(`399.444`>0)
    IMG_HDW <-IMG_HDW %>% slice(1:nrow(IMG_HDW_010nm))
    
    ###Lets save our new df
    write.csv(IMG_HDW       , paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_df.csv", sep = ""), row.names = FALSE)
    write.csv(IMG_HDW_010nm , paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_010nm.csv", sep = ""), row.names = FALSE)
    write.csv(IMG_HDW_050nm , paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_050nm.csv", sep = ""), row.names = FALSE)
    write.csv(IMG_HDW_100nm , paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_100nm.csv", sep = ""),row.names = FALSE)

    directories <- list(df = paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_df.csv", sep = ""),
                     nm10 = paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_010nm.csv", sep = ""),
                     nm50 = paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_050nm.csv", sep = ""),
                     nm100 = paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_100nm.csv", sep = ""))
    return(directories)
  }, warning = function(warning) {
    message <- paste ("WARNING - While resampling bands", imageDirectory)
    message <- paste(message, warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste ("ERROR - While resampling bands", imageDirectory)
    message <- paste(message, error, sep = " : ")
    stop(message)
  })
  
}