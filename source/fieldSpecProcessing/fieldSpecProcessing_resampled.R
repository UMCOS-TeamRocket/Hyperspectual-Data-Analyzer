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
    
    ####Lets run logical test for both datarames
    tst2<-lapply(spectralLibrary_HDW_010nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
    tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
    tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2
    #tst2 %>% subset(V1 <0) %>% View()
    
    #spectralLibrary_HDW_05nm[-1:-7] %>% #dim() ] 1975  333
    #  dplyr::select(`444.444`) %>% 
    #  subset(`444.444`<0) %>% nrow() ##There is only one row here that has negative values, we could try this on multiple columns
    ##all those columns that we know have rows that have negative values
    
    ##Lets remove this row
    #spectralLibrary_HDW_05nm<-spectralLibrary_HDW_05nm%>%subset(`444.444`>0) ##dim()  1974  333
    ##you could run logical test above just to check the dataset before moving on
    
    ####Lets run that test on "spectralLibrary_HDW_10nm"
    tst3<-lapply(spectralLibrary_HDW_050nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
    tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
    tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2
    #tst3 %>% subset(V1 <0) %>% View() ##There a bunch of negative values across 128 columns, this might be one row, lets test this
    
    ####Lets run that test on "spectralLibrary_HDW_10nm"
    tst4<-lapply(spectralLibrary_HDW_100nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
    tst4$V1%>%range()##There are no weird values, those are values outside of 0 and 2
    tst4$V2%>%range()##There are no weird values, those are values outside of 0 and 2
    #tst4 %>% subset(V1 <0) %>% View() ##There a bunch of negative values across 128 columns, this might be one row, lets test this
    
    #spectralLibrary_HDW_10nm[-1:-7] %>% #dim() ] 1975  333
    #  dplyr::select(`529.444`) %>% 
    #  subset(`529.444`<0) %>% nrow() ##There is only one row here that has negative values, we could try this on multiple columns
    ##all those columns that we know have rows that have negative values
    
    ###Lets save our new dfs
    write.csv(spectralLibrary_HDW_010nm, paste(paste("output/hdwImagery/", outputName, sep = ""), "_HDW_010nm_equal25.csv", sep = ""), row.names = FALSE)
    write.csv(spectralLibrary_HDW_050nm, paste(paste("output/hdwImagery/", outputName, sep = ""), "_HDW_050nm_equal25.csv", sep = ""), row.names = FALSE)
    write.csv(spectralLibrary_HDW_100nm, paste(paste("output/hdwImagery/", outputName, sep = ""), "_HDW_100nm_equal25.csv", sep = ""), row.names = FALSE)
    
  }, warning = function(warning) {
    message <- paste("WARNING - While resampling", directory)
    message <- paste(message, warning, sep = " : ")
    print(message)
  }, error = function(error) {
    message <- paste("ERROR - While resampling", directory)
    message <- paste(message, error, sep = " : ")
    print(message)
  })
}