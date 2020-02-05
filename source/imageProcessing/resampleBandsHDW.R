####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

resampleBandsHDW <- function(imageDirectory, fileName = "image") {
  tryCatch({
    print(imageDirectory)
    ##Reads in image as dataframe 
    IMG_HDW<-brick(imageDirectory)%>%rasterToPoints()%>%as.data.frame()
  
    ##Reads in bandpasses for imagery to be used later
    HDW_ng_wv<-scan("output/hdwImagery/Headwall_wv", numeric())
  
    ##lets remove all those bads that had noise
    IMG_HDW[275:328]<-NULL
  
    ##change colnames to correct band names
    colnames(IMG_HDW)[-1:-2]<-HDW_ng_wv
  
    ##Remove all pixels with NA values
    IMG_HDW<-na.omit(IMG_HDW)
  
    ##Now lets check the range of the values in the image
    test<-lapply(IMG_HDW[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
    #test%>%View()
    test%>%lapply(range) ### All values fall between 0 and 1.2 and there are no NA values
  
    ##create a datframe with the coordinates for imagery to be used later
    cords<-IMG_HDW%>%dplyr::select(1,2)
  
    ##Lets remove this row
    #alaskaSpeclib_HDW_50nm<-alaskaSpeclib_HDW_50nm%>%subset(`529.444`>0) ##dim()  1974  333
    ##you could run logical test above just to check the dataset before moving on
    ##Do the same steps above for imagery
    IMG_HDW_010nm<-IMG_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,10 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
    IMG_HDW_050nm<-IMG_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,50 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
    IMG_HDW_100nm<-IMG_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,100))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
  
    ###Lets run logical test for all dataframes
    tst2<-lapply(IMG_HDW_010nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
    tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
    tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2
    #tst2%>%subset(V1<0)%>%view()
  
    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_HDW_010nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values
  
    ##Lets remove these rows
    IMG_HDW_010nm<-IMG_HDW_010nm%>%subset(`399.444`>0)
    IMG_HDW_010nm<-IMG_HDW_010nm%>%subset(`409.444`>0)
    IMG_HDW_010nm<-IMG_HDW_010nm%>%subset(`419.444`>0)
  
    ###Lets run that test on "IMG_HDW_50nm"
    tst3<-lapply(IMG_HDW_050nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
    tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
    tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2
    #tst3%>%subset(V1<0)%>%view()
  
    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_HDW_050nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values
  
    ##Lets remove these rows
    IMG_HDW_050nm<-IMG_HDW_050nm%>%subset(`399.444`>0)
    IMG_HDW_050nm<-IMG_HDW_050nm%>%subset(`409.444`>0)
    IMG_HDW_050nm<-IMG_HDW_050nm%>%subset(`419.444`>0)
  
    ###Lets run that test on "IMG_HDW_100nm"
    tst4<-lapply(IMG_HDW_100nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
    tst4$V1%>%range()##There are no weird values, those are values outside of 0 and 2
    tst4$V2%>%range()##There are no weird values, those are values outside of 0 and 2
    #tst4%>%subset(V1<0)%>%view()
  
    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_HDW_100nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values
  
    ##Lets remove these rows
    IMG_HDW_100nm<-IMG_HDW_100nm%>%subset(`399.444`>0)
  
    ###Lets save our new dfs
    write.csv(IMG_HDW       , paste(paste("output/hdwImagery/images/", fileName, sep = ""), "_HDW_df.csv", sep = ""), row.names = FALSE)
    write.csv(IMG_HDW_010nm , paste(paste("output/hdwImagery/images/", fileName, sep = ""), "_HDW_010nm.csv", sep = ""), row.names = FALSE)
    write.csv(IMG_HDW_050nm , paste(paste("output/hdwImagery/images/", fileName, sep = ""), "_HDW_050nm.csv", sep = ""), row.names = FALSE)
    write.csv(IMG_HDW_100nm , paste(paste("output/hdwImagery/images/", fileName, sep = ""), "_HDW_100nm.csv", sep = ""),row.names = FALSE)
    
    directories <- c(paste(paste("output/hdwImagery/images/", fileName, sep = ""), "_HDW_df.csv", sep = ""),
                     paste(paste("output/hdwImagery/images/", fileName, sep = ""), "_HDW_010nm.csv", sep = ""),
                     paste(paste("output/hdwImagery/images/", fileName, sep = ""), "_HDW_050nm.csv", sep = ""),
                     paste(paste("output/hdwImagery/images/", fileName, sep = ""), "_HDW_100nm.csv", sep = ""))
    
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