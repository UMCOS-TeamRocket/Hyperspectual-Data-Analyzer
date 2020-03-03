####################Calculates the resampled bands for the spectral library developed from bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

multiSpectrolab<- function(IMG, band){
  IMG<-spectrolab::resample(IMG, seq(399.444,899.424,band))
  return(IMG)
}

toDataFrame<-function(IMG, cords){
  IMG<-as.data.frame(IMG)
  IMG<-IMG%>%cbind(cords)
  IMG<-IMG%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
  return(IMG)
}


resampleBands <- function(imageDirectory, fileName = "image") {
  tryCatch({
    ##Reads in image as dataframe 
    IMG<-brick(imageDirectory)%>%rasterToPoints()%>%as.data.frame()
  
    ##Reads in bandpasses for imagery to be used later
    ng_wv<-scan("output/WV", numeric())
  
    ##lets remove all those bads that had noise
    IMG[275:328]<-NULL
  
    ##change colnames to correct band names
    colnames(IMG)[-1:-2]<-ng_wv
  
    ##Remove all pixels with NA values
    IMG<-na.omit(IMG)
  
    ##create a datframe with the coordinates for imagery to be used later
    cords<-IMG%>%dplyr::select(1,2)
  
    #TODO
    #Make these lines more efficient
    ##Do the same steps above for imagery
    
    IMG_resamp<-IMG%>%dplyr::select(-x,-y)
    IMG_resamp<-spectrolab::as.spectra(IMG_resamp)
    
    #SLOW ONE
    tme<- Sys.time()
    IMG_010nm<-multiSpectrolab(IMG_resamp,10)
    IMG_050nm<-multiSpectrolab(IMG_resamp,50)
    IMG_100nm<-multiSpectrolab(IMG_resamp,100)
    print(Sys.time()-tme)

    IMG_010nm<-toDataFrame(IMG_010nm, cords)
    IMG_050nm<-toDataFrame(IMG_050nm, cords)
    IMG_100nm<-toDataFrame(IMG_100nm, cords)

    
    
    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_010nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values

    ##Lets remove these rows
    IMG_010nm<-IMG_010nm%>%subset(`399.444`>0)

    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_050nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values

    ##Lets remove these rows
    IMG_050nm<-IMG_050nm%>%subset(`399.444`>0)

    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_100nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values
  
    ##Remove these rows
    IMG_100nm<-IMG_100nm%>%subset(`399.444`>0)
    IMG <-IMG %>% slice(1:nrow(IMG_010nm))
    
    ###Lets save our new df
    write.csv(IMG       , paste(paste("output/imagery/", fileName, sep = ""), "_df.csv", sep = ""), row.names = FALSE)
    write.csv(IMG_010nm , paste(paste("output/imagery/", fileName, sep = ""), "_010nm.csv", sep = ""), row.names = FALSE)
    write.csv(IMG_050nm , paste(paste("output/imagery/", fileName, sep = ""), "_050nm.csv", sep = ""), row.names = FALSE)
    write.csv(IMG_100nm , paste(paste("output/imagery/", fileName, sep = ""), "_100nm.csv", sep = ""),row.names = FALSE)

    directories <- list(df = paste(paste("output/imagery/", fileName, sep = ""), "_df.csv", sep = ""),
                     nm10 = paste(paste("output/imagery/", fileName, sep = ""), "_010nm.csv", sep = ""),
                     nm50 = paste(paste("output/imagery/", fileName, sep = ""), "_050nm.csv", sep = ""),
                     nm100 = paste(paste("output/imagery/", fileName, sep = ""), "_100nm.csv", sep = ""))
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