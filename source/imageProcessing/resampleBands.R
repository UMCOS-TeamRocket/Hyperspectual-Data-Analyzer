####################Calculates the resampled bands for the spectral library developed from bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)
library(foreach)
library(doParallel)

#Function to run resampling multithreaded
multiSpectrolab<- function(IMG, band){
  IMG<-spectrolab::resample(IMG, seq(399.444,899.424,band))
  return(IMG)
}

#Function to turn spectra into a dataframe
toDataFrame<-function(IMG_ALL, cords, i){
  #Multithreading puts data into one matrix
  #These two lines splits it back up
  IMG<-as.data.frame(IMG_ALL[1,i])
  colnames(IMG)<-c(unlist(IMG_ALL[2,i]))
  
  IMG<-IMG%>%cbind(cords)
  IMG<-IMG%>%dplyr::select(x,y,everything())
  return(IMG)
}

#params:
##imageDirectry: string
#
#output:
##bandList: list of R objects. length of 4

resampleBands <- function(imageDirectory) {
  tryCatch({
    ##Reads in image as dataframe 
    IMG<-brick(imageDirectory)%>%rasterToPoints()%>%as.data.frame()
  
    ##Reads in bandpasses for imagery to be used later
    ng_wv<-scan("output/intermediateFiles/WV", numeric())
  
    ##lets remove all those bads that had noise
    IMG[275:328]<-NULL
  
    ##change colnames to correct band names
    colnames(IMG)[-1:-2]<-ng_wv
  
    ##Remove all pixels with NA values
    IMG<-na.omit(IMG)
  
    ##create a datframe with the coordinates for imagery to be used later
    cords<-IMG%>%dplyr::select(1,2)
  
    
    IMG_resamp<-IMG%>%dplyr::select(-x,-y)
    IMG_resamp<-spectrolab::as.spectra(IMG_resamp)
    

    #Bbands for images
    bands<-c(10,50,100)
    
    #Set amount of cores to use
    cores <- detectCores()
    if(cores>2){
      cores<-3
    } 
    c1<- makeCluster(cores)
    registerDoParallel(c1)
    tme<- Sys.time()
    
    #Run resampling in parallel
    IMG_ALL<-foreach(i=1:3, .combine=cbind, .packages='spectrolab') %dopar%{
      a<-spectrolab::resample(IMG_resamp, seq(399.444,899.424,bands[i]))
    }
    print(Sys.time()-tme)
    stopCluster(c1)
    
    #Convert everything to a dataframe
    IMG_010nm<-toDataFrame(IMG_ALL, cords, 1)
    IMG_050nm<-toDataFrame(IMG_ALL, cords, 2)
    IMG_100nm<-toDataFrame(IMG_ALL, cords, 3)

    
    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_010nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values

    ##Remove these rows
    IMG_010nm<-IMG_010nm%>%subset(`399.444`>0)

    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_050nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values

    ##Remove these rows
    IMG_050nm<-IMG_050nm%>%subset(`399.444`>0)

    #Don't need to run 3 lines below unless there are weird values in dataset above
    IMG_100nm[-1:-2]%>%
      dplyr::select(`399.444`)%>% 
      subset(`399.444`<0)%>% nrow() ##2 rows have negative values
  
    ##Remove these rows
    IMG_100nm<-IMG_100nm%>%subset(`399.444`>0)
    IMG <-IMG %>% slice(1:nrow(IMG_010nm))
    
    bandList<-list(IMG = IMG, 
                   IMG_010nm = IMG_010nm,
                   IMG_050nm = IMG_050nm,
                   IMG_100nm = IMG_100nm)
    

    return(bandList)
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