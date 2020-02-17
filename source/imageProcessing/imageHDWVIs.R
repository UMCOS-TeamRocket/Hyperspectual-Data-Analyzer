####################Calculates the Vegitation indices for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

createImgHDWVi <- function(imgHdwDfDirectory, fileName = "image") {
  tryCatch({
    ##Reads in image as dataframe
    IMG_HDW<-read.csv(imgHdwDfDirectory, check.names = FALSE)
    ##Now lets check the range of the values in the image
    test<-lapply(IMG_HDW[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
    #test%>%View()
    test%>%lapply(range) ### All values fall between 0 and 1.2 and there are no NA values
    
    ##Reads in bandpasses for imagery to be used later
    HDW_ng_wv<-scan("output/Headwall_wv", numeric())
    
    ###you'll need to convert your dfs to a matrix before VIS can be applied
    ##lets fo this for df created from the image and our spectral library of scans
    IMG_HDW_matrix   <-as.matrix(IMG_HDW   [-1:-2])
    
    ##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
    IMG_HDW_speclib   <-speclib  (IMG_HDW_matrix   ,HDW_ng_wv[1:272])
    
    ##creates a vectror of names of all the vegitation indices...there are 115 of these
    VIs<-vegindex()
    #VIs<-VIs[-58] ##Vegitation indices mREIP won't work so remove it from list
    
    ##Vegitation indices mREIP won't work so remove it from list
    VIs<-VIs[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]
    
    ##Creates dataframe with Vegitation indices
    IMG_VIs       <-vegindex(IMG_HDW_speclib       ,index=VIs)
    
    ##rename columns
    colnames(IMG_VIs  )<-VIs
    
    ##lets do a logical test on IMG_HDW_VIs to see if strange values exist
    test3<-lapply(IMG_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
   
    IMG_VIs_A  <-cbind(IMG_HDW   [1:2],IMG_VIs   )
    
    ##Now we have to ensure that all column names have no spaces nor arithmetic operators
    newcolnames<-c("Boochs"        ,"Boochs2"       ,"CARI"          ,"Carter"        ,"Carter2"      
                   ,"Carter3"       ,"Carter4"       ,"Carter5"       ,"Carter6"       ,"CI"            ,"CI2"           ,"ClAInt"       
                   ,"CRI1"          ,"CRI2"          ,"CRI3"          ,"CRI4"          ,"D1"            ,"D2"            ,"Datt"         
                   ,"Datt2"         ,"Datt3"         ,"Datt4"         ,"Datt5"         ,"Datt6"         ,"DD"            ,"DDn"          
                   ,"DPI"           ,"DWSI4"         ,"EGFN"          ,"EGFR"          ,"EVI"           ,"GDVI_2"        ,"GDVI_3"       
                   ,"GDVI_4"        ,"GI"            ,"Gitelson"      ,"Gitelson2"     ,"GMI1"          ,"GMI2"          ,"GreenNDVI"   
                   ,"Maccioni"      ,"MCARI"         ,"MCARIOSAVI"    ,"MCARI2"        ,"MCARI2OSAVI2"  ,"mND705"        ,"mNDVI"        
                   ,"MPRI"          ,"MSAVI"         ,"mSR"           ,"mSR2"          ,"mSR705"        ,"MTCI"          ,"MTVI"         
                   ,"NDVI"          ,"NDVI2"         ,"NDVI3"         ,"NPCI"          ,"OSAVI"         ,"OSAVI2"        ,"PARS"         
                   ,"PRI"           ,"PRICI2"        ,"PRI_norm"      ,"PSND"          ,"PSRI"          ,"PSSR"          ,"RDVI"         
                   ,"REP_LE"        ,"REP_Li"        ,"SAVI"          ,"SIPI"          ,"SPVI"          ,"SR"            ,"SR1"          
                   ,"SR2"           ,"SR3"           ,"SR4"           ,"SR5"           ,"SR6"           ,"SR7"           ,"SR8"          
                   ,"SRPI"          ,"Sum_Dr1"       ,"Sum_Dr2"       ,"TCARI"         ,"TCARIOSAVI"    ,"TCARI2"        ,"TCARI2OSAVI2"
                   ,"TGI"           ,"TVI"           ,"Vogelmann"     ,"Vogelmann2"    ,"Vogelmann3"    ,"Vogelmann4") 
    
    colnames(IMG_VIs_A )[-1:-2]<-newcolnames
    ##DIM725940 274
    
    ##lets do a logical test again to the image 
    test6<-lapply(IMG_VIs_A[-1:-2], range)%>%as.data.frame%>%t()%>%as.data.frame()
    test6%>%view()###There are NaNs and infs, lets remove them, dim() 1974  333
    
   
    ##Now that we have our VIs calculated we can go ahead and export these dataframes
    write.csv(IMG_VIs_A, paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_VIs.csv", sep = ""))
    
    return(paste(paste("output/hdwImagery/", fileName, sep = ""), "_HDW_VIs.csv", sep = ""))
  }, warning = function(warning) {
    message <- paste ("WARNING - While creating image VI", imgHdwDfDirectory)
    message <- paste(message, warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste ("ERROR - While creating image VI", imgHdwDfDirectory)
    message <- paste(message, error, sep = " : ")
    stop(message)
  })
}