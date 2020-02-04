library(spectrolab)
library(tidyverse)
library(hsdar)

fieldSpecVI <- function(directory, outputName = "spectralLibrary") {
  tryCatch({
    ##Reads in spectral library....dim 1974  333
    spectralLibrary_HDW<-read.csv(directory)
    
    ####Lets run that test again
    tst<-lapply(spectralLibrary_HDW[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
    tst$V1%>%range()##There are no weird values, those are values outside of 0 and 2
    tst$V2%>%range()##There are no weird values, those are values outside of 0 and 2
    
    ##Reads in bandpasses for imagery to be used later
    HDW_ng_wv<-scan("output/Headwall_wv", numeric())
    
    ###you'll need to convert your dfs to a matrix before VIS can be applied
    ##lets fo this for df created from the image and our spectral library of scans
    spectralLibrary_HDW_matrix<-as.matrix(spectralLibrary_HDW[-1:-7])
    
    ##lets check the column attributes to see if any weird values were introduced
    spectralLibrary_HDW_matrix%>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()
    
    ##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
    spectralLibrary_HDW_speclib<-speclib(spectralLibrary_HDW_matrix,HDW_ng_wv)
    
    ##creates a vectror of names of all the vegitation indices...there are 115 of these
    VIs<-vegindex()
    VIs<-VIs[-58] ##Vegitation indices mREIP won't work so remove it from list
    
    ##Creates dataframe with Vegitation indices
    spectralLibrary_HDW_VIs<-vegindex(spectralLibrary_HDW_speclib,index=VIs)
    
    ##lets do a logical test on spectralLibrary_HDW_VIs to see if strange values exist
    test4<-lapply(spectralLibrary_HDW_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
    #test4%>%View()##There are columns where NaNs exist because the spectral range of the sensor is 400nm-100nm
    ##Remember the field spectral library was resampled on the headwall sensor's bandpasses
    ##This means some Veg indices won't generate values because those bands are not present
    ##Lets remove all those columns with values that have NaNs and Infs
    spectralLibrary_HDW_VIs<-spectralLibrary_HDW_VIs%>%dplyr::select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI
                                                                 ,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-'SWIR FI',-'SWIR LI',-'SWIR SI',-'SWIR VI')
    
    ##we need to combine the other columns with our new VI variables
    spectralLibrary_HDW_VIs<-cbind(spectralLibrary_HDW[1:7],spectralLibrary_HDW_VIs)
    
    Newcolnames<-c("Boochs"        ,"Boochs2"       ,"CARI"          ,"Carter"        ,"Carter2"      
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
    
    colnames(spectralLibrary_HDW_VIs)[-1:-7]<-Newcolnames
    
    ##lets do a logical test again
    test5<-lapply(spectralLibrary_HDW_VIs[-1:-7], range)%>%as.data.frame%>%t()%>%as.data.frame()
    test5%>%View()###There are no NaNs or Infs
    
    ##Now that we have our VIs calculated we can go ahead and export these dataframes
    write.csv(spectralLibrary_HDW_VIs, paste(paste("output/hdwSpectralLibraries/", outputName, sep = ""), "_HDW_VIs_equal25.csv", sep = ""), row.names = FALSE)
    
  }, warning = function(warning) {
    message <- paste("WARNING - While processing VIs", directory)
    message <- paste(message, warning, sep = " : ")
    print(message)
  }, error = function(error) {
    message <- paste("ERROR - While processing VIs", directory)
    message <- paste(message, error, sep = " : ")
    print(message)
  })
}