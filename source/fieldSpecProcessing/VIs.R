library(spectrolab)
library(tidyverse)
library(hsdar)

fieldSpecVI <- function(directory, outputName = "spectralLibrary") {
  tryCatch({
    ##Reads in spectral library....dim 1974  333
    spectralLibrary<-read.csv(directory)
    
    ##Reads in bandpasses for imagery to be used later
    ng_wv<-scan("output/WV", numeric())
    
    ###you'll need to convert your dfs to a matrix before VIS can be applied
    ##lets fo this for df created from the image and our spectral library of scans
    spectralLibrary_matrix<-as.matrix(spectralLibrary[-1:-7])
    
    ##lets check the column attributes to see if any weird values were introduced
    spectralLibrary_matrix%>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()
    
    ##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
    spectralLibrary_speclib<-speclib(spectralLibrary_matrix ,ng_wv)
    
    ##creates a vectror of names of all the vegitation indices...there are 115 of these
    VIs<-vegindex()
    VIs<-VIs[-58] ##Vegitation indices mREIP won't work so remove it from list
    
    ##Creates dataframe with Vegitation indices
    spectralLibrary_VIs<-vegindex(spectralLibrary_speclib,index=VIs)
    
    ##Remember the field spectral library was resampled on the sensor's bandpasses
    ##This means some Veg indices won't generate values because those bands are not present
    ##Lets remove all those columns with values that have NaNs and Infs
    spectralLibrary_VIs<-spectralLibrary_VIs%>%dplyr::select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI
                                                                 ,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-'SWIR FI',-'SWIR LI',-'SWIR SI',-'SWIR VI')
    
    ##we need to combine the other columns with our new VI variables
    spectralLibrary_VIs<-cbind(spectralLibrary[1:7],spectralLibrary_VIs)
    
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
    
    colnames(spectralLibrary_VIs)[-1:-7]<-Newcolnames
    
    ##Now that we have our VIs calculated we can go ahead and export these dataframes
    write.csv(spectralLibrary_VIs, paste(paste("output/outputSpectralLibraries/", outputName, sep = ""), "_VIs_equal25.csv", sep = ""), row.names = FALSE)
    
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
