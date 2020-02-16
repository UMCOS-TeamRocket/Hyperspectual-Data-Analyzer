library(spectrolab)
library(ranger)
library(raster)
library(tidyverse)
library(hsdar)
library(parallel)

predictFunction <- function(classifierDirectory, imageDirectory, hdwDirectory, outputName) {
  tryCatch({
    c1 <- detectCores()-2
    print(c1)
    #print(hdwDirectory)
    print(imageDirectory)
    
    #Reads in Imagery
    image<-brick(imageDirectory)
    
    ##Converts to a dataframe
    imageLatLong<-rasterToPoints(image)%>% as.data.frame()
    imageLatLong[275:328]<-NULL
    imageLatLong<-na.omit(imageLatLong)
    imageLatLong<-imageLatLong[-c(449905, 521215), ]
    
    #dataHDW <-hdwDirectory
    dataHDW<-read.csv(hdwDirectory)
    
    ##Marks raster as unrotated
    image@rotated<-FALSE

    #Read in classifier
    classifier <- readRDS(classifierDirectory)
    temp<-read.csv("output/hdwSpectralLibraries/library_data_HDW.csv")
    print("break")
   dataHDW<- select(dataHDW,-c(y_VIs))
    ##Save the confusion Matrix for these models
    confusionMatrix<-classifier$confusion%>%as.data.frame()
    write.csv(confusionMatrix,"output/ConfusionMatrix",row.names = F)
    #print(dataHDW)
    ##uses model from spectral library to predict images
    Results <-predict(classifier, dataHDW[-1:-2], num.threads = c1)
    
    tmp <- Results$predictions
    Results<-as.data.frame(tmp)%>%'names<-'("predicted")
    
    colnames(Results) <- c("predicted")
    ##converts prediction from rf model to dataframe and changes column name to predicted
    #Results<-as.data.frame(Results)%>%'names<-'("predicted")
    
    ## Grabs x, y values from original image and combines with unique values from prediction
    #imageLatLong<-imageLatLong %>% slice(1: nrow(Results))
    print(nrow(imageLatLong))
    print(nrow(dataHDW))
    print(nrow(temp))
    print(nrow(image))
    Results<-cbind(Results,imageLatLong[1:2]) %>% dplyr::select(predicted,x,y)
    print("2")
    ###Creates Unique PFT_IDs
    Unique<-unique(as.data.frame.complex(Results$predicted))

    Unique$PFT_ID<-seq(1:nrow(Unique))

    names(Unique)[1]<-"predicted"
    
    ###Create dataframe with unique PFT_ID values and location info
    
    Results<-merge(Results,Unique, by="predicted")%>% dplyr::select(x,y,PFT_ID)

    ##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
    suppressWarnings(raster<-rasterFromXYZ(Results, crs = crs(image)))
    
    Graminoid <-raster==1
    dwarfShrub<-raster==2
    moss      <-raster==3
    forb      <-raster==4
    lichen    <-raster==5
    shrub     <-raster==6
    tree      <-raster==7
    
    ##We need to change all those values within the raster to 1, 
    ##so the sum of all the pixels in each quadrat can be calculated later
    denom  <-raster>=1
    
    ##DF OF METEDATA
  
    

    
   
    
    ###########################################Plot 1############################################################
    ###save plot as a jpeg
    chm_colors <- c("darkgreen","chartreuse3","gold","deepskyblue","saddlebrown","orange2","wheat1","black")
    
    jpeg(paste(paste("output/plots/", outputName, sep = ""), ".jpg", sep = ""), width=1200, height=700)
    plot(
      raster,
      legend = FALSE,
      axes=FALSE,
      col = chm_colors[-8],
      box= FALSE
    )
    legend(
      "right",
      legend = c("Graminoid","Tree", "Dwarf Shrub","Shrub","Forb","Moss","Lichen"),
      fill =chm_colors,
      border = FALSE,
      bty = "n",
      cex=1.3,
      xjust =1,
      horiz = FALSE,
      inset = -0.007,
      par(cex=0.4)
      
    )             
    dev.off()
    
    return(paste(paste("output/plots/", outputName, sep = ""), ".jpg", sep = ""))
  }, warning = function(warning) {
    message <- paste("WARNING - While predicting", warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste("ERROR - While predicting", error, sep = " : ")
    stop(message)
  })
}
