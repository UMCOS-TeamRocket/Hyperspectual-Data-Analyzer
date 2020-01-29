library(raster)
library(tidyverse)

predict <- function(classifierDirectory, imageAvDirectory, imageHdwDirectory, imageAvViDirectory, outputName) {
  tryCatch({
    #Reads in Imagery
    image_AV<-brick(imageAvDirectory)
    image_HDW<-brick(imageHdwDirectory)
    
    ##MAKES SURE PROJECTIONS ARE THE SAME
    crs(image_HDW)<-crs(image_AV)
    
    ##Marks raster as unrotated
    image_AV@rotated<-FALSE
    
    ##Converts to a dataframe
    image_AV<-rasterToPoints(image_AV)%>% as.data.frame()
    
    image_AV_VIs <-read.csv(imageAvViDirectory)
    
    #Read in classifier
    classifier <- readRDS(classifierDirectory)
    
    ##uses model from spectral library to predict images
    Results_AV_VIs    <-predict(classifier, image_AV_VIs[-1:-2])
    
    ##converts prediction from rf model to dataframe and changes column name to predicted
    Results_AV_VIs<-as.data.frame(Results_AV_VIs)%>%'names<-'("predicted")
    
    ## Grabs x, y values from original image and combines with unique values from prediction 
    Results_AV_VIs<-cbind(Results_AV_VIs,image_AV[1:2]) %>% dplyr::select(predicted,x,y)
    
    ###Creates Unique PFT_IDs
    Unique_AV_VIs<-unique(as.data.frame(Results_AV_VIs$predicted)) 
    Unique_AV_VIs$PFT_ID<-seq(1:nrow(Unique_AV_VIs))
    names(Unique_AV_VIs)[1]<-"predicted"
    
    ###Create dataframe with unique PFT_ID values and location info
    Results_AV_VIs<-merge(Results_AV_VIs,Unique_AV_VIs, by="predicted")%>% dplyr::select(x,y,PFT_ID)
    
    ##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
    raster<-rasterFromXYZ(Results_AV_VIs, crs = crs(image_AV))
    
    ###########################################Plot 1############################################################
    ###save plot as a jpeg
    chm_colors <- c("darkgreen","chartreuse3","gold","deepskyblue","saddlebrown")
    
    jpeg(paste(paste("output/plots/", outputName, sep = ""), ".jpg", sep = ""), width=1200, height=700)
    plot(
      raster,
      legend = FALSE,
      axes=FALSE,
      col = chm_colors,
      box= FALSE,
    )
    plot(extent(image_HDW),lwd=4,add=T)
    legend(
      "top",
      legend = c("Dwarf Shrub","Shrub","moss","Forb","Graminoid"),
      fill =chm_colors,
      border = FALSE,
      bty = "n",
      cex=2,
      xjust =1,
      horiz = TRUE,
      inset = -0.005,
      par(cex=0.4)
    )             
    dev.off()
    
    return(paste(paste("output/plots/", outputName, sep = ""), ".jpg", sep = ""))
  }, warning = function(warning) {
    message <- paste("WARNING - While predicting", warning, sep = " : ")
    print(message)
  }, error = function(error) {
    message <- paste("ERROR - While predicting", error, sep = " : ")
    print(message)
  })
}