library(raster)
library(tidyverse)

predictFunction <- function(classifierDirectory, imageDirectory, hdwDirectory, outputName) {
  tryCatch({
    #Reads in Imagery
    image<-brick(imageDirectory)
    
    ##Marks raster as unrotated
    image@rotated<-FALSE

    ##Converts to a dataframe
    image<-rasterToPoints(image)%>% as.data.frame()

    image_file <-read.csv(image)

    #Read in classifier
    classifier <- readRDS(classifierDirectory)

    ##uses model from spectral library to predict images
    Results    <-predict(classifier, image_file[-1:-2])
    
    ##converts prediction from rf model to dataframe and changes column name to predicted
    Results<-as.data.frame(Results)%>%'names<-'("predicted")
    
    ## Grabs x, y values from original image and combines with unique values from prediction 
    Results<-cbind(Results,image_D[1:2]) %>% dplyr::select(predicted,x,y)
    
    ###Creates Unique PFT_IDs
    Unique<-unique(as.data.frame(Results$predicted)) 
    Unique$PFT_ID<-seq(1:nrow(Unique))
    names(Unique)[1]<-"predicted"
    
    ###Create dataframe with unique PFT_ID values and location info
    Results<-merge(Results,Unique, by="predicted")%>% dplyr::select(x,y,PFT_ID)
    
    ##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
    raster<-rasterFromXYZ(Results, crs = crs(image_D))
    
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
    warning(message)
  }, error = function(error) {
    message <- paste("ERROR - While predicting", error, sep = " : ")
    stop(message)
  })
}