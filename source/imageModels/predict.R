library(spectrolab)
library(ranger)
library(raster)
library(tidyverse)
library(hsdar)
library(parallel)

predictFunction <- function(classifierDirectory, imageDirectory, directory, outputName) {
  tryCatch({
    #Get Core Numbers
    c1 <- detectCores()
    if(c1>2){
      c1<-c1-2
    }
    
    #Reads in Imagery
    image<-brick(imageDirectory)
    
    #Grabs x and y coordinates, to be combined later
    imageLatLong<-rasterToPoints(image)%>% as.data.frame()
    imageLatLong[275:328]<-NULL
    imageLatLong<-na.omit(imageLatLong)
    imageLatLong<-imageLatLong[-c(449905, 521215), ]
    
    #data <-directory
    data<-read.csv(directory)
    
    ##Marks raster as unrotated
    image@rotated<-FALSE

    #Read in classifier
    classifier <- readRDS(classifierDirectory)
    
    #Remove random column from data
    if("y_VIs" %in% colnames(data))
    {
      data<- select(data,-c(y_VIs))
    }
    
    imageLatLong <-imageLatLong %>% slice(1:nrow(data))
    
    ##Save the confusion Matrix for these models
    confusionMatrix<-classifier$confusion%>%as.data.frame()
    write.csv(confusionMatrix,"output/ConfusionMatrix",row.names = F)

    ##uses model from spectral library to predict images
    results <-predict(classifier, data[-1:-2], num.threads = c1)
    
    plants <- as.list(levels(results$predictions))
    
    
    #Convert predictions into a dataframe
    pred <- results$predictions
    results<-as.data.frame(pred)%>%'names<-'("predicted")

    colnames(results) <- c("predicted")
    
    ## Grabs x, y values from original image and combines with unique values from prediction
    results<-cbind(results,imageLatLong[1:2]) %>% dplyr::select(predicted,x,y)
    
    ###Creates unique PFT_IDs
    unique<-unique(as.data.frame.complex(results$predicted))

    unique$PFT_ID<-seq(1:nrow(unique))

    names(unique)[1]<-"predicted"
    
    ###Create dataframe with unique PFT_ID values and location info
    results<-merge(results,unique, by="predicted")%>% dplyr::select(x,y,PFT_ID)

    ##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
    #A warning pops up with no solution, so it gets suppressed
    suppressWarnings(raster<-rasterFromXYZ(results, crs = crs(image)))
    
    
    foreach(i=1:length(plants), .combine=cbind) %do%{
      vector <- (unlist(plants))
    }
    
    foreach(i=1:length(plants)) %do%{
      plants[[i]] <-raster==i
    }
    
    colorChart<-read.csv("source/www/colorChart.csv")
    plotColors<-as.vector(colorChart[1:length(plants),])

    #create color chart
    #plotColors <- randomColor(count = 100)
    #write.csv(plotColors, "source/www/colorChart.csv", row.names = FALSE)

    jpeg(paste(paste("output/plots/", outputName, sep = ""), ".jpg", sep = ""), width=7200, height=4200)
    plot(
      raster,
      legend = FALSE,
      axes=FALSE,
      col = plotColors,
      box= FALSE
    )
    legend(
      "right",
      legend = vector,
      fill =plotColors,
      border = FALSE,
      bty = "n",
      cex=10,
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
