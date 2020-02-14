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
    print(hdwDirectory)
    print(imageDirectory)
    
    #Reads in Imagery
    image<-brick(imageDirectory)
    
    ##Converts to a dataframe
    imageLatLong<-rasterToPoints(image)%>% as.data.frame()
    imageLatLong[275:328]<-NULL
    imageLatLong<-na.omit(imageLatLong)
    imageLatLong<-imageLatLong[-c(449905, 521215), ]
    
    dataHDW <-read.csv(hdwDirectory)
    #TODO
    #remove hard coding
    quadrats <-readOGR("data/Test_imagery_HDW", "Test_IMG_quads")

    ##Marks raster as unrotated
    image@rotated<-FALSE

    #Read in classifier
    classifier <- readRDS(classifierDirectory)
    
    ##Save the confusion Matrix for these models
    confusionMatrix<-classifier$confusion%>%as.data.frame()
    write.csv(confusionMatrix,"output/ConfusionMatrix",row.names = F)

    ##uses model from spectral library to predict images
    Results <-predict(classifier, dataHDW[-1:-2], num.threads = c1)

    tmp <- Results$predictions
    Results<-as.data.frame(tmp)%>%'names<-'("predicted")
    
    colnames(Results) <- c("predicted")
    ##converts prediction from rf model to dataframe and changes column name to predicted
    #Results<-as.data.frame(Results)%>%'names<-'("predicted")
    
    ## Grabs x, y values from original image and combines with unique values from prediction 
    #How important is the dplyr select? Makes things break
    #Results<-merge(Results,imageLatLong[1:2]) %>% dplyr::select(predicted,x,y)
    print("Result Time")
    print(str(imageLatLong$x))
    print("break")
   # print(imageLatLong)
    
    
    nr <- nrow(Results)
    print(nr)
    imageLatLong<-imageLatLong %>% slice(1:nr)
    Results<-cbind(Results,imageLatLong[1:2]) %>% dplyr::select(predicted,x,y)
 
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
    dataMeta<-quadrats@data%>%as.data.frame()
    
    #Creates object with the total Pixels for each quadrat
    quadTotals  <-raster::extract(x=denom  ,y=quadrats  ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")
    
    #Creates object with the total Pixels for each Functional group
    Graminoid_sum <-raster::extract(x=Graminoid ,y=quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid_P" )
    dwarfShrub_sum<-raster::extract(x=dwarfShrub,y=quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub_P")
    moss_sum      <-raster::extract(x=moss      ,y=quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_P"      )
    forb_sum      <-raster::extract(x=forb      ,y=quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb_P"      )
    lichen_sum    <-raster::extract(x=lichen    ,y=quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_P"    )
    shrub_sum     <-raster::extract(x=shrub     ,y=quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_P"     )
    tree_sum      <-raster::extract(x=tree      ,y=quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree_P"      )
    
    ##Lets combine the datframes created above
    pixelTotals<-Reduce(cbind,list(quadTotals
                                                 ,Graminoid_sum 
                                                 ,dwarfShrub_sum
                                                 ,moss_sum      
                                                 ,forb_sum      
                                                 ,lichen_sum    
                                                 ,shrub_sum     
                                                 ,tree_sum      ))
    
    ##Now we want to calculate the % cover for each Functional group in each quadrat
    percentCover<-pixelTotals[,2:8]/(pixelTotals[,1])*100
    percentCover<-percentCover%>%
      mutate(CLASS_ID=rownames(percentCover))%>%
      dplyr::select(CLASS_ID,everything())
    
    ##Lets merge the metadata with these new dataframes
    percentCover <-merge(dataMeta,  percentCover  ,by="CLASS_ID")
    percentCover<-percentCover%>%
      arrange(CLASS_NAME)%>%
      dplyr::select(-CLASS_CLRS,-CLASS_ID)%>%
      mutate(CLASS_ID=rownames(percentCover))%>%dplyr::select(CLASS_ID,everything())
    
    
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
    plot(quadrats,border="white",lwd=2,add=TRUE)
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
