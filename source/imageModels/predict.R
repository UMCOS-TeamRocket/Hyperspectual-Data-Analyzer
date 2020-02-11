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
    print(class(Results))
    print(str(Results))
    
    tmp <- data.frame(Results$predictions,Results$num.trees,Results$num.independent.variables,Results$num.samples,Results$treetype)
    
    Results<-as.data.frame(tmp)%>%'names<-'("predicted")
    
    colnames(Results) <- c("predicted", "num.trees", "variables", "samples", "treetype")
    ##converts prediction from rf model to dataframe and changes column name to predicted
    #Results<-as.data.frame(Results)%>%'names<-'("predicted")
    
    print(colnames(Results))
    
    ## Grabs x, y values from original image and combines with unique values from prediction 
    Results<-merge(Results,imageLatLong[1:2]) %>% dplyr::select(predicted,x,y)
    
    ###Creates Unique PFT_IDs
    Unique<-unique(as.data.frame.complex(Results$predicted))
    
    Unique$PFT_ID<-seq(1:nrow(Unique))
    
    names(Unique)[1]<-"predicted"
    
    ###Create dataframe with unique PFT_ID values and location info
    Results<-merge(Results,Unique, by="predicted")%>% dplyr::select(x,y,PFT_ID)

    
    print("now it will break")
    ##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
    raster<-rasterFromXYZ(Results, crs = crs(image), digits=5)
    
    Shrub     <-subset(Unique_HDW,Unique_HDW$predicted=="Shrub")     %>%as.data.frame()%>%dplyr::select("PFT_ID")
    Moss      <-subset(Unique_HDW,Unique_HDW$predicted=="Moss")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
    Tree      <-subset(Unique_HDW,Unique_HDW$predicted=="Tree")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
    Graminoid <-subset(Unique_HDW,Unique_HDW$predicted=="Graminoid") %>%as.data.frame()%>%dplyr::select("PFT_ID")
    Lichen    <-subset(Unique_HDW,Unique_HDW$predicted=="Lichen")    %>%as.data.frame()%>%dplyr::select("PFT_ID")
    Forb      <-subset(Unique_HDW,Unique_HDW$predicted=="Forb")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
    DwarfShrub<-subset(Unique_HDW,Unique_HDW$predicted=="Dwarf Shrub")%>%as.data.frame()%>%dplyr::select("PFT_ID")
    
    ###Filters the image on each functional group
    Test_IMG_Shrub     <-Test_IMG_raster==Shrub     [1,1]
    Test_IMG_Moss      <-Test_IMG_raster==Moss      [1,1]
    Test_IMG_Tree      <-Test_IMG_raster==Tree      [1,1]
    Test_IMG_Graminoid <-Test_IMG_raster==Graminoid [1,1]
    Test_IMG_Lichen    <-Test_IMG_raster==Lichen    [1,1]
    Test_IMG_Forb      <-Test_IMG_raster==Forb      [1,1]
    Test_IMG_DwarfShrub<-Test_IMG_raster==DwarfShrub[1,1]
    
    ##We need to change all those values within the raster to 1, 
    ##so the sum of all the pixels in each quadrat can be calculated later
    Test_IMG_denom  <-Test_IMG_raster>=1
    
    ##DF OF METEDATA
    Test_IMG_meta  <-Test_IMG_quadrats@data%>%as.data.frame()
    
    #Creates object with the total Pixels for each quadrat
    Test_IMG_Quad_totals  <-raster::extract(x=Test_IMG_denom  ,y=Test_IMG_quadrats  ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")
    
    #Creates object with the total Pixels for each Functional group
    Test_IMG_Graminoid_sum <-raster::extract(x=Test_IMG_Graminoid ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid_P" )
    Test_IMG_DwarfShrub_sum<-raster::extract(x=Test_IMG_DwarfShrub,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub_P")
    Test_IMG_Moss_sum      <-raster::extract(x=Test_IMG_Moss      ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_P"      )
    Test_IMG_Forb_sum      <-raster::extract(x=Test_IMG_Forb      ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb_P"      )
    Test_IMG_Lichen_sum    <-raster::extract(x=Test_IMG_Lichen    ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_P"    )
    Test_IMG_Shrub_sum     <-raster::extract(x=Test_IMG_Shrub     ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_P"     )
    Test_IMG_Tree_sum      <-raster::extract(x=Test_IMG_Tree      ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree_P"      )
    
    ##Lets combine the datframes created above
    image_pixeltotals<-Reduce(cbind,list(Test_IMG_Quad_totals
                                         ,Test_IMG_Graminoid_sum
                                         ,Test_IMG_DwarfShrub_sum
                                         ,Test_IMG_Moss_sum      
                                         ,Test_IMG_Forb_sum      
                                         ,Test_IMG_Lichen_sum    
                                         ,Test_IMG_Shrub_sum     
                                         ,Test_IMG_Tree_sum      ))
    
    ##Now we want to calculate the % cover for each Functional group in each quadrat
    image_PercentCover<-image_pixeltotals[,2:8]/(image_pixeltotals[,1])*100
    image_PercentCover<-image_PercentCover%>%
      mutate(CLASS_ID=rownames(image_PercentCover))%>%
      dplyr::select(CLASS_ID,everything())
    
    ##Lets merge the metadata with these new dataframes
    image_PercentCover <-merge(Test_IMG_meta,  image_PercentCover  ,by="CLASS_ID")
    image_PercentCover<-image_PercentCover%>%
      arrange(CLASS_NAME)%>%
      dplyr::select(-CLASS_CLRS,-CLASS_ID)%>%
      mutate(CLASS_ID=rownames(image_PercentCover))%>%dplyr::select(CLASS_ID,everything())
    
    ##Import quadrat estimates made by jane and merge to estimates generated by models
    ##This was not doen for the test image
    #Test_IMGEstimates<-read.csv("Original_data/Test_imagery_HDW/Test_IMGEstimates_Jane.csv")
    #Test_IMGEstimate_Tab<-cbind(image_PercentCover,Test_IMGEstimates)
    #Test_IMGEstimate_Tab<-Test_IMGEstimate_Tab%>%dplyr::select(CLASS_ID,CLASS_NAME,Graminoid_P,
    #                                                           Graminoid_A,DwarfShrub_P,DwarfShrub_A,Moss_P,Moss_A,Forb_P,
    #                                                           Forb_A,Lichen_P,Lichen_A,Shrub_P,Shrub_A,Tree_P,Tree_A,Litter_A)
    #
    #write.csv(Test_IMGEstimate_Tab ,"Test_Outputs/2_HDW_imagery/2_Models/AK_imagery/Test_IMG_model_PercentCover.csv")
    
    ##Import quadrat estimates made by jane and merge to estimates generated by models
    #Test_IMGEstimates<-read.csv("Original_data/Test_imagery_HDW/Test_IMGEstimates_Jane.csv")
    #Test_IMGEstimate_Tab<-merge(image_PercentCover,Test_IMGEstimates,by="CLASS_NAME")
    
    ###########################################Plot 1############################################################
    ###save plot as a jpeg
    
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