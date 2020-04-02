library(tidyverse)
library(hsdar)

generateImage <- function(raster, outputName, plotColors, plantVector) {

  png(paste(paste("output/plots/", outputName, sep = ""), ".png", sep = ""), 
      width=(ncol(raster)*10), 
      height=(nrow(raster)*10)
  )
  plot(
    raster,
    legend = FALSE,
    axes=FALSE,
    col = plotColors,
    box= FALSE
  )
  dev.off()
  
  png(paste(paste("output/plots/", outputName, "_Legend", sep = ""), ".png", sep = ""), 
      width=(500), 
      height=(length(plantVector)*50)
  )
  plot(
    raster,
    legend = FALSE,
    axes=FALSE,
    col = "white",
    box= FALSE
  )
  
  legend(
    "right",
    xpd=TRUE,
    legend = plantVector,
    fill =plotColors,
    border = FALSE,
    bty = "n",
    cex= 2.5,
    xjust =1,
    horiz = FALSE,
    inset = -0.007,
    par(cex=0.4)
    
  )   
  dev.off()
  

  return(paste(paste("output/plots/", outputName, sep = ""), ".png", sep = ""))
  }