library(tidyverse)
library(hsdar)

generateImage <- function(raster, outputName, plotColors, plantVector) {
  
  plotFileName <- paste("output/plots/", outputName, ".png", sep = "")
  plotLegendFileName <- paste("output/plots/", outputName, "_Legend.png", sep = "")

  png(plotFileName, 
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
  
  png(plotLegendFileName, 
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
  

  return(list(plot = plotFileName,
              legend = plotLegendFileName))
  }