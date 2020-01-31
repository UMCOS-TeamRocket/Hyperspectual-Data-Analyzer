library(spectrolab)
library(tidyverse)

#fieldSpecDirectory is the directory to the folder containing the Field Spec data
#the function will look at each directory under fieldSpecDirectory recursively and create a spectra.rds file

processFieldSpec <- function(fieldSpecDirectory) {
  directories <- list.dirs(path = fieldSpecDirectory, full.names = TRUE, recursive = TRUE)
  index <- 0
  errors <- c()
  
  withProgress(message = 'Processing Field Spectra', min = 0, max = length(directories), value = 0, {
    for (directory in directories) {
      tryCatch({
        #increase progress bar and change detail text
        setProgress(index, detail = directory)
        
        ####Read in data as spectra (all scans collected at this location)
        spectra <- read_spectra(directory, format="sed")
        
        ##Fix Names (removes file extensions)
        names(spectra) <- gsub(".sed","",names(spectra))
        
        ###Create Metadata
        metadata<-as.data.frame(names(spectra))
        names(metadata)[1]<-"ScanID"
        
        ###Create column PFT and column area
        metadata <- metadata %>% mutate(PFT=substr(metadata$ScanID,start = 1,stop = 6))
        
        separatedString <- strsplit(directory, "/")
        area <- separatedString[[1]][4]
        metadata$area <- area
        
        ##Set metadata
        meta(spectra) = data.frame(metadata, stringsAsFactors = FALSE)
        
        fileName <- paste("output/fieldSpec", area, sep = "/")
        fileName <- paste(fileName, "spectra.rds", sep = "_")
        
        ##save spectra (Raw)
        saveRDS(spectra, fileName)
        
      }, warning = function(warning) {
        message <- paste("WARNING - While processing by site", directory)
        message <- paste(message, warning, sep = " : ")
        errors <<- c(errors, message)
      }, error = function(error) {
        message <- paste("ERROR - While processing by site", directory)
        message <- paste(message, error, sep = " : ")
        errors <<- c(errors, message)
      }, finally = {
        index <- index + 1
      })
    }
    
    setProgress(length(directories))
  })
  
  return(errors)
}
