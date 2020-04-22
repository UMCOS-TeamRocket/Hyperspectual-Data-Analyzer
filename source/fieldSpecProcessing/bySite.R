library(spectrolab)
library(tidyverse)

#fieldSpecDirectory is a string. It is the directory to the folder containing the Field Spec data
#the function will look at each directory under fieldSpecDirectory recursively and create a spectra.rds file

processFieldSpec <- function(fieldSpecDirectory) {
  #get a list of all the directories that are under fieldSpecDirectory
  directories <- list.dirs(path = fieldSpecDirectory, full.names = TRUE, recursive = TRUE)
  index <- 0
  errors <- c()
  
  #withProgress displays the progress dialog in the bottom right of the screen
  withProgress(message = 'Processing Field Spectra', min = 0, max = length(directories), value = 0, {
    #create a spectra.rds file for each directory in the list of directories
    for (directory in directories) {
      tryCatch({
        #increase progress bar and change detail text
        setProgress(index, detail = directory)
        
        sedError <- FALSE
        tryCatch({
          ####Read in data as spectra (all scans collected at this location)
          spectra <- read_spectra(directory, format="sed")
          
        }, warning = function(warning) {
          #Catch any warnings or errors and save them in the errors variable
          message <- paste("WARNING - While processing by site:", directory, "does not contain any .sed files")
          
          #add message to list of error messages
          errors <<- c(errors, message)
          
          #R doesnt know it is in a loop while in the warning/error function of the try catch
          #so we need to set a flag that is checked outside of these functions
          sedError <<- TRUE
        }, error = function(error) {
          message <- paste("ERROR - While processing by site:", directory, "does not contain any .sed files")
          errors <<- c(errors, message)
          sedError <<- TRUE
        })
        
        if (sedError) {
          #increment index for the progress bar
          index <- index + 1
          
          #skip to the next iteration of the for loop
          next
        }
        
        ##Fix Names (removes file extensions)
        names(spectra) <- gsub(".sed","",names(spectra))
        
        ###Create Metadata
        metadata<-as.data.frame(names(spectra))
        names(metadata)[1]<-"ScanID"
        
        ###Create column PFT and column area
        metadata <- metadata %>% mutate(PFT=substr(metadata$ScanID,start = 1,stop = 6))
        
        #separate the directory into pieces
        separatedString <- strsplit(directory, "/")
        
        #get the name of the current folder (ex. if directory is "data/field_spec/alaska", "alaska" is stored in the variable 'area')
        area <- separatedString[[1]][length(separatedString[[1]])]
        metadata$area <- area
        
        ##Set metadata
        meta(spectra) = data.frame(metadata, stringsAsFactors = FALSE)
        
        #create the directory and filename of the .rds file for this set of field spec data
        fileName <- paste("output/fieldSpec", area, sep = "/")
        fileName <- paste(fileName, "spectra.rds", sep = "_")
        
        ##save spectra (Raw)
        saveRDS(spectra, fileName)
        
      }, warning = function(warning) {
        #Catch any warnings or errors and save them in the errors variable
        message <- paste("WARNING - While processing by site", directory)
        message <- paste(message, warning, sep = " : ")
        errors <<- c(errors, message)
      }, error = function(error) {
        message <- paste("ERROR - While processing by site", directory)
        message <- paste(message, error, sep = " : ")
        errors <<- c(errors, message)
      }, finally = {
        #regardless of an error occurs or not, increase the index so the progress bar still increments
        index <- index + 1
      })
    }
    
    setProgress(length(directories))
  })
  
  #return any errors that occured
  return(errors)
}
