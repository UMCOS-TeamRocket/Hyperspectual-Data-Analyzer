source("source/fieldSpecProcessing/createSpectralLibrary.R")
source("source/imageProcessing/headwallSpectralLibrary.R")
source("source/fieldSpecProcessing/resampled.R")
source("source/fieldSpecProcessing/VIs.R")
source("source/filedSpecProcessing/allPreds.R")

generateSpectralLibraryFiles <- function(spectraDirectories, name) {
  tryCatch({
    createSpectralLibrary(spectraDirectories, name)
    
    spectralLibraryRDSDirectory <- paste(paste("output/spectralLibraries/", name, sep = ""), ".rds", sep = "")
    headwallSpectralLibrary(spectralLibraryRDSDirectory, name)
    
    hdwDfEqual25Directory <- paste(paste("output/hdwImagery/", name, sep = ""), "_HDW_df_equal25.csv", sep = "")
    fieldSpecResampled(hdwDfEqual25Directory, name)
    
    fieldSpecVI(hdwDfEqual25Directory, name)
    
    hdw10nm <- paste(paste("output/hdwImagery/", name, sep = ""), "_HDW_010nm_equal25.csv", sep = "")
    hdw50nm <- paste(paste("output/hdwImagery/", name, sep = ""), "_HDW_050nm_equal25.csv", sep = "")
    hdw100nm <- paste(paste("output/hdwImagery/", name, sep = ""), "_HDW_100nm_equal25.csv", sep = "")
    hdwVi <- paste(paste("output/hdwImagery/", name, sep = ""), "_HDW_VIs_equal25.csv", sep = "")
    hdwDirectories <- c(hdw10nm, hdw50nm, hdw100nm, hdwVi)
    
    fieldSpecAllPreds(hdwDirectories, name)
  }), warning = function(warning) {
    print(warning)
  }, error = function(error) {
    print(error)
  })
}