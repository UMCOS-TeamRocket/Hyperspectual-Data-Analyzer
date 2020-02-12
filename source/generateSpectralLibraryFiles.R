source("source/fieldSpecProcessing/createSpectralLibrary.R")
source("source/fieldSpecProcessing/headwallSpectralLibrary.R")
source("source/fieldSpecProcessing/resampled.R")
source("source/fieldSpecProcessing/VIs.R")
source("source/fieldSpecProcessing/allPreds.R")

generateSpectralLibraryFiles <- function(spectraDirectories, name) {
  withProgress(message = 'Generating Spectral Library Files', min = 0, max = 1, value = 0, {
    tryCatch({
      setProgress(0, detail = "Creating Spectral Library")
      
      createSpectralLibrary(spectraDirectories, name)
      
      setProgress(0.2, detail = "Creating Headwall Spectral Library")
      
      spectralLibraryRDSDirectory <- paste(paste("output/spectralLibraries/", name, sep = ""), ".rds", sep = "")
      headwallSpectralLibrary(spectralLibraryRDSDirectory, name)
      
      setProgress(0.4, detail = "Resampling")
      
      hdwDfEqual25Directory <- paste(paste("output/hdwSpectralLibraries/", name, sep = ""), "_HDW_df_equal25.csv", sep = "")
      fieldSpecResampled(hdwDfEqual25Directory, name)
      
      setProgress(0.6, detail = "VI")
      
      fieldSpecVI(hdwDfEqual25Directory, name)
      
      hdw10nm <- paste(paste("output/hdwSpectralLibraries/", name, sep = ""), "_HDW_010nm_equal25.csv", sep = "")
      hdw50nm <- paste(paste("output/hdwSpectralLibraries/", name, sep = ""), "_HDW_050nm_equal25.csv", sep = "")
      hdw100nm <- paste(paste("output/hdwSpectralLibraries/", name, sep = ""), "_HDW_100nm_equal25.csv", sep = "")
      hdwVi <- paste(paste("output/hdwSpectralLibraries/", name, sep = ""), "_HDW_VIs_equal25.csv", sep = "")
      hdwDirectories <- c(hdw10nm, hdw50nm, hdw100nm, hdwVi)
      
      setProgress(0.8, detail = "All Preds")
      
      fieldSpecAllPreds(hdwDirectories, name)
      
      setProgress(1)
    }, warning = function(warning) {
      warning(warning)
    }, error = function(error) {
      stop(error)
    })
  })
 
}