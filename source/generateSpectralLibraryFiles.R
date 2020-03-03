source("source/fieldSpecProcessing/createSpectralLibrary.R")
source("source/fieldSpecProcessing/spectralLibrary.R")
source("source/fieldSpecProcessing/resampled.R")
source("source/fieldSpecProcessing/VIs.R")
source("source/fieldSpecProcessing/allPreds.R")

generateSpectralLibraryFiles <- function(spectraDirectories, name) {
  withProgress(message = 'Generating Spectral Library Files', min = 0, max = 1, value = 0, {
    tryCatch({
      setProgress(0, detail = "Creating Spectral Library")
      
      createSpectralLibrary(spectraDirectories, name)
      
      setProgress(0.2, detail = "Creating Spectral Library Pt. 2")
      
      spectralLibraryRDSDirectory <- paste(paste("output/spectralLibraries/", name, sep = ""), ".rds", sep = "")
      generateSpectralLibrary(spectralLibraryRDSDirectory, name)
      
      setProgress(0.4, detail = "Resampling")
      
      dfEqual25Directory <- paste(paste("output/outputSpectralLibraries/", name, sep = ""), "_df_equal25.csv", sep = "")
      fieldSpecResampled(dfEqual25Directory, name)
      
      setProgress(0.6, detail = "VI")
      
      fieldSpecVI(dfEqual25Directory, name)
      
      band_10nm <- paste(paste("output/outputSpectralLibraries/", name, sep = ""), "_010nm_equal25.csv", sep = "")
      band_50nm <- paste(paste("output/outputSpectralLibraries/", name, sep = ""), "_050nm_equal25.csv", sep = "")
      band_100nm <- paste(paste("output/outputSpectralLibraries/", name, sep = ""), "_100nm_equal25.csv", sep = "")
      Vi <- paste(paste("output/outputSpectralLibraries/", name, sep = ""), "_VIs_equal25.csv", sep = "")
      directories <- c(band_10nm, band_50nm, band_100nm, Vi)
      
      setProgress(0.8, detail = "All Preds")
      
      fieldSpecAllPreds(directories, name)
      
      setProgress(1)
    }, warning = function(warning) {
      warning(warning)
    }, error = function(error) {
      stop(error)
    })
  })
 
}