source("source/fieldSpecProcessing/createSpectralLibrary.R")
source("source/fieldSpecProcessing/spectralLibrary.R")
source("source/fieldSpecProcessing/resampled.R")
source("source/fieldSpecProcessing/VIs.R")
source("source/fieldSpecProcessing/allPreds.R")

generateSpectralLibraryFiles <- function(spectraDirectories, name) {
  withProgress(message = 'Generating Spectral Library Files', min = 0, max = 1, value = 0, {
    tryCatch({
      setProgress(0, detail = "Creating Spectral Library")
      spectralLibraryDirectories <- createSpectralLibrary(spectraDirectories, name)
      
      setProgress(0.2, detail = "Creating Spectral Library Pt. 2")
      generateSpectralLibrary(spectralLibraryDirectories, name)
      
      setProgress(0.4, detail = "Resampling")
      resampledDirectories <- fieldSpecResampled(name)

      setProgress(0.6, detail = "VI")
      viDirectory <- fieldSpecVI(name)
      
      setProgress(0.8, detail = "All Preds")
      fieldSpecAllPreds(resampledDirectories,viDirectory, name)
      
      setProgress(1)
    }, warning = function(warning) {
      warning(warning)
    }, error = function(error) {
      stop(error)
    })
  })
 
}