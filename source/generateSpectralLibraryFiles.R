source("source/fieldSpecProcessing/createSpectralLibrary.R")
source("source/fieldSpecProcessing/spectralLibraryDf.R")
source("source/fieldSpecProcessing/resampled.R")
source("source/fieldSpecProcessing/VIs.R")
source("source/fieldSpecProcessing/allPreds.R")

#calls all necessary scripts to create a spectral library
#params:
##spectraDirectories: list of strings. each element is a directory to a previously generated spectra object.
##name: string. used to name the spectral library file that is created
generateSpectralLibraryFiles <- function(spectraDirectories, name) {
  withProgress(message = 'Generating Spectral Library Files', min = 0, max = 1, value = 0, {
    tryCatch({
      setProgress(0, detail = "Creating Spectral Library")
      spectralLibrary <- createSpectralLibrary(spectraDirectories)
      
      setProgress(0.2, detail = "Creating Spectral Library Pt. 2")
      generateSpectralLibraryDf(spectralLibrary)
      
      setProgress(0.4, detail = "Resampling")
      resampled <- fieldSpecResampled("output/intermediateFiles/spectralLibrary_df_equal25.csv")

      setProgress(0.6, detail = "VI")
      vi <- fieldSpecVI("output/intermediateFiles/spectralLibrary_df_equal25.csv")
      
      setProgress(0.8, detail = "All Preds")
      fieldSpecAllPreds(resampled, vi, name)
      
      setProgress(1)
    }, warning = function(warning) {
      warning(warning)
    }, error = function(error) {
      stop(error)
    })
  })
 
}