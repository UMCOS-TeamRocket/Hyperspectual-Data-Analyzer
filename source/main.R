main <- function(){
  source("source/fieldSpecProcessing/allPreds.r", echo = TRUE)
  source("source/fieldSpecProcessing/bySite.r", echo = TRUE)
  source("source/fieldSpecProcessing/createSpectralLibrary.r", echo = TRUE)
  source("source/fieldSpecProcessing/resampled.r", echo = TRUE)
  source("source/fieldSpecProcessing/VIs.r", echo = TRUE)
  source("source/imageProcessing/headwallSpectralLibrary.r", echo = TRUE)
  source("source/imageProcessing/fieldSpecResampled_HDW.r", echo = TRUE)
}
main()

