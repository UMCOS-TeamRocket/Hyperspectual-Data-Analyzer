library(here)

#create all output subdirectoies
#TODO: showWarnings is set to FALSE in order to prevent a crash if the directories already exist. Another way around this?
createOutputDirectories <- function() {
  dir.create(file.path(here(), "output/classifiers"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(here(), "output/fieldSpec"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(here(), "output/hdwSpectralLibraries"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(here(), "output/plots"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(here(), "output/spectralLibraries"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(here(), "output/hdwImagery"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(here(), "output/logs"), recursive = TRUE, showWarnings = FALSE)
}
