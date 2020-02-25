library(jsonlite)

#write_json <- function(object, filename){
#  writeLines(toJSON(object), filename)
#}

spectralLibrary <- "NASADATA"
classifier <-"peterAlgo"
outputFile <- "aroostookMap"
strings = data.frame(spectralLibrary)
numberOfSampledVariables <- 1
numberOfTreesToGrow <- 500


configData = c(spectralLibrary, classifier, outputFile, numberOfSampledVariables, numberOfTreesToGrow)

#write_json(head(text), "config.json")
#fromJSON("config.json")

configData
write_json(configData, "config1.json")

configDataRead = fromJSON("config1.json")
configDataRead