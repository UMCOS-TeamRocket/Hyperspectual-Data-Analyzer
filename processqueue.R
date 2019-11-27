processQueue <- function(queue) {
  for (process in queue) {
    dataCube <- process[1]
    classifier <- process[2]
    
    print(paste("Processing Data Cube:", dataCube))
    #TODO: LOAD CLASSIFIER
    #TODO: GET DATA CUBE FILE PATH?
    #TODO: PREDICT
  }
}