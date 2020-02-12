imageOutputModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(2, p("Output:", style = "color: white; size: 20pt; padding-left: 10px;")),
      
    ),
    br(),
    uiOutput(ns("imageOutputs")),
    #tags$head(tags$style(HTML("#output {background-color: #383a40; border-color: #383a40; color: white; font-size: 15px; padding-left: 10px;}")))
  )
}

#paramater data: reactive values. contains $outputImageDirectories and $outputStatistics
##    $outputImageDirectories: list of strings (file paths to images)
##    $outputStatistics: list of strings
imageOutputModuleServer <- function(input, output, session, data) {
  #add UI element for each image and statistics
  output$imageOutputs <- renderUI({
    if (length(data$outputImageDirectories) == 0) {
      tagList()
    } else {
      image_output_list <- lapply(1:length(data$outputImageDirectories), function(i) {
        imageName <- paste("image", i, sep="")
        textName <- paste(imageName, "Stats", sep = "")
        list(
          fluidRow(
            column(5, imageOutput(session$ns(imageName))),
            column(3, textOutput(session$ns(textName)))
          )
        )
      })
      
      #add each element of plot_output_list to a tagList and return that to renderUI()
      do.call(tagList, unlist(image_output_list, recursive = FALSE))
    }
  })
  
  #display images and statistics
  observe({
    if (length(data$outputImageDirectories) > 0) {
      for (i in 1:length(data$outputImageDirectories)) {
        imageName <- paste("image", i, sep="")
        textName <- paste(imageName, "Stats", sep = "")
        
        output[[imageName]] <- renderImage({
          list(src = data$outputImageDirectories[[i]],
               width = "75%",
               height = "75%",
               alt = "Could not find image")
        }, deleteFile = FALSE)
        
        output[[textName]] <- renderText({data$outputStatistics[[i]]})
      }
    }
  })
}
