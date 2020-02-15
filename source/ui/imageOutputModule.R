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
        imageName <- paste0("image", i, sep="")
        textName <- paste0(imageName, "Stats", sep = "")
        
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
      for(i in 1:length(data$outputImageDirectories)) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderImage()/renderText() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          imageName <- paste0("image", i, sep="")
          textName <- paste0(imageName, "Stats", sep = "")
          
          directoryString <- data$outputImageDirectories[[i]]
          
          output[[imageName]] <- renderImage({
            list(src = directoryString,
                 width = "75%",
                 height = "75%",
                 alt = "Could not find image")
          }, deleteFile = FALSE)
          
          text = data$outputStatistics[[i]]
          
          output[[textName]] <- renderText({text})
        })
      }
      
      i <- 1
      while(i <= length(data$outputImageDirectories)) {
        
        
        i = i + 1
      }
    }
  })
}
