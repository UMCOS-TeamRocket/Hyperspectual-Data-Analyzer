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
        #create a name for image and text output
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
      for(i in 1:length(data$outputImageDirectories)) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderImage()/renderText() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          #create the names the same way we did in the previous function
          imageName <- paste("image", i, sep="")
          textName <- paste(imageName, "Stats", sep = "")
          
          #get the directory to the image
          directoryString <- data$outputImageDirectories[[i]]
          
          #render the image in the image output with the name stored in imageName
          output[[imageName]] <- renderImage({
            list(src = directoryString,
                 width = "600",
                 height = "350",
                 alt = "Could not find image")
          }, deleteFile = FALSE)
          
          #character vector of data to be sidplayed along with the image
          textVector = data$outputStatistics[[i]]
          
          #text is a character vector of size 3
          outputText <- textVector[1]
          for (i in 2:length(textVector)) {
            outputText <- paste(outputText, textVector[i], sep = " :: ")
          }
          
          #render the text in the UI
          output[[textName]] <- renderText({outputText})
        })
      }
    }
  })
}
