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

#paramater data: reactive values. contains $directories and $statistics
##    $directories: list of strings (file paths to images)
##    $statistics: list of strings
imageOutputModuleServer <- function(input, output, session, data) {
  #add UI element for each image and statistics
  output$imageOutputs <- renderUI({
    if (length(data$directories) == 0) {
      tagList()
    } else {
      image_output_list <- lapply(1:length(data$directories), function(i) {
        #create a name for image and text output
        imageName <- paste("image", i, sep="")
        legendName <- paste("legend", i, sep="")
        textName <- paste(imageName, "Stats", sep = "")
        
        list(
          fluidRow(
            column(4, imageOutput(session$ns(imageName))),
            column(4, imageOutput(session$ns(legendName))),
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
    if (length(data$directories) > 0) {
      for(i in 1:length(data$directories)) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderImage()/renderText() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          #create the names the same way we did in the previous function
          imageName <- paste("image", i, sep="")
          legendName <- paste("legend", i, sep="")
          textName <- paste(imageName, "Stats", sep = "")
          
          #get the directory to the image
          plotDirectory <- data$directories[[i]][["plot"]]
          legendDirectory <- data$directories[[i]][["legend"]]
          
          #log info
          flog.info(paste("Displaying Image: ", plotDirectory), name = "logFile")
          
          #render the plot
          output[[imageName]] <- renderImage({
            list(src = plotDirectory,
                 width = "600",
                 height = "350",
                 alt = "Could not find image")
          }, deleteFile = FALSE)
          
          #render the legend
          output[[legendName]] <- renderImage({
            list(src = legendDirectory,
                 width = "600",
                 height = "350",
                 alt = "Could not find image")
          }, deleteFile = FALSE)
          
          #character vector of data to be sidplayed along with the image
          textVector = data$statistics[[i]]
          
          #text is a character vector of size 3
          outputText <- textVector[1]
          for (i in 2:length(textVector)) {
            outputText <- paste(outputText, textVector[i], sep = " // ")
          }
          
          #render the text in the UI
          output[[textName]] <- renderText({outputText})
        })
      }
    }
  })
}
