imageOutputModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(2, p("Output:", style = "color: white; size: 20pt; padding-left: 10px;")),
      
    ),
    br(),
    
    
    img(src="CopyOfLight lichen2.jpg", height="100%", width="100%", align="center"),
    
    imageOutput("Output"),
    tags$head(tags$style(HTML("#output {background-color: #383a40; border-color: #383a40; color: white; font-size: 15px; padding-left: 10px;}")))
  )
}

imageOutputModuleServer <- function(input, output, session) {
  #TODO: implement output
}