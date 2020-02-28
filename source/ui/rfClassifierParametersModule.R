rfClassifierParametersUI <- function(id) {
  #namespace for the module
  ns <- NS(id)
  
  tagList (
    #change the color of the min and max values on the slider to white
    tags$style(HTML(".irs-max {color: white;}
                                              .irs-min {color: white;}")),
    
    setSliderColor(c("#D2403A", "#D2403A"), c(1, 2)),
    sliderInput(ns("mtry"), label = div(style="color: white;", "Number Of Sampled Variables:"),
                min = 0, max = 10,
                value = 3,
    ),
    sliderInput(ns("ntree"), label = div(style="color: white;", "Number of trees to grow:"),
                min = 0, max = 1000,
                value = 500),
    checkboxInput(ns("importance"), label = div(style="color: white;", "Importance"), value = TRUE)
  )
}

#return reactive variables for the three parameters in a list
rfClassifierParametersServer <- function(input, output, session) {
  return(
    list(
      mtry = reactive({input$mtry}),
      ntree = reactive({input$ntree}),
      importance = reactive({input$importance})
    )
  )
}