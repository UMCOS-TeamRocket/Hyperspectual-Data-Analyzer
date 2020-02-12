viewDataModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    #Change the background color of the Select Data/Generate Class tab buttons to black when selected and gray otherwise
    tags$style(HTML("
                                            .tabbable > .nav > li > a {background-color: #383a40;  color:white}
                                            .tabbable > .nav > li > a[data-value='Classifiers'] {border-color: #2b2b2b; background-color: #2b2b2b;  color:white}
                                            .tabbable > .nav > li > a[data-value='Output'] {border-color: #2b2b2b; background-color: #2b2b2b;   color:white}
                                            .tabbable > .nav > li > a[data-value='Config'] {border-color: #2b2b2b; background-color: #2b2b2b;   color:white}
                                            .tabbable > .nav > li[class=active] > a {border-color: #383a40; background-color: #383a40; color:white}")),
    
    tabsetPanel(type = "tabs",
                #CLASSIFIER DATA TAB
                tabPanel("Classifiers", style = "background-color: #383a40;"),
                #OUTPUT TAB
                tabPanel("Output", style = "background-color:#383a40;"),
                #CONFIGURATION TAB  
                tabPanel("Config", style = "background-color: #383a40;"))
  )
}

viewDataModuleServer <- function(input, output, session) {
  #TODO: implement view data logic
}