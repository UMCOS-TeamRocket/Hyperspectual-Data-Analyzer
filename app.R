library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
          .navbar .navbar-nav {float: right}
          .navbar .navbar-header {float: left;}
          .navbar-inner { background-color: #23262b }
          .navbar-static-top {
  position: static;
  margin-bottom: 0px;
  background-color: #23262b

          }
        "))
  ),
  navbarPage("Hyperspectral Data Analyzer",
             tabPanel("Home",
                      br(),
                      br(),
                          sidebarLayout(
                            
                            sidebarPanel(style = "background-color: #383a40; border-color: #383a40;",
                              #Change the background color of the Select Data/Generate Class tab buttons to black when selected and gray otherwise
                              tags$style(HTML("
                                            .tabbable > .nav > li > a {background-color: #383a40;  color:white}
                                            .tabbable > .nav > li > a[data-value='Select Data'] {border-color: #2b2b2b; background-color: #2b2b2b;  color:white}
                                            .tabbable > .nav > li > a[data-value='Generate Classifier'] {border-color: #2b2b2b; background-color: #2b2b2b;   color:white}
                                            .tabbable > .nav > li[class=active] > a {border-color: #383a40; background-color: #383a40; color:white}
                              ")),
                              tabsetPanel(type = "tabs",
                                          #SELECT DATA TAB
                                          tabPanel("Select Data", style = "background-color: #383a40;",
                                                   #Drop down for classifier selection
                                                   selectInput("classifierSelect", 
                                                               label = div(style="color: white;", "Classifier:"),
                                                               c("Classifier 1", "Classifier 2", "Classifier 3")),
                                                   
                                                   tags$head(tags$style(
                                                          HTML('
                                                            body, input, button, select { 
                                                                font-family: "Calibri";
                                                                background-color: #121212;
                                                            }')
                                                   )),
                                                   
                                                   #File browser for data cubes
                                                   #accept gives the browser a hint of what kind of files the server is expecting.
                                                   fileInput("dataCubeFileInput", label = div(style="color: white;", "Data Cube:"),, accept = c(".txt")),
                                                     
                                                   
                                                   #Button to generate a classifier
                                                   actionButton("addToQueueButton", "Add to Queue")
                                          ),
                                          
                                          #GENERATE CLASSIFIER TAB
                                          tabPanel("Generate Classifier", style = "background-color:#383a40;",
                                                   #File browser for data cubes
                                                   #accept gives the browser a hint of what kind of files the server is expecting.
                                                   textInput("speciesSelect", label = div(style="color: white;", "Species:"), value = "Species"),
                                                   
                                                   tags$head(tags$style(
                                                     HTML('
                                                            body, input, button, select { 
                                                                font-family: "Calibri";
                                                                background-color: #23262b;
                                                            }')
                                                   )),
                                                  
                                                   #File browser for data cubes
                                                   #accept gives the browser a hint of what kind of files the server is expecting.
                                                   textInput("ClassifierNameInput", label = div(style="color: white;", "Classifier Name:"), value = ""),
                                                   
                                                   #Button to generate a classifier
                                                   actionButton("generateClassifierButton", "Generate Classifier")
                                                 
                                        )
                                          
                              ),
                              
                            ),
                            
                            #RANDOM PARAMETERS
                            mainPanel(
                              style = "background-color: #383a40;",
                              
                              #change the color of the min and max values on the slider to white
                              tags$style(HTML(".irs-max {color: white;}
                                              .irs-min {color: white;}")),
                              
                              setSliderColor(c("red", "red", "red", "red"), c(1, 2, 3, 4)),
                              sliderInput("integer", label = div(style="color: white;", "Integer:"),
                                          min = 0, max = 1000,
                                          value = 500,
                                          ),
                              sliderInput("integer", label = div(style="color: white;", "Integer:"),
                                          min = 0, max = 1000,
                                          value = 500),
                              sliderInput("integer", label = div(style="color: white;", "Integer:"),
                                          min = 0, max = 1000,
                                          value = 500),
                              sliderInput("integer", label = div(style="color: white;", "Integer:"),
                                          min = 0, max = 1000,
                                          value = 500),
                              
                              
                            )  
                        
                      ),
                      br(),
                      br(),
                      
                      #QUEUE
                      fluidRow(
                        style = "background-color: #383a40;",
                        br(),
                        fluidRow(
                          column(2, p("Queue:", style = "color: white; size: 20pt; padding-left: 10px;")),
                          column(1, actionButton("runQueue", "Run")),
                          column(1, actionButton("stopQueue", "Stop"))
                        ),
                        br(),
                        
                        tags$head(tags$style("#queue {color: white; font-size: 15px; padding-left: 10px;}")),
                        textOutput("queue"),
                        br(),
                      ),
          
                      img(src="logo.png", height="10%", width="10%", align="right")
             ),
             
             tabPanel("Upload Data",
                      verbatimTextOutput("Upload Data")
             ),
             
             tabPanel("View Data",
                      verbatimTextOutput("View Data")
             )
  )
  
)

server <- function(input, output) {
  observeEvent(input$generateClassifierButton, {
    #this code is executed when the generateClassifierButton is pressed
    print("button clicked")
  })
  
  #update queue
  queueText <- c()
  #if add to queue button is pressed
  observeEvent(input$addToQueueButton, {
    #chck if a file has been selected
    if (is.null(input$dataCubeFileInput$name)) {
      print("no file selected")
    } else {
      #create string to add to queue
      string <- "{{Data Cube: "
      string <- paste(string, input$dataCubeFileInput$name, sep = "")
      string <- paste(string, "} Classifier: ", sep = "")
      string <- paste(string, input$classifierSelect, sep = "")
      string <- paste(string, "}", sep = "")
      
      #TODO: currently erasing previous elements. only the last string entered is displayed
      queueText <- c(queueText, string)
    }
    
    if (length(queueText) == 0) {
      output$queue <- renderText({"queue is empty"})
    } else {
      #create one string seperated by new lines
      string <- ""
      for (i in 1:length(queueText)) {
        string <- paste(string, queueText[i], sep = "\n")
      }
      
      output$queue <- renderText({string})
    }
  })
  
  output$queue <- renderText({"queue is empty"})
}

shinyApp(ui = ui, server = server)