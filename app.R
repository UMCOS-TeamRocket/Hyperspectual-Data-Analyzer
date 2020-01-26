library(shiny)
library(shinyWidgets)
library(magrittr)

source("processQueue.R")
source("generateRFClassifier.R")

ui <- 
  fluidPage(
  tags$head(
   tags$style(HTML("
          .navbar .navbar-nav {float: right}
          .navbar .navbar-header {float: left;}
          .navbar-inner { background-color: #23262b }
          .navbar-static-top 
          {
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
                                                               label = div(style="color: white;", "Classifier:"), c()),

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
                                                   selectInput("speciesSelect", label = div(style="color: white;", "Species:"), c("Shrub", "Lichen"), multiple = TRUE),

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
                          column(1, actionButton("clearQueue", "clear"))
                        ),
                        br(),

                        verbatimTextOutput("queue"),
                        tags$head(tags$style(HTML("#queue {background-color: #383a40; border-color: #383a40; color: white; font-size: 15px; padding-left: 10px;}"))),
                        br(),
                      ),

                      img(src="logo.png", height="10%", width="10%", align="right")
             ),
             #UPLOAD DATA TAB
             tabPanel("Upload Data",
                      fluidPage(

                        br(),
                        #UPLOAD CLASSIFIER
                        sidebarPanel(
                          width = 12,
                          style = "background-color: #383a40;",
                          fileInput(
                          inputId = "files",
                          label = div(style="color: white;", "Drag and Drop or Browse:"),
                          multiple = TRUE,
                          buttonLabel = "Browse",
                          placeholder = "No file selected"
                          ),
                          textInput("uploadClassifierNameInput", label = div(style="color: white;", "Classifier Name:"), value = ""),
                          
                          #Button to upload a classifier
                          actionButton("uploadClassifierButton", "Upload Classifier"),

                        ),
                        #UPLOAD DATA CUBE
                        sidebarPanel(
                          width = 12,
                          style = "background-color: #383a40;",
                          fileInput(
                            inputId = "files",
                            label = div(style="color: white;", "Drag and Drop or Browse:"),
                            multiple = TRUE,
                            buttonLabel = "Browse",
                            placeholder = "No file selected"
                          ),
                          textInput("uploadDataCubeInput", label = div(style="color: white;", "Data Cube Name:"), value = ""),
                          
                          #Button to upload a dataCube
                          actionButton("uploadDataCube", "Upload Data Cube"),
                          
                        ),
                        
                      ),
                      img(src="logo.png", height="10%", width="10%", align="right")
             ),
             #VIEW DATA TAB
             tabPanel("View Data",
                      sidebarPanel(style = "background-color: #383a40; border-color: #383a40;",
                                   #Change the background color of the Select Data/Generate Class tab buttons to black when selected and gray otherwise
                                   tags$style(HTML("
                                            .tabbable > .nav > li > a {background-color: #383a40;  color:white}
                                            .tabbable > .nav > li > a[data-value='Classifiers'] {border-color: #2b2b2b; background-color: #2b2b2b;  color:white}
                                            .tabbable > .nav > li > a[data-value='Output'] {border-color: #2b2b2b; background-color: #2b2b2b;   color:white}
                                            .tabbable > .nav > li > a[data-value='Config'] {border-color: #2b2b2b; background-color: #2b2b2b;   color:white}
                                            .tabbable > .nav > li[class=active] > a {border-color: #383a40; background-color: #383a40; color:white}
                              ")),
                                   tabsetPanel(type = "tabs",
                                               #CLASSIFIER DATA TAB
                                               tabPanel("Classifiers", style = "background-color: #383a40;",
                                                  
                                               ),

                                               #OUTPUT TAB
                                               tabPanel("Output", style = "background-color:#383a40;",
                                               ),
                                               #CONFIGURATION TAB  
                                               tabPanel("Config", style = "background-color: #383a40;",
                                                        
                                               )


                                   ),
                      ),


             )
  )
  
)



server <- function(input, output, session) {
  #VARIABLES
  queueText <- c() #vector of strings to be displayed in the ui queue
  queue <- list() #vector of string vectors. each element = (data cube file directory, classifier name)
  classifierChoices <- c("Classifier 1", "Classifier 2")
  
  
  #INITIALIZE
  #initialize queue
  output$queue <- renderText({"queue is empty"})
  updateSelectInput(session, "classifierSelect", label = div(style="color: white;", "Classifier:"), classifierChoices)
  
  #add to queue button is pressed
  observeEvent(input$addToQueueButton, {
    #chck if a file has been selected
    if (is.null(input$dataCubeFileInput$name)) {
      print("no file selected")
    } else {
      #add process to queue
      queue[[length(queue) + 1]] <<- c(input$dataCubeFileInput$name, input$classifierSelect)
      
      #create string to add to queue
      string <- "{{Data Cube: "
      string <- paste(string, input$dataCubeFileInput$name, sep = "")
      string <- paste(string, "} Classifier: ", sep = "")
      string <- paste(string, input$classifierSelect, sep = "")
      string <- paste(string, "}", sep = "")
      
      queueText <<- c(queueText, string)
    }
    
    if (length(queueText) == 0) {
      output$queue <- renderText({"queue is empty"})
    } else {
      #create one string seperated by new lines
      string <- queueText[1]
      
      if (length(queueText) > 1) {
        for (i in 2:length(queueText)) {
          string <- paste(string, queueText[i], sep = "\n")
        }
      }
      
      output$queue <- renderText({string})
    }
  })
  
  #Clear all items from queue
  observeEvent(input$clearQueue, {
    queueText <<- c()
    queue <<- c()
    
    output$queue <- renderText({"queue is empty"})
  })
  
  #Run all processes in queue
  observeEvent(input$runQueue, {
    if (length(queue) > 0) {
      print("run queue")
      print(queue)
      
      processQueue(queue)
    } else {
      print("queue is empty")
    }
  })
  
  #Generate Classifier
  observeEvent(input$generateClassifierButton, {
    if (is.null(input$ClassifierNameInput)) {
      print("no classifier name entered")
    } else {
      #generateRFClassifier(name, parameterList)
      
      #add to the list of classifier names
      classifierChoices <<- c(classifierChoices, input$ClassifierNameInput)
      
      #add the new classifier name to the drop down in the select data tab
      updateSelectInput(session, "classifierSelect", label = div(style="color: white;", "Classifier:"), classifierChoices)
      
      print("classifier generated")
    }
  })
  #Upload classifier
  observeEvent(input$uploadClassifierButton, {
    if (is.null(input$uploadClassifierNameInput)) {
      print("no classifier name entered")
    } else {
      
      #add to the list of classifier names
      classifierChoices <<- c(classifierChoices, input$uploadClassifierNameInput)
      
      #add the new classifier name to the drop down in the select data tab
      updateSelectInput(session, "classifierSelect", label = div(style="color: white;", "Classifier:"), classifierChoices)
      
      print("classifier uploaded")
    }
  })
}

shinyApp(ui = ui, server = server)