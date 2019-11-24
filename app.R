library(shiny)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
          .navbar .navbar-nav {float: right}
          .navbar .navbar-header {float: left}
          .navbar-inner { background-color: #121212 }
          .navbar-static-top {
  position: static;
  margin-bottom: 0px;
  background-color: #121212
  
          }
        "))
  ),
  navbarPage("Spectral Site",
             tabPanel("Home",
                      br(),
                      br(),
                          sidebarLayout(
                            
                            sidebarPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Select Data", 
                                                   fluidRow(
                                                     column(6,
                                                            #Drop down for classifier selection
                                                            selectInput("classifierSelect", 
                                                                        "Classifier:", 
                                                                        c("Classifier 1", "Classifier 2", "Classifier 3"))
                                                     ),
                                                     column(1,
                                                            #Button to generate a classifier
                                                            actionButton("selectClassifierButton", "Select")
                                                     )
                                                   ),
                                                   tags$head(tags$style(
                                                          HTML('
                                                            body, input, button, select { 
                                                                font-family: "Calibri";
                                                                background-color: #121212;
                                                            }')
                                                   )),
                                                   fluidRow(
                                                     column(8,
                                                            #File browser for data cubes
                                                            #accept gives the browser a hint of what kind of files the server is expecting.
                                                            fileInput("dataCubeFileInput", "Data Cube:", accept = c(".txt"))
                                                     ),
                                                     
                                                     
                                                   ),
                                                   fluidRow(
                                                     column(8,
                                                            #File browser for data cubes
                                                            #accept gives the browser a hint of what kind of files the server is expecting.
                                                            textInput("Text Input", "Process Name:", value = "Enter text..."),
                                                     ),
                                                     

                                                   ),
                                                   fluidRow(
                                        
                                                     column(1,
                                                            #Button to generate a classifier
                                                            actionButton("addToQueueButton", "Add to Queue")
                                                     )
                                                     
                                                   ),
                                                   ),
                                          tabPanel("Generate Class", 
                                                   fluidRow(
                                                     column(8,
                                                            #File browser for data cubes
                                                            #accept gives the browser a hint of what kind of files the server is expecting.
                                                            textInput("Classifier", "Generate Classifier:", value = "Species"),
                                                     ),
                                                  
                                                     column(1,
                                                            #Button to generate a classifier
                                                            actionButton("generateClassifierButton", "Generate")
                                                     )
                                                   ),
                                                   tags$head(tags$style(
                                                     HTML('
                                                            body, input, button, select { 
                                                                font-family: "Calibri";
                                                                background-color: #121212;
                                                            }')
                                                   )),
                                                  
                                                   fluidRow(
                                                     column(8,
                                                            #File browser for data cubes
                                                            #accept gives the browser a hint of what kind of files the server is expecting.
                                                            textInput("ClassifierNameInput", "Classifier Name:", value = ""),
                                                     ),
                                                     
                                                     
                                                   ),
                                                   fluidRow(
                                                     
                                                     column(1,
                                                            #Button to generate a classifier
                                                            actionButton("addClassifierButton", "Add Classifier")
                                                     )
                                                     
                                                   ),
                                                   
                                                   )
                                          
                              ),
                              
                            ),
                            
                            mainPanel(
                              
                              tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
                              tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
                              tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: red}")),
                              tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: red}")),
                             
                              sliderInput("integer", "Integer:",
                                          min = 0, max = 1000,
                                          value = 500),
                              sliderInput("integer", "Integer:",
                                          min = 0, max = 1000,
                                          value = 500),
                              sliderInput("integer", "Integer:",
                                          min = 0, max = 1000,
                                          value = 500),
                              sliderInput("integer", "Integer:",
                                          min = 0, max = 1000,
                                          value = 500),
                              
                              
                            )  
                        
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
  
  #create an initial histogram with 100 random numbers
  output$hist <- renderPlot({
    hist(rnorm(100))
  })
  
  observeEvent(input$plot, {
    #create a histogram with 100 random numbers
    output$hist <- renderPlot({
      hist(rnorm(100))
    })
  })
}

shinyApp(ui = ui, server = server)