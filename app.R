library(shiny)

ui <- fluidPage(
  fluidRow(
    column(3,
      #Drop down for classifier selection
      selectInput("classifierSelect", 
            "Classifier:", 
            c("Classifier 1", "Classifier 2", "Classifier 3"))
    ),
    column(1,
      #Button to generate a classifier
      actionButton("generateClassifierButton", "Generate Classifier")
    )
  ),
  
  fluidRow(
    column(5,
      #File browser for data cubes
      #accept gives the browser a hint of what kind of files the server is expecting.
      fileInput("dataCubeFileInput", "Data Cube:", accept = c(".txt"))
    ),
    
    column(1, actionButton("plot", "New Plot"))
  ),
  
  fluidRow(column(12, plotOutput("hist")))
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