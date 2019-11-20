library(shiny)

ui <- fluidPage(
  #Drop down for classifier selection
  selectInput("classifierSelect", 
              "Classifier:", 
              c("Classifier 1", "Classifier 2", "Classifier 3")),
  
  #Button to generate a classifier
  actionButton("generateClassifierButton", "Generate Classifier"),
  
  #File browser for data cubes
  #accept gives the browser a hint of what kind of files the server is expecting.
  fileInput("dataCubeFileInput", "Data Cube:", accept = c(".txt")),
  
  actionButton("plot", "New Plot"),
  
  plotOutput("hist")
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