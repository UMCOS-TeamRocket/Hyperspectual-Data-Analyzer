# Hyperspectual-Data-Analyzer
COS 397 Capstone Project

Some Shiny Notes:
Reactive Functions
  render*()
    renderDataTable()
    renderImage()
    renderPlot()
    renderPrint()
    renderTable()
    renderText()
    renderUI()
    
  reactive()
    builds a reactive object.
    ex. data <- reactive({rnorm(input$blah)})
    data will be updated whenever the blah input value is changed
    data can be used to pass the same value to different functions
    call these reactive expressions like a function ex. data()
    
Prevent reactions with
  isolate()
  ex. changing the title of a plot without updating the values of the plot
  
Trigger Code with
  observeEvent() execute code block when a specific reactive value changes
  observe() similar to observeEvent, except instead of telling it what to react to, it react to any reactive value in its code block
  
Delay reactions
  eventReactive() use this for situations similar to the example given under reactive()
  
reactiveValues()
  creates a list of reactive values
  manipulate these values ex. rv <- reactiveValues(data = input$blah), rv$Data <- ...
  
SEE shinyFlow.PNG

fluidRow()
  adds rows to the grid that is the webpage
column()
  adds a column within a row. width of a column is 1 to 12 units