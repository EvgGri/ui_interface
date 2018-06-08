library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Intelligent project management"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      radioButtons("dist", "Issue class type:",
                   c("Digital" = "norm",
                     "Loyalty" = "unif",
                     "Biometrics" = "lnorm",
                     "State services" = "exp",
                     "Geolocation" = "norm",
                     "Artificial Intelligence" = "unif",
                     "Medicine" = "lnorm",
                     "Chat-bot" = "exp")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Number of elements in class:",
                  value = 3,
                  min = 1,
                  max = 5),
      
      h5(textOutput("currentTime"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#75AADB", border = "white")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d()
  })
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Cur. time info:", Sys.time())
  })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)