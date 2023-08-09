library(shiny)

source("f1-teammates.R")

# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Formula 1 Head-To-Head Teammate Comparison App"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for choosing dataset ----
      selectInput(inputId = "driver",
                  label = "Choose a driver:",
                  choices = sort(unique(dat$driverName)),
                  selected = "Verstappen, Max"),

      width=2.5
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  selected="Plot",
                  tabPanel("Driver Info", tableOutput("driverInfo")),
                  tabPanel("Plot", ggvisOutput("plot")),
                  tabPanel("Table",
                           tableOutput("table"),br(),
                           "Legend:",br(),
                           "firstRace, lastRace - Year of first and last race, respectively, as teammates",br(),
                           "nRaces  - Number of races between selected driver and teammate",br(),
                           "pWin  - Proportion of races in which selected driver finished higher than teammate",br(),
                           "avgPositionDiff  - Mean difference between finishing position of selected driver and teammate",br(),
                           "totalPointsDiff  - Total difference in points scored between selected driver and teammate",br(),


                           ),
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {

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
    
    plotTeammateViz(input$driver)

  })

  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    getTeammateTable(input$driver)
  })

  # Generate an HTML table view of the data ----
  output$driverInfo <- renderTable({
    getDriverInfo(input$driver)
  })

}

# Create Shiny app ----
shinyApp(ui, server)