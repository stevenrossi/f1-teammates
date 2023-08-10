library(shiny)
library(shinythemes)

source("f1-teammates.R")

# Define UI for random distribution app ----
ui <- fluidPage(

  theme=shinytheme("united"),

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
                  tags$footer("By ",
                              tags$a("Steven Rossi",href="https://www.github.com/stevenrossi"),
                              "using the",
                              tags$a("Ergast Formula One dataset",href="https://ergast.com/mrd/"),
                              ".",
                    align = "left",
                    style = "position:relative;
                             bottom:10px;
                             width:100%;
                             height:5px;
                             color: #e7552c;
                             padding: 10px;
                             background-color: white;
                             z-index: 1000;"
                             )
      )
    ),
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {

  plotTeammateViz <- reactive({#driver1 = "Gasly, Pierre" )
    
    driver1 <- input$driver

    dat1   <- filter(dat,driverName==driver1) %>%
              mutate(line=0)
    dat1$plotx <- 1:nrow(dat1)
  
    driverSurname <- gsub(",.*","",driver1)
  
    # Function for generating tooltip text
    tooltipFun <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.null(x$x)) return(NULL)
  
      movie <- dat[dat$x == x$x, ]
      paste0("<b>", movie$raceName,
             "</b><br>Year: ", movie$year,
             "</b><br>Round: ", movie$round,
             "<br> Team: ", movie$constructorName,
             "<br> Finishing positions:<br>&emsp;",
             driverSurname,": ",movie$positionOrder,
             "<br>&emsp;",gsub(",.*","",movie$Teammate),": ",movie$teammatePosition
      )
    }
  
    vis <- reactive({
      dat1 %>%
        ggvis(~yearx, ~relPosition) %>%
        layer_points(size := 50, size.hover := 200, fill=~Teammate,
          fillOpacity := 0.7, fillOpacity.hover := 1,
          shape=~raceWinner, key:=~x) %>%
        add_tooltip(tooltipFun, "hover") %>%
        add_axis("x",
                 title = "Year",
                 format="d") %>%
        add_axis("y", title = "Finishing position relative to teammate") %>%
        layer_paths(~yearx,~line,stroke:="black",strokeWidth:=3) %>%
        add_legend( scales = "fill",
                    orient = "left" ) %>%
        add_legend( scales = "shape",
                    title = "Race Winner",
                    orient = "right",
                    values = c(driver1,"Teammate","Neither")) %>%
        set_options(width = 1000, height = 500)
  
  
    })
  
    vis %>% bind_shiny("plot")
  })



  # Generate a plot of the data ----
  output$plot <- renderPlot({
    
    plotTeammateViz()

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