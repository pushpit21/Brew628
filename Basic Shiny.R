library(shiny)
library(leaflet)
library(dplyr)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Breweries Map and Data"),
  tabsetPanel(
    tabPanel("Map and Table", 
             selectInput("city", "Choose a city:", choices = c("Indianapolis", "Philadelphia")),
             fluidRow(
               column(6, DTOutput("table")),
               column(6, leafletOutput("map"))
             )
    ),
    tabPanel("Sentiment Analysis and LDA", 
             # FILL IN LATER
    ),
    tabPanel("Advice", 
             # FILL IN LATER
    ),
    tabPanel("Contact Us",
             h3("Contact Information"),
             p("Names: Philip Lin, Shantam Bhuraria, Pushpit Kaushik"),
             p("Email: pjlin@wisc.edu, sbhuraria@wisc.edu, pkaushik@wisc.edu")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Read data from the URLs
  indy_data_url <- "https://raw.githubusercontent.com/pushpit21/Brew628/sentimental_analysis/indi_beer_data.csv"
  philly_data_url <- "https://raw.githubusercontent.com/pushpit21/Brew628/sentimental_analysis/philly_beer_data.csv"
  
  # Initialize reactive variables to store the data
  indy_data <- reactiveVal()
  philly_data <- reactiveVal()
  
  # Load data for Indianapolis
  observe({
    indy_data(read.csv(indy_data_url))
  })
  
  # Load data for Philadelphia
  observe({
    philly_data(read.csv(philly_data_url))
  })
  
  # A reactive expression that returns the dataset based on selection
  selected_data <- reactive({
    if(input$city == "Indianapolis") {
      indy_data()
    } else {
      philly_data()
    }
  })
  
  # Render table
  output$table <- renderDT({
    data <- selected_data()
    if(is.null(data)) return()
    
    # Data Preprocessing: Aggregate by unique business ID and sort by avg_stars
    data_unique <- data %>%
      group_by(business_id) %>%
      summarise(
        name = first(name),
        address = first(address),
        postal_code = first(postal_code),
        avg_stars = mean(avg_star, na.rm = TRUE),
        .groups = 'drop'  
      ) %>%
      arrange(desc(avg_stars)) # Sort in descending order by avg_stars
    
    # Create a table without the business_id column
    datatable(data_unique %>% select(-business_id), options = list(pageLength = 10, searchHighlight = TRUE))
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    data <- selected_data()
    if(is.null(data)) return()
    
    # Data Preprocessing: Remove duplicate business IDs
    data_unique <- data %>%
      group_by(business_id) %>%
      summarise(
        name = first(name),
        avg_stars = mean(avg_star, na.rm = TRUE),
        latitude = first(latitude),
        longitude = first(longitude),
        .groups = 'drop'
      ) %>%
      arrange(desc(avg_stars)) # Sort in descending order by avg_stars
    
    # Define map center based on city
    map_center <- if(input$city == "Indianapolis") {
      c(-86.1581, 39.7684)
    } else {
      c(-75.1652, 39.9526) # Longitude and latitude for Philadelphia
    }
    
    # Create the map
    leaflet(data = data_unique) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = map_center[1], lat = map_center[2], zoom = 12) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                       popup = ~paste(name, '<br>', avg_stars, 'stars'), 
                       radius = 6, fillOpacity = 0.8, color = "#00a1e1")
  })
}

# Run the application
shinyApp(ui = ui, server = server)