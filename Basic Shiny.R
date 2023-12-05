library(leaflet)
library(dplyr)
library(DT)
library(readr)
library(shiny)
library(ggplot2)

# Load the data
indy_data_bar <- read_csv("https://raw.githubusercontent.com/pushpit21/Brew628/breweries_income_attributes/business_analysis_indy.csv")
philly_data_bar <- read_csv("https://raw.githubusercontent.com/pushpit21/Brew628/breweries_income_attributes/business_analysis_philly.csv")

# Filter breweries with average rating 4 or above in Indianapolis
high_rated_indy <- indy_data_bar %>% filter(stars >= 4)

# Analyzing postal codes with high-rated breweries in Indianapolis
postal_code_analysis_indy <- high_rated_indy %>%
  group_by(postal_code) %>%
  summarise(across(c(stars, RestaurantsPriceRange2, Median_Income), mean, na.rm = TRUE)) %>%
  arrange(desc(stars))

# Filter breweries with average rating 4 or above in Philadelphia
high_rated_philly <- philly_data_bar %>% filter(stars >= 4)

# Analyzing postal codes with high-rated breweries in Philadelphia
postal_code_analysis_philly <- high_rated_philly %>%
  group_by(postal_code) %>%
  summarise(across(c(stars, RestaurantsPriceRange2, Median_Income), mean, na.rm = TRUE)) %>%
  arrange(desc(stars))

# Define UI
ui <- fluidPage(
  titlePanel("Breweries Analysis"),
  tabsetPanel(
    tabPanel("Map and Table", 
             selectInput("city", "Choose a city:", choices = c("Indianapolis", "Philadelphia")),
             fluidRow(
               column(6, DTOutput("table")),
               column(6, leafletOutput("map"))
             )
    ),
    tabPanel("Demographic and Attrs",
             sidebarLayout(
               sidebarPanel(
                 selectInput("city", "Select City:", choices = c("Indianapolis", "Philadelphia")),
                 checkboxGroupInput("attribute", "Select Attribute:",
                                    choices = c("HasTV", "full_bar", "DogsAllowed", "OutdoorSeating", "HappyHour"))
               ),
               mainPanel(
                 plotOutput("attributePlot"), # Existing plot for attributes
                 plotOutput("scatterPlot"),   # New plotOutput for the scatter plot
                 textOutput("cityInsights"),  # City-based insights
                 textOutput("attributeInsights")  # Attribute-based insights
               )
             )
    ),
    tabPanel("Sentiment Analysis and LDA", 
             selectInput("city", "Select City:", choices = c("Indianapolis", "Philadelphia")),
             fluidRow(
               column(6, plotOutput("sentimentPlot")),
               column(6, plotOutput("correlationPlot"))
             ),
             uiOutput("sentimentPlotText")
    ),
    tabPanel("Operational Hours Analysis", 
             selectInput("city_select", "Choose a city:", choices = c("Indianapolis", "Philadelphia")),
             fluidRow(
               column(6, plotOutput("quarterlyPlot")),
               column(6, plotOutput("monthlyPlot"))
             ),
             uiOutput("operationalText")
    ),
    tabPanel("Advice", 
             actionButton("resetButton", "Reset"),
             uiOutput("dynamicContent")
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
  selectedBarPlot <- reactiveVal()
  
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
  
  # Output for attribute analysis plot
  output$attributeInsights <- renderText({
    req(input$attribute)
    selected_attr <- input$attribute
    city <- input$city
    
    # Function to generate insights based on city and attribute
    generate_insights <- function(city, attr) {
      if(city == "Indianapolis") {
        indy_insights <- list(
          HasTV = "Presence of TV: Breweries without TVs tend to have slightly higher ratings.",
          full_bar = "Full Bar: Establishments with a full bar have lower ratings.",
          DogsAllowed = "Dogs Allowed: No significant difference in ratings.",
          OutdoorSeating = "OutdoorSeating: Rated lower on average than those without.",
          HappyHour = "HappyHour: Slightly higher average ratings, but not significant."
        )
        return(indy_insights[[attr]])
      } else {
        philly_insights <- list(
          HasTV = "Presence of TV: Lower ratings in breweries with TVs.",
          full_bar = "Full Bar: Breweries with full bars have lower ratings.",
          DogsAllowed = "Dogs Allowed: Policy on dogs does not significantly affect ratings.",
          OutdoorSeating = "OutdoorSeating: Significantly lower ratings than those without.",
          HappyHour = "HappyHour: Similar to Indianapolis, no significant influence on ratings."
        )
        return(philly_insights[[attr]])
      }
    }
    
    # Combine insights for all selected attributes
    insights <- sapply(selected_attr, function(attr) generate_insights(city, attr))
    paste(insights, collapse = "\n\n")
  })
  
  # Define the data for the plots
  topics <- c('Customer Views', 'Beer Selection', 'Group Experience', 'Ambience', 'Food')
  
  # Data for Indianapolis
  indy_sentiments <- c(0.1583, 0.2843, 0.122, 0.2505, 0.17722)
  indy_correlations <- c(-0.365895298347868, 0.38958586252027183, -0.18150857336471823, 0.10060617306904178, -0.12402223518854723)
  
  # Data for Philadelphia
  philly_sentiments <- c(0.1765, 0.227, 0.14821, 0.27605, 0.1645)
  philly_correlations <- c(0.14900777995278564, 0.1702181203461521, 0.3703436801405931, 0.2983731237455559, 0.38506143973578805)
  
  # Define reactive expressions for the data based on the selected city
  selected_city_data <- reactive({
    if (input$city_select == "Indianapolis") {
      list(sentiments = indy_sentiments, correlations = indy_correlations)
    } else if (input$city_select == "Philadelphia") {
      list(sentiments = philly_sentiments, correlations = philly_correlations)
    }
  })
  
  # Render the sentiment bar plot
  output$sentimentPlot <- renderPlot({
    data <- selected_city_data()
    ggplot(data.frame(Topic = topics, Value = data$sentiments), aes(x = Topic, y = Value)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Most Talked About", y = "Sentiment Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the correlation bar plot
  output$correlationPlot <- renderPlot({
    data <- selected_city_data()
    ggplot(data.frame(Topic = topics, Value = data$correlations), aes(x = Topic, y = Value)) +
      geom_bar(stat = "identity", fill = "salmon") +
      labs(title = "Correlation with Avg Star Rating and Sentiment With Weight", y = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Define the texts for each city
  philly_text <- "In the bustling city of Philadelphia, the highest correlations with average star ratings were linked to Group Experience, Food, and Ambience. This suggests that breweries in Philly thrive when they prioritize group dynamics, culinary experiences, and the overall ambiance. For brewery owners, these insights translate into actionable strategies. In Philadelphia, focusing on fostering vibrant group atmospheres, elevating culinary offerings, and enhancing the overall ambiance can significantly impact customer satisfaction."
  indy_text <- "Indianapolis exhibited a distinctive preference profile. Here, negative correlations with Group Experience and Food were notable, while positive correlations were observed with Beer Selection and Ambience. This implies that in Indy, customers are more responsive to a diverse beer selection and an inviting ambiance rather than group-focused experiences and extensive food offerings. The emphasis should shift towards curating a diverse beer selection and crafting a welcoming ambiance, de-emphasizing extensive group experiences and food options."
  
  # Render the UI output for sentiment plot interpretation
  output$sentimentPlotText <- renderUI({
    text <- if (input$city_select == "Indianapolis") {
      indy_text
    } else if (input$city_select == "Philadelphia") {
      philly_text
    } else {
      "Please select a city to see the interpretation."  # Default text
    }
    
    # Return an HTML box with the text
    wellPanel(
      tags$h4("Interpretation"),
      tags$p(text)
    )
  })
  
  output$adviceText <- renderUI({
    HTML("
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>Diversify Focus Areas:</strong> Prioritize factors beyond happy hour, outdoor seating, or a full bar. Invest in creating a unique, TV-free customer experience, as these elements resonate more strongly with patrons.
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>Tailor Offerings to Cities:</strong>
      <ul>
        <li><em>Philadelphia:</em> Elevate group experiences, culinary offerings, and ambiance for top-notch customer satisfaction. Consider moderate investments in revamping communal spaces and enhancing culinary options.</li>
        <li><em>Indianapolis:</em> Emphasize a diverse beer selection and inviting ambiance, adjusting focus from extensive group experiences and food offerings. Channel investments into expanding beer varieties and enhancing overall ambiance.</li>
      </ul>
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>Adapting to Seasonal Fluctuations:</strong> Recognize the potential for reduced operational costs in the 4th quarter, seizing the opportunity for efficient resource allocation during holidays and harsh winters. Strategically align operational hours with heightened activity in the 2nd and 3rd quarters, fostering increased customer engagement and potential revenue growth.
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>City-Centric Offerings and Pricing Strategies:</strong>
      <ul>
        <li>In Indianapolis, the spread of high-rated breweries across various postal codes emphasizes a city-wide appreciation for quality breweries. Tailor offerings to cater to diverse tastes, considering the success of high-rated breweries in areas with varying median incomes (e.g., 46219 and 46240).</li>
        <li>In Philadelphia, hotspots for high-rated breweries, such as 19144 and 19128, showcase the importance of understanding local preferences. Embrace the diversity in median incomes within successful areas.</li>
        <li>Emphasize a balanced approach to pricing, as mid-range pricing (\"2\") prevails among successful breweries in both cities. This ensures accessibility without compromising quality, contributing to sustained customer loyalty and positive ratings.</li>
      </ul>
    </div>
  ")
  })
  
  selectedCity <- reactiveVal()
  
  # Render the UI output for sentiment plot interpretation
  output$sentimentPlotText <- renderUI({
    text <- if (input$city_select == "Indianapolis") {
      indy_text
    } else if (input$city_select == "Philadelphia") {
      philly_text
    } else {
      "Please select a city to see the interpretation."  # Default text
    }
    
    # Return an HTML box with the text
    wellPanel(
      tags$h4("Interpretation"),
      tags$p(text)
    )
  })
  
  output$adviceText <- renderUI({
    HTML("
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>Diversify Focus Areas:</strong> Prioritize factors beyond happy hour, outdoor seating, or a full bar. Invest in creating a unique, TV-free customer experience, as these elements resonate more strongly with patrons.
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>Tailor Offerings to Cities:</strong>
      <ul>
        <li><em>Philadelphia:</em> Elevate group experiences, culinary offerings, and ambiance for top-notch customer satisfaction. Consider moderate investments in revamping communal spaces and enhancing culinary options.</li>
        <li><em>Indianapolis:</em> Emphasize a diverse beer selection and inviting ambiance, adjusting focus from extensive group experiences and food offerings. Channel investments into expanding beer varieties and enhancing overall ambiance.</li>
      </ul>
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>Adapting to Seasonal Fluctuations:</strong> Recognize the potential for reduced operational costs in the 4th quarter, seizing the opportunity for efficient resource allocation during holidays and harsh winters. Strategically align operational hours with heightened activity in the 2nd and 3rd quarters, fostering increased customer engagement and potential revenue growth.
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>City-Centric Offerings and Pricing Strategies:</strong>
      <ul>
        <li>In Indianapolis, the spread of high-rated breweries across various postal codes emphasizes a city-wide appreciation for quality breweries. Tailor offerings to cater to diverse tastes, considering the success of high-rated breweries in areas with varying median incomes (e.g., 46219 and 46240).</li>
        <li>In Philadelphia, hotspots for high-rated breweries, such as 19144 and 19128, showcase the importance of understanding local preferences. Embrace the diversity in median incomes within successful areas.</li>
        <li>Emphasize a balanced approach to pricing, as mid-range pricing (\"2\") prevails among successful breweries in both cities. This ensures accessibility without compromising quality, contributing to sustained customer loyalty and positive ratings.</li>
      </ul>
    </div>
  ")
  })
  
  output$scatterPlot <- renderPlot({
    req(input$city, input$attribute) # Ensure city and attributes are selected
    
    # Select data based on city
    data <- if(input$city == "Indianapolis") {
      indy_data_bar
    } else {
      philly_data_bar
    }
    
    # Filter data based on the selected attributes
    filtered_data <- data %>%
      filter_at(vars(one_of(input$attribute)), any_vars(. == TRUE))
    
    # Create scatter plot
    ggplot(filtered_data, aes(x = Median_Income, y = stars, color = as.factor(postal_code))) +
      geom_point() +
      labs(title = paste("Scatter Plot for", input$city),
           x = "Median Income",
           y = "Average Rating")
  })
  
  output$attributePlot <- renderPlot({
    req(input$attribute)
    
    # Select data based on city
    data <- if(input$city == "Indianapolis") {
      indy_data_bar
    } else {
      philly_data_bar
    }
    
    # Calculate average ratings
    plot_data <- lapply(input$attribute, function(attr) {
      with <- mean(data %>% filter(.[[attr]] == TRUE) %>% pull(stars), na.rm = TRUE)
      without <- mean(data %>% filter(.[[attr]] == FALSE) %>% pull(stars), na.rm = TRUE)
      
      data_frame(
        Attribute = attr,
        RatingType = "With",
        AvgRating = with
      ) %>%
        bind_rows(
          data_frame(
            Attribute = attr,
            RatingType = "Without",
            AvgRating = without
          )
        )
    }) %>% bind_rows()
    
    # Create side-by-side bar plot
    ggplot(plot_data, aes(x = Attribute, y = AvgRating, fill = RatingType)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("With" = "blue", "Without" = "red")) +
      labs(title = paste("Average Ratings by Selected Attributes in", input$city),
           y = "Average Rating") +
      theme_minimal()
  })
  
  # City-based insights
  output$cityInsights <- renderText({
    if(input$city == "Indianapolis") {
      return("Indianapolis Insights: HappyHour, OutdoorSeating, and full bars are associated with lower ratings. Focus on brew quality and ambiance.")
    } else {
      return("Philadelphia Insights: Neither HappyHour, OutdoorSeating, nor full bars enhance brewery ratings. Focus on brew quality and service.")
    }
  })
  
  # Attribute-based insights
  output$attributeInsights <- renderText({
    req(input$attribute)
    selected_attr <- input$attribute
    city <- input$city
    
    # Function to generate insights based on city and attribute
    generate_insights <- function(city, attr) {
      if(city == "Indianapolis") {
        indy_insights <- list(
          HasTV = "Presence of TV: Breweries without TVs tend to have slightly higher ratings.",
          full_bar = "Full Bar: Establishments with a full bar have lower ratings.",
          DogsAllowed = "Dogs Allowed: No significant difference in ratings.",
          OutdoorSeating = "OutdoorSeating: Rated lower on average than those without.",
          HappyHour = "HappyHour: Slightly higher average ratings, but not significant."
        )
        return(indy_insights[[attr]])
      } else {
        philly_insights <- list(
          HasTV = "Presence of TV: Lower ratings in breweries with TVs.",
          full_bar = "Full Bar: Breweries with full bars have lower ratings.",
          DogsAllowed = "Dogs Allowed: Policy on dogs does not significantly affect ratings.",
          OutdoorSeating = "OutdoorSeating: Significantly lower ratings than those without.",
          HappyHour = "HappyHour: Similar to Indianapolis, no significant influence on ratings."
        )
        return(philly_insights[[attr]])
      }
    }
    
    # Combine insights for all selected attributes
    insights <- sapply(selected_attr, function(attr) generate_insights(city, attr))
    paste(insights, collapse = "\n\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)