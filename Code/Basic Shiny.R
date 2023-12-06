library(leaflet)
library(dplyr)
library(DT)
library(readr)
library(ggplot2)
library(shiny)
library(shinyjs)
library(shinyWidgets)

# Load the data
indy_data_bar <- read_csv("https://raw.githubusercontent.com/pushpit21/Brew628/breweries_income_attributes/business_analysis_indy.csv")
philly_data_bar <- read_csv("https://raw.githubusercontent.com/pushpit21/Brew628/breweries_income_attributes/business_analysis_philly.csv")
indy_data_url <- "https://raw.githubusercontent.com/pushpit21/Brew628/sentimental_analysis/indi_beer_data.csv"
philly_data_url <- "https://raw.githubusercontent.com/pushpit21/Brew628/sentimental_analysis/philly_beer_data.csv"

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
  useShinyjs(),
  tags$style(HTML("
    #darkmode-switch {
        transform: scale(0.75); /* Adjust the size by changing the scale value */
        position: absolute;
        right: 10px; /* Adjusts the right margin for positioning */
        top: 10px; /* Adjusts the top margin for positioning */
        z-index: 100; /* Ensures the switch stays on top of other elements */
    }
      
    .dark-mode {
      background-color: #121212;
      color: #c0c0c0; /* Adjusted to a slightly darker shade of light grey for better contrast */
      /* Other dark mode styles */
    }
    
    /* Specific styles for checkbox labels in dark mode */
    .dark-mode .shiny-input-container label {
        color: #c0c0c0; /* Adjusted label text to a slightly darker shade of light grey for better contrast */
    }
    
    /* Styles for checkboxes in dark mode */
    .dark-mode .shiny-input-container input[type='checkbox'] {
        accent-color: #c0c0c0; /* Changes the checkmark color */
    }
    
    .dark-mode .shiny-input-container input[type='checkbox']:before {
        border-color: #c0c0c0; /* Changes the border color of the checkbox */
    }
    
    /* You can also style the selectInput dropdown for dark mode if needed */
    .dark-mode .shiny-input-container select {
        background-color: #333; /* Dark background for dropdown */
        color: #c0c0c0; /* Light text color for dropdown options */
        border-color: #c0c0c0; /* Light border color for dropdown */
    }
    
    .dark-mode .dataTables_wrapper .dataTable {
        color: #c0c0c0; /* Light text color for better readability */
    }
    
    .dark-mode .dataTables_wrapper .dataTable th {
        background-color: #333; /* Darker background for table headers */
        color: #fff; /* White text for headers */
    }
    
    .dark-mode .dataTables_wrapper .dataTable td {
        background-color: #222; /* Slightly lighter background for table cells */
    }
")),
  titlePanel("Breweries Analysis"),
  div(
    switchInput(
      inputId = "darkmode", 
      label = "", 
      value = FALSE
    ),
    id = "darkmode-switch"
  ),
  tabsetPanel(
    tabPanel("Map and Table", 
             selectInput("cityMap", "Choose a city:", choices = c("Indianapolis", "Philadelphia")),
             fluidRow(
               column(8, DTOutput("table")),
               column(4, leafletOutput("map"))
             )
    ),
    tabPanel("Demographic and Attributes",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cityDemo", "Select City:", choices = c("Indianapolis", "Philadelphia")),
                 checkboxGroupInput("attribute", "Select Attribute(s):",
                                    choices = c("TV Available" = "HasTV", 
                                                "Full Bar" = "full_bar", 
                                                "Dog-Friendly" = "DogsAllowed", 
                                                "Outdoor Seating Available" = "OutdoorSeating", 
                                                "Happy Hour Offers" = "HappyHour"))
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
             selectInput("citySent", "Select City:", choices = c("Indianapolis", "Philadelphia")),
             fluidRow(
               column(6, plotOutput("sentimentPlot")),
               column(6, plotOutput("correlationPlot"))
             ),
             uiOutput("sentimentPlotText")
    ),
    tabPanel("Operational Hours Analysis", 
             selectInput("cityOp", "Choose a city:", choices = c("Indianapolis", "Philadelphia")),
             fluidRow(
               column(6, plotOutput("quarterlyPlot")),
               column(6, plotOutput("monthlyPlot"))
             ),
             uiOutput("operationalText")
    ),
    tabPanel("Advice", 
             selectInput("cityAdvice", "For specific advice, select a city:", 
                         choices = c("General Advice" = "General Advice", "Indianapolis" = "Indianapolis", "Philadelphia" = "Philadelphia"),
                         selected = "General Advice"),
             uiOutput("adviceText"),
             uiOutput("adviceSpecific")
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
  # Dark Mode
  observe({
    if(input$darkmode) {
      # Apply dark mode styles
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      # Remove dark mode styles
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })
  
  selectedBarPlot <- reactiveVal()

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
    if(input$cityMap == "Indianapolis") {
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
        avg_reviews = mean(review_count, na.rm = TRUE),  # Add review_count
        .groups = 'drop'  
      ) %>%
      arrange(desc(avg_stars)) # Sort in descending order by avg_stars
    
    # Rename columns for user-friendliness
    colnames(data_unique)[which(names(data_unique) == "name")] <- "Business Name"
    colnames(data_unique)[which(names(data_unique) == "address")] <- "Address"
    colnames(data_unique)[which(names(data_unique) == "postal_code")] <- "Postal Code"
    colnames(data_unique)[which(names(data_unique) == "avg_stars")] <- "Average Stars"
    colnames(data_unique)[which(names(data_unique) == "avg_reviews")] <- "Number of Reviews"
    
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
    map_center <- if(input$cityMap == "Indianapolis") {
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
    city <- input$cityDemo
    
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
    if (input$citySent == "Indianapolis") {
      list(sentiments = indy_sentiments, correlations = indy_correlations)
    } else if (input$citySent == "Philadelphia") {
      list(sentiments = philly_sentiments, correlations = philly_correlations)
    }
  })
  
  # Render the sentiment bar plot
  output$sentimentPlot <- renderPlot({
    data <- selected_city_data()
    ggplot(data.frame(Topic = topics, Value = data$sentiments), aes(x = Topic, y = Value)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Most Talked About", y = "Mean of Weights") +
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
    text <- if (input$citySent == "Indianapolis") {
      indy_text
    } else if (input$citySent == "Philadelphia") {
      philly_text
    } else {
      "Please select a city to see the interpretation."  
    }
    
    # Return an HTML box with the text
    wellPanel(
      tags$h4("Interpretation"),
      tags$p(text)
    )
  })
  
  output$adviceText <- renderUI({
    if(is.null(input$cityAdvice) || input$cityAdvice == "General Advice") {
      HTML("
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>1. Diversify Focus Areas:</strong> Prioritize factors beyond happy hour, outdoor seating, or a full bar. Invest in creating a unique, TV-free customer experience, as these elements resonate more strongly with patrons.
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>2. Tailor Offerings to Cities:</strong>
      <ul>
        <li><em>Philadelphia:</em> Elevate group experiences, culinary offerings, and ambiance for top-notch customer satisfaction. Consider moderate investments in revamping communal spaces and enhancing culinary options.</li>
        <li><em>Indianapolis:</em> Emphasize a diverse beer selection and inviting ambiance, adjusting focus from extensive group experiences and food offerings. Channel investments into expanding beer varieties and enhancing overall ambiance.</li>
      </ul>
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>3. Adapting to Seasonal Fluctuations:</strong> 
      <li>Recognize the potential for reduced operational costs in the 4th quarter, seizing the opportunity for efficient resource allocation during holidays and harsh winters.</li>
      <li>Strategically align operational hours with heightened activity in the 2nd and 3rd quarters, fostering increased customer engagement and potential revenue growth.</li>
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>4. City-Centric Offerings and Pricing Strategies:</strong>
      <ul>
        <li>In Indianapolis, the spread of high-rated breweries across various postal codes emphasizes a city-wide appreciation for quality breweries. Tailor offerings to cater to diverse tastes, considering the success of high-rated breweries in areas with varying median incomes (e.g., 46219 and 46240).</li>
        <li>In Philadelphia, hotspots for high-rated breweries, such as 19144 and 19128, showcase the importance of understanding local preferences. Embrace the diversity in median incomes within successful areas.</li>
        <li>Emphasize a balanced approach to pricing, as mid-range pricing (\"2\") prevails among successful breweries in both cities. This ensures accessibility without compromising quality, contributing to sustained customer loyalty and positive ratings.</li>
      </ul>
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>5. Full Bar: </strong>In both cities, having a full bar is statistically significant and negatively impacts ratings. Consider alternative beverage options or a more selective bar menu that aligns with local preferences.
    </div>
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <strong>6. Outdoor Seating: </strong>This feature also negatively impacts ratings in both cities. Re-evaluate the use of outdoor spaces, focusing perhaps more on ambiance and comfort or repurposing these areas differently.
    </div>  ")
    } else {
      # Return NULL when a city is selected, which makes the advice text disappear
      return(NULL)
    }
  })
  
  output$scatterPlot <- renderPlot({
    req(input$cityDemo, input$attribute) # Ensure city and attributes are selected
    
    # Select data based on city
    data <- if(input$cityDemo == "Indianapolis") {
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
      scale_x_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"), breaks = scales::breaks_extended()) +
      labs(title = paste("Scatter Plot for", input$cityDemo),
           x = "Median Income (in Thousands USD)",
           y = "Average Rating")
  })
  
  output$attributePlot <- renderPlot({
    req(input$attribute)
    
    # Select data based on city
    data <- if(input$cityDemo == "Indianapolis") {
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
      scale_fill_manual(values = c("With" = "skyblue", "Without" = "salmon")) +
      labs(title = paste("Average Ratings by Selected Attributes in", input$cityDemo),
           y = "Average Rating") +
      theme_minimal()
  })
  
  # City-based insights
  output$cityInsights <- renderText({
    if(input$cityDemo == "Indianapolis") {
      return("Indianapolis Insights: HappyHour, OutdoorSeating, and full bars are associated with lower ratings. Focus on brew quality and ambiance.")
    } else {
      return("Philadelphia Insights: Neither HappyHour, OutdoorSeating, nor full bars enhance brewery ratings. Focus on brew quality and service.")
    }
  })
  
  # Attribute-based insights
  output$attributeInsights <- renderText({
    req(input$attribute)
    selected_attr <- input$attribute
    city <- input$cityDemo
    
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
  
  # Define the specific advice for each city
  indy_advice <- HTML("
  <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
    <strong>1. Beer Selection and Ambiance: </strong>Focus on offering a diverse beer selection and creating an inviting ambiance. These elements have positive correlations with customer ratings in Indianapolis.
  </div>
  <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
    <strong>2. De-emphasize Group Experience and Food Offerings: </strong>Unlike Philadelphia, Indianapolis customers place less emphasis on group experiences and food offerings.
  </div>
  <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
    <strong>3. Operational Hours Adjustment: </strong>Consider extended operational hours during the 2nd and 3rd quarters to capitalize on increased activity, and potentially reduce hours in the 4th quarter due to the holiday season and harsh winter.
  </div>  <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
    <strong>4. Location Strategy: </strong>Pay attention to various postal codes, as high-rated breweries are spread across the city. Both high and mid-income areas appreciate quality breweries.
  </div>
  ")
  
  philly_advice <- HTML("
  <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
    <strong>1. Group Experience, Food, and Ambiance: </strong>Elevate these aspects as they are highly correlated with customer satisfaction in Philadelphia. Focus on enhancing the culinary offerings and the overall ambiance of your brewery.
  </div>
  <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
    <strong>2. Seasonal Operational Strategy: </strong>Like Indianapolis, consider adapting your operational hours based on seasonal trends, with more hours in the warmer 2nd and 3rd quarters.
  </div>
  <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
    <strong>3. Target Specific Postal Codes: </strong>Focus on areas like 19144 and 19128, which are hotspots for high-rated breweries. These areas have a wide range of median incomes, suggesting a diverse customer base.
  ")
  
  # Render the specific advice based on selected city
  output$adviceSpecific <- renderUI({
    if(input$cityAdvice == "Indianapolis") {
      indy_advice
    } else if(input$cityAdvice == "Philadelphia") {
      philly_advice
    } else {
      # Default text before a city is selected
      HTML("<p></p>")
    }
  })
  
  # Operational Tab data
  indy_monthly_data <- data.frame(
    Month = 1:12,
    Value = c(-771.263457, -304.717806, 653.571030, 1007.588418, 680.784464, 79.751414, 59.387713, 263.855755, 191.168941, -715.004458, -526.949855, -618.172159)
  )
  
  indy_quarterly_data <- data.frame(
    Quarter = 1:4,
    Value = c(-422.410233, 1768.124296, 514.412408, -1860.126471)
  )
  
  philly_monthly_data <- data.frame(
    Month = 1:12,
    Value = c(-613.214288, -157.962538, 550.167971, 459.679665, 307.908620, -331.196847, 175.326439, 219.126017, 340.321114, -437.731564, -401.942547, -410.482042)
  )
  
  philly_quarterly_data <- data.frame(
    Quarter = 1:4,
    Value = c(-221.008855, 736.391438, 734.773570, -1250.156153)
  )
  
  # Reactive expression for monthly plot
  monthlyPlotData <- reactive({
    if(input$cityOp == "Indianapolis") {
      list(data = indy_monthly_data, color = "skyblue")
    } else if(input$cityOp == "Philadelphia") {
      list(data = philly_monthly_data, color = "salmon")
    } else {
      NULL  # When no city is selected
    }
  })
  
  # Reactive expression for quarterly plot
  quarterlyPlotData <- reactive({
    if(input$cityOp == "Indianapolis") {
      list(data = indy_quarterly_data, color = "skyblue")
    } else if(input$cityOp == "Philadelphia") {
      list(data = philly_quarterly_data, color = "salmon")
    } else {
      NULL  # When no city is selected
    }
  })
  
  # Render monthly bar plot
  output$monthlyPlot <- renderPlot({
    plotData <- monthlyPlotData()
    if(is.null(plotData)) return(NULL)
    
    ggplot(plotData$data, aes(x = Month, y = Value)) +
      geom_col(fill = plotData$color) +
      labs(title = "Monthly Analysis", x = "Month", y = "Value") +
      theme_minimal()
  })
  
  # Render quarterly bar plot
  output$quarterlyPlot <- renderPlot({
    plotData <- quarterlyPlotData()
    if(is.null(plotData)) return(NULL)
    
    ggplot(plotData$data, aes(x = Quarter, y = Value)) +
      geom_col(fill = plotData$color) +
      labs(title = "Quarterly Analysis", x = "Quarter", y = "Value") +
      theme_minimal()
  })
  
  # Define the texts for each city
  philly_text <- HTML("
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <li>Recognize the potential for reduced operational costs in the 4th quarter, seizing the opportunity for efficient resource allocation during holidays and harsh winters.</li>
      <li>Strategically align operational hours with heightened activity in the 2nd and 3rd quarters, fostering increased customer engagement and potential revenue growth.</li>
    </div>
  ")
  indy_text <- HTML("
    <div style='margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px;'>
      <li>Recognize the potential for reduced operational costs in the 4th quarter, seizing the opportunity for efficient resource allocation during holidays and harsh winters.</li>
      <li>Strategically align operational hours with heightened activity in the 2nd and 3rd quarters, fostering increased customer engagement and potential revenue growth.</li>
    </div>
  ")
  
  # Render the UI output for sentiment plot interpretation
  output$operationalText <- renderUI({
    text <- if (input$citySent == "Indianapolis") {
      indy_text
    } else if (input$citySent == "Philadelphia") {
      philly_text
    } else {
      "Please select a city to see the interpretation."  
    }
    
    # Return an HTML box with the text
    wellPanel(
      tags$h4("Interpretation"),
      tags$p(text)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
