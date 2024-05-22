library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(scales)
library(readr)

# Check if the directory exists
dir.create("~/Desktop/Air Quality and Cancer Analysis", recursive = TRUE)


setwd("~/Desktop/Air Quality and Cancer Analysis")

# Define the user interface
ui <- fluidPage(
  titlePanel("Impact of Air Quality on Cancer Rates Across the U.S."),
  h3("Introduction"),
  p("Explore the complex relationships between air quality and cancer death rates across the U.S."),
  
  tabsetPanel(
    tabPanel("Research Overview",
             h4("What is Your Research?"),
             p("This research focuses on analyzing the relationship between air quality indicators such as AQI and cancer death rates in the U.S. This study aims to identify potential environmental and policy factors that could influence public health outcomes."),
             h4("Why Did You Choose This Research?"),
             p("I chose this research topic due to the increasing importance of environmental factors in public health discussions and the potential to impact policy decisions through data-driven insights.")
    ),
    tabPanel("Project Scope & Backlog",
             h4("Research Requirements"),
             p("The research requires comprehensive data on air quality and health outcomes across different geographic locations over multiple years."),
             h4("Scope of the Project"),
             p("The project covers data collection, cleaning, analysis, and visualization to assess trends and correlations between air quality and cancer rates."),
             h4("Backlog of Ideas and Concepts"),
             p("Future enhancements may include predictive modeling to forecast trends and the expansion of data sources to include other environmental factors.")
    ),
    tabPanel("Visualizations",
             tabsetPanel(
               tabPanel("AQI vs Cancer Death Rates", 
                        plotOutput("scatterPlot"),
                        p("This chart plots the relationship between median AQI values and cancer death rates across various states, highlighting potential correlations.")),
               tabPanel("State Comparison", 
                        plotOutput("stackedBarDeathRate"),
                        p("This visualization compares the average AQI and cancer death rates in the top states with the highest rates, illustrating how air quality may correlate with health outcomes.")),
               tabPanel("AQI Trends Over Time", 
                        plotOutput("aqiOverTime"),
                        p("This graph shows trends in AQI over time for states with significant changes in cancer rates, indicating potential long-term environmental impacts.")),
               tabPanel("Pollutant Days - AQI", 
                        plotOutput("pollutantDaysAQI"),
                        p("Analyzes days with different pollutant levels in states with the worst AQI, providing insights into common air quality issues.")),
               tabPanel("Pollutant Days - Death Rates", 
                        plotOutput("pollutantDaysDeathRates"),
                        p("Focuses on the number of pollutant days in states with high cancer death rates, exploring links between specific pollutants and health outcomes."))
             )
    ),
    tabPanel("Summary or Conclusion",
             h4("Summary of Findings"),
             p("This section will summarize the key findings from the study, highlighting major trends and potential areas for policy intervention."),
             h4("Conclusion"),
             p("Conclusions drawn from the research will be discussed here, along with recommendations for policymakers and public health officials.")
    )
  )
)
# Define server logic
server <- function(input, output) {
  # Read and merge datasets
  state_names <- c(
    "AK" = "Alaska", "AL" = "Alabama", "AR" = "Arkansas", "AZ" = "Arizona",
    "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut", "DC" = "District of Columbia",
    "DE" = "Delaware", "FL" = "Florida", "GA" = "Georgia", "HI" = "Hawaii",
    "IA" = "Iowa", "ID" = "Idaho", "IL" = "Illinois", "IN" = "Indiana",
    "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana", "MA" = "Massachusetts",
    "MD" = "Maryland", "ME" = "Maine", "MI" = "Michigan", "MN" = "Minnesota",
    "MO" = "Missouri", "MS" = "Mississippi", "MT" = "Montana", "NC" = "North Carolina",
    "ND" = "North Dakota", "NE" = "Nebraska", "NH" = "New Hampshire", "NJ" = "New Jersey",
    "NM" = "New Mexico", "NV" = "Nevada", "NY" = "New York", "OH" = "Ohio",
    "OK" = "Oklahoma", "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island",
    "SC" = "South Carolina", "SD" = "South Dakota", "TN" = "Tennessee",
    "TX" = "Texas", "UT" = "Utah", "VA" = "Virginia", "VT" = "Vermont",
    "WA" = "Washington", "WI" = "Wisconsin", "WV" = "West Virginia", "WY" = "Wyoming"
  )
  
  # Read the CSV files without any modification to column names
  deathrate <- read_csv('uscs_map_death_all.csv')
  airquality <- read_csv('AQI By State 1980-2022.csv')
  
  # Function to replace spaces with periods in column names
  replace_spaces <- function(name) {
    gsub(" ", ".", name)
  }
  
  # Update column names by replacing spaces with periods
  colnames(deathrate) <- replace_spaces(colnames(deathrate))
  colnames(airquality) <- replace_spaces(colnames(airquality))
  
  # Assuming 'state_names' is correctly defined elsewhere
  # Use the modified column names for operations like mutate
  deathrate <- deathrate %>%
    mutate(State = state_names[State])
  
  # Combine the data, ensuring the key for joining has correct column name if modified
  combined_data <- left_join(airquality, deathrate, by = "State")
  
  # Prepare the data for visualizations
  state_data <- combined_data %>%
    group_by(State) %>%
    summarise(
      Average_Median_AQI = mean(`Median.AQI`, na.rm = TRUE),
      Average_Death_Rate = mean(Rate, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Visual 1: Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(combined_data, aes(x = `Median.AQI`, y = Rate)) +
      geom_point() +
      geom_smooth(method = "lm", color = "blue") +
      labs(title = "Relationship Between Median AQI and Cancer Death Rates", x = "Median AQI", y = "Cancer Death Rate") +
      theme_minimal()
  })
  
  # Visual 2: Stacked Bar Chart
  output$stackedBarDeathRate <- renderPlot({
    top5_death_rates <- state_data %>%
      arrange(desc(Average_Death_Rate)) %>%
      slice_head(n = 5) %>%
      pivot_longer(
        cols = c(Average_Median_AQI, Average_Death_Rate),
        names_to = "Metric",
        values_to = "Value"
      )
    
    ggplot(top5_death_rates, aes(x = State, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Average_Median_AQI" = "red", "Average_Death_Rate" = "blue")) +
      labs(title = "Top 5 States by Death Rate: AQI and Cancer Death Rates",
           x = "State", y = "Value") +
      theme_minimal()
  })
  
  # Visual 3: AQI Over Time
  output$aqiOverTime <- renderPlot({
    filtered_aqi_time_data <- combined_data %>%
      filter(State %in% top5_death_rates$State) %>%
      mutate(Decade = floor(Year / 10) * 10)
    
    ggplot(filtered_aqi_time_data, aes(x = Year, y = `Median.AQI`, color = State, group = State)) +
      geom_line() +
      labs(title = "Trend of Median AQI Over Time in States with High Cancer Death Rates",
           x = "Year", y = "Median AQI") +
      theme_minimal() +
      theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  })
  
  # Visual 4 and 5: Pollutant Days for AQI and Death Rates
  output$pollutantDaysAQI <- renderPlot({
    pollutant_data_aqi <- combined_data %>%
      filter(State %in% top5_aqi$State) %>%
      select(State, `Days.CO`, `Days.NO2`, `Days.Ozone`) %>%
      pivot_longer(cols = c(`Days.CO`, `Days.NO2`, `Days.Ozone`), names_to = "Pollutant", values_to = "Days")
    
    ggplot(pollutant_data_aqi, aes(x = State, y = Days, fill = Pollutant)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_brewer(palette = "Set1", name = "Pollutant Type") +
      labs(title = "Number of Pollutant Days in Top 5 States by Median AQI",
           x = "State", y = "Number of Days") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$pollutantDaysDeathRates <- renderPlot({
    pollutant_data_death_rates <- combined_data %>%
      filter(State %in% top5_death_rates$State) %>%
      select(State, `Days.CO`, `Days.NO2`, `Days.Ozone`) %>%
      pivot_longer(cols = c(`Days.CO`, `Days.NO2`, `Days.Ozone`), names_to = "Pollutant", values_to = "Days")
    
    ggplot(pollutant_data_death_rates, aes(x = State, y = Days, fill = Pollutant)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_brewer(palette = "Set1", name = "Pollutant Type") +
      labs(title = "Number of Pollutant Days in Top 5 States by Cancer Death Rates",
           x = "State", y = "Number of Days") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

