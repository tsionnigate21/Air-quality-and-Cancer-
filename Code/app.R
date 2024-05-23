library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(scales)
library(readr)

# Check if the directory exists
#dir.create("~/Documents/DATA/Data 332/files given/group project datas", recursive = TRUE)


#setwd("~/Documents/DATA/Data 332/files given/group project data")

# Define the user interface
ui <- fluidPage(
  titlePanel("Impact of Air Quality on Cancer Rates Across the U.S."),
  h3("Introduction"),
  p("Explore the complex relationships between air quality and cancer death rates across the U.S."),
  
  tabsetPanel(
    tabPanel("Research Overview",
             h4("Our Research and why we chose it"),
             p("We became interested in analyzing cancer death rates after one day hearing on the radio that one in every two women and one in every three men will develop cancer during their lifetime. The number of cancer incidents has only been rising, and the death rate has been alarmingly high. Even though our research does not delve deeply into the casualties of cancer, we believe that this could be a good start in having a general understanding of the relationship between air quality and cancer death rates."),
             h4("Why Did You Choose This Research?"),
             p("I chose this research topic due to the increasing importance of environmental factors in public health discussions and the potential to impact policy decisions through data-driven insights.")
    ),
    tabPanel("Project Scope & Backlog",
             h4("Research Requirements"),
             p("Our cancer death rate research required access to datasets on air quality and cancer death rates in various states across the United States. It then required us to join the data, clean it, and generate insights using different kinds of charts. We then published our findings on the Shiny App."),
             h4("Scope of the Project"),
             p("Our project's scope was to explore the potential relationship between air quality and cancer mortality rates in various states across the United States from 1980 to 2022. Using publicly available Kaggle datasets, we investigated correlations between the Air Quality Index (AQI) and cancer death rates. Using statistical models and data from the previous decades, we investigated trends and the effects of AQI on cancer rates. We hoped to provide some insights into this dataset, possibly demonstrating a link between AQI and death rates. While our findings do not demonstrate that AQI is a direct cause of cancer mortality, they do suggest that more extensive research is required to fully understand the potential links and underlying causes."),
             h4("Backlog of Ideas and Concepts"),
             p("We wanted to make a heat map with air quality days and a Point Map with AQI Ratings to see blah blah blah. However, we couldn’t get to it because of time constraints. We also found that Kentucky had a very high death rate, so we wanted to see.")
    ),
    tabPanel("Visualizations",
             tabsetPanel(
               tabPanel("AQI vs Cancer Death Rates", 
                        plotOutput("scatterPlot"),
                        p("This scatterplot illustrates a slight positive correlation between air quality and cancer mortality, suggesting that regions with poorer air quality tend to have higher cancer death rates. The linear regression line, subtly rising as AQI increases, visually supports this trend. However, the data points display considerable variability, indicating that while air quality might influence cancer rates, other factors also play significant roles. Notably, the concentration of data points at lower AQI values suggests that most observations are from areas with relatively better air quality, while the sparser data at higher AQI values, accompanied by some outliers, may warrant further research to understand anomalies or regional specifics affecting this relationship.")),
               tabPanel("State Comparison", 
                        plotOutput("stackedBarDeathRate"),
                        p("The graph displays the Median AQI and cancer death rates for several states side-by-side. In each state, the AQI levels are substantially higher than the corresponding cancer death rates, illustrated with AQI represented in red and death rates in blue. This stark contrast highlights the vast difference in scale between environmental air quality measurements and mortality rates due to cancer. Notably, states like Louisiana and Mississippi, which have higher values in both categories compared to others like Kentucky and West Virginia, suggest a potential correlation where higher pollution levels may be associated with higher cancer mortality.")),
               tabPanel("AQI Trends Over Time", 
                        plotOutput("aqiOverTime"),
                        p("The graph 'Trend of Median AQI Over Time in States with High Cancer Death Rates' visualizes the AQI trends from 1980 through the 2020s in states noted for their high cancer death rates. Notably, Kentucky exhibits a pronounced peak in AQI, suggesting periods of significantly poorer air quality relative to other states such as Arkansas, Louisiana, Mississippi, and West Virginia. This observation correlates with the fact that Kentucky also reports the highest cancer death rates among these states, suggesting a potential link between prolonged exposure to poor air quality and increased cancer mortality. This graph serves as a preliminary visual exploration, hinting at a possible association between air quality and health outcomes. While it does not establish causation, the alignment of high AQI and cancer rates in Kentucky encourages further analytical and epidemiological studies to explore this relationship more deeply.")),
               tabPanel("Pollutant Days - AQI", 
                        plotOutput("pollutantDaysAQI"),
                        p("Analyzes days with different pollutant levels in states with the worst AQI, providing insights into common air quality issues.")),
               tabPanel("Pollutant Days - Death Rates", 
                        plotOutput("pollutantDaysDeathRates"),
                        p("The graph 'Number of Pollutant Days in States with Highest Median AQI' showcases the prevalence of pollutants such as CO, NO2, and Ozone in states with significant air pollution issues, including California, Indiana, North Carolina, Ohio, and Texas. When juxtaposed with previous analysis showing states with high cancer death rates, this data suggests a potential correlation between high AQI and increased cancer mortality. States with elevated levels of pollutants like Ozone, which dominates the graph, could be further investigated to understand the impact on public health, particularly cancer rates."))
             )
    ),
    tabPanel("Summary or Conclusion",
             h4("Findings"),
             p("Overall, the analysis, while not conclusively proving causation, strongly supports the hypothesis that higher AQI levels are correlated with higher cancer death rates. The consistency across multiple types of visualizations and statistical examinations underscores the potential health impacts of prolonged exposure to poor air quality."),
             
             h4("Conclusion"),
             p("The research provides valuable insights into the relationship between air quality and cancer death rates, highlighting the potential health risks associated with poor air quality. While the findings do not demonstrate causation, they suggest that air quality is a significant factor that warrants further investigation and action to protect public health.")
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
