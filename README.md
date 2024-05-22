# Air-Quality-and-Cancer
Cancer Death rates &amp; Air Quality 
## Introduction 
 ![image](https://github.com/tsionnigate21/Air-quality-and-Cancer-/assets/159511253/e59cec16-d62a-4bee-ba21-ff05c34156b2)
This Shiny application explores the complex relationships between air quality and cancer 
death rates across the United States. The project includes data cleaning, merging datasets,
and generating insights through various visualizations. Below is a detailed explanation 
of the steps involved, including code snippets from the provided script.


## Data Dictionary 📖
1. Variables we used for our analysis are:
2. Geo_Loc: Geographic location, possibly including latitude and longitude or a specific address.
3. Year: The year in which the data was collected.
4. State: The U.S. state to which the data pertains.
5. Good.Days: Number of days categorized as having 'Good' air quality.
6. Hazardous.Days: Days categorized as 'Hazardous'.
7. Median.AQI: The median AQI value for the year.
8. Days.CO: Number of days with carbon monoxide as the primary pollutant.
9. Days.NO2: Number of days with nitrogen dioxide as the primary pollutant.
10. Days.Ozone: Number of days with ozone as the primary pollutant.
11. Rate: May refer to a rate of an event or condition, such as cancer incidence or mortality rates, related to the population size.

## Data Cleaning 🧹
1. Clean the Data
We replace spaces in column names with periods to make them easier to work with in R:
```
replace_spaces <- function(name) {
  gsub(" ", ".", name)
}
colnames(deathrate) <- replace_spaces(colnames(deathrate))
colnames(airquality) <- replace_spaces(colnames(airquality))
```
2. Standardize State Names
The deathrate data uses state abbreviations, so we map these to full state names for consistency:
```
state_names <- c(
  "AK" = "Alaska", "AL" = "Alabama", "AR" = "Arkansas", "AZ" = "Arizona",
  # (Other state mappings)
)
deathrate <- deathrate %>%
  mutate(State = state_names[State])
```


## Analysis 
### User Interface (UI) Definition
The UI is defined using the fluidPage function which includes a title, introduction, research overview, project scope, visualizations, and summary or conclusion.
```
ui <- fluidPage(
  titlePanel("Impact of Air Quality on Cancer Rates Across the U.S."),
  h3(""),
  p(""),
  tabsetPanel(
    tabPanel("",
             h4(""),
             p(""),
```
### Server Logic
Our server logic handles reading and merging datasets, data cleaning, and generating visualizations.
### Reading and Merging Datasets
Fist, we read the CSV files and merge them

## Data Preparation
1. Read the Data
First, we read the two CSV files containing air quality and cancer death rate data:
```
deathrate <- read_csv('uscs_map_death_all.csv')
airquality <- read_csv('AQI By State 1980-2022.csv')
```


2. Merge the Datasets
We merge the air quality and cancer death rate datasets using the left_join function on the "State" column:
```combined_data <- left_join(airquality, deathrate, by = "State")```

3. Summarize the Data
To generate meaningful visualizations, we summarize the data by calculating the average median AQI and average cancer death rate for each state:
```
state_data <- combined_data %>%
  group_by(State) %>%
  summarise(
    Average_Median_AQI = mean(`Median.AQI`, na.rm = TRUE),
    Average_Death_Rate = mean(Rate, na.rm = TRUE)
  ) %>%
  ungroup()
```
## Visualizations
1. Scatter Plot: AQI vs. Cancer Death Rates
 Our scatter plot shows the relationship between median AQI values and cancer death
 rates across various states. It helps to identify potential correlations between air quality and cancer mortality.
```
output$scatterPlot <- renderPlot({
  ggplot(combined_data, aes(x = `Median.AQI`, y = Rate)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    labs(title = "Relationship Between Median AQI and Cancer Death Rates", 
         x = "Median AQI", 
         y = "Cancer Death Rate") +
    theme_minimal()
})
```
2. Stacked Bar Chart: State Comparison
This stacked bar chart compares the average AQI and cancer death rates in the top states with the highest rates.
It illustrates how air quality may correlate with health outcomes.
```
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
         x = "State", 
         y = "Value") +
    theme_minimal()
})
```
3. Line Plot: AQI Trends Over Time
This line plot shows trends in AQI over time for states with significant
changes in cancer rates, indicating potential long-term environmental impacts.
```
output$aqiOverTime <- renderPlot({
  filtered_aqi_time_data <- combined_data %>%
    filter(State %in% top5_death_rates$State) %>%
    mutate(Decade = floor(Year / 10) * 10)
  
  ggplot(filtered_aqi_time_data, aes(x = Year, y = `Median.AQI`, color = State, group = State)) +
    geom_line() +
    labs(title = "Trend of Median AQI Over Time in States with High Cancer Death Rates",
         x = "Year", 
         y = "Median AQI") +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))
})
```
4. Bar Chart: Pollutant Days for AQI & Pollutant Days for Death Rates
This bar chart analyzes days with different pollutant levels in states with
the worst AQI, providing insights into common air quality issues.
We used the functions
filter(State %in% top5_death_rates$State),
select(State, Days.CO, Days.NO2, Days.Ozone),
pivot_longer(...), 
geom_bar(stat = "identity", position = "stack")
scale_fill_brewer(...): 
labs(...): 
theme_minimal(): 
theme(axis.text.x 
```
output$pollutantDaysAQI <- renderPlot({
  pollutant_data_aqi <- combined_data %>%
    filter(State %in% top5_aqi$State) %>%
    select(State, `Days.CO`, `Days.NO2`, `Days.Ozone`) %>%
    pivot_longer(cols = c(`Days.CO`, `Days.NO2`, `Days.Ozone`), names_to = "Pollutant", values_to = "Days")
```

# Shiny App Links 
https://yanetgezu.shinyapps.io/Airs/
http://127.0.0.1:4598/

