install.packages("tidyverse")
library("tidyverse")
install.packages("dplyr")
install.packages("maps")
install.packages("ggplot2")
install.packages("plotly")
library("plotly")
library(maps)
library(dplyr)
library(ggplot2)

cleaned_unicef1 <- read.csv("cleaned_unicef1.csv")
cleaned_metadata <- read.csv("cleaned_metadata.csv")

data_join <- full_join(cleaned_metadata, cleaned_unicef1)

data_join <- full_join(cleaned_metadata, cleaned_unicef1, by = join_by(country, year))
data_join <- full_join(cleaned_metadata, cleaned_unicef1, by = c("country" , "year" ))
full_join(cleaned_metadata, cleaned_unicef1, by = join_by(country, year))
full_join(cleaned_metadata, cleaned_unicef1, by = c("country" , "year" ))



# bar graph
average_deprivation <- cleaned_unicef1 %>%
  group_by(country) %>%
  summarize(AverageDeprivation = mean(obs_value, na.rm = TRUE))
top_countries <- average_deprivation %>%
  arrange(desc(AverageDeprivation)) %>%
  top_n(10, AverageDeprivation) %>%
  mutate(country = factor(country, levels = rev(unique(country))))  # Ensure correct ordering in the plot

ggplot(top_countries, aes(x = country, y = AverageDeprivation, fill = country)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(  # Adjust y-axis scale to fit the data range properly
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_fill_viridis_d(direction = -1, guide = "none") +  # Color scale
  labs(title = "A Closer Look at Inequality: Child Welfare by Country",
       subtitle = "Top 10 Countries by Average Percentage of Children Suffering from At Least Two Deprivations 2011-2018",
       x = "Country",
       y = "Average Percentage of Children in Deprivation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Improve legibility of country names
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"  # Removing the legend as it's redundant
  )


time_series_data <- cleaned_unicef1 %>%
  filter(year >= 2010 & year <= 2020) %>%
  arrange(country, year)

# Function to format percentages for the y-axis
format_percent <- function(x) {
  paste0(formatC(x * 100, format = "f", digits = 1), "%")
}















#scatter plot with a linear regression line

library(tidyverse)
library(ggplot2)


# Ensure the country column is in the same format
cleaned_metadata$country <- tolower(cleaned_metadata$country)
cleaned_unicef1$country <- tolower(cleaned_unicef1$country)

# Join the datasets
combined_data <- inner_join(cleaned_unicef1, cleaned_metadata, by = "country")

# Select and rename the necessary columns
combined_data <- combined_data %>%
  select(
    country,
    `GDP per capita` = `GDP per capita (constant 2015 US$)`,  # Renaming for easier reference
    `Percentage Children Suffering` = obs_value  # Simplified naming
  )

# Create the scatter plot with a linear regression line
ggplot(combined_data, aes(x = `GDP per capita`, y = `Percentage Children Suffering`, color = country)) +
  geom_point(alpha = 0.8) +  # Points with transparency
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Linear regression line with confidence interval
  labs(
    title = "Economic Prosperity and Its Impact on Child Well-being",
    subtitle = "Analyzing the correlation between GDP per capita and child deprivation rates",
    x = "GDP Per Capita (constant 2015 USD)",
    y = "Percentage of Children Experiencing Deprivations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    plot.caption = element_text(size = 10, face = "italic")
  ) +
  scale_color_viridis_d()  # Enhanced color scale

# Print the plot
print(ggplot)








#map plotting here

map_world <- map_data("world")


cleaned_metadata$country <- tolower(cleaned_metadata$country)
cleaned_unicef1$country <- tolower(cleaned_unicef1$country)
combined_data <- full_join(cleaned_metadata, cleaned_unicef1, by = "country", relationship = "many-to-many")
map_cleaned_metadata <- full_join(map_world, combined_data, by = c("region" = "country"))
manual_percent_format <- function(x) {
  scales::percent(x / 100) 
}


ggplot(map_cleaned_metadata, aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon(color = "white", linewidth = 0.5) +  # Ensures country borders are visible
  coord_fixed(1.3) +  # Keeps the map's aspect ratio correct
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      name = "Percentage Suffering\nAt Least Two Deprivations",
                      labels = scales::percent_format(scale = 1)) +  # Uses a blue gradient and formats labels as percentages
  labs(title = "Global Distribution of Child Deprivation",
       subtitle = "Percentage of children suffering at least two deprivations",
       caption = "Data source: UNICEF") +
  theme_minimal() +
  theme(legend.position = "right",  # Adjusts legend position
        plot.title = element_text(size = 16, face = "bold"),  # Enhances title aesthetics
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "italic"))





