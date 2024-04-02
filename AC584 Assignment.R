install.packages("tidyverse")
install.packages("plotly")


library(tidyverse)
library(plotly)

# Disable Scientific Number Format
options(scipen = 999)

# Insert of Data files
unicef_indicator_1_1_ <- read_csv("unicef_indicator_1 (1).csv")
unicef_indicator_2_1_ <- read_csv("unicef_indicator_2 (1).csv")
unicef_metadata_1_ <- read_csv("unicef_metadata (1).csv")


# Final Data object
  # Converting doubles to characters
unicef_indicator_2_1_ <- mutate(unicef_indicator_2_1_, time_period = as.character(time_period))
unicef_metadata_1_ <- mutate(unicef_metadata_1_, year = as.character(year))
  # Joining data files
final_data_object <- unicef_indicator_1_1_ %>%
  full_join(unicef_indicator_2_1_, by = c("country", "alpha_2_code", "alpha_3_code", "numeric_code", "indicator", "time_period",
                                                "obs_value", "sex", "unit_multiplier", "unit_of_measure", "observation_status", "observation_confidentaility",
                                                "time_period_activity_related_to_when_the_data_are_collected", "current_age")) %>%
  full_join(unicef_metadata_1_, by = c("country", "alpha_2_code", "alpha_3_code", "numeric_code", "time_period" = "year"))
  # Removing unnecessary variables from final data object & changing names of variables
final_data_object  <- select(final_data_object, -alpha_2_code, -alpha_3_code, -numeric_code, -iso3c)



map_world <- map_data("world")

#Map filtered to breastfeeding rates
library(dplyr)
library(ggplot2)

final_data_object %>%
  filter(indicator == "Continued breastfeeding (20-23 months)") %>%
  full_join(map_world, by = c("country" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +  # adjust the aspect ratio if needed
  theme_void() +  # removes axis labels and ticks
  theme(text = element_text(face = "italic")) +  # sets italic font
  labs(fill = expression(bold("Breastfeeding Rate") ~ italic("(%")))  # sets legend title with bold and italic parts






# Timeseries - Avg. breastfeeding rates across developing countries
# Creation of Avg. yearly breastfeeding rates data
library(dplyr)
library(ggplot2)
library(plotly)

avg_breastfeeding_yearly <- final_data_object %>%
  filter(indicator == "Continued breastfeeding (20-23 months)") %>%
  group_by(time_period) %>%
  summarize(avg_obs_value = mean(obs_value, na.rm = TRUE))

# Convert time_period to a Date object
avg_breastfeeding_yearly$time_period <- as.Date(paste0(avg_breastfeeding_yearly$time_period, "-01-01"))

# Ensure all years shown from 1986 onwards
avg_breastfeeding_yearly <- subset(avg_breastfeeding_yearly, time_period >= as.Date("1986-01-01"))

# Format time_period to display only the year
avg_breastfeeding_yearly$year <- format(avg_breastfeeding_yearly$time_period, "%Y")

# Round Avg. Breastfeeding Rate values to 2 decimal places
avg_breastfeeding_yearly$avg_obs_value <- round(avg_breastfeeding_yearly$avg_obs_value, 2)

# Create the ggplot object with simplified tooltip
p <- ggplot(avg_breastfeeding_yearly, aes(x = time_period, y = avg_obs_value, text = paste("Time Period: ", year, "<br>Avg. Breastfeeding Rate (%): ", avg_obs_value), group = 1)) +
  geom_line(color = "blue", size = 1, linetype = "solid") +
  labs(x = "Time Period", y = "Average Breastfeeding Rate", title = "<b>Time Series Plot of Avg. Breastfeeding Rates<b>") +
  theme_minimal()

# Convert the ggplot object to a plotly object
p <- ggplotly(p, tooltip = "text")

# Display the interactive plot
p











# Scatter Plot

library(ggplot2)
library(plotly)

  # Convert time_period to a Date object
avg_breastfeeding_yearly$time_period <- as.Date(avg_breastfeeding_yearly$time_period)

  # Format time_period to display only the year (YYYY)
avg_breastfeeding_yearly$year <- format(avg_breastfeeding_yearly$time_period, "%Y")

  # Round avg_obs_value to two decimal places
avg_breastfeeding_yearly$avg_obs_value_rounded <- round(avg_breastfeeding_yearly$avg_obs_value, 2)

  # Create the ggplot object with scatterplot
p <- ggplot(avg_breastfeeding_yearly, aes(x = time_period, y = avg_obs_value,
                                          text = paste("Time Period: ", year, "<br>",
                                                       "Avg. Breastfeeding Rate: ", avg_obs_value_rounded, "%"))) +
  geom_point(color = "blue") +  # Scatterplot
  labs(x = "Time Period", y = "Average Breastfeeding Rate (%)", title = "<b>Average Breastfeeding Rates in Developing Countries (%)<b>") +
  theme_minimal()

  # Convert the ggplot object to a plotly object
p <- ggplotly(p, tooltip = "text", dynamicTicks = TRUE)

  # Fit linear regression
lm_model <- lm(avg_obs_value ~ time_period, data = avg_breastfeeding_yearly)
regression_line <- data.frame(time_period = seq(min(avg_breastfeeding_yearly$time_period), max(avg_breastfeeding_yearly$time_period), by = "1 month"),
                              avg_obs_value = predict(lm_model, data.frame(time_period = seq(min(avg_breastfeeding_yearly$time_period), max(avg_breastfeeding_yearly$time_period), by = "1 month"))))

  # Add linear regression line
p <- add_trace(p, data = regression_line, x = ~time_period, y = ~avg_obs_value, type = 'scatter', mode = 'lines', line = list(color = 'red'))
  
  # Display the interactive plot
p





#Bar Chart

library(plotly)

# Convert the column to numeric
final_data_object$`GDP per capita (constant 2015 US$)` <- as.numeric(final_data_object$`GDP per capita (constant 2015 US$)`)

# Group by time_period and calculate the average GDP per capita for each year
avg_gdp_yearly <- final_data_object %>%
  group_by(time_period) %>%
  summarize(avg_gdp_value = mean(`GDP per capita (constant 2015 US$)`, na.rm = TRUE))

# Filter the data to include only values from 1985 onwards
avg_gdp_yearly <- avg_gdp_yearly[avg_gdp_yearly$time_period >= 1985, ]

# Create an interactive bar chart with customized tooltips
plot_ly(data = avg_gdp_yearly, x = ~time_period, y = ~avg_gdp_value, type = 'bar', marker = list(color = 'skyblue'),
        text = ~paste("Time Period: ", time_period, "<br>",
                      "Avg. GDP per capita (US$): $", round(avg_gdp_value, 2)), hoverinfo = "text") %>%
  layout(title = "<b>Yearly Average GDP per Capita<b> (US$)",
         xaxis = list(title = "Year", tickangle = 45, tickmode = "array", tickvals = seq(min(avg_gdp_yearly$time_period), max(avg_gdp_yearly$time_period), by = 5)),
         yaxis = list(title = "Average GDP per Capita (US$)"))









                              