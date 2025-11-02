library(tidyverse)
library(purrr)
source("scripts/functions/Auto_Arima.R")

# 01 Importing Data ----------------------------------------
Interesting_Indicators <- c("Percentage of children under 5 years affected by wasting (percent)",
                            # "Dietary energy supply used in the estimation of the prevalence of undernourishment (kcal/cap/day)",
                            "Percentage of children under 5 years of age who are stunted (modelled estimates) (percent)",
                            "Prevalence of anemia among women of reproductive age (15-49 years) (percent)",
                            "Prevalence of low birthweight (percent)")

data_India <- read.csv("data/02_data_of_india_overyear_and_world_averages/FAOSTAT_India_data_en_11-2-2025.csv") %>% 
  filter(!grepl("-", Year)) %>% 
  filter(Item %in% Interesting_Indicators) %>% 
  select(Area,Year, Item, Value) %>% 
  mutate(
    # Create a new column that is a proper date set to January 1st of that year
    Year = make_date(year = Year, month = 1, day = 1)
  )


head(data_India, 5)
unique(data_India$Item)


## Importing Averages -------------------
### reading averages 
avg_anaemia <- read.csv("data/india_overyear/average_anemia.csv")%>% 
  mutate(
    # Create a new column that is a proper date set to January 1st of that year
    Year = make_date(year = Year, month = 1, day = 1)
  )%>% 
  mutate(Item = Interesting_Indicators[3])



avg_birthweight <- read.csv("data/india_overyear/average_birthweight.csv")%>% 
  mutate(
    # Create a new column that is a proper date set to January 1st of that year
    Year = make_date(year = Year, month = 1, day = 1)
  )%>% 
  mutate(Item = Interesting_Indicators[4])

avg_stunted <- read.csv("data/india_overyear/average_stunted.csv")%>% 
  mutate(
    # Create a new column that is a proper date set to January 1st of that year
    Year = make_date(year = Year, month = 1, day = 1)
  )%>% 
  mutate(Item = Interesting_Indicators[2])

avg_wasting <- read.csv("data/india_overyear/average_wasting.csv")%>%
  rename("Year" = "year") %>% 
  rename("Avg" = "avg") %>% 
  mutate(
    # Create a new column that is a proper date set to January 1st of that year
    Year = make_date(year = Year, month = 1, day = 1)
  ) %>% 
  mutate(Item = Interesting_Indicators[1])

### binded all the average together
avg_values_over_world <- rbind(avg_anaemia, avg_birthweight, avg_stunted, avg_wasting)

avg_values_over_world$Avg <- as.numeric(avg_values_over_world$Avg)



# 02. Plotting the time series data -----------------------------------------
## Adding trend line and averages with their confidence interval -------------------------------------

# Prepare data with proper grouping for legend
data_India_plot <- data_India %>%
  mutate(Type = "India")

avg_values_plot <- avg_values_over_world %>%
  rename(Value = Avg) %>%
  mutate(Type = "World Average")

# Combine datasets
combined_data <- bind_rows(data_India_plot, avg_values_plot)

# Create plot with proper legend ordering
over_the_year <- ggplot(combined_data, aes(x = Year, y = Value, color = Item, linetype = Type)) +
  
  # Raw lines (India only)
  geom_line(
    data = filter(combined_data, Type == "India"),
    alpha = 0.2, 
    linewidth = 0.5
  ) + 
  
  # Smooth lines (India only)
  geom_smooth(
    data = filter(combined_data, Type == "India"),
    method = "loess",
    se = TRUE,
    linewidth = 1.5,
    alpha = 0.15
  ) +
  
  # World average lines
  geom_line(
    data = filter(combined_data, Type == "World Average"),
    alpha = 0.6,
    linewidth = 1
  ) +
  
  # Enhanced labels
  labs(
    title = "Different Indicator Values Over Time in India",
    subtitle = "Comparison with World Average",
    x = "Year",
    y = "Value",
    color = "Indicator",
    linetype = "Data Source"
  ) +
  
  # Manual linetype to control order
  scale_linetype_manual(
    values = c("India" = "solid", "World Average" = "dashed"),
    breaks = c("India", "World Average")  # Controls legend order
  ) +
  
  # Color palette
  scale_color_brewer(palette = "Set2") +
  
  # Date axis formatting
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  guides(
    linetype = guide_legend(
      override.aes = list(color = "grey50")  # Makes both India and World Average grey in legend
    )
  ) +
  
  # Enhanced theme
  theme_minimal(base_size = 12) +
  theme(
    # Title styling
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0, margin = margin(b = 15), color = "gray30"),
    
    # Axis styling
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.line = element_line(color = "gray30", linewidth = 0.5),
    
    # Legend styling
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "gray80", linewidth = 0.5),
    legend.key.size = unit(1.2, "lines"),
    legend.box = "vertical",  # Stack legends vertically
    
    # Grid styling
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
    
    # Background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "gray98", color = NA),
    
    # Margins
    plot.margin = margin(15, 15, 15, 15)
  )

windows(16, 10)
over_the_year

# ggsave("assets/data_prediction_ARIMA/trend_over_the_year.png", over_the_year_trend, dpi = 600)



# 03. Performing Auto ARIMA ----------------

plot_pred_auto_ARIMA <- plot_arima_forecasts_consistent(
  data_India, 
  avg_values_over_world, 
  forecast_horizon = 10,
  color_palette = "Set2"  # This matches your previous ggplot
)

windows(16, 10)
plot_pred_auto_ARIMA
# ggsave("assets/data_prediction_ARIMA/ARIMA_model.png", plot_pred_auto_ARIMA, dpi = 600)
