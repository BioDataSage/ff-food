library(tidyverse)

Interesting_Indicators <- c("Percentage of children under 5 years affected by wasting (percent)",
                            "Dietary energy supply used in the estimation of the prevalence of undernourishment (kcal/cap/day)",
                            "Percentage of children under 5 years of age who are stunted (modelled estimates) (percent)",
                            "Prevalence of anemia among women of reproductive age (15-49 years) (percent)",
                            "Prevalence of low birthweight (percent)")

data_India <- read.csv("data/FAOSTAT_India_data_en_11-2-2025.csv") %>% 
  filter(!grepl("-", Year)) %>% 
  filter(Item %in% Interesting_Indicators) %>% 
  select(Area,Year, Item, Value) %>% 
  mutate(
    # Create a new column that is a proper date set to January 1st of that year
    Year = make_date(year = Year, month = 1, day = 1)
  )


head(data_India, 5)

unique(data_India$Item)


ggplot(data_India[data_India$Item != "Dietary energy supply used in the estimation of the prevalence of undernourishment (kcal/cap/day)", ],
       aes(x = Year, y = Value, color = Item)) +
  
  # Optional: Keep the original jagged lines faint in the background
  geom_line(alpha = 0.3) + 
  
  # Add the smooth line (this is the spline)
  geom_smooth(
    method = "loess",     # LOESS is good for non-linear time series without a strict model
    se = TRUE,           # Set to TRUE if you want to show the standard error ribbon
    size = 1.2            # Make the smooth line thicker
  ) +
  
  labs(
    title = "Value Over Time for Different Items in India (Smoothed)",
    x = "Year",
    y = "Value",
    color = "Item Type"
  ) +
  theme_minimal() +
  # Use scale_x_date to control the display of dates
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + # Adjusted breaks for more labels
  
  # *** Add this theme modification to rotate the text ***
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )




