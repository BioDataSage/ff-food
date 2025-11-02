#' ARIMA Forecasting with Consistent Color Scheme
#'
#' Creates auto.arima forecasts maintaining consistent colors for each indicator
#' across India and World Average, matching the previous time series plot.
#'
#' @param data_india Data frame containing India data with columns: Year, Item, Value
#' @param data_world_avg Data frame containing World Average data with columns: Year, Item, Avg
#' @param forecast_horizon Integer. Number of years to forecast ahead. Default is 10.
#' @param confidence_level Numeric. Confidence level for prediction intervals (0-1). Default is 0.95.
#' @param ncol Integer. Number of columns in the grid layout. Default is 2.
#' @param color_palette Character. Color palette to use. Options: "Set2", "Set1", "Dark2", "viridis", "manual".
#'   Default is "Set2" to match your previous plot.
#' @param manual_colors Named vector of colors for each item (if color_palette = "manual")
#'
#' @return A grid of ggplot objects showing forecasts for each indicator
#'
#' @export
plot_arima_forecasts_consistent <- function(data_india, 
                                            data_world_avg, 
                                            forecast_horizon = 10,
                                            confidence_level = 0.95,
                                            ncol = 2,
                                            color_palette = "Set2",
                                            manual_colors = NULL) {
  
  # Get unique items
  items <- unique(data_india$Item)
  
  # Create color mapping for items
  if (color_palette == "manual" && !is.null(manual_colors)) {
    item_colors <- manual_colors
  } else if (color_palette == "viridis") {
    item_colors <- setNames(viridis::viridis(length(items)), items)
  } else {
    # Use RColorBrewer palettes
    item_colors <- setNames(RColorBrewer::brewer.pal(max(3, length(items)), color_palette)[1:length(items)], items)
  }
  
  # Store plots
  plot_list <- list()
  
  # Loop through each item
  for (item in items) {
    
    # Get the color for this item
    item_color <- item_colors[item]
    
    # Filter data for current item
    india_data <- data_india %>%
      filter(Item == item) %>%
      arrange(Year)
    
    world_data <- data_world_avg %>%
      filter(Item == item) %>%
      arrange(Year)
    
    # Create time series objects
    start_year <- as.numeric(format(min(india_data$Year), "%Y"))
    
    ts_india <- ts(india_data$Value, start = start_year, frequency = 1)
    ts_world <- ts(world_data$Avg, start = start_year, frequency = 1)
    
    # Fit auto.arima models
    fit_india <- auto.arima(ts_india, seasonal = FALSE)
    fit_world <- auto.arima(ts_world, seasonal = FALSE)
    
    # Generate forecasts
    forecast_india <- forecast(fit_india, h = forecast_horizon, level = confidence_level * 100)
    forecast_world <- forecast(fit_world, h = forecast_horizon, level = confidence_level * 100)
    
    # Prepare data for plotting
    last_year <- max(india_data$Year)
    forecast_years <- seq(as.Date(paste0(as.numeric(format(last_year, "%Y")) + 1, "-01-01")),
                          by = "year", length.out = forecast_horizon)
    
    # Historical data
    hist_data <- bind_rows(
      india_data %>% mutate(Type = "India", Series = "Historical") %>% select(Year, Value, Type, Series),
      world_data %>% mutate(Type = "World Average", Series = "Historical", Value = Avg) %>% select(Year, Value, Type, Series)
    )
    
    # Forecast data
    fc_india <- data.frame(
      Year = forecast_years,
      Value = as.numeric(forecast_india$mean),
      Lower = as.numeric(forecast_india$lower),
      Upper = as.numeric(forecast_india$upper),
      Type = "India",
      Series = "Forecast"
    )
    
    fc_world <- data.frame(
      Year = forecast_years,
      Value = as.numeric(forecast_world$mean),
      Lower = as.numeric(forecast_world$lower),
      Upper = as.numeric(forecast_world$upper),
      Type = "World Average",
      Series = "Forecast"
    )
    
    fc_data <- bind_rows(fc_india, fc_world)
    
    # Get model info
    india_model <- as.character(fit_india)
    world_model <- as.character(fit_world)
    
    # Create plot with consistent item color
    p <- ggplot() +
      # Historical data - India (solid line)
      geom_line(data = filter(hist_data, Type == "India"),
                aes(x = Year, y = Value),
                color = item_color,
                linewidth = 1,
                alpha = 0.8) +
      
      # Historical data - World Average (dashed line, same color)
      geom_line(data = filter(hist_data, Type == "World Average"),
                aes(x = Year, y = Value),
                color = item_color,
                linewidth = 1,
                linetype = "dashed",
                alpha = 0.6) +
      
      # Forecast lines - India (solid)
      geom_line(data = fc_india,
                aes(x = Year, y = Value),
                color = item_color,
                linewidth = 1.2,
                linetype = "solid") +
      
      # Forecast lines - World Average (dashed)
      geom_line(data = fc_world,
                aes(x = Year, y = Value),
                color = item_color,
                linewidth = 1.2,
                linetype = "dashed") +
      
      # Confidence intervals - India
      geom_ribbon(data = fc_india,
                  aes(x = Year, ymin = Lower, ymax = Upper),
                  alpha = 0.2, 
                  fill = item_color) +
      
      # Confidence intervals - World Average
      geom_ribbon(data = fc_world,
                  aes(x = Year, ymin = Lower, ymax = Upper),
                  alpha = 0.15, 
                  fill = item_color) +
      
      # Vertical line at forecast start
      geom_vline(xintercept = as.numeric(last_year), 
                 linetype = "dotted", 
                 color = "gray40", 
                 linewidth = 0.5) +
      
      # Annotation for forecast region
      annotate("text", 
               x = as.Date(paste0(as.numeric(format(last_year, "%Y")) + forecast_horizon/2, "-01-01")),
               y = Inf, 
               label = "Forecast", 
               vjust = 1.5, 
               color = "gray50", 
               size = 3,
               fontface = "italic") +
      
      # Labels and theme
      labs(
        title = item,
        subtitle = paste0("India: ", india_model, " | World: ", world_model),
        x = "Year",
        y = "Value"
      ) +
      
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = item_color),
        plot.subtitle = element_text(size = 7, hjust = 0.5, color = "gray40", margin = margin(b = 5)),
        axis.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "gray98", color = NA)
      )
    
    # Add manual legend
    p <- p + 
      annotate("segment", 
               x = min(hist_data$Year), 
               xend = min(hist_data$Year) + 365*2,
               y = Inf, yend = Inf,
               color = item_color, 
               linewidth = 1, 
               linetype = "solid") +
      annotate("text",
               x = min(hist_data$Year) + 365*2.5,
               y = Inf,
               label = "India",
               hjust = 0, vjust = 1.5,
               size = 3) +
      annotate("segment", 
               x = min(hist_data$Year) + 365*5, 
               xend = min(hist_data$Year) + 365*7,
               y = Inf, yend = Inf,
               color = item_color, 
               linewidth = 1, 
               linetype = "dashed") +
      annotate("text",
               x = min(hist_data$Year) + 365*7.5,
               y = Inf,
               label = "World Avg",
               hjust = 0, vjust = 1.5,
               size = 3)
    
    plot_list[[item]] <- p
  }
  
  # Arrange plots in grid with title
  grid.arrange(
    grobs = plot_list, 
    ncol = ncol,
    top = grid::textGrob(
      "ARIMA Forecasts: India vs World Average",
      gp = grid::gpar(fontsize = 16, fontface = "bold")
    )
  )
  
  # Return invisible list of plots
  invisible(plot_list)
}

