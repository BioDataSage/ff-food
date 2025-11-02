library(forecast)
# Load a datasets
anemia_women <- read.csv("data/india_overyear/anemia_women.csv")
low_birthweight <- read.csv("data/india_overyear/low_birthweight.csv")
child_stunted <- read.csv("data/india_overyear/child_stunted_full.csv")
wasted <- read.csv("data/india_overyear/wasting.csv")
avg_anemia <- read.csv("data/india_overyear/average_anemia.csv")
avg_birthweight <- read.csv("data/india_overyear/average_birthweight.csv")
avg_stunted <- read.csv("data/india_overyear/average_stunted.csv")
avg_wasting <- read.csv("data/india_overyear/average_wasting.csv")



#Load the data from FAOSTAT website
dataA <- ts(anemia_women$Amemia_Women, start = c(anemia_women$Year[1]), frequency = 1)
dataB <- ts(low_birthweight$low_birthweight, start = c(low_birthweight$Year[1]), frequency = 1)
dataC <- ts(child_stunted$Percent.Children, start = c(child_stunted$Year[1]), frequency = 1)
dataW <- ts(wasted$Wasting, start = c(wasted$Year[1]), frequency = 1)
dataA_avg <-ts(avg_anemia$Avg, start = c(avg_anemia$Year[1]), frequency = 1)
dataB_avg <-ts(avg_birthweight$Avg, start = c(avg_birthweight$Year[1]), frequency = 1)
dataC_avg <-ts(avg_stunted$Avg, start = c(avg_stunted$Year[1]), frequency = 1)
dataW_avg <-ts(avg_wasting$Avg, start = c(avg_wasting$Year[1]), frequency = 1)



#Plotting the data
#Plot the data for prevalence of anemia in women 
plot(dataA, 
     main = "Comparison of data",
     xlab = "Year", 
     ylab = "Percent",
     ylim = range(20,100), 
     col = "blue") 

# Add data for prevalence of low birth weight 
lines(dataB, col = "red")

#Adds data for prevalence of stunted children 
lines(dataC, col = "green")

lines(dataW, col = "purple")

#Adding a legend
legend("topleft", 
       legend = c("Prevelance of low birth weight" ,"Prevelance of children under 5 who are stunted", "Prevelance of anemia among woman of reproductive age"),
       col = c("red", "green", "blue"),
       lty = c(1, 1, 1), # Line types: 1=solid
       cex = 0.8) # Adjust legend text size


# Use auto.arima to find the best model
arima_model_a <- auto.arima(dataA, seasonal = FALSE) 
arima_model_b <- auto.arima(dataB, seasonal = FALSE) 
arima_model_c <- auto.arima(dataC, seasonal = FALSE) 
arima_model_w <- auto.arima(dataW, seasonal = FALSE) 
arima_model_a_avg <- auto.arima(dataA_avg, seasonal = FALSE) 
arima_model_b_avg <- auto.arima(dataB_avg, seasonal = FALSE) 
arima_model_c_avg <- auto.arima(dataC_avg, seasonal = FALSE) 
arima_model_w_avg <- auto.arima(dataW_avg, seasonal = FALSE) 

fit_auto <- auto.arima(dataW, trace = TRUE)


# Forecast the next 5 years (h = number of periods to forecast)
forecast_values_a <- forecast(arima_model_a, h = 26)
forecast_values_b <- forecast(arima_model_b, h = 26)
forecast_values_c <- forecast(arima_model_c, h = 26)
forecast_values_w <- forecast(arima_model_w, h = 26)
forecast_values_a_avg <- forecast(arima_model_a_avg, h = 26)


# Plot the forecast
plot(main = "Prevalence of anemia among women of reproductive age (15-49 years)", xlab = "Year", ylab = "Percent", forecast_values_a)
plot(main = "Prevalence of low birthweight (percent)", xlab = "Year", ylab = "Percent", forecast_values_b)
plot(main = "Percentage of children under 5 years of age who are stunted", xlab = "Year", ylab = "Percent", forecast_values_c)
plot(main = "Percentage of children under 5 years of age who are stunted", xlab = "Year", ylab = "Percent", forecast_values_w)

#Plot the data and prediction for prevalence of anemia in women 
plot(forecast_values_a, 
     main = "Prevalence of anemia among women of reproductive age (15-49 years)",
     xlab = "Year", 
     ylab = "Percent",
     ylim = range(20,100), 
     flty = 2, col = "blue") 


lines(forecast_values_a_avg$mean, col = "orange", lty = 1) 
lines(avg_anemia, col = "orange")


# Add data and prediction for prevalence of low birth weight 
lines(forecast_values_b$mean, col = "red", lty = 1) 
lines(low_birthweight, col = "red")

#Adds data and prediction for prevalence of stunted children 
lines(forecast_values_c$mean, col = "green", lty = 1) 
lines(child_stunted, col = "green")


#Adding a legend
legend("topleft", 
       legend = c("Prevelance of low birth weight" ,"Prevelance of children under 5 who are stunted", "Prevelance of anemia among woman of reproductive age"),
       col = c("red", "green", "blue"),
       lty = c(1, 1, 1), # Line types: 1=solid
       cex = 0.8) # Adjust legend text size
#average
