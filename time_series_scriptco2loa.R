# Time series analysis
# Coding Club (ourcodingclub@gmail.com)
# https://ourcodingclub.github.io/tutorials/time/
# 2017_10_13

# Packages ----
library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)

# Set working directory ----
setwd("~/R_code/CC-time-series-master")

# Load data ----
monthly_co2 <- read.csv("co2_loa.csv")  # co2 month concentration

# check the form of the data set
head(monthly_co2)
class(monthly_co2)

# Coerce monthly_co2 to `Date` class ----
# add the 1st day of the month and coerce the data to date
class(monthly_co2$month)
monthly_co2$month_date <- as.Date(paste(monthly_co2$month, "-01", sep = ""))

# Check it worked
class(monthly_co2$month_date) 


# Plot time series data ----

# Using scale_x_date
ggplot(monthly_co2, aes(x = month_date, y = co2_conc)) + 
  geom_line() + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  theme_classic()

# Viewing trend using loess smooth
ggplot(monthly_co2, aes(x = month_date, y = co2_conc)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, span = 0.6) + 
  theme_classic()

# Explore seasonal trends ----

# Extract month and year and store in new column
monthly_co2$year <- format(monthly_co2$month_date, format = "%Y")
monthly_co2$month_num <- format(monthly_co2$month_date, format = "%m")

# Plot months
ggplot(monthly_co2, aes(x = month_num, y = co2_conc, group = year)) + 
  geom_line(aes(colour = year))

#Statistical Analysis of TIME SERIES DATA
# Using ts objects to decompose trends ----

# Transform to `ts` class
# Specify start and end year, measurement frequency (monthly = 12)
monthly_co2_ts <- ts(monthly_co2$co2_conc, start = 1958, end = 2008, freq = 12)  

# Decompose using `stl()`
monthly_co2_stl <- stl(monthly_co2_ts, s.window = "period")

# Generate plots
# top=original data, second=estimated seasonal, third=estimated smooth trend, 
# bottom=estimated irregular element i.e. unaccounted for variation
plot(monthly_co2_stl)  
# variation in co2 conc for each month
monthplot(monthly_co2_ts, choice = "seasonal")  
seasonplot(monthly_co2_ts)	

# Forecasting ----

# Split data into testing and training
monthly_co2_model <- window(x = monthly_co2_ts, start=c(1958), end=c(1985))
monthly_co2_test <- window(x = monthly_co2_ts, start=c(1985))

# Creating model objects of each type of ets model
co2_ets_auto <- ets(monthly_co2_model)
co2_ets_mmm <- ets(monthly_co2_model, model = "MMM")
co2_ets_zzz<- ets(monthly_co2_model, model = "ZZZ")
co2_ets_mmm_damped <- ets(monthly_co2_model, model = "MMM", damped = TRUE)


# Creating forecast objects
# `h = 1000` means that the forecast will be 1000 time periods long, 
# in our case a time period is one month i.e from the test period into the future
co2_ets_fc <- forecast(co2_ets_auto, h = 1000)  
co2_ets_mmm_fc <- forecast(co2_ets_mmm, h = 1000)
co2_ets_zzz_fc <- forecast(co2_ets_zzz, h = 1000)
co2_ets_mmm_damped_fc <- forecast(co2_ets_mmm_damped, h = 1000)

# Convert forecasts to data frames 
co2_ets_fc_df <- cbind("Month" = rownames(as.data.frame(co2_ets_fc)),
                       as.data.frame(co2_ets_fc))  # Creating a data frame
# Removing whitespace from column names
names(co2_ets_fc_df) <- gsub(" ", "_", names(co2_ets_fc_df))  
co2_ets_fc_df$Date <- as.Date(paste("01-", co2_ets_fc_df$Month, sep = ""), 
                              format = "%d-%b %Y")  # prepending day of month to date
co2_ets_fc_df$Model <- rep("ets")  # Adding column of model type

co2_ets_mmm_fc_df <- cbind("Month" = rownames(as.data.frame(co2_ets_mmm_fc)), 
                           as.data.frame(co2_ets_mmm_fc))
names(co2_ets_mmm_fc_df) <- gsub(" ", "_", names(co2_ets_mmm_fc_df))
co2_ets_mmm_fc_df$Date <- as.Date(paste("01-", co2_ets_mmm_fc_df$Month, sep = ""), 
                                  format = "%d-%b %Y")
co2_ets_mmm_fc_df$Model <- rep("ets_mmm")

co2_ets_zzz_fc_df <- cbind("Month" = rownames(as.data.frame(co2_ets_zzz_fc)), 
                           as.data.frame(co2_ets_zzz_fc))
names(co2_ets_zzz_fc_df) <- gsub(" ", "_", names(co2_ets_zzz_fc_df))
co2_ets_zzz_fc_df$Date <- as.Date(paste("01-", co2_ets_zzz_fc_df$Month, sep = ""), 
                                  format = "%d-%b %Y")
co2_ets_zzz_fc_df$Model <- rep("ets_zzz")

co2_ets_mmm_damped_fc_df <- cbind("Month" = rownames(as.data.frame(co2_ets_mmm_damped_fc)), as.data.frame(co2_ets_mmm_damped_fc))
names(co2_ets_mmm_damped_fc_df) <- gsub(" ", "_", names(co2_ets_mmm_damped_fc_df))
co2_ets_mmm_damped_fc_df$Date <- as.Date(paste("01-", co2_ets_mmm_damped_fc_df$Month, sep = ""), format = "%d-%b %Y")
co2_ets_mmm_damped_fc_df$Model <- rep("ets_mmm_damped")

# Combining into one data frame
forecast_all <- rbind(co2_ets_fc_df, co2_ets_mmm_fc_df, co2_ets_zzz_fc_df, 
                      co2_ets_mmm_damped_fc_df)

# Plotting with ggplot
ggplot() +
  geom_line(data = monthly_co2, aes(x = month_date, y = co2_conc)) +  # Plotting original data
  geom_line(data = forecast_all, aes(x = Date, y = Point_Forecast, colour = Model)) +  # Plotting model forecasts
  theme_classic()

# Comparing accuracy of forecasts
accuracy(co2_ets_fc, monthly_co2_test)
accuracy(co2_ets_mmm_fc, monthly_co2_test)
accuracy(co2_ets_zzz_fc, monthly_co2_test)
accuracy(co2_ets_mmm_damped_fc, monthly_co2_test)

# Extracting forecast estimates ----
co2_ets_fc_df %>%
  filter(Month == "Jan 2050") %>%
  select(Month, Point_Forecast)

co2_ets_zzz_fc_df %>%
  filter(Month == "Jan 2050") %>%
  select(Month, Point_Forecast)




