###################################################
# Leave one Year out Accuracy for Logistic Regression  -  Ice ON
###################################################


# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

# Load any necessary packages and functions 
source("source/00_libraries.R")

# Load in data   

# Ice presence, conductivity, water temperature, and flow for full time series 
full_timeseries <- read.csv("derived_data/00_imputed_data_trimmed_winter.csv")
full_timeseries$Date <- as.POSIXct(full_timeseries$Date)

# Create another data frame that contains just the years for which we also have ice observations 
loch_raw <- full_timeseries %>%
  filter(waterYear >= 2014)

# Met data from Bear Lake SnoTel site 322
full_met <- read.csv("derived_data/00_snotel_322.csv")
full_met$date <- as.POSIXct(full_met$date)
# __________________________________________________
# 01. For loop for Leave one Year out Accuracy for Logistic Regression -- Ice ON
# __________________________________________________

# # Pull in ice on data:
ice_on_data <- read.csv("Input_Files/met_hydro_winter_ice_on.csv")

# initialize i to step through for loop 
i <- 3 


# create an object that holds all of the waterYears in the full dataset 
years <- unique(ice_on_data$waterYear) 

# Create an obect to hold the out of sample accuracy for each year 
ice_on_accuracy_log <- rep(NA, length(years))
ice_on_diff_log <- rep(NA, length(years))

# for each year in your list of years 
for (i in 1:length(years)){
  
  # seperate into train and test data 
  test_year <- years[i]
  training_data <- ice_on_data[ice_on_data$waterYear != test_year, ]
  test_data <- ice_on_data[ice_on_data$waterYear == test_year, ]
  
  
  # Train a logistic regression model on training data 
  trained_log_model <- glm(ice_or_no ~ temp_7day_mean + z_cond_uScm, 
                           data = training_data, 
                           family = binomial)
  
  # use the trained logistic regression model to predict the presence or absence of ice in the test data 
  predicted_ice_prob_log <- predict(trained_log_model, newdata = test_data, type = "response")  # do I need something here that selects the column for ice presence like in Katie's code? (column 2)
  
  # Convert the probability into a prediction 
  predicted_ice_log <- ifelse(predicted_ice_prob_log > 0.5, 1, 0)
  
  # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
  ice_on_accuracy_log[i] <- mean(predicted_ice_log == test_data$ice_or_no, na.rm = TRUE)
  
  # Calculate the number of days away from observed ice off the 
  
  # extract the day when we first observed no ice 
  ice_on_obs <- which(test_data$ice_or_no == 1)[1]
  
  # extract the day when the model first predicted no ice 
  ice_on_pred <- which(predicted_ice_log == 1)[1] %>% 
    as.numeric()
  
  # take the difference betweent those two days and save it in the days_off_log 
  ice_on_diff_log[i] <- ice_on_obs -  ice_on_pred
  
}


# Look at the number of days off from predicted ice off each of your predictions are 
ice_on_diff_log_df <- as.data.frame(ice_on_diff_log)
ggplot(data = ice_on_diff_log_df, aes(x = ice_on_diff_log)) + 
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
  labs(
    x = "Observed - Predicted Ice On Day"
  ) +
  theme_minimal()
mean(ice_on_diff_log)
mean(abs(ice_on_diff_log)) # on average how far away from zero are you

mean(ice_on_accuracy_log)

# Take a look at accuracy over each year for log model
ice_on_accuracy_yr_summary <- cbind(years, ice_on_accuracy_log) %>% 
  as.data.frame()
ice_on_accuracy_yr_summary %>%
  ggplot(aes(x = years, y = ice_on_accuracy_log)) + 
  geom_point(color = "forestgreen", size = 3) + 
  theme_minimal(base_size = 16) + 
  labs(
    x = "Year Held Out", 
    y = "Accuracy", 
    title = "Logistic Regression Out of Sample Accuracy for each Year"
  )