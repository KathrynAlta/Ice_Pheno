###################################################
# Leave one Year out Accuracy for Logistic Regression  -  Ice OFF
###################################################


# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

# Load any necessary packages and functions 
source("source/00_libraries.R")
########### OG load in data 
# # Load in data   
# 
# # Ice presence, conductivity, water temperature, and flow for full time series 
# full_timeseries <- read.csv("derived_data/00_imputed_data_trimmed_spring.csv")
# full_timeseries$Date <- as.POSIXct(full_timeseries$Date)
# 
# # Create another data frame that contains just the years for which we also have ice observations 
# loch_raw <- full_timeseries %>%
#   filter(waterYear >= 2014)

############### Katie's data load-in process:
# Load in data   
met_only <- read.csv("derived_data/00_met_daily_fullyr.csv") %>% select(-X)
hydro_only <- read.csv("derived_data/00_hydro_daily_fullyr.csv")  %>% select(-X) # This has 239 duplicate dates
ice_only <- read.csv("derived_data/00_ice_daily_fullyr.csv") %>% select(-X)

# fix hydro_only

hydro_only_fixed <- hydro_only %>%
  group_by(Date) %>%
  summarise(
    calYear = first(na.omit(calYear)),
    waterYear = first(na.omit(waterYear)),
    wy_doy = max(wy_doy, na.rm = TRUE),
    cond_uScm = first(na.omit(cond_uScm)),
    water_temp_C = first(na.omit(water_temp_C)),
    Flow = first(na.omit(Flow)),
    cumulative_dis = first(na.omit(cumulative_dis)),
    .groups = "drop"
  )

anyDuplicated(hydro_only_fixed$Date)

# Add Ice data to create the three data frames you are going to work with 
met_data_full_timeseries <- full_join(ice_only, met_only)
hydro_data_full_timeseries <- full_join(ice_only, hydro_only_fixed)
sink_data_full_timeseries <- full_join(hydro_data_full_timeseries, met_data_full_timeseries)

# Trim data frames to only spring and only since 2014
met_data <- filter_by_year_and_doy(met_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
  filter(waterYear >= 2014)

hydro_data <- filter_by_year_and_doy(hydro_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
  filter(waterYear >= 2014)

sink_data <- filter_by_year_and_doy(sink_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
  filter(waterYear >= 2014 & waterYear <= 2024)


# __________________________________________________
# 01. For loop for Leave one Year out Accuracy for Logistic Regression -- Ice OFF -- HYDRO
# __________________________________________________


  # initialize i to step through for loop 
  i <- 3 


  # create an object that holds all of the waterYears in the full dataset 
    years <- unique(sink_data$waterYear)

# Create an obect to hold the out of sample accuracy for each year 
    accuracy_log <- rep(NA, length(years))
    ice_off_diff_log <- rep(NA, length(years))

# for each year in your list of years 
    for (i in 1:length(years)){

       # seperate into train and test data 
        test_year <- years[i]
        training_data <- sink_data[sink_data$waterYear != test_year, ]
        test_data <- sink_data[sink_data$waterYear == test_year, ]

      
      # Train a logistic regression model on training data 
        trained_log_model <- glm(ice_presence ~ Flow + cumulative_dis + water_temp_C + cond_uScm, 
          data = training_data, 
          family = binomial)
        
      # use the trained logistic regression model to predict the presence or absence of ice in the test data 
        predicted_ice_prob_log <- predict(trained_log_model, newdata = test_data, type = "response")  # do I need something here that selects the column for ice presence like in Katie's code? (column 2)
        
        # Convert the probability into a prediction 
        predicted_ice_log <- ifelse(predicted_ice_prob_log > 0.5, 1, 0)
        
        # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
        accuracy_log[i] <- mean(predicted_ice_log == test_data$ice_presence, na.rm = TRUE)
        
        # Calculate the number of days away from observed ice off the 
        
        # extract the day when we first observed no ice 
        ice_off_obs <- which(test_data$ice_presence == 0)[1]
        
        # extract the day when the model first predicted no ice 
        ice_off_pred <- which(predicted_ice_log == 0)[1] %>% 
          as.numeric()
        
        # take the difference betweent those two days and save it in the days_off_log 
        ice_off_diff_log[i] <- ice_off_obs -  ice_off_pred

    }
    
    
    # Look at the number of days off from predicted ice off each of your predictions are 
    ice_off_diff_log_df <- as.data.frame(ice_off_diff_log)
    ggplot(data = ice_off_diff_log_df, aes(x = ice_off_diff_log)) + 
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
      labs(
        x = "Observed - Predicted Ice Off Day"
      ) +
      theme_minimal()
    mean(ice_off_diff_log)
    mean(abs(ice_off_diff_log)) # on average how far away from zero are you
    
    mean(accuracy_log, na.rm = TRUE)
    
    # Take a look at accuracy over each year for log model
    accuracy_yr_summary <- cbind(years, accuracy_log) %>% 
      as.data.frame()
    accuracy_yr_summary %>%
      ggplot(aes(x = years, y = accuracy_log)) + 
      geom_point(color = "forestgreen", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "Logistic Regression Out of Sample Accuracy for each Year"
      )
    
    
    
    
    # __________________________________________________
    # 02. For loop for Leave one Year out Accuracy for Logistic Regression -- Ice OFF -- MET
    # __________________________________________________
    
    
    # initialize i to step through for loop 
    i <- 3 
    
    
    # create an object that holds all of the waterYears in the full dataset 
    years <- unique(sink_data$waterYear)
    
    # Create an obect to hold the out of sample accuracy for each year 
    accuracy_log <- rep(NA, length(years))
    ice_off_diff_log <- rep(NA, length(years))
    
    # for each year in your list of years 
    for (i in 1:length(years)){
      
      # seperate into train and test data 
      test_year <- years[i]
      training_data <- sink_data[sink_data$waterYear != test_year, ]
      test_data <- sink_data[sink_data$waterYear == test_year, ]
      
      
      # Train a logistic regression model on training data 
      trained_log_model <- glm(ice_presence ~ Flow + cumulative_dis + water_temp_C + cond_uScm, 
                               data = training_data, 
                               family = binomial)
      
      # use the trained logistic regression model to predict the presence or absence of ice in the test data 
      predicted_ice_prob_log <- predict(trained_log_model, newdata = test_data, type = "response")  # do I need something here that selects the column for ice presence like in Katie's code? (column 2)
      
      # Convert the probability into a prediction 
      predicted_ice_log <- ifelse(predicted_ice_prob_log > 0.5, 1, 0)
      
      # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
      accuracy_log[i] <- mean(predicted_ice_log == test_data$ice_presence, na.rm = TRUE)
      
      # Calculate the number of days away from observed ice off the 
      
      # extract the day when we first observed no ice 
      ice_off_obs <- which(test_data$ice_presence == 0)[1]
      
      # extract the day when the model first predicted no ice 
      ice_off_pred <- which(predicted_ice_log == 0)[1] %>% 
        as.numeric()
      
      # take the difference betweent those two days and save it in the days_off_log 
      ice_off_diff_log[i] <- ice_off_obs -  ice_off_pred
      
    }
    
    
    # Look at the number of days off from predicted ice off each of your predictions are 
    ice_off_diff_log_df <- as.data.frame(ice_off_diff_log)
    ggplot(data = ice_off_diff_log_df, aes(x = ice_off_diff_log)) + 
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
      labs(
        x = "Observed - Predicted Ice Off Day"
      ) +
      theme_minimal()
    mean(ice_off_diff_log)
    mean(abs(ice_off_diff_log)) # on average how far away from zero are you
    
    mean(accuracy_log, na.rm = TRUE)
    
    # Take a look at accuracy over each year for log model
    accuracy_yr_summary <- cbind(years, accuracy_log) %>% 
      as.data.frame()
    accuracy_yr_summary %>%
      ggplot(aes(x = years, y = accuracy_log)) + 
      geom_point(color = "forestgreen", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "Logistic Regression Out of Sample Accuracy for each Year"
      )
    
    
    # __________________________________________________
    # 03. For loop for Leave one Year out Accuracy for Logistic Regression -- Ice OFF -- SINK
    # __________________________________________________
    
    
    # initialize i to step through for loop 
    i <- 3 
    
    
    # create an object that holds all of the waterYears in the full dataset 
    years <- unique(sink_data$waterYear)
    
    # Create an obect to hold the out of sample accuracy for each year 
    accuracy_log <- rep(NA, length(years))
    ice_off_diff_log <- rep(NA, length(years))
    
    # for each year in your list of years 
    for (i in 1:length(years)){
      
      # seperate into train and test data 
      test_year <- years[i]
      training_data <- sink_data[sink_data$waterYear != test_year, ]
      test_data <- sink_data[sink_data$waterYear == test_year, ]
      
      
      # Train a logistic regression model on training data 
      trained_log_model <- glm(ice_presence ~ Flow + cumulative_dis + water_temp_C + cond_uScm, 
                               data = training_data, 
                               family = binomial)
      
      # use the trained logistic regression model to predict the presence or absence of ice in the test data 
      predicted_ice_prob_log <- predict(trained_log_model, newdata = test_data, type = "response")  # do I need something here that selects the column for ice presence like in Katie's code? (column 2)
      
      # Convert the probability into a prediction 
      predicted_ice_log <- ifelse(predicted_ice_prob_log > 0.5, 1, 0)
      
      # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
      accuracy_log[i] <- mean(predicted_ice_log == test_data$ice_presence, na.rm = TRUE)
      
      # Calculate the number of days away from observed ice off the 
      
      # extract the day when we first observed no ice 
      ice_off_obs <- which(test_data$ice_presence == 0)[1]
      
      # extract the day when the model first predicted no ice 
      ice_off_pred <- which(predicted_ice_log == 0)[1] %>% 
        as.numeric()
      
      # take the difference betweent those two days and save it in the days_off_log 
      ice_off_diff_log[i] <- ice_off_obs -  ice_off_pred
      
    }
    
    
    # Look at the number of days off from predicted ice off each of your predictions are 
    ice_off_diff_log_df <- as.data.frame(ice_off_diff_log)
    ggplot(data = ice_off_diff_log_df, aes(x = ice_off_diff_log)) + 
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
      labs(
        x = "Observed - Predicted Ice Off Day"
      ) +
      theme_minimal()
    mean(ice_off_diff_log)
    mean(abs(ice_off_diff_log)) # on average how far away from zero are you
    
    mean(accuracy_log, na.rm = TRUE)
    
    # Take a look at accuracy over each year for log model
    accuracy_yr_summary <- cbind(years, accuracy_log) %>% 
      as.data.frame()
    accuracy_yr_summary %>%
      ggplot(aes(x = years, y = accuracy_log)) + 
      geom_point(color = "forestgreen", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "Logistic Regression Out of Sample Accuracy for each Year"
      )
    
    
    
    
    
    
    
############## Explaining years that aren't as accurate by looking at missing data: (log reg. by default eliminates an entire row if there's any NAs)
    sum(complete.cases(sink_data[, c("ice_presence", "Flow", "cumulative_dis", "water_temp_C", "cond_uScm")]))
    # only 773 rows with no NAs
    nrow(sink_data)
    # 1050 rows total = 277 rows omitted from model
    
    # Pulling out rows with NAs:
    na_plot <- sink_data %>%
      select(
        waterYear,
        wy_doy,
        ice_presence,
        Flow,
        cumulative_dis,
        water_temp_C,
        cond_uScm
      ) %>%
      pivot_longer(
        cols = c(
          ice_presence,
          Flow,
          cumulative_dis,
          water_temp_C,
          cond_uScm
        ),
        names_to = "Variable",
        values_to = "Value"
      ) %>%
      mutate(Missing = is.na(Value))
    
    ggplot(
      na_plot,
      aes(x = wy_doy,
          y = factor(waterYear),
          fill = Missing)
    ) +
      geom_tile() +
      facet_wrap(~Variable, ncol = 1) +
      scale_fill_manual(
        values = c(
          "FALSE" = "white",
          "TRUE" = "red"
        ),
        labels = c("Present", "Missing")
      ) +
      labs(
        x = "Water Year Day",
        y = "Water Year",
        fill = ""
      ) +
      theme_bw()
    