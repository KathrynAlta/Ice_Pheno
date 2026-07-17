#######################################
# Random Forest Ice OFF  
#######################################

# KAG NEXT STEPS: 
# [ ] put the leave one out accuracy into a function 
# [ ] run the looa function for each model (met, hydro, sink)
# [ ] standardize the ouputs (accuracy and predictons and hindcasts)
# [ ] write another script to copare the outputs across modesl 

# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

    # Load any necessary packages amd functions 
        source("source/00_libraries.R")
        source("source/00_functions.R")
        source("source/feature_engineering.R")

       
    # Load in data   
        met_only <- read.csv("derived_data/00_met_daily_fullyr.csv") %>% select(-X)
        hydro_only <- read.csv("derived_data/00_hydro_daily_fullyr.csv")  %>% select(-X)
        ice_only <- read.csv("derived_data/00_ice_daily_fullyr.csv") %>% select(-X)

    # Add Ice data to create the three data frames you are going to work with 
        met_data_full_timeseries <- full_join(ice_only, met_only)
        hydro_data_full_timeseries <- full_join(ice_only, hydro_only)
        sink_data_full_timeseries <- full_join(hydro_data_full_timeseries, met_data_full_timeseries)

    # Trim data frames to only spring and only since 2014
        met_data <- filter_by_year_and_doy(met_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
            filter(waterYear >= 2014)

        hydro_data <- filter_by_year_and_doy(hydro_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
            filter(waterYear >= 2014)

        sink_data <- filter_by_year_and_doy(sink_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
            filter(waterYear >= 2014)

    # # Looking at data 
    #     # individual predictors to sanity check 
    #       full_timeseries   %>%
    #         ggplot(aes(x = wy_doy, y = water_temp_C)) + 
    #         geom_point(alpha = 0.5) + 
    #         theme_minimal() + 
    #         facet_wrap(~waterYear,scales = "free_x")

    #     # plot all together 
    #         loch_raw %>%
    #             mutate(
    #             cond_scaled = scales::rescale(cond_uScm, to = range(ice_or_no, na.rm = TRUE)),
    #             temp_scaled = scales::rescale(water_temp_C, to = range(ice_or_no, na.rm = TRUE)), 
    #             cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(ice_or_no, na.rm = TRUE)), 
    #             q_scaled = scales::rescale(Flow, to = range(ice_or_no, na.rm = TRUE))
    #             ) %>%
    #             ggplot(aes(x= Date)) +
    #             geom_point(aes(y = ice_or_no), color = "skyblue3", alpha = 0.75) + 
    #             geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
    #             geom_point(aes(y = temp_scaled), color = "salmon3", alpha = 0.75) + 
    #             geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) + 
    #             geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) + 
    #             theme_minimal() + 
    #         facet_wrap(~waterYear, scales = "free")

# __________________________________________________
# Feature Engineering 
# __________________________________________________

# Feature Engineering for eachdata set 
hydro_data <- feature_engineer_hydro_ice_off(hydro_data)

met_data <- feature_engineer_met_ice_off(met_data)

sink_data <- feature_engineer_hydro_ice_off(sink_data)
sink_data <- feature_engineer_met_ice_off(sink_data)



# check for class imbalance 
    # --> need to deal with slight class imbalance  in the modeling, we have fewer data points with no ice than with ice  
    balance_count <- ice_only %>%
      filter(waterYear >= 2014 & waterYear <= 2025) %>%
      count(waterYear, ice_presence) %>%
      tidyr::pivot_wider(
          names_from = ice_presence,
          values_from = n,
          names_prefix = "ice_"
        )
    balance_count$proportion_0 <- balance_count$ice_0 /(balance_count$ice_0 + balance_count$ice_1)

# KAG: you made it to here on the plane to Seattle on 2026-07-17

# __________________________________________________
# Leave one Year out Accuracy for Random Forest 
# __________________________________________________
    
# Write a function to run the leave one year out accuracy 
leave_one_out_accuracy_rf <- function(model_input){
  # create an object that holds all of the waterYears in the full dataset 
        years <- unique(model_input$waterYear) 

    # Create an obect to hold the out of sample accuracy for each year 
        accuracy_rf <- rep(NA, length(years))
        ice_off_diff_rf <- rep(NA, length(years))
        obs_ice_off <- rep(NA, length(years))
        pred_ice_off <- rep(NA, length(years))

    # for each year in your list of years 
    for (i in 1:length(years)){

        # seperate into train and test data 
        test_year <- years[i]
        training_data <- model_input[model_input$waterYear != test_year, ]
        test_data <- model_input[model_input$waterYear == test_year, ]
      
        # train a random forest model on training data
        n_abs <- sum(training_data$ice == 0) # get the number of days when ice was absence to use to account for class imbalance 
        trained_rf_model <- randomForest(ice ~ ., data=training_data[, -c(1:4)], ntree = 1000, sampsize=c(n_abs, n_abs))
      
        # use the trained random forest model to predict the presence or absence of ice in the test data 
        predicted_ice_prob_rf <- predict(trained_rf_model, newdata=test_data[, -c(1:5)], type="prob")[,2] # the 2 is because we only want the probability of ice presence (2nd column) not the probability of absence 
      
        # Convert the probability into a prediction 
        threshold <- 0.5 # Set the threshold probability of when you call ice presence present 
        predicted_ice_rf <- 1 * (predicted_ice_prob_rf > threshold) # if predicted probability is greater than the threshold then set to present 
      
        # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
        accuracy_rf[i] <-  mean(predicted_ice_rf == test_data$ice)
      
        # Calculate the number of days away from observed ice off the 
      
            # extract the day when we first observed no ice 
            ice_off_obs <- which(test_data$ice == 0)[1]
      
            # extract the day when the model first predicted no ice 
            ice_off_pred <- which(predicted_ice_rf == 0)[1] %>% 
              as.numeric()
      
            # take the difference betweent those two days and save it in the days_off_rf 
            ice_off_diff_rf[i] <- ice_off_obs -  ice_off_pred
      
            # Save the observed and predicted ice off dates 
            obs_ice_off[i] <- ice_off_obs
            pred_ice_off[i] <- ice_off_pred
      
    }
  
  # Put together outputs 
  output <- cbind(
        years, 
        accuracy_rf, 
        ice_off_diff_rf, 
        obs_ice_off, 
        pred_ice_off
    ) %>%
    as.data.frame() %>%
    rename(
        year = years
    )
  
  return(output)
  
}

# Apply the function to each of your model datasets (met, hydro, sink)

hydro_accuracy <- leave_one_out_accuracy_rf(hydro_data)
met_accuracy <- leave_one_out_accuracy_rf(met_data)
sink_accuracy <- leave_one_out_accuracy_rf(sink_data)

# initial pass of looking 
hydro_accuracy$model <- "hydro"
met_accuracy$model <- "met"
sink_accuracy$model <- "sink"

model_accuracy <- rbind(
    hydro_accuracy, 
    met_accuracy, 
    sink_accuracy
) %>%
  mutate(
    year = as.numeric(year)
  )

model_accuracy %>%
  ggplot(
    aes(
        x = year, 
        y = accuracy_rf, 
        color = model
    )
  ) + 
  geom_jitter(alpha = 0.75) + 
  theme_minimal(base_size = 16)

model_accuracy %>%
  ggplot(
    aes(
        x = model, 
        y = ice_off_diff_rf, 
        color = model
    )
  ) + 
  geom_boxplot() + 
  geom_point(alpha = 0.5) + 
  theme_minimal(base_size = 16)

# doesn't look like there is any noticable difference between the three modesl 
    

#__________________________________
# Plots and looking at output 
#__________________________________ 

# Observed vs. predicted plots 
        obs_pred <- cbind(years, obs_ice_off, pred_ice_off) %>%
          as.data.frame()
        names(obs_pred)[names(obs_pred) == "years"] <- "waterYear"

        obs_pred %>%
            ggplot(aes(x =obs_ice_off, y =  pred_ice_off)) + 
            geom_point(color = "forestgreen", size = 3) + 
            theme_minimal(base_size = 16) + 
            geom_abline(
                slope = 1,
                intercept = 0,
                linetype = "dashed",
                color = "grey60"
            ) + 
            labs(
                x = "Observed ice off day of year", 
                y = "Predicted ice off day of year", 
                title = "Random Forest Observed vs. Predicted"
            )


    # Look at the number of days off from predicted ice off each of your predictions are 
            # ice_off_diff_rf_df <- as.data.frame(cbind(years, ice_off_diff_rf))
            # names(ice_off_diff_rf_df)[names(ice_off_diff_rf_df) == "years"] <- "waterYear"

            obs_pred$ice_off_diff <- obs_pred$obs_ice_off - obs_pred$pred_ice_off

            # Histogram of days off 
            ggplot(data = obs_pred, aes(x = ice_off_diff )) + 
                geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
                labs(
                    x = "Observed - Predicted Ice Off Day"
                ) +
                theme_minimal()
            mean(ice_off_diff_rf)
            mean(abs(ice_off_diff_rf)) # on average how far away from zero are you

    # scatter plot by year 
            obs_pred %>%
                ggplot(aes(x = waterYear, y = ice_off_diff )) + 
                geom_point(color = "forestgreen", size = 3) + 
                theme_minimal(base_size = 16) + 
                geom_hline(
                    yintercept = 0, 
                    color = "grey60", 
                    linetype = "dashed"
                ) + 
                labs(
                x = "Year Held Out", 
                y = "Obs - Pred ice off days", 
                title = "RF Out of Sample Distance from observed ice off date "
            )


    # Take a look at accuracy over each year for rf models 
            mean(accuracy_rf)

            model_performance <- cbind(obs_pred, accuracy_rf) %>% 
                as.data.frame()
            
            names(model_performance)[names(model_performance) == "accuracy_rf"] <- "oos_accuracy" # out of sample accuracy

        model_performance %>%
            ggplot(aes(x = waterYear, y = oos_accuracy)) + 
            geom_point(color = "forestgreen", size = 3) + 
            theme_minimal(base_size = 16) + 
            labs(
                x = "Year Held Out", 
                y = "Accuracy", 
                title = "Random Forest Out of Sample Accuracy for each Year"
            )

    # Save model performance 
    write.csv(model_performance, "derived_data/04_model_performance_ice_off_rf.csv")


# __________________________________________________
# Hindcast   
# __________________________________________________

# Trim data for hindcasting  ---------------------

    # Set date as POSIXct 
    full_timeseries$Date <- as.POSIXct(full_timeseries$Date)
    names(full_timeseries)

    # Trim full time series to only 
    hind_data <- full_timeseries %>%
      filter(Date < min(model_input$Date)) %>% #include the timepoints prior to the start of the training data
      filter(wy_doy >= min(model_input$wy_doy) & wy_doy <= max(model_input$wy_doy)) %>% # trim full time series to only include timepoints in the spring 
      select(-c(ice_presence, ice_or_no)) %>%
      tidyr::drop_na() # remove any rows with na values in any column 

    # Plot your hindcast data 
    # hind_data %>%
    #     # filter(waterYear == 2023) %>%
    #     ggplot(aes(x = Date, y = Flow)) + 
    #     geom_point(alpha = 0.5) + 
    #     theme_minimal() + 
    #     facet_wrap(~waterYear, scales = "free_x")

        # hind_data %>%
        #     mutate(
        #         cond_scaled = scales::rescale(cond_uScm, to = range(water_temp_C, na.rm = TRUE)),
        #         cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(water_temp_C, na.rm = TRUE)), 
        #         q_scaled = scales::rescale(Flow, to = range(water_temp_C, na.rm = TRUE))
        #     ) %>%
        #     ggplot(aes(x= Date)) +
        #     geom_point(aes(y = water_temp_C), color = "salmon3", alpha = 0.75) + 
        #     geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
        #     geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) +
        #     geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) +  
        #     theme_minimal() + 
        # facet_wrap(~waterYear, scales = "free")

# Feature Engineering on Hindcast data  ---------------------

    # Organize columns a bit first 
    hind_data <- subset(hind_data, select = c("waterYear", "wy_doy", "Date", "water_temp_C", "cumulative_dis", "cond_uScm", "Flow"))

    # Add a column that counts the number of days that the water temp has been above thresholds 
    hind_data <- hind_data %>% # HERE 
        group_by(waterYear) %>% # group by water year because we want this count within each water year 
        arrange(Date, .by_group = TRUE) %>% # make sure everything is in order by day 
        mutate(
            cum_days_water_temp_above2 = cumsum(water_temp_C > 2),
            cum_days_water_temp_above4 = cumsum(water_temp_C > 4), 
            cum_days_water_temp_above6 = cumsum(water_temp_C > 6),
        ) %>% #calculate the cumulative sum of rows (days) when water temp was above 4C 
        ungroup()

    # Add rates of change at different time intervals for temp, conductivity, and cumulative discharge 
    hind_data <- hind_data %>%
        group_by(waterYear) %>%
        arrange(Date, .by_group = TRUE) %>% #arrange by date within the groups 
        mutate(
            # Note this is set up so that the difference in temp is since the first day of the water year for the early days 
            # Rate of change in temperature 
            water_water_temp_change_3day = (water_temp_C - lag(water_temp_C, n = 3, default = first(water_temp_C))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
            water_water_temp_change_5day = (water_temp_C - lag(water_temp_C, n = 5, default = first(water_temp_C))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
            water_water_temp_change_10day = (water_temp_C - lag(water_temp_C, n = 10, default = first(water_temp_C))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
            water_water_temp_change_15day = (water_temp_C - lag(water_temp_C, n = 15, default = first(water_temp_C))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
            water_water_temp_change_20day = (water_temp_C - lag(water_temp_C, n = 20, default = first(water_temp_C))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 
            # Rate of change in cumulative flow 
            cumq_change_3day = (cumulative_dis - lag(cumulative_dis, n = 3, default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
            cumq_change_5day = (cumulative_dis - lag(cumulative_dis, n = 5,  default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
            cumq_change_10day = (cumulative_dis - lag(cumulative_dis, n = 10, default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
            cumq_change_15day = (cumulative_dis - lag(cumulative_dis, n = 15,  default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
            cumq_change_20day = (cumulative_dis - lag(cumulative_dis, n = 20,  default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 
            # Rate of change in conductivity
            cond_change_3day = (cond_uScm - lag(cond_uScm, n = 3, default = first(cond_uScm))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
            cond_change_5day = (cond_uScm - lag(cond_uScm, n = 5, default = first(cond_uScm))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
            cond_change_10day = (cond_uScm - lag(cond_uScm, n = 10, default = first(cond_uScm))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
            cond_change_15day = (cond_uScm - lag(cond_uScm, n = 15, default = first(cond_uScm))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
            cond_change_20day = (cond_uScm - lag(cond_uScm, n = 20, default = first(cond_uScm))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 

        ) %>%
        ungroup()

    str(hind_data)

    # Remove any rows with NA in the water_water_temp_change_3day column 
    hind_data <- hind_data %>%
        tidyr::drop_na()

# Random Forest Hindcast model ----------------------------------------------

        # to account for a slight class imbalance we want to make that when training the model, it is grabbing the same number as days with ice and without ice 
        n_abs <- sum(model_input$ice == 0) # get the number of days when ice was absence 

        # Train the random forest model using all the data we have ice presence data for 
        trained_hind_rf_model <- randomForest(ice ~ ., data=model_input[, -c(1:3)], ntree = 500, sampsize=c(n_abs, n_abs))
                # input the training data but remove the first 3 columns (date, water year, and day of water year)
                # this says rpredict ice based on every other column in this data, run an ensamble of 500 trees and give me the outcome

        # Use the trained random forest model to predict the probability of ice on for each day in the test dataset
            predicted_hind_prob_rf <- predict( trained_hind_rf_model, newdata=hind_data[, -c(1:3)], type="prob")[,2] # the 2 is because we only want the probability of ice presence (2nd column) not the probability of absence 

        # Set the threshold probability of when you call ice presence present 
            threshold <- 0.5

        # The output of all of these models is the probability of presence, in order to get a charcterization of presence absence set a threshold 
            hind_ice_rf <- 1 * (predicted_hind_prob_rf > threshold)

      
# Visualize and Save Daily Hindcast Results  ----------------------------------------------

    # Save predicted probabilities and binary 

            # create a data frame of the daily ice predictions 
            daily_pred <- hind_data %>%
            subset(select = c("waterYear", "wy_doy", "Date"))
            daily_pred$predicted_ice_probability <- predicted_hind_prob_rf
            daily_pred$predicted_ice_binary <- hind_ice_rf

            # plot predictions to sanity check at daily 
            daily_pred %>%
            ggplot(aes(
                    x = Date, 
                    y = predicted_ice_probability)
                ) + 
                geom_point(
                    alpha = 0.5, 
                    color = "skyblue3"
                ) + 
                theme_minimal() + 
                geom_hline(
                    yintercept = 0.50, 
                    color = "grey60", 
                    linetype = "dashed"
                ) + 
                facet_wrap(~waterYear, scales = "free")

            # Save the daily ice predictions 
            write.csv(daily_pred, "derived_data/04_hindcast_daily_ice_off_prob_rf.csv")

    # Plot hindcast ice with predictor variables 
        hind_data$ice_rf <- hind_ice_rf

        hind_data %>%
            mutate(
                cond_scaled = scales::rescale(cond_uScm, to = range(ice_rf, na.rm = TRUE)),
                temp_scaled = scales::rescale(water_temp_C, to = range(ice_rf, na.rm = TRUE)), 
                cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(ice_rf, na.rm = TRUE)), 
                q_scaled = scales::rescale(Flow, to = range(ice_rf, na.rm = TRUE))
            ) %>%
            ggplot(aes(x= Date)) +
                geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
                geom_point(aes(y = temp_scaled), color = "salmon3", alpha = 0.75) + 
                geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) + 
                geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) + 
                geom_point(aes(y = ice_rf), color = "skyblue3", alpha = 0.75) + 
                theme_minimal() + 
                labs(
                    x = "Date", 
                    title = "Random Forest Ice Predictions"
                ) + 
                facet_wrap(~waterYear, scales = "free")

# First Day of Ice Off  ----------------------------------------------

    # Extract the first day in each water year when the model predicts ice off
        hind_results <- subset(hind_data, select = c("waterYear" , "wy_doy", "Date", "ice_rf" ))
        head(hind_results)

        hind_summary <- hind_results %>%
            group_by(waterYear) %>%
            arrange(Date, .by_group = TRUE) %>%
            summarize(
                rf_ice_off_date = first(Date[ice_rf == 0]), 
                rf_ice_off_dowy = first(wy_doy[ice_rf == 0])
            )

    # Format the hindcast summary ouput 
        names(hind_summary)[names(hind_summary) == "rf_ice_off_date"] <- "ice_off_date"
        names(hind_summary)[names(hind_summary) == "rf_ice_off_dowy"] <- "ice_off_dowy"
        hind_summary$model <- "RandomForest"
        hind_summary <- hind_summary %>% # order columns 
            subset(select = c("model", "waterYear", "ice_off_date", "ice_off_dowy"))

    # save hindcast ice off dates 
            write.csv(hind_summary, "derived_data/04_hindcast_ice_off_dates_rf.csv")

    # Plot hindcast first day of ice off 
    hind_summary %>%
      ggplot(aes(
        x = waterYear, 
        y = rf_ice_off_dowy
      )) + 
      geom_point(
        size = 3, 
        color = "forestgreen", 
        alpha = 0.8
      ) + 
      theme_minimal() + 
      labs(
        x = "Water Year", 
        y = "Hindcast date of ice off"
      )


