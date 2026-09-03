#######################################
# Random Forest Ice OFF  
#######################################

# KAG NEXT STEPS: 
# [X] put the leave one out accuracy into a function 
# [X] run the looa function for each model (met, hydro, sink)
# [ ] standardize the ouputs (accuracy and predictons and hindcasts)
# [ ] write another script to copare the outputs across modesl 

# Stop and Fix: hydro and sink models not running for 2020, 2021, 2022, and 2023 --> but it looks like the data is there? Check hydro, hmm also hydro should be working because I got it to work for ML class  
    # somehow looosing flow and cumulative flow for 2021 

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
        met_data_full_timeseries <- left_join(ice_only, met_only)
        hydro_data_full_timeseries <- left_join(ice_only, hydro_only)
        sink_data_full_timeseries <- left_join(hydro_data_full_timeseries, met_data_full_timeseries)

    # Trim data frames to only spring and only since 2014
        met_trimmed <- filter_by_year_and_doy(met_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
            filter(waterYear >= 2014)

        hydro_trimmed <- filter_by_year_and_doy(hydro_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
            filter(waterYear >= 2014)

        sink_trimmed <- filter_by_year_and_doy(sink_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
            filter(waterYear >= 2014)

    # Trouble shooting hydro data missing? KAG 20260722 

        hydro_data  %>%
            mutate(
            Date = as.POSIXct(Date), 
            cond_scaled = scales::rescale(cond_uScm, to = range(c(0,1), na.rm = TRUE)),
            temp_scaled = scales::rescale(water_temp_C, to = range(c(0,1), na.rm = TRUE)), 
            cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(c(0,1), na.rm = TRUE)), 
            q_scaled = scales::rescale(Flow, to = range(c(0,1), na.rm = TRUE))
            ) %>%
            ggplot(aes(x= Date)) + 
            geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
            geom_point(aes(y = temp_scaled), color = "salmon3", alpha = 0.75) + 
            geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) + 
            geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) + 
            theme_minimal() + 
        facet_wrap(~waterYear, scales = "free")

# __________________________________________________
# Feature Engineering 
# __________________________________________________

# Feature Engineering for eachdata set 
hydro_data <- feature_engineer_hydro_ice_off(hydro_trimmed) %>%
  tidyr::drop_na()  # Remove any rows with NA

            # hydro trimmed has all of the water years, but coming out of the feature engineering function it does not 

met_data <- feature_engineer_met_ice_off(met_trimmed) %>%
  tidyr::drop_na()  # Remove any rows with NA

sink_data <- sink_trimmed %>%
    feature_engineer_hydro_ice_off() %>%
    feature_engineer_met_ice_off() %>%
    tidyr::drop_na()  # Remove any rows with NA



# check for class imbalance 
    # --> need to deal with slight class imbalance  in the modeling, we have fewer data points with no ice than with ice  
    balance_count <- ice_only %>%
      filter(waterYear >= 2014 & waterYear <= 2025) %>%
      count(waterYear, ice) %>%
      tidyr::pivot_wider(
          names_from = ice,
          values_from = n,
          names_prefix = "ice_"
        )
    balance_count$proportion_0 <- balance_count$ice_0 /(balance_count$ice_0 + balance_count$ice_1)

# __________________________________________________
# Leave one Year out Accuracy for Random Forest 
# __________________________________________________

# might be inputing issue with the 7 day window? 

# Dummy data for trouble shooting 
# model_input <- hydro_data
# i <- 8 
    
# Write a function to run the leave one year out accuracy 
leave_one_out_accuracy_rf <- function(model_input){
  # create an object that holds all of the waterYears in the full dataset 
        years <- unique(model_input$waterYear) 

    # Create an object to hold the out of sample accuracy for each year 
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

# Format Data Output  
hydro_accuracy$model <- "hydro"
met_accuracy$model <- "met"
sink_accuracy$model <- "sink"

model_performance <- rbind(
    hydro_accuracy, 
    met_accuracy, 
    sink_accuracy
) %>%
rename(
    waterYear = year
) %>%
mutate(
    waterYear = as.numeric(waterYear)
)

#__________________________________
# Plot Model Performance 
#__________________________________ 

    # Accuracy_______

        # Scatterpolot of Leave One Year Out Model Accuracy over time 
            model_performance %>%
            ggplot(
                aes(
                    x = waterYear, 
                    y = accuracy_rf, 
                    color = model
                )
            ) + 
            geom_point(
                alpha = 0.75, 
                size = 3
            ) + 
            scale_color_manual(
                values= c(
                    "hydro" = "dodgerblue", 
                    "met" = "goldenrod", 
                    "sink" = "forestgreen"
                )
                ) +
            labs(
                x = "Water Year Left Out", 
                y = "Leave One Year Out Accuracy",  
                title = "Random Forest"
            ) + 
            theme_minimal(base_size = 16)

        # Boxplot of accuracy by model in out 
            model_performance %>%
                ggplot(
                    aes(
                        x = model, 
                        y = accuracy_rf, 
                        color = model
                    )
                ) + 
                geom_boxplot() + 
                geom_point(alpha = 0.5) + 
                scale_color_manual(
                    values= c(
                        "hydro" = "dodgerblue", 
                        "met" = "goldenrod", 
                        "sink" = "forestgreen"
                    )
                    ) +
                labs(
                    x = "Model Input Data", 
                    y = "Leave One Year Out Accuracy"
                ) + 
                theme_minimal(base_size = 16)

    # Obs - Predicted _______

        # Scartter Plot of obs - pred 
                model_performance %>%
                ggplot(
                    aes(
                        x = waterYear, 
                        y = ice_off_diff_rf, 
                        color = model
                    )
                ) + 
                geom_point(alpha = 0.75, size = 3) + 
                scale_color_manual(
                    values= c(
                        "hydro" = "dodgerblue", 
                        "met" = "goldenrod", 
                        "sink" = "forestgreen"
                    )
                    ) +
                labs(
                    x = "Water Year Left Out", 
                    y = "Obs - Pred Ice Off DOWY",  
                ) + 
                theme_minimal(base_size = 16)

        # Boxplot obs - pred ice off date by model 
            model_performance %>%
            ggplot(
                aes(
                    x = model, 
                    y = ice_off_diff_rf, 
                    color = model
                )
            ) + 
            geom_boxplot() + 
            geom_point(alpha = 0.5) + 
            scale_color_manual(
                values= c(
                    "hydro" = "dodgerblue", 
                    "met" = "goldenrod", 
                    "sink" = "forestgreen"
                )
            ) +
            labs(
                x = "Model Input Data", 
                y = "Obs - Pred Ice Off DOWY"
            ) + 
            theme_minimal(base_size = 16)

    # Observed vs. predicted plots 
        # --> something weird is happenign with met data obs vs. pred here 
        model_performance %>%
            ggplot(
                aes(
                    x = obs_ice_off, 
                    y = pred_ice_off, 
                    color = model
                )
            ) + 
            geom_point(alpha = 0.75) + 
            theme_minimal(base_size = 16) + 
            geom_abline(
                slope = 1,
                intercept = 0,
                linetype = "dashed",
                color = "grey60"
            ) + 
            scale_color_manual(
                values= c(
                    "hydro" = "dodgerblue", 
                    "met" = "goldenrod", 
                    "sink" = "forestgreen"
                )
            ) +
            labs(
                x = "Observed ice off day of year", 
                y = "Predicted ice off day of year", 
                title = "Random Forest Observed vs. Predicted"
    )

# Save model performance 
    write.csv(model_performance, "derived_data/04_model_performance_ice_off_rf.csv")


# __________________________________________________
# Hindcast   
# __________________________________________________

# Hindcast Data Munging _________________
    # Add Ice data to create the three data frames you are going to work with 
        met_data_full_timeseries <- left_join(ice_only, met_only)
        hydro_data_full_timeseries <- left_join(ice_only, hydro_only,)
        sink_data_full_timeseries <- full_join(hydro_data_full_timeseries, met_data_full_timeseries)

    # Feature Engineering 
        hydro_data_full_timeseries_fe <- feature_engineer_hydro_ice_off(hydro_data_full_timeseries)

        met_data_full_timeseries_fe <- feature_engineer_met_ice_off(met_data_full_timeseries)

        sink_data_full_timeseries_fe <- feature_engineer_hydro_ice_off(sink_data_full_timeseries)
        sink_data_full_timeseries_fe <- feature_engineer_met_ice_off(sink_data_full_timeseries_fe)

    # Make training and model data for each inout type 

        # Hydro Data 
        train_data_hydro <- filter_by_year_and_doy(hydro_data_full_timeseries_fe, c(170,288))  %>% # March 18 - July 15 (spring)
                filter(waterYear >= 2014) # when we ahve data for ice on and off 

        hind_data_hydro <- filter_by_year_and_doy(hydro_data_full_timeseries_fe, c(170,288)) 

        # Met data 
        train_data_met <- filter_by_year_and_doy(met_data_full_timeseries_fe, c(170,288))  %>% # March 18 - July 15 (spring)
                filter(waterYear >= 2014) # when we ahve data for ice on and off 

        hind_data_met <- filter_by_year_and_doy(met_data_full_timeseries_fe, c(170,288))  

        # Sink Data 
        train_data_sink <- filter_by_year_and_doy(sink_data_full_timeseries_fe, c(170,288))  %>% # March 18 - July 15 (spring)
                filter(waterYear >= 2014) # when we ahve data for ice on and off 

        hind_data_sink <- filter_by_year_and_doy(sink_data_full_timeseries_fe, c(170,288))  

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


# Hindcast Modeling _________________
# give model day of water year?? 

     # to account for a slight class imbalance we want to make that when training the model, it is grabbing the same number as days with ice and without ice 
        n_abs_hydro <- sum(train_data_hydro$ice == 0, na.rm = TRUE) # get the number of days when ice was absence 
        n_abs_met <- sum(train_data_met$ice == 0, na.rm = TRUE) 
        n_abs_sink <- sum(train_data_sink$ice == 0, na.rm = TRUE) 

    # Train the random forest model using all the data we have ice presence data for 
        trained_hind_rf_model_hydro <- randomForest(ice ~ ., # this says predict ice based on every other column in this data, 
                                    data=train_data_hydro[, -c(1:4)],  # input the training data but remove the first 3 columns (date, water year, and day of water year)
                                    ntree = 500, # run an ensamble of 500 trees 
                                    sampsize=c(n_abs_hydro, n_abs_hydro)
                                )

        trained_hind_rf_model_met <- randomForest(ice ~ ., # this says predict ice based on every other column in this data, 
                                    data=train_data_met[, -c(1:4)],  # input the training data but remove the first 3 columns (date, water year, and day of water year)
                                    ntree = 500, # run an ensamble of 500 trees 
                                    sampsize=c(n_abs_met, n_abs_met)
                                )

        trained_hind_rf_model_sink <- randomForest(ice ~ ., # this says predict ice based on every other column in this data, 
                                    data=train_data_met[, -c(1:4)],  # input the training data but remove the first 3 columns (date, water year, and day of water year)
                                    ntree = 500, # run an ensamble of 500 trees 
                                    sampsize=c(n_abs_sink, n_abs_sink)
                                )
               
    # Use the trained random forest model to predict the probability of ice on for each day in the test dataset
        predicted_hind_prob_rf_hydro <- predict(trained_hind_rf_model_hydro , 
                                            newdata=hind_data_hydro[, -c(1:4)], 
                                            type="prob")[,2] # the 2 is because we only want the probability of ice presence (2nd column) not the probability of absence 

        predicted_hind_prob_rf_met <- predict(trained_hind_rf_model_met, 
                                            newdata=hind_data_met[, -c(1:4)], 
                                            type="prob")[,2] # the 2 is because we only want the probability of ice presence (2nd column) not the probability of absence 

        predicted_hind_prob_rf_sink <- predict(trained_hind_rf_model_sink, 
                                            newdata=hind_data_sink[, -c(1:4)], 
                                            type="prob")[,2] # the 2 is because we only want the probability of ice presence (2nd column) not the probability of absence 

        # Set the threshold probability of when you call ice presence present 
            threshold <- 0.5

        # The output of all of these models is the probability of presence, in order to get a charcterization of presence absence set a threshold 
            predicted_hind_binary_rf_hydro <- 1 * (predicted_hind_prob_rf_hydro > threshold)
            hindcast_dialy_hydro <- cbind(hind_data_hydro, 
                                        predicted_hind_prob_rf_hydro,
                                        predicted_hind_binary_rf_hydro 
                                     ) %>%
                                     select(
                                        c(Date, calYear, waterYear, wy_doy, ice, predicted_hind_prob_rf_hydro, predicted_hind_binary_rf_hydro)
                                     ) %>%
                                     rename(
                                        obs_ice = ice, 
                                        ice_pred_prob = predicted_hind_prob_rf_hydro, 
                                        ice_pred_binary = predicted_hind_binary_rf_hydro
                                     )

        # Bella wants you to save both the probabiliy and the binary to plot so put them together 

# __________________________________________________
# Visualize and Save Daily Hindcast Results 
# __________________________________________________      

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


