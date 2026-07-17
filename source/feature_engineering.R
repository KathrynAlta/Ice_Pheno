##################################
# Feature Engineering Fucntions 
##################################
# KAG 2026-07-17 
# Functions to carry out feature engineering to set up for machine learning models for each data type 

# ______________________________________________
# Feature Engineering for hydrologic data for ice off -----
# ______________________________________________

feature_engineer_hydro_ice_off <- function(hydro_input){

    # Add a column that counts the number of days that the water temp has been above thresholds 
    hydro_input <- hydro_input %>%
        group_by(waterYear) %>% # group by water year because we want this count within each water year 
        arrange(Date, .by_group = TRUE) %>% # make sure everything is in order by day 
        mutate(
            Date = as.POSIXct(Date), 
            ice = as.factor(ice),  # change to factor to be able to predict 
            cum_days_water_temp_above2 = cumsum(water_temp_C > 2),
            cum_days_water_temp_above4 = cumsum(water_temp_C > 4), 
            cum_days_water_temp_above6 = cumsum(water_temp_C > 6),
        ) %>% #calculate the cumulative sum of rows (days) when water temp was above 4C 
        ungroup()

    # Add rates of change at different time intervals for temp, conductivity, and cumulative discharge 
    hydro_output <- hydro_input  %>%
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
        ungroup() %>%
        tidyr::drop_na()  # Remove any rows with NA
  
  return(hydro_output)
}


# ______________________________________________
# Feature Engineering for Meteorlogical data -----
# ______________________________________________

feature_engineer_met_ice_off <- function(met_input){
    
    # cumulative number of days where the min , max, and mean air temperature mean have been above certain threshodls 
        met_output <- met_input %>%
            mutate(
              Date = as.POSIXct(Date), 
              ice = as.factor(ice),  # change to factor to be able to predict 
            ) %>%
            group_by(waterYear) %>% # group by water year because we want this count within each water year 
            arrange(Date, .by_group = TRUE) %>% # make sure everything is in order by day 
            mutate(# Rate of change in swe and total precip 
                swe_change_3day = (swe - lag(swe, n = 3, default = first(swe))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
                swe_change_5day = (swe - lag(swe, n = 5, default = first(swe))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
                swe_change_10day = (swe - lag(swe, n = 10, default = first(swe))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
                swe_change_15day = (swe - lag(swe, n = 15, default = first(swe))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
                swe_change_20day = (swe - lag(swe, n = 20, default = first(swe))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 

                precip_cumulative_change_3day = (precip_cumulative - lag(precip_cumulative, n = 3, default = first(precip_cumulative))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
                precip_cumulative_change_5day = (precip_cumulative - lag(precip_cumulative, n = 5, default = first(precip_cumulative))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
                precip_cumulative_change_10day = (precip_cumulative - lag(precip_cumulative, n = 10, default = first(precip_cumulative))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
                precip_cumulative_change_15day = (precip_cumulative - lag(precip_cumulative, n = 15, default = first(precip_cumulative))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
                precip_cumulative_change_20day = (precip_cumulative - lag(precip_cumulative, n = 20, default = first(precip_cumulative))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 
            ) %>% 
            mutate( # Cumulative number of days of air temp above a certain threshold 
                cum_days_mean_air_temp_above0 = cumsum(airT_mean > 0),
                cum_days_mean_air_temp_above5 = cumsum(airT_mean > 5), 
                cum_days_mean_air_temp_above10 = cumsum(airT_mean > 10),

                cum_days_max_air_temp_above0 = cumsum(airT_max > 0),
                cum_days_max_air_temp_above5 = cumsum(airT_max > 5), 
                cum_days_max_air_temp_above10 = cumsum(airT_max > 10),

                cum_days_min_air_temp_above0 = cumsum(airT_min > 0),
                cum_days_min_air_temp_above5 = cumsum(airT_min > 5), 
                cum_days_min_air_temp_above10 = cumsum(airT_min > 10),
            ) %>% #calculate the cumulative sum of rows (days) when water temp was above 4C 
            ungroup() %>%
            tidyr::drop_na()  # Remove any rows with NA
  
  return(met_output)
}





    



