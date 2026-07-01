#######################################
# 00 Data Munging 
#######################################
# # KAG & BDG 2026-06-15

# KAG 20260630 Trouble shooting missing data for ice on 
# I think that there is an issue with dropping data during the joining and subsetting in 2c
# making plots at every step and trying to figure out where things drop out 
# goal to compile data for ice on models 

# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

library(here)
source(here::here("source", "00_libraries.R"))
source(here::here("source", "00_functions.R"))

# __________________________________________________
# 01. Flow 
# __________________________________________________
# Pulling in outlet flow from NWIS and adding cumulative flow:
    # LV Site Number:
    lv_no <- '401733105392404'

    # define parameter for discharge (00060)
    param <- '00060'

    # get daily values from NWIS
    lv_dat <- readNWISdv(siteNumbers = lv_no, parameterCd = param,
                        startDate = '1983-10-01', endDate = '2025-09-30')

    # rename columns using renameNWISColumns from package dataRetrieval
    # this renames the column for Flow from the parameter ID to "Flow"
    lv_dat <- renameNWISColumns(lv_dat)

    # Removing column with USGS code for observations
    lv_dat <- select(lv_dat, -contains('_cd'))

    # Adding the water year to the df
    lv_dat <- addWaterYear(lv_dat)

    # calculating cumulative discharge for each year by first grouping by water year,
    # and then using the "cumsum" function. Add day of water year for plotting purposes.
    cumulative_dat <- group_by(lv_dat, waterYear) %>%
      mutate(cumulative_dis = cumsum(Flow), 
            wy_doy = seq(1:n()))

    # ungroup the dataframe
    cumulative_dat_ungroup <- cumulative_dat %>%
      ungroup() %>%
      as.data.frame()

    # rename the df, remove the site number column, make sure dates are in date format. This is the final df
    cumulative_flow_df <- cumulative_dat_ungroup %>% 
      select(-site_no) %>% 
      mutate(Date = as.Date(Date, tz = "MST", format = "%Y-%m-%d"))

# __________________________________________________
# 02. Temp and Conductivity 
# __________________________________________________

#2a)  Pulling in Temp and Conductivity from NWIS:
    # setting parameters for temp and cond
    parameterCd <- c('00095','00010')
    # making the request from NWIS
    lv_dat_outlet_input <- readWQPqw(paste0("USGS-", lv_no), parameterCd)

    #Clean up dataframe
    lv_dat_outlet <- lv_dat_outlet_input %>% # take this dataframe that you downloaded from NWIS in the line above 
      select( 
          ActivityStartDate, # now select only some columns because it comes in with a BUNCH 
          ActivityConductingOrganizationText, 
          CharacteristicName, 
          ResultMeasureValue
      ) %>%
      pivot_wider(
          names_from = CharacteristicName, # the names of the new columns come from the values in this column 
          values_from = ResultMeasureValue, # the values that fill those columns come from this column 
          values_fn = mean # if there are multiple values that go here then take the mean 
      ) %>%
      rename(
          cond_uScm="Specific conductance", # change the column names to somethign more manageable 
          temperature_C_raw="Temperature, water",
          Date = "ActivityStartDate"
      ) %>%
      select(-ActivityConductingOrganizationText) %>% # remove this column because we don't need it anymore 
      mutate(
          wy_doy = hydro.day(Date) # create a new column for the day of the water year 
      ) %>%
      addWaterYear() %>% # add water year as a column 
      distinct(Date, .keep_all = TRUE) %>%
      as_tsibble(., key = waterYear, index = Date) %>% #time series tibble
      fill_gaps() #%>%  #makes the missing data implicit
    # View(lv_dat_outlet) # This is weekly data from 1982-2023, but missing temp & cond after 2019

      # Plot to sanity check 
    #  lv_dat_outlet %>%
    #       ggplot(
    #           aes(x = Date, 
    #               y =  temperature_C_raw
    #           )
    #       ) + 
    #       geom_point(alpha = 0.75, color = "salmon3") + 
    #       theme_minimal()  + 
    #       facet_wrap(~waterYear, scales = "free") 
      # okay this data starts to look spotty in 2021

# 2b)  Pulling in Daily Temp and Cond from Graham with USGS:

    # These CSVs contain data from 2019-2023
    out_cond_dat <- read.csv("Input_Files/Loch_O_daily_conductivity.csv")
    out_temp_dat <- read.csv("Input_Files/Loch_O_daily_temperature.csv")

    # Joining those two using FULL join because there are fewer conductivity observations
    out_condTemp_dat19_23 <- full_join(out_cond_dat,out_temp_dat, by = "Date")
    # This CSV contains data from 2011-2019
    out_condTemp_dat11_19 <- read.csv("Input_Files/LochDaily_TempCond_2011-2019.csv")
    #View(out_condTemp_dat11_19)

    out_condTemp_allDates <- merge(out_condTemp_dat11_19, out_condTemp_dat19_23, all = TRUE)
    #View(out_condTemp_allDates)

    # Adding water year and water year doy to the daily data
    out_cond_temp_daily <- out_condTemp_allDates %>% 
      mutate(
          Date = as.Date(Date, tz = "MST", format = "%Y-%m-%d")
      ) %>% 
      mutate(wy_doy = hydro.day(Date)) %>% 
      addWaterYear() %>% 
      distinct(Date, .keep_all = TRUE)
    #View(out_cond_temp_daily)

  # Plot to sanity check 
      # conductivity: color = "olivedrab4". temperature:  color = "salmon3"
    out_cond_temp_daily %>%
          # filter(waterYear == 2023) %>%
          ggplot(
              aes(x = Date, 
                  y = cond_uScm
              )
          ) + 
          geom_point(alpha = 0.75, color =  "olivedrab4") + 
          theme_minimal()  + 
           facet_wrap(~waterYear, scales = "free") 

    out_cond_temp_daily %>%
          # filter(waterYear == 2023) %>%
          ggplot(
              aes(x = Date, 
                  y = Temperature_C
              )
          ) + 
          geom_point(alpha = 0.75, color =   "salmon3") + 
          theme_minimal()   + 
           facet_wrap(~waterYear, scales = "free") 


## KAG 20260630 I think that part of the missing data issue is coming from here in 2c 

# 2c) Subsetting weekly temp and cond observations from the daily observations to fill in gap from 2019 onwards
    # Create a weekly dataset from the WY2020-2024 data
    TLoch_weekly_19_23 <- out_cond_temp_daily %>% 
      mutate(dayOfWeek = wday(Date, label = TRUE, abbr = FALSE)) %>% 
      filter(dayOfWeek == "Tuesday")
    TLoch_weekly_19_23<- TLoch_weekly_19_23 %>% select(-dayOfWeek) %>% rename(temperature_C_raw = Temperature_C)

    # Joining weekly data from 2019-2023 with the weekly data from 1982-2023 to fill gaps:
    updated_lv_dat_outlet <- lv_dat_outlet %>%
      left_join(TLoch_weekly_19_23, by = "Date", suffix = c("_old", "_new")) %>%
      mutate(
        # Replace missing values in lv_dat_outlet with values from TLoch_weekly_19_23
        temperature_C_raw = ifelse(is.na(temperature_C_raw_old), temperature_C_raw_new, temperature_C_raw_old),
        cond_uScm = ifelse(is.na(cond_uScm_old), cond_uScm_new, cond_uScm_old)
      ) %>%
      # Keep only original column names
      select(Date, temperature_C_raw, cond_uScm)

    TCond_weekly_all <- updated_lv_dat_outlet %>% mutate(wy_doy = hydro.day(Date)) %>% rename(waterYear = waterYear_old, temperature_C_weekly = temperature_C_raw,cond_uScm_weekly=cond_uScm)
    # THIS IS WEEKLY DATA WITH FILLED GAPS FOR THE ENTIRE TIME SERIES (1982-2023) - TCond_weekly_all

    # Filling gaps in weekly data with a max gap of interpolation as 7 days
    TCond_imputed_all <- TCond_weekly_all %>%
      mutate(cond_uScm_impute = imputeTS::na_interpolation(cond_uScm_weekly, maxgap = 7),
            temperature_C_impute = imputeTS::na_interpolation(temperature_C_weekly, maxgap = 7))
    # THIS IS THE IMPUTED DATA FOR THE ENTIRE TIME SERIES (1982-2023) - TCond_imputed_all

## Katie trouble shooting imputed vs. daily data 202060630 
      TCond_weekly_all %>%
          # filter(waterYear == 2022 | waterYear == 2023) %>%
          ggplot(aes(x = Date, y =  temperature_C_weekly)) + 
          geom_point(alpha = 0.75, color = "salmon3") + 
          theme_minimal()  + 
          facet_wrap(~waterYear, scales = "free_x")

    TCond_weekly_all %>%
              # filter(waterYear == 2022 | waterYear == 2023) %>%
              ggplot(aes(x = Date, y =  cond_uScm_weekly)) + 
              geom_point(alpha = 0.75, color =  "olivedrab4" ) + 
              theme_minimal()  + 
              facet_wrap(~waterYear, scales = "free_x")

    # weekly temperature until august 2023
      # 2019 goes oct to oct 
      # 2023 only may to oct (guessing the sensor went down somehow)
    # Weekly cond 

# __________________________________________________
# 03. Put together flow, conductivity, and temperature and export as a single file 
# __________________________________________________
# Binding the weekly data and imputed data with cumulative flow

    ## Weekly:
    flow_temp_cond_weekly <- full_join(cumulative_flow_df, TCond_weekly_all, by = "Date")
    #View(flow_temp_cond_weekly)

    ## Imputed:
    flow_temp_cond_impute <- full_join(cumulative_flow_df, TCond_imputed_all, by = "Date")
    #View(flow_temp_cond_impute)

    # # save outputs 
    #   write.csv(flow_temp_cond_impute, "derived_data/00_flow_temp_cond_impute.csv")
    #   write.csv(flow_temp_cond_weekly, "derived_data/00_flow_temp_cond_weekly.csv")

# __________________________________________________
# 04. Bring in OBSERVED ice presence on The Loch
# __________________________________________________

# 4a) Read in raw data files (annotated from weekly photos )

    #  observed ice-off dates from 2013-2023 
    obs_ice_off_dates <- read_xlsx("raw_data/ice_off_dates20240918.xlsx")
    head(obs_ice_off_dates)

    # observed ice-on dates from 2013-2023 
    obs_ice_on_dates <- read_xlsx("raw_data/ice_on_dates_20250414.xlsx") %>%
        mutate(doy = yday(Date))
    head(obs_ice_on_dates)

    # % Ice cover: this is a df with the wy_doy date of 100% ice and the wy_doy date with 0% ice for 2013-2023
    obs_ice_melt_windows <- read_xlsx("raw_data/ice_100_to_0_dates_20241114.xlsx")
    head(obs_ice_melt_windows)

    # Reading CSV for ice duration with 0% ice as ice-off
    ice_off_binary <- read.csv("raw_data/binary_iceOff_20241001.csv") %>% 
        select(c(Date,ice.0.1.,wy_doy)) %>% 
        rename(ice_or_no=ice.0.1.) %>% 
        mutate(Date = mdy(Date))
    head(ice_off_binary)

#4b) Format observed ice data 

    # recode ice_or_no into 2 classes
    ice_off_binary$ice_presence <- ifelse(ice_off_binary$ice_or_no == 0,
                                          0,
                                          1
    )
    # set labels for ice
    ice_off_binary$ice_presence <- factor(ice_off_binary$ice_presence,
                                          levels = c(1, 0),
                                          labels = c("ice", "no ice")
    )

# __________________________________________________
# 05. Combining Data Frames 
# __________________________________________________

# making a df for only ice_presence and Date for simpler joining
ice_presence_df <- ice_off_binary %>% 
    select(Date, ice_or_no, ice_presence)
head(ice_presence_df)

# combine binary ice on or off with DAILY temp and conductivity
out_dat_and_ice_daily <- full_join(out_cond_temp_daily,ice_presence_df, by = "Date")
head(out_dat_and_ice_daily)

# combine daily conductivity and temperature with ice presence and cumulative flow
    # This df will be used in models for daily observations. It is trimmed to only include observations that are inside
    # the time frame of ice observations -> (drop_na(ice_or_no))
flow_temp_cond_daily_ice <- left_join(out_dat_and_ice_daily, cumulative_flow_df, by="Date") %>% # join together these two data frames based on date
    select(-c(wy_doy.y, waterYear.y)) %>% # remove these duplicate columns 
    rename( wy_doy = wy_doy.x,waterYear = waterYear.x) %>% # rename these two columns to remove the .x (you also could have removed the duplicates before joining)
    drop_na(ice_or_no) # drop any rows with NAs in the ice or no column
head(flow_temp_cond_daily_ice) # take a look at the resulting data frame 

# combine weekly conductivity and temperature observations with ice presence (this includes cumulative flow already)
flow_temp_cond_weekly_ice <- full_join(flow_temp_cond_weekly, ice_presence_df, by = "Date") %>% 
    select(Date, wy_doy.x, waterYear.x, Flow, cumulative_dis, cond_uScm_weekly, temperature_C_weekly, ice_or_no,ice_presence) %>% 
    rename(wy_doy=wy_doy.x, waterYear=waterYear.x)
head(flow_temp_cond_weekly_ice)

# combine imputed conductivity and temperature observations with ice presence (this includes cumulative flow already)
flow_temp_cond_imputed_ice <- full_join(flow_temp_cond_impute, ice_presence_df, by = "Date") %>% 
  select(Date, wy_doy.x, waterYear.x, Flow, cumulative_dis, cond_uScm_impute, temperature_C_impute, ice_or_no,ice_presence) %>% 
  rename(wy_doy=wy_doy.x, waterYear=waterYear.x)
head(flow_temp_cond_imputed_ice)

# __________________________________________________
# 06. Triming Data frames to time windows 
# __________________________________________________

# 6a) trimming and formatting for spring (ice OFF)
    # Imputed 1982 - 2024
    imputed_data_trimmed <- filter_by_year_and_doy(flow_temp_cond_imputed_ice, c(170,288)) # March 18 - July 15
    # Weekly 1982-2024
    weekly_data_trimmed <- filter_by_year_and_doy(flow_temp_cond_weekly_ice, c(170,288)) # March 18 - July 15
    # Daily 2014-2023
    daily_data_trimmed <- filter_by_year_and_doy(flow_temp_cond_daily_ice, c(170,288)) # March 18 - July 15

    # save trimmed data for spring ice OFF 
    write.csv(imputed_data_trimmed, "derived_data/00_imputed_data_trimmed_spring.csv")
    write.csv(weekly_data_trimmed, "derived_data/00_weekly_data_trimmed_spring.csv")
    write.csv(daily_data_trimmed, "derived_data/00_daily_data_trimmed_spring.csv")

# 6b) trimming and formatting for fall (ice ON)
# extra steps here because we cross the water year boundary 

    # October 1 to December 15
      # Imputed 1982 - 2024
      oct_dec_impute <- filter_by_year_and_doy(flow_temp_cond_imputed_ice, c(1,76)) # October 1 - December 15
      # Weekly 1982-2024
      oct_dec_weekly <- filter_by_year_and_doy(flow_temp_cond_weekly_ice, c(1,76)) # October 1 - December 15
      # Daily  2014-2023
      oct_dec_daily <- filter_by_year_and_doy(flow_temp_cond_daily_ice, c(1,76)) # October 1 - December 15

    # September 15 - October 1 
        # Imputed 1982 - 2024
        sept_oct_impute <- filter_by_year_and_doy(flow_temp_cond_imputed_ice, c(349,365)) # September 15 - October 1
        # Weekly 1982-2024
        sept_oct_weekly <- filter_by_year_and_doy(flow_temp_cond_weekly_ice, c(349,365)) # September 15 - October 1
        # Daily 2014-2023
        sept_oct_daily <- filter_by_year_and_doy(flow_temp_cond_daily_ice, c(349,365)) # September 15 - October 1

    # bind all dates together 
      sept_dec_impute <- rbind(sept_oct_impute,oct_dec_impute)
      sept_dec_weekly <- rbind(sept_oct_weekly,oct_dec_weekly)
      sept_dec_daily <- rbind(sept_oct_daily,oct_dec_daily)

    # sorting dates - creating ordered indices for dates
      ordered_indices_impute <- order(sept_dec_impute$Date)
      ordered_indices_weekly <- order(sept_dec_weekly$Date)
      ordered_indices_daily <- order(sept_dec_daily$Date)

    # applying indices to data frames:
      imputed_data_trimmed_winter <- sept_dec_impute[ordered_indices_impute, ]
      weekly_data_trimmed_winter <- sept_dec_weekly[ordered_indices_weekly, ]
      daily_data_trimmed_winter <- sept_dec_daily[ordered_indices_daily, ]

    # Plot to sanity check 
          weekly_data_trimmed_winter %>%
                mutate(
                cond_scaled = scales::rescale(cond_uScm_weekly, to = range(ice_or_no, na.rm = TRUE)),
                temp_scaled = scales::rescale(temperature_C_weekly, to = range(ice_or_no, na.rm = TRUE)), 
                cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(ice_or_no, na.rm = TRUE)), 
                q_scaled = scales::rescale(Flow, to = range(ice_or_no, na.rm = TRUE))
                ) %>%
                ggplot(aes(x= Date)) +
                geom_point(aes(y = ice_or_no), color = "skyblue3", alpha = 0.75) + 
                geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
                geom_point(aes(y = temp_scaled), color = "salmon3", alpha = 0.75) + 
                geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) + 
                geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) + 
                theme_minimal() + 
            facet_wrap(~waterYear, scales = "free")

    # Create a new data frame putting together imputed and daily to get the full time series 

    # save trimmed data for fall ice ON  
        write.csv(imputed_data_trimmed_winter, "derived_data/00_imputed_data_trimmed_winter.csv")
        write.csv(weekly_data_trimmed_winter, "derived_data/00_weekly_data_trimmed_winter.csv")
        write.csv(daily_data_trimmed_winter, "derived_data/00_daily_data_trimmed_winter.csv")


# ___________________________________________
# 07. Snotel: temp and precip 
# ___________________________________________

    # if you need to check to find the nearsest snowtwl 
        # snotel_sites <- snotel_info()[snotel_info()$state %in% "CO", ] 
        # # bear lake site ID  =  322  

    snotel <- snotel_download(site_id =  322 ,path = tempdir(),  network = "sntl", internal = TRUE)
    head(snotel)

    # Format date column 
    snotel$date <- as.POSIXct(snotel$date)

    # make a water year column 
    snotel$waterYear <- calcWaterYear(snotel$date)


    # subset to only the water years that we are interested in 
    snotel <- snotel %>%
        filter(waterYear >= 1984 )%>%
        select(c("date", "waterYear",   "site_name", 
                "snow_water_equivalent", "precipitation", "precipitation_cumulative", 
                "temperature_max", "temperature_min", "temperature_mean"))


    # plot to sanity check 
    snotel %>%
      ggplot(aes(x = date, y = precipitation)) + 
      geom_line() + 
      theme_minimal() + 
      facet_wrap(~waterYear, scales = "free_x")

    # save output 
    write.csv(snotel, "derived_data/00_snotel_322.csv")

# __________________________________________________
# 08. Daily Weather
# __________________________________________________

    # Weather station data (temp, wind):
    weatherData <- read.csv("raw_data/lvws_met_19911217_20240909.csv")
    weatherData$datetime <- as.POSIXct(weatherData$datetime)

    # Aggregate to air temp and wind speed
    weather_daily <- weatherData %>%
      mutate(Date = as.POSIXct(datetime)) %>% # Extract the date part
      group_by(Date) %>% # Group by date
      summarise(
        airT_mean = mean(airt, na.rm = TRUE),
        # T_air_6_m_mean = mean(T_air_6_m, na.rm = TRUE),
        airT_max = max(airt, na.rm = TRUE),
        airT_min = min(airt, na.rm = TRUE),
        # T_air_6_m_max = max(T_air_6_m, na.rm = TRUE),
        # T_air_6_m_min = min(T_air_6_m, na.rm = TRUE),
        wind_10m_mean = mean(wnd_10, na.rm = TRUE),
        wind_10m_max = max(wnd_10, na.rm = TRUE),
        # WSpd_6_m_mean = mean(WSpd_6_m, na.rm = TRUE),
        #SWin_2m6m_daily_mean = mean(SWin_2m6m_mean, na.rm = TRUE)
      ) %>% addWaterYear()

    # plot to sanity check 
    weather_daily %>%
      ggplot(aes(x = Date, y = airT_mean)) + 
      geom_line() + 
      theme_minimal() + 
      facet_wrap(~waterYear, scales = "free_x")

    # save output 
    write.csv(weather_daily, "derived_data/00_weather_daily.csv")

