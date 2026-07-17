#######################################
# 00 Data Munging 
#######################################
# # KAG & BDG 2026-06-15

# This script creates three output files (one for each kind of input data): ice, hydrology, meteorlogical 
# Each with file contains daily data for the full timeseries, 
# then in each subsequent script you can call the full time series and trim to the season and years that you want
# note that code to plot after each step is still included (but commented out) for trouble shooting purposes when adding more data. This code can be deleted when all data has been compiled and checked

# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

library(here)
source(here::here("source", "00_libraries.R"))
source(here::here("source", "00_functions.R"))

# __________________________________________________
# 01. Ice Presence 
# __________________________________________________

    # Read in raw data files (annotated from weekly photos )
      ice_binary <- read.csv("raw_data/binary_iceOff_20241001.csv") %>% # read in this data file 
          select(
              c(Date, wy_doy, ice.0.1.)
          ) %>% # select only these columns 
          rename(ice_presence = ice.0.1.) %>% # remname this columnb 
          mutate(Date = mdy(Date)) %>%
          addWaterYear() %>%
          mutate(
            wy_doy = hydro.day(Date), 
            calYear = substring(Date, 1, 4)
          ) %>% 
          select("Date", "calYear", "waterYear", "wy_doy", "ice_presence")%>%
          rename(
              ice = ice_presence # rename ice to shorten 
            ) %>%
            mutate(
              ice = as.factor(ice),  # change to factor to be able to predict 
              Date = as.POSIXct(Date)
            ) 

    # # Plot to sanity check 
    #     head(ice_binary)
    #     ice_binary %>%
    #       filter(waterYear >= 2014) %>%
    #       ggplot(
    #           aes(
    #             x = Date, 
    #             y = ice_presence
    #           )
    #         ) + 
    #         geom_point(
    #           alpha = 0.75, 
    #           color = "darkslategrey"
    #         ) + 
    #         theme_minimal() + 
    #         scale_x_date(
    #           date_breaks = "2 months",
    #           date_labels = "%b"
    #         ) +
    #         facet_wrap(~waterYear, scales = "free_x")

    # save output df 
    write.csv(ice_binary, "derived_data/00_ice_daily_fullyr.csv")

     

# __________________________________________________
# 02. Hydrologic Data 
# __________________________________________________

# ______________________
# 2.1 Flow 
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
          mutate(
                cumulative_dis = cumsum(Flow), 
                wy_doy = hydro.day(Date)
              )

        # ungroup the dataframe
        cumulative_dat_ungroup <- cumulative_dat %>%
          ungroup() %>%
          as.data.frame()

        # rename the df, remove the site number column, make sure dates are in date format. This is the final df
        cumulative_flow_df <- cumulative_dat_ungroup %>% 
          select(-site_no) %>% 
          mutate(Date = as.Date(Date, tz = "MST", format = "%Y-%m-%d")) %>%
          select(Date, waterYear, wy_doy, Flow, cumulative_dis) %>%
          as_tsibble(., key = waterYear, index = Date) %>% #time series tibble
          mutate(
                  Flow = imputeTS::na_interpolation(Flow , maxgap = 7), # Filling gaps in weekly data with a max gap of interpolation as 7 days
                  cumulative_dis = imputeTS::na_interpolation(cumulative_dis, maxgap = 7)
                )

        # Plot to sanity check --> we have all data with no gaps from 1984 to 2025
          # cumulative_flow_df %>%
          #   ggplot(
          #     aes(
          #       x = Date, 
          #       y = cumulative_dis
          #     )
          #   ) + 
          #   geom_point(
          #     color = "mediumorchid4", 
          #     alpha = 0.75
          #   )+ 
          #   theme_minimal() + 
          #   facet_wrap(~waterYear, scales = "free_x")

# ______________________
# 2.2 Temp and Conductivity 


    #1.21)  Pulling in Temp and Conductivity from NWIS:
        # setting parameters for temp and cond
        parameterCd <- c('00095','00010')
        # making the request from NWIS
        temp_cond_nwis_raw <- readWQPqw(paste0("USGS-", lv_no), parameterCd)

        #Clean up dataframe
        temp_cond_nwis <- temp_cond_nwis_raw %>% # take this dataframe that you downloaded from NWIS in the line above 
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
              cond_uScm ="Specific conductance", # change the column names to somethign more manageable 
              water_temp_C ="Temperature, water",
              Date = "ActivityStartDate"
          ) %>%
          select(-ActivityConductingOrganizationText) %>% # remove this column because we don't need it anymore 
          distinct(Date, .keep_all = TRUE) %>%
          addWaterYear() %>% # add water year as a column 
          as_tsibble(., key = waterYear, index = Date) %>% #time series tibble
          fill_gaps() %>%  #makes the missing data implicit
          select(Date, waterYear,  cond_uScm, water_temp_C)  %>%
          filter(
              waterYear <= 2011 & 
              waterYear >= 1984
          ) %>% # use the data from graham for 2012 onward and we are only going back to 1984 
          mutate(
                  wy_doy = hydro.day(Date),  # create a new column for the day of the water year      
                  cond_uScm = imputeTS::na_interpolation(cond_uScm, maxgap = 7), # Filling gaps in weekly data with a max gap of interpolation as 7 days
                  water_temp_C = imputeTS::na_interpolation(water_temp_C, maxgap = 7)
                )

          str(temp_cond_nwis) # This is weekly data from 1982-2023, but missing temp & cond after 2019

          # Plot to sanity check 
          #  temp_cond_nwis %>%
          #       ggplot(
          #           aes(x = Date, 
          #               y =  water_temp_C
          #           )
          #       ) + 
          #       geom_point(alpha = 0.75, color = "salmon3") + 
          #       theme_minimal()  + 
          #       facet_wrap(~waterYear, scales = "free") 

            # temp_cond_nwis %>%
            #     ggplot(
            #         aes(x = wy_doy, 
            #             y = cond_uScm 
            #         )
            #     ) + 
            #     geom_point(alpha = 0.75, color = "olivedrab4") + 
            #     theme_minimal()  + 
            #     facet_wrap(~waterYear, scales = "free") 
          # okay this data starts to look spotty in 2021

    # 2.22)  Pulling in Daily Temp and Cond from Graham with USGS:

        # Temp and conductivity 2011-2019
            temp_cond_usgs_2011_2019 <- read.csv("Input_Files/LochDaily_TempCond_2011-2019.csv")
            # str(temp_cond_usgs_2011_2019)

        # Temp and conductivity 2019-2023

            # Load each file 
            cond_usgs_2019_2023 <- read.csv("Input_Files/Loch_O_daily_conductivity.csv")
            temp_usgs_2019_2023 <- read.csv("Input_Files/Loch_O_daily_temperature.csv")

            # Join temp and cond together (using FULL join because there are fewer conductivity observations) 
            temp_cond_usgs_2019_2023 <- full_join(cond_usgs_2019_2023,
                                                  temp_usgs_2019_2023, 
                                                  by = "Date")
            # str(temp_cond_usgs_2019_2023)

        # Merge together data from multiple time intervals 
        temp_cond_usgs <- merge(temp_cond_usgs_2011_2019, 
                                temp_cond_usgs_2019_2023, 
                                all = TRUE)
       

        # Adding water year and water year doy to the daily data
        temp_cond_usgs <- temp_cond_usgs %>% 
          mutate(
              Date = as.Date(Date, tz = "MST", format = "%Y-%m-%d")
          ) %>% 
          addWaterYear() %>% # add water year as a column 
          distinct(Date, .keep_all = TRUE) %>%
          mutate(wy_doy = hydro.day(Date)) %>% 
          rename(
              water_temp_C ="Temperature_C" # change the column names to somethign more manageable 
          ) %>%
          select(Date, waterYear, wy_doy, cond_uScm, water_temp_C) 

        

    # 2.23) Put together data from NWIS and USGS to get the full time series 
          
          temp_cond_df <- dplyr::bind_rows(temp_cond_nwis, 
                              temp_cond_usgs
                        )

      # Plot to sanity check 
          # conductivity: color = "olivedrab4". temperature:  color = "salmon3"

          # temp_cond_df %>%
          #         ggplot(
          #             aes(x = wy_doy, 
          #                 y = cond_uScm
          #             )
          #         ) + 
          #         geom_point(alpha = 0.75, color =  "olivedrab4") + 
          #         theme_minimal()  + 
          #         facet_wrap(~waterYear, scales = "free") # + 
          #         # scale_x_date(
          #         #     date_breaks = "2 months",
          #         #     date_labels = "%b"
          #         # ) 

          # temp_cond_df %>%
          #         ggplot(
          #             aes(x = Date, 
          #                 y = water_temp_C
          #             )
          #         ) + 
          #         geom_point(alpha = 0.75, color =   "salmon3") + 
          #         theme_minimal()   + 
          #         scale_x_date(
          #             date_breaks = "2 months",
          #             date_labels = "%b"
          #         ) + 
          #         facet_wrap(~waterYear, scales = "free") 


# ______________________
# 2.3 Put together flow, conductivity, and temperature and export as a single file 

      # Format both as data frames to that they play nice (unclear why the time series tibbles are enemies )
      cumulative_flow_data_frame <- as.data.frame(cumulative_flow_df)
      temp_cond_data_frame <- as.data.frame(temp_cond_df)

      # join together all hydro data:
      hydro_daily <- full_join(temp_cond_data_frame , 
                              cumulative_flow_data_frame)

      # add a calendar year column 
        hydro_daily$calYear <- substring(hydro_daily$Date, 1, 4)

      # Change the order of the columns for clarity 
        hydro_daily <- hydro_daily %>%
          select("Date","calYear", "waterYear", "wy_doy", "cond_uScm", "water_temp_C", "Flow", "cumulative_dis")

      str(hydro_daily)

      # Plot all together to check 
      # hydro_daily %>%
      #     filter(calYear >= 1984) %>%
      #     mutate(
      #         cond_scaled = scales::rescale(cond_uScm, to = range(c(0, 1), na.rm = TRUE)),
      #         temp_scaled = scales::rescale(water_temp_C, to = range(c(0, 1), na.rm = TRUE)), 
      #         cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(c(0, 1), na.rm = TRUE)), 
      #         q_scaled = scales::rescale(Flow, to = range(c(0, 1), na.rm = TRUE))
      #     ) %>%
      #     ggplot(aes(x= Date)) + 
      #         geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
      #         geom_point(aes(y = temp_scaled), color = "salmon3", alpha = 0.75) + 
      #         geom_point(aes(y = q_scaled), color = "steelblue1", alpha = 0.75) + 
      #         geom_point(aes(y = cumulative_q_scaled), color = "steelblue4", alpha = 0.75) + 
      #         theme_minimal() + 
      #         facet_wrap(~calYear, scales = "free")

      #save outputs 
      write.csv(hydro_daily, "derived_data/00_hydro_daily_fullyr.csv")

# ___________________________________________
# 03. Meteorlogical Data 
# ___________________________________________

# ______________________
# 3.1  Snotel: temp and precip 

    # if you need to check to find the nearsest snowtwl 
        # snotel_sites <- snotel_info()[snotel_info()$state %in% "CO", ] 
        # # bear lake site ID  =  322  

    snotel_raw <- snotel_download(site_id =  322 ,path = tempdir(),  network = "sntl", internal = TRUE)
    # head(snotel_raw)

    snotel <- snotel_raw %>%
      rename( # change the column names to somethign more manageable 
        Date = "date", 
        swe = "snow_water_equivalent",
        precip = "precipitation",
        precip_cumulative = "precipitation_cumulative"
      ) %>%
      addWaterYear() %>% # use custom funtion to add a water year 
      select( 
        "waterYear", "Date", "swe", "precip", "precip_cumulative" # select only the columbns you need
      ) %>% 
      mutate(
        Date = as.Date(Date), 
        calYear = substring(Date, 1, 4), # add calendar year 
        wy_doy = hydro.day(Date) # add day of hyddro year 
      ) %>% 
      filter(waterYear >= 1984) %>% # we are only interested in after 1984
      mutate(# Filling gaps in weekly data with a max gap of interpolation as 7 days
        precip = imputeTS::na_interpolation(precip , maxgap = 7), 
        precip_cumulative = imputeTS::na_interpolation(precip_cumulative , maxgap = 7),
      )

          # plot to sanity check 
          # snotel %>%
          #   ggplot(
          #     aes(
          #       x = Date, 
          #       y = precip_cumulative
          #     )
          #   ) + 
          #   geom_point(
          #     alpha = 0.5
          #   ) + 
          #   theme_minimal() + 
          #   facet_wrap(~waterYear, scales = "free_x")

# ______________________
# 3.2 Daily Weather from met station 

    # Weather station data (temp, wind):
    weatherData <- read.csv("raw_data/lvws_met_19911217_20240909.csv")

    # Aggregate to air temp and wind speed
    weather_daily <- weatherData %>%
      mutate( # Extract the date part
        Date = as.Date(datetime)
      ) %>% 
      group_by(Date) %>% # Group by date
      summarise( # Calculate daily averages 
        airT_mean = mean(airt, na.rm = TRUE),
        airT_max = max(airt, na.rm = TRUE),
        airT_min = min(airt, na.rm = TRUE),
        wind_10m_mean = mean(wnd_10, na.rm = TRUE),
        wind_10m_max = max(wnd_10, na.rm = TRUE),
      ) %>% 
      addWaterYear() %>% # Add water year 
      mutate(
        calYear = substring(Date, 1, 4), # add calendar year 
        wy_doy = hydro.day(Date) # add day of hyddro year 
      ) %>%
      mutate( # interpolate missing values when there is less than a 7 day gap 
        airT_mean = imputeTS::na_interpolation(airT_mean, maxgap = 7), 
        airT_max = imputeTS::na_interpolation(airT_max, maxgap = 7), 
        airT_min = imputeTS::na_interpolation(airT_min, maxgap = 7), 
        wind_10m_mean = imputeTS::na_interpolation(wind_10m_mean, maxgap = 7), 
        wind_10m_max = imputeTS::na_interpolation(wind_10m_max, maxgap = 7) 
      )

        # plot to sanity check 
        # weather_daily %>%
        #   ggplot(
        #     aes(
        #       x = Date, 
        #       y = airT_mean
        #     )
        #   ) + 
        #   geom_line() + 
        #   theme_minimal() + 
        #   facet_wrap(~waterYear, scales = "free_x")

# ______________________
# 3.3 Combine Met Data 

met_daily <- full_join(snotel, weather_daily) %>%
  select(
    "Date", "calYear", "waterYear", "wy_doy", "swe", "precip", "precip_cumulative", 
    "airT_mean", "airT_max", "airT_min", "wind_10m_mean", "wind_10m_max"
  )

# Plot all together to check 
  # met_daily %>%
  #     filter(calYear >= 1984) %>%
  #     mutate(
  #         swe = scales::rescale(swe, to = range(c(0, 1), na.rm = TRUE)),
  #         precip = scales::rescale(precip, to = range(c(0, 1), na.rm = TRUE)), 
  #         precip_cumulative = scales::rescale(precip_cumulative, to = range(c(0, 1), na.rm = TRUE)), 
  #         airT_mean = scales::rescale(airT_mean, to = range(c(0, .8), na.rm = TRUE)), 
  #         airT_max = scales::rescale(airT_max, to = range(c(0, 1), na.rm = TRUE)),
  #         airT_min = scales::rescale(airT_min, to = range(c(0, .6), na.rm = TRUE)), 
  #         wind_10m_mean = scales::rescale(wind_10m_mean, to = range(c(0, 1), na.rm = TRUE)), 
  #         wind_10m_max = scales::rescale(wind_10m_max, to = range(c(0, 1), na.rm = TRUE))
  #     ) %>%
  #     ggplot(aes(x= Date)) + 
  #         geom_point(aes(y = swe), color = "maroon", alpha = 0.75) + 
  #         geom_point(aes(y = precip), color = "mediumpurple1", alpha = 0.75) + 
  #         geom_point(aes(y = precip_cumulative), color = "mediumpurple4", alpha = 0.75) + 
  #         geom_point(aes(y = airT_mean), color = "orangered2", alpha = 0.75) + 
  #         geom_point(aes(y = airT_max), color = "orangered4", alpha = 0.75) + 
  #         geom_point(aes(y = airT_min), color = "orangered", alpha = 0.75) + 
  #         geom_point(aes(y = wind_10m_mean), color = "goldenrod1", alpha = 0.75) + 
  #         geom_point(aes(y = wind_10m_max), color = "goldenrod4", alpha = 0.75) + 
  #         theme_minimal() + 
  #         facet_wrap(~calYear, scales = "free")

  #save outputs 
      write.csv(met_daily , "derived_data/00_met_daily_fullyr.csv")







# --------------------------------------------------- OLD  ---------------------------------------------------

# OLD: Instead of trimming here and createing a bunch of intermediate files just save the full data file then trim to waht you want at the top of each script 

# # __________________________________________________
# # 04. Triming Data frames to time windows 
# # __________________________________________________

# # ______________________
# # 4.1 trimming and formatting for spring (ice OFF)
#     # Imputed 1982 - 2024
#     imputed_data_trimmed <- filter_by_year_and_doy(flow_temp_cond_imputed_ice, c(170,288)) # March 18 - July 15
#     # Weekly 1982-2024
#     weekly_data_trimmed <- filter_by_year_and_doy(flow_temp_cond_weekly_ice, c(170,288)) # March 18 - July 15
#     # Daily 2014-2023
#     daily_data_trimmed <- filter_by_year_and_doy(flow_temp_cond_daily_ice, c(170,288)) # March 18 - July 15

#     # save trimmed data for spring ice OFF 
#     write.csv(imputed_data_trimmed, "derived_data/00_imputed_data_trimmed_spring.csv")
#     write.csv(weekly_data_trimmed, "derived_data/00_weekly_data_trimmed_spring.csv")
#     write.csv(daily_data_trimmed, "derived_data/00_daily_data_trimmed_spring.csv")

# # ______________________
# # 4.2 trimming and formatting for fall (ice ON)
# # extra steps here because we cross the water year boundary 

#     # October 1 to December 15
#       # Imputed 1982 - 2024
#       oct_dec_impute <- filter_by_year_and_doy(flow_temp_cond_imputed_ice, c(1,76)) # October 1 - December 15
#       # Weekly 1982-2024
#       oct_dec_weekly <- filter_by_year_and_doy(flow_temp_cond_weekly_ice, c(1,76)) # October 1 - December 15
#       # Daily  2014-2023
#       oct_dec_daily <- filter_by_year_and_doy(flow_temp_cond_daily_ice, c(1,76)) # October 1 - December 15

#     # September 15 - October 1 
#         # Imputed 1982 - 2024
#         sept_oct_impute <- filter_by_year_and_doy(flow_temp_cond_imputed_ice, c(349,365)) # September 15 - October 1
#         # Weekly 1982-2024
#         sept_oct_weekly <- filter_by_year_and_doy(flow_temp_cond_weekly_ice, c(349,365)) # September 15 - October 1
#         # Daily 2014-2023
#         sept_oct_daily <- filter_by_year_and_doy(flow_temp_cond_daily_ice, c(349,365)) # September 15 - October 1

#     # bind all dates together 
#       sept_dec_impute <- rbind(sept_oct_impute,oct_dec_impute)
#       sept_dec_weekly <- rbind(sept_oct_weekly,oct_dec_weekly)
#       sept_dec_daily <- rbind(sept_oct_daily,oct_dec_daily)

#     # sorting dates - creating ordered indices for dates
#       ordered_indices_impute <- order(sept_dec_impute$Date)
#       ordered_indices_weekly <- order(sept_dec_weekly$Date)
#       ordered_indices_daily <- order(sept_dec_daily$Date)

#     # applying indices to data frames:
#       imputed_data_trimmed_winter <- sept_dec_impute[ordered_indices_impute, ]
#       weekly_data_trimmed_winter <- sept_dec_weekly[ordered_indices_weekly, ]
#       daily_data_trimmed_winter <- sept_dec_daily[ordered_indices_daily, ]

#     # Plot to sanity check 
#           weekly_data_trimmed_winter %>%
#                 mutate(
#                 cond_scaled = scales::rescale(cond_uScm_weekly, to = range(ice_or_no, na.rm = TRUE)),
#                 temp_scaled = scales::rescale(temperature_C_weekly, to = range(ice_or_no, na.rm = TRUE)), 
#                 cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(ice_or_no, na.rm = TRUE)), 
#                 q_scaled = scales::rescale(Flow, to = range(ice_or_no, na.rm = TRUE))
#                 ) %>%
#                 ggplot(aes(x= Date)) +
#                 geom_point(aes(y = ice_or_no), color = "skyblue3", alpha = 0.75) + 
#                 geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
#                 geom_point(aes(y = temp_scaled), color = "salmon3", alpha = 0.75) + 
#                 geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) + 
#                 geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) + 
#                 theme_minimal() + 
#             facet_wrap(~waterYear, scales = "free")

#     # Create a new data frame putting together imputed and daily to get the full time series 

#     # save trimmed data for fall ice ON  
#         write.csv(imputed_data_trimmed_winter, "derived_data/00_imputed_data_trimmed_winter.csv")
#         write.csv(weekly_data_trimmed_winter, "derived_data/00_weekly_data_trimmed_winter.csv")
#         write.csv(daily_data_trimmed_winter, "derived_data/00_daily_data_trimmed_winter.csv")




