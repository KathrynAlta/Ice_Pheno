#######################################
# Model Comparison 
#######################################

# MODEL COMPARISON CODE ----------------------------------------------

    # Pivot hindcasted days longer 
        # hind_summary <- hind_summary %>%
        #     tidyr::pivot_longer(
        #         cols = c(rf), # designate the columns that you want to pivot longer
        #         names_to = "model", # take the names of the current columns and put them in a new column called model
        #         values_to = "wy_doy" # the values in those columns go to a column called 
        #     )

    # # Format the observed first day of ice off to go with the modeled 
    #     loch_out_summary <- loch_out %>%
    #         group_by(waterYear) %>%
    #         arrange(Date, .by_group = TRUE) %>%
    #         summarize(
    #             wy_doy = first(wy_doy[ice == 0])
    #         )
    #     loch_out_summary$model <- "obs"
    #     loch_out_summary <- subset(loch_out_summary, select = c("waterYear", "model", "wy_doy"))

    # # put together observed and modeled ice off doy 
    #     ice_off_summary <- rbind(loch_out_summary, hind_summary)

    # # plot ice off date over time
    #         set.seed(123)  # makes the jitter reproducible

    #     ice_off_summary %>%
          
    #     filter(waterYear != 1987) %>%
        
    #     mutate(
    #         error_days = case_when(
    #         model == "rf"  ~ 5,
    #         model == "xgb" ~ 9,
    #         TRUE ~ 0
    #         ),
            
    #         ymin = wy_doy - error_days,
    #         ymax = wy_doy + error_days,
            
    #         # manually jitter the x positions
    #         waterYear_jitter = waterYear + runif(n(), -0.25, 0.25)
    #     ) %>%
        
    #     ggplot(aes(x = waterYear_jitter, y = wy_doy, color = model)) +
        
    #     geom_errorbar(
    #         aes(ymin = ymin, ymax = ymax),
    #         width = 0.15,
    #         alpha = 0.5
    #     ) +
        
    #     geom_point(
    #         alpha = 0.75,
    #         size = 3
    #     ) +
        
    #     scale_color_manual(values = c(
    #         "rf" = "forestgreen",
    #         "xgb" = "maroon",
    #         "obs" = "cornflowerblue"
    #     )) +
        
    #     theme_minimal(base_size = 16) +
        
    #     labs(
    #         y = "Ice Off Date (day of water year)",
    #         x = "Water Year"
    #     )