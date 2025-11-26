# Temporary Storage Area Design Optimiser Tool (TSA-DOT): Mode 1
# Author: Dr Martyn Roberts (University of Aberdeen & The James Hutton Institute)
# Date: 24/01/2025

# This R script uses the TSA-DOT to evaluate and optimise natural flood management measures.
# Mode 1 explores the impact of altering key TSA design criteria on flood mitigation effectiveness.

# Key Design Parameters:
# - Maximum Height/Storage Capacity
# - Pipe Diameter
# - Pipe Height
# - Soil Infiltration Rate

# Features:
# - Preloaded Scenarios:
#   - Observed historical storm event
#   - Simulated increased runoff scenario
# - Bund Dimensions:
#   - Preloaded soil bund dimensions of the Tarland TSA
# - User Inputs:
#   - Users can select between the historical or increased runoff scenario
#   - Modify design parameters to observe impacts on TSA flood mitigation
# - Input Validation:
#   - "User Input Summary Table" ensures pipe diameter does not exceed maximum TSA height
#   - Provides an overview of selected parameters

# Outputs:
# 1. Time Series Plot:
#    - Qin (Fast Near Surface Flow): Mainly dominated by overland flow
#    - Qout Quick (Quick Outflow): Water leaving the TSA through the outlet pipe or as overflow
#    - Attenuation Effect: Difference between Qin and Qout Quick indicates attenuation capacity
#    - Overflow: Periods when the TSA is full and overflowing during the storm event
# 2. Volume Plot:
#    - Displays the volume of water held behind the TSA during the storm event
# 3. TSA Volume to Depth Plot:
#    - Displays the depth to volume relationship for the Tarland TSA  
# 3. TSA Effectiveness Table:
#    - Highlights performance across five metrics:

# Metrics and Effective Values:
# 1. Storage Efficiency Index (SEI):
#    - Quantifies TSA performance in utilising storage capacity while minimising overflow
#    - Interpretation:
#      - SEI = 0: Optimal storage utilisation with no overflow
#      - Positive Values: Underutilised storage
#      - Negative Values: Designs prone to overflow
#      - Preferred Range: Closer to 0 (positive or negative)

# 2. Mean Retention Time:
#    - Average time for the TSA to return to near-empty (<5 m3)
#    - Effective Range: 10-20 hours (ensures rapid drainage while preventing prolonged inundation)

# 3. Peak Flow Attenuation:
#    - Additional storage capacity available within ±2 hours of peak inflow (Qin)
#    - Benchmark: 1,000 m3/km2 (effective for reducing flood peaks)

# 4. Peak Flow Reduction:
#    - Difference between peak inflow (Qin) and peak quick outflow (outlet discharge + overflow)
#    - Assesses the TSA’s ability to decrease peak flow rates

# 5. Change in Travel Time of the Peak:
#    - Measures delay between inflow and quick outflow peaks
#    - Indicates the TSA’s effectiveness in shifting peak discharge timing to reduce downstream impacts

# Notes:
# - Analysis of peak flow metrics explores only the maximum peak flow for each storm event


# Instructions to Run the Script:
# --------------------------------
# To run all the code in this script:
# - Highlight all the text (Ctrl + A on Windows / Command + A on Mac)
# - Then press Ctrl + Enter (Windows) or Command + Enter (Mac) to execute.


# Packages ----------------------------------------------------------------

packages <- c("tidyverse", "patchwork", "shiny")

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}


# Helper Functions --------------------------------------------------------

## Interpolation Utility
interpolate_values <- function(x, x_vals, y_vals) {
  # Check if x is out of range
  out_of_range <- x < min(x_vals) | x > max(x_vals)
  if (any(out_of_range)) {
    warning("Value(s) out of range: ", paste(x[out_of_range], collapse = ", "))
    x[out_of_range] <- NA
  }
  # Find intervals
  interval_index <- findInterval(x, x_vals, rightmost.closed = TRUE) + 1
  # Linear interpolation
  y_pred <- ((y_vals[interval_index] - y_vals[interval_index - 1]) * 
               (x - x_vals[interval_index - 1]) / 
               (x_vals[interval_index] - x_vals[interval_index - 1]) + 
               y_vals[interval_index - 1])
  return(round(y_pred, 3))
}

# Main Functions ----------------------------------------------------------

## TSA Volume
TSA_volume.fn <- function(depth, TSA_dimensions) {
  interpolate_values(depth, TSA_dimensions$depth, TSA_dimensions$volume)
}

## TSA Area
TSA_area.fn <- function(depth, TSA_dimensions) {
  interpolate_values(depth, TSA_dimensions$depth, TSA_dimensions$area)
}

## TSA Depth
TSA_depth.fn <- function(volume, TSA_dimensions) {
  interpolate_values(volume, TSA_dimensions$volume, TSA_dimensions$depth)
}

## Outlet Pipe
outlet_pipe.fn <- function(diameter, waterDepth, pipe_base, Cd, maxTSA_height) {
  # Handle missing inputs
  if (any(is.na(c(diameter, waterDepth, pipe_base, Cd)))) return(rep(0, length(diameter)))
  
  waterDepth <- pmin(waterDepth, maxTSA_height)
  h <- pmax(waterDepth - pipe_base, 0)  # Water height above pipe base
  r <- diameter / 2  # Radius of the pipe
  
  # Calculate submerged area
  h_submerged <- pmin(h, diameter)
  submerged_area <- ifelse(waterDepth > pipe_base,
                           r^2 * acos((r - h_submerged) / r) - 
                             (r - h_submerged) * sqrt(2 * r * h_submerged - h_submerged^2),
                           0)
  
  # Calculate discharge (m³/s) and convert to m³/15min
  discharge <- Cd * submerged_area * sqrt(2 * 9.81 * h)
  discharge[is.na(discharge)] <- 0  # Replace NA with 0
  return(round(discharge * (60 * 15), 3))
}

## Soil Infiltration
soil_infiltration.fn <- function(TSA_depth, df, soil_name, TSA_dimensions) {
  soil_rate <- df$soil_rate[df$soil_name == soil_name]
  if (length(soil_rate) == 0) stop("soil_name not found in data frame")
  TSA_area <- TSA_area.fn(TSA_depth, TSA_dimensions)
  return(soil_rate * TSA_area)
}

## TSA Water Balance Model
TSA_model.fn <- function(storm_df, CA, maxTSA_height, pipe_diameter, pipe_height, Cd, soil_df, soil_name, TSA_dimensions) {
  
  # TSA dimensions
  maxTSA_volume <- round(TSA_volume.fn(maxTSA_height, TSA_dimensions), 2)
  maxTSA_area <- TSA_area.fn(maxTSA_height, TSA_dimensions)
  
  # Update pipe_height based on the condition, handling "No Pipe" (NA)
  pipe_height <- ifelse(
    is.na(pipe_height), NA, 
    ifelse(pipe_height == "Bottom", 0 + (pipe_diameter / 2),
           ifelse(pipe_height == "Middle", maxTSA_height / 2,
                  ifelse(pipe_height == "Top", maxTSA_height - (pipe_diameter / 2), pipe_height)))
  )
  
  # Outlet pipe parameters, handling "No Pipe" (NA)
  pipe_area <- ifelse(is.na(pipe_diameter), NA, round(pi * (pipe_diameter ^ 2 / 4), 3))
  pipe_base_m <- if (is.na(pipe_height) || is.na(pipe_diameter)) {
    NA
  } else {
    max(0, pipe_height - (pipe_diameter / 2))
  }
  pipe_base_vol <- ifelse(is.na(pipe_base_m), NA, round(TSA_volume.fn(pipe_base_m, TSA_dimensions), 2))
  
  # Dataframe initialization
  TSA_model <- storm_df %>% 
    mutate(Qin = (Qin_mm / 1000) * CA, 
           Qin = replace(Qin, 1, 0),
           S = 0, dS = 0, depth = 0, PET_m3 = 0, soil_m3 = 0, pipe_m3 = 0, overflow_m3 = 0, Qout = 0,
           PET_m3 = round((PET_mm / 1000) * maxTSA_area, digits = 3))  
  
  # Function to perform row-wise calculation
  calc_row <- function(prev_row, curr_row) {
    dS <- curr_row$Qin - prev_row$Qout
    S <- prev_row$S + dS
    depth <- TSA_depth.fn(volume = S, TSA_dimensions = TSA_dimensions)
    
    # Soil infiltration
    soil_m3 <- soil_infiltration.fn(TSA_depth = depth, df = soil_df, soil_name = soil_name, TSA_dimensions = TSA_dimensions)
    if (S - soil_m3 < 0) {
      soil_m3 <- S
    }
    
    # Pipe discharge, handled only if pipe parameters are defined
    pipe_m3 <- if (!is.na(pipe_diameter) && !is.na(pipe_height)) {
      outlet_pipe.fn(diameter = pipe_diameter, waterDepth = depth, pipe_base = pipe_base_m, Cd = Cd, maxTSA_height = maxTSA_height)
    } else {
      0  # No pipe discharge
    }
    if (S - pipe_m3 < 0) {
      pipe_m3 <- S
    }
    
    # Overflow
    overflow_m3 <- if (S > maxTSA_volume) {
      S - maxTSA_volume 
    } else {
      0
    }
    
    Qout <- curr_row$PET_m3 + soil_m3 + pipe_m3 + overflow_m3
    if (S - Qout < 0) {
      Qout <- S
    }
    
    Qout_quick <- pipe_m3 + overflow_m3
    
    list(S = S, dS = dS, depth = depth, soil_m3 = soil_m3, pipe_m3 = pipe_m3, overflow_m3 = overflow_m3, Qout = Qout, Qout_quick = Qout_quick)
  }
  
  # Use accumulate to calculate the new values row-wise
  results <- accumulate(2:nrow(TSA_model), 
                        .init = list(S = 0, dS = 0, depth = 0, soil_m3 = 0, pipe_m3 = 0, overflow_m3 = 0, Qout = 0, Qout_quick = 0),
                        ~ calc_row(.x, TSA_model[.y, ]))
  
  # Extract the results into the DataFrame
  TSA_model <- TSA_model %>%
    mutate(S = map_dbl(results, "S"),
           dS = map_dbl(results, "dS"),
           depth = map_dbl(results, "depth"),
           soil_m3 = map_dbl(results, "soil_m3"),
           pipe_m3 = map_dbl(results, "pipe_m3"),
           overflow_m3 = map_dbl(results, "overflow_m3"),
           Qout = map_dbl(results, "Qout"),
           Qout_quick = map_dbl(results, "Qout_quick")
    )
}


# Simulate function
simulation <- function(storm_df, maxHeight, pipeDiameter, pipeHeight, soil_infiltration, soil_df) {
  # Apply the model to the storm dataframe
  result <- TSA_model.fn(
    storm_df = storm_df, 
    CA = CA,  
    maxTSA_height = maxHeight,
    pipe_diameter = pipeDiameter,
    pipe_height = pipeHeight,
    Cd = Cd,  
    soil_df = soil_df,  
    soil_name = soil_infiltration,
    TSA_dimensions = TSA_dimensions  
  )
  
  # Add scenario-specific metadata to the result
  result <- result %>%
    mutate(
      maxVol = round(TSA_volume.fn(maxHeight, TSA_dimensions), 0),
      maxHeight = maxHeight,
      pipeDiameter = pipeDiameter,
      pipeHeight = pipeHeight,
      soil_infiltration = soil_infiltration,
      scenario_id = 1
    )
  
  return(result)  # Return the result for the scenario
}



# Flood mitigation effectiveness functions  -------------------------------

flood_metrics.fn <- function(df, CA, threshold) {
  
  peak_Qin_time <- df$date_time[which.max(df$Qin)]
  peak_Qout_time <- df$date_time[which.max(df$Qout_quick)]
  
  ## Criterion 1 - Storage Efficiency Index (SEI) ##
  # Define the modified SEI function
  SEI.fn <- function(df) {
    max_overflow <- max(df$overflow_m3, na.rm = TRUE)
    max_storage <- max(df$S, na.rm = TRUE)
    maxVol <- df$maxVol[1]
    
    # Calculate the storage utilisation component
    storage_utilisation <- (maxVol - max_storage) / maxVol
    
    # Calculate the overflow component using a logarithmic scale
    overflow_component <- log1p(max_overflow) / log1p(maxVol)
    
    # Combine the components to calculate SEI
    SEI <- round(storage_utilisation - overflow_component, 3)
    
    return(SEI)
  }
  
  # Example usage with a dataframe df
  SEI <- SEI.fn(df)
  
  
  
  ## Criterion 2 - Mean retention time & end volume ##
  calculate_retention_time <- function(df, threshold) {
    # Ensure data is sorted by date_time
    df <- df[order(df$date_time), ]
    
    # Identify periods where S > threshold
    above_threshold <- which(df$S > threshold)
    
    # If no values exceed the threshold, return NA and 0 for end_volume
    if (length(above_threshold) == 0) {
      return(list(mean_retention_time = NA, end_volume = 0))
    }
    
    # Find the start and end times of the period above threshold
    start_time <- df$date_time[min(above_threshold)]
    end_time <- df$date_time[max(above_threshold)]
    
    # Calculate retention times
    retention_times <- difftime(df$date_time[above_threshold], start_time, units = "hours")
    
    # Calculate mean retention time
    mean_retention_time <- round(mean(as.numeric(retention_times)), 1)
    
    # Check if S returns to below the threshold
    returns_below_threshold <- any(df$S[max(above_threshold):nrow(df)] < threshold)
    
    # Calculate end volume
    end_volume <- round(df$S[nrow(df)], 0)
    
    if (!returns_below_threshold) {
      return(list(mean_retention_time = NA, end_volume = end_volume))
    } else {
      return(list(mean_retention_time = mean_retention_time, end_volume = end_volume))
    }
  }
  
  retention_df <- calculate_retention_time(df = df, threshold = threshold)
  
  mean_retention_time <- retention_df$mean_retention_time
  
  end_volume <- retention_df$end_volume
  
  
  ## Criterion 3 - Peak floodwater attenuation +/- 2h ##
  subset_df <- df %>%
    filter(date_time >= (peak_Qin_time - 2 * 3600) & date_time <= (peak_Qin_time + 2 * 3600))
  
  CA_km2 <- CA / 1e6
  
  Qp_attenuation <- round((sum(subset_df$Qin, na.rm = TRUE) - sum(subset_df$Qout, na.rm = TRUE) + sum(subset_df$dS, na.rm = TRUE)) / CA_km2, 2)
  
  
  ## Criterion 4 - Peak flow reduction ##
  peak_Qin <- max(df$Qin, na.rm = TRUE)
  peak_Qout <- max(df$Qout_quick, na.rm = TRUE)
  
  Qp_reduction <- round(((peak_Qin - peak_Qout) / peak_Qin) * 100, 1)
  
  
  ## Criterion 5 - Qp Travel time ##
  Qp_travel_time <- as.numeric(difftime(peak_Qout_time, peak_Qin_time, units = "hours"))
  Qp_travel_time <- ifelse(Qp_reduction == 100, NA, Qp_travel_time)
  
  
  ## TSA effectiveness DF ##
  scenario_id <- df$scenario_id[1]
  maxHeight <- df$maxHeight[1]
  maxVol <- df$maxVol[1]
  pipeDiameter <- df$pipeDiameter[1]
  pipeHeight <- df$pipeHeight[1]
  soil_infiltration <- df$soil_infiltration[1]
  
  
  flood_metric_df <- 
    data.frame(scenario_id, maxHeight, maxVol, pipeDiameter, pipeHeight, soil_infiltration,
               SEI, mean_retention_time, end_volume, Qp_attenuation, Qp_reduction, Qp_travel_time)
  
  return(flood_metric_df)
  
}



# Plotting functions ------------------------------------------------------

# Function to aggregate data by hour and include specified columns
aggregate_hourly <- function(df) {
  df %>%
    mutate(hour = floor_date(date_time, "hour")) %>%  # Group by hour
    group_by(hour, scenario_id, maxVol, maxHeight, pipeDiameter, pipeHeight, soil_infiltration) %>%  
    summarize(
      date_time = first(hour),  # Keep the hour as date_time
      overflow_m3 = sum(overflow_m3, na.rm = TRUE),
      Qin_mmhr = (sum(Qin, na.rm = TRUE) / CA) * 1000,  
      Qout_quick_mmhr = (sum(Qout_quick, na.rm = TRUE) / CA) * 1000,  
      overflow_mmhr = (sum(overflow_m3, na.rm = TRUE) / CA) * 1000,
      TSA_volume = mean(S, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
  mutate(maxStorage = ifelse(TSA_volume > maxVol, maxVol, TSA_volume))  
} 



common_theme <- theme_classic() +
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.key.size = unit(5, "mm"),
    legend.key = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    axis.line = element_blank()
  )


# Timeseries plot
create_combined_plot <- function(plot_DF, common_theme) {
  ## Qin and Qout_quick ## 
  Qin_plot <- plot_DF %>%
    ggplot() +
    geom_line(aes(x = date_time, y = Qin_mmhr, color = "Qin", linetype = "Qin")) +
    geom_line(aes(x = date_time, y = Qout_quick_mmhr, 
                  color = "Qout quick", 
                  linetype = "Qout quick"
    )) +
    scale_color_manual(
      values = c("Qin" = "#0072B2", 
                 "Qout quick" = "#009E73"),
      name = NULL) +
    scale_linetype_manual(
      values = c("Qin" = "solid",
                 "Qout quick" = "dashed"),
      name = NULL
    ) +
    labs(
      x = "Date",
      y = expression(Q ~ (mm ~ hr ^ {-1}))
    ) +
    scale_x_datetime(
      date_breaks = "1 day",  
      date_labels = "%d-%m-%Y"
    ) +
    common_theme +
    theme(
      axis.title.y.right = element_blank(),
      axis.text.y.right = element_blank(),
      axis.title.x.bottom = element_blank(),
      axis.text.x.bottom = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      legend.key = element_rect(fill = "white", color = NA),
      legend.title = element_blank()
    )
  
  ## Overflow ##
  overflow_plot <- plot_DF %>%
    ggplot(aes(x = date_time, y = overflow_mmhr)) +
    geom_bar(stat = 'identity', position = 'identity', fill = "#009E73") +
    labs(
      x = "Date",
      y = expression(Overflow ~ (mm ~ hr ^ {-1}))
    ) +
    scale_x_datetime(
      date_breaks = "1 day",  
      date_labels = "%d-%m-%Y",
      sec.axis = dup_axis()
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    common_theme +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
    )
  
  
  ## TSA volume ##
  TSA_vol_plot <- plot_DF %>%
    ggplot(aes(x = date_time, y = maxStorage)) +
    geom_line(color = "#009E73") +
    geom_hline(aes(yintercept = maxVol), color = "#009E73", linetype = "dotted") +
    annotate("text", 
             x = min(plot_DF$date_time), 
             y = unique(plot_DF$maxVol) + (0.05 * unique(plot_DF$maxVol)),
             label = "Max storage capacity", 
             vjust = -0.5, hjust = 0, color = "#009E73", size = 12 / .pt) +
    labs(
      x = "Date",
      y = expression(TSA ~ volume ~ (m ^ {3}))
    ) +
    scale_x_datetime(
      date_breaks = "1 day",  
      date_labels = "%d-%m-%Y",
      sec.axis = dup_axis()
    ) +
    coord_cartesian(ylim = c(0, unique(plot_DF$maxVol) * 1.1)) +
    common_theme +
    theme(
      axis.title.x.top = element_blank(),
      axis.text.x.top = element_blank(),
      legend.position = "none"
    )
  
  combined_plot <- Qin_plot / overflow_plot / TSA_vol_plot + 
    plot_layout(ncol = 1, nrow = 3, heights = c(0.3, 0.3, 0.4))
  
  return(combined_plot)
}


# Volume to depth plot
create_vol_depth_plot <- function(TSA_dimensions, common_theme) {
  Vol_depth_plot <- TSA_dimensions %>%
    ggplot(aes(x = depth, y = volume)) +
    geom_line() +
    scale_y_continuous(
      breaks = seq(0, ceiling(max(TSA_dimensions$volume) / 1000) * 1000, by = 1000),
      limits = c(0, ceiling(max(TSA_dimensions$volume) / 1000) * 1000)
    ) +
    scale_x_continuous(
      breaks = seq(0, max(TSA_dimensions$depth), by = 0.5)
    ) +
    labs(
      x = "Depth (m)",
      y = expression(Volume ~ (m ^ {3}))
    ) +
    common_theme
  
  return(Vol_depth_plot)
}


# Data --------------------------------------------------------------------

# TSA dimensions - Tarland
TSA_dimensions <- data.frame(
  depth = c(0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 1, 1.5, 2, 2.5, 3),
  area = c(0, 20, 260, 375, 450, 520, 580, 635, 675, 715, 760, 805, 1700, 3330, 6500, 12900, 17250),
  volume = c(0, 0.2, 5, 20, 45, 65, 95, 125, 155, 200, 230, 265, 900, 2100, 4300, 9200, 15200)
)

date_time <- seq(
  from = as.POSIXct("2023-10-19 11:30", format = "%Y-%m-%d %H:%M", tz = "UTC"),
  to = as.POSIXct("2023-10-23 04:45", format = "%Y-%m-%d %H:%M", tz = "UTC"),
  by = "15 mins"
)

Qin_mm <- c(0.012666667, 0.019666667, 0.024, 0.023966667, 0.0249, 0.023566667, 
            0.0179, 0.0159, 0.0184, 0.015266667, 0.0159, 0.017133333, 0.016066667, 
            0.0169, 0.021466667, 0.030466667, 0.049166667, 0.066066667, 0.096066667, 
            0.132933333, 0.164166667, 0.1753, 0.1972, 0.192733333, 0.186866667, 0.1789, 
            0.173766667, 0.1441, 0.143866667, 0.142766667, 0.151066667, 0.156266667, 
            0.170333333, 0.197733333, 0.212933333, 0.210533333, 0.217733333, 0.220133333, 
            0.219733333, 0.229333333, 0.240133333, 0.224533333, 0.203333333, 0.164933333, 
            0.125133333, 0.117433333, 0.125266667, 0.132266667, 0.1434, 0.1505, 0.149566667, 
            0.151866667, 0.144666667, 0.145866667, 0.140566667, 0.142966667, 0.139466667, 
            0.146066667, 0.1398, 0.143166667, 0.137833333, 0.136733333, 0.136833333, 
            0.140766667, 0.1387, 0.1411, 0.1423, 0.139066667, 0.1388, 0.1353, 0.1321, 
            0.134566667, 0.136733333, 0.1332, 0.134133333, 0.1342, 0.127333333, 0.1234, 
            0.1191, 0.1139, 0.1087, 0.106233333, 0.0976, 0.0978, 0.083666667, 0.0784, 
            0.0676, 0.070666667, 0.058866667, 0.0601, 0.0561, 0.050366667, 0.042366667, 
            0.039333333, 0.041366667, 0.039033333, 0.0393, 0.0394, 0.041533333, 0.035333333, 
            0.0317, 0.031133333, 0.028366667, 0.027533333, 0.024333333, 0.0201, 0.025166667, 
            0.0227, 0.0189, 0.021833333, 0.022633333, 0.014133333, 0.0153, 0.013633333, 
            0.009433333, 0.007766667, 0.011433333, 0.0059, 0.004566667, 0.0064, 0.008733333, 
            0.0038, 0.007366667, 0.0067, 0.004633333, 0.004833333, 0.0079, 0.019633333, 
            0.029166667, 0.033766667, 0.041566667, 0.047933333, 0.0426, 0.039966667, 
            0.051466667, 0.0675, 0.096733333, 0.1224, 0.145833333, 0.163166667, 0.1595, 
            0.145633333, 0.136366667, 0.134833333, 0.1342, 0.138133333, 0.1423, 0.154, 
            0.1578, 0.166866667, 0.168, 0.1624, 0.1525, 0.144966667, 0.1364, 0.137666667, 
            0.142466667, 0.143666667, 0.151633333, 0.1524, 0.1523, 0.1528, 0.155366667, 
            0.161633333, 0.160433333, 0.155333333, 0.155533333, 0.148333333, 0.141633333,
            0.1395, 0.144833333, 0.1446, 0.1494, 0.1452, 0.146166667, 0.139666667, 0.1371, 
            0.142266667, 0.141766667, 0.1397, 0.143066667, 0.1475, 0.142466667, 0.151366667, 
            0.1557, 0.1583, 0.1636, 0.168733333, 0.168133333, 0.172533333, 0.169733333, 
            0.156133333, 0.135333333, 0.1321, 0.133066667, 0.128366667, 0.125833333, 
            0.136133333, 0.136833333, 0.134266667, 0.1231, 0.1192, 0.118566667, 0.108633333, 
            0.112833333, 0.125866667, 0.125833333, 0.118433333, 0.126133333, 0.112533333, 
            0.1132, 0.1156, 0.1178, 0.1131, 0.1161, 0.107466667, 0.095, 0.088933333, 0.0858, 
            0.074566667, 0.072433333, 0.0736, 0.063266667, 0.057433333, 0.0548, 0.0509, 
            0.041333333, 0.041333333, 0.041766667, 0.0401, 0.037166667, 0.0402, 0.0352, 
            0.034733333, 0.032866667, 0.028266667, 0.0277, 0.026533333, 0.0247, 0.020633333, 
            0.017866667, 0.017533333, 0.014, 0.013366667, 0.012266667, 0.011666667, 0.009933333, 
            0.006933333, 0.0092, 0.0093, 0.005566667, 0.0054, 0.008733333, 0.004133333, 
            0.002633333, 0.0059, 0.006566667, 0.005066667, 0.004666667, 0.0056, 0.000666667, 
            0.0019, 0.000433333, 0.0037, 0.002833333, 0.002833333, 0.002133333, 0.0014, 
            0.002733333, 0.000633333, 0.000566667, 0.001533333, 0.003866667, 0, 0, 
            0.000266667, 0, 0, 0, 8e-04, 0.001133333, 0.000333333, 0, 0.000533333, 
            0.000333333, 0.000533333, 0.001333333, 0.002533333, 0.000333333, 0.000733333, 
            0.000733333, 0.000333333, 0.000533333, 0.001733333, 0.001333333, 0.000933333,
            0.001333333, 0.000733333, 0.000333333, 0.000533333, 0.000933333, 0.000533333, 
            0.000733333, 0.001133333, 0.000333333, 0.000533333, 0.000533333, 0.001733333, 
            0.001133333, 0.001333333, 0.002533333, 0.003533333, 0, 0.000133333, 
            0.001333333, 0, 0, 0.002533333, 0.000333333, 0.000933333, 0.000533333, 
            0.001333333, 0, 0.001333333, 0.000133333, 0.000333333, 0, 0.001133333, 
            0.001333333, 0.001333333, 0, 0.000533333, 0.000933333, 0, 0, 0.002733333, 
            0.000933333, 0.000733333, 0.001133333, 0.000933333, 0.000333333, 0.001333333, 
            0.000133333, 0.000133333, 0.002733333, 0, 0, 0.000733333, 0.001333333, 0, 
            0.001133333, 0.001933333, 0.000533333, 0, 0, 0.000366667, 0.000133333, 0.000166667)

Qin40_mm <- Qin_mm * 1.4

PET_mm <- c(rep(0.001398601, 19), rep(0, 62), rep(0.001398601, 34), rep(0, 62), rep(0.001398601, 20), 
            rep(0.002797203, 12), rep(0.001398601, 6), rep(0, 58), rep(0.002797203, 4), 
            rep(0.004195804, 10), rep(0.002797203, 4), rep(0.004195804, 2), rep(0.002797203, 12), 
            rep(0, 53))

# Storm DFs
Qin_DF <- data.frame(date_time, Qin_mm, PET_mm)
Qin40_DF <- data.frame(date_time, Qin40_mm, PET_mm)
Qin40_DF <- Qin40_DF %>% 
  rename(Qin_mm = Qin40_mm)


# Contributing area (m²)
CA <- 300000

# Outlet pipe discharge coefficient
Cd <- 0.65


## Flood mitigation effectiveness ##
near_empty_threshold <- 5


# Configuration for scaling and weighting
config <- list(
  Qp_attenuation_lower_limit = 0,               # Minimum Peak Qin retention to be effective
  retention_min = 10,              # Minimum effective retention time (hours)
  retention_max = 20,              # Maximum effective retention time (hours)
  retention_limit = 24 * 7,        # Absolute limit (e.g., 7 days)
  Qp_reduction_lower_limit = 0,                 # Minimum peak Q reduction to be effective
  optimal_Qp_travel_delay = 12,   # Set the optimum delay time to be adjustable
  Qp_travel_lower_limit = 0,       # Lower limit for travel time
  Qp_travel_upper_limit = 24,      # Upper limit for travel time
  
  weights = list(                  # Normalised weights for each criterion (should add up to 1)
    SEI = 0.2, 
    Qp_attenuation = 0.2, 
    mean_retention_time = 0.2, 
    Qp_reduction = 0.2, 
    Qp_travel_time = 0.2
  )
)


# -------------------------------------------------------------------------

# UI Code
ui <- fluidPage(
  titlePanel("TSA-DOT: Mode 1"),
  
  fluidRow(
    column(4,
           wellPanel(
             h4("Storm Event"),
             radioButtons(
               inputId = "storm_type",
               label = "Select storm:",
               choices = list(
                 "Historical storm (Peak Qin = 1 mm/hr)" = "Qin_DF",
                 "Increased runoff scenario (Qin +40%)" = "Qin40_DF"
               ),
               selected = "Qin_DF"
             ),
             
             h4("TSA Design"),
             h6("Max. height (m):"),
             sliderInput(
               inputId = "maxHeight",
               label = NULL,
               min = 0.4,
               max = 2.5,
               step = 0.1,
               value = 1.0
             ),
             
             h5("Outlet Pipe Design"),
             radioButtons(
               inputId = "pipe_option",
               label = "Outlet pipe:",
               choices = c("No Pipe", "Pipe"),
               selected = "No Pipe"
             ),
             conditionalPanel(
               condition = "input.pipe_option == 'Pipe'",
               h6("Pipe diameter (m):"),
               sliderInput(
                 inputId = "pipeDiameter",
                 label = NULL,
                 min = 0.1,
                 max = 2,
                 step = 0.1,
                 value = 0.1
               ),
               h6("Pipe position:"),
               selectInput(
                 inputId = "pipeHeight",
                 label = NULL,
                 choices = c("Bottom", "Middle", "Top"),
                 selected = "Bottom"
               )
             ),
             
             h6("Soil infiltration rate (mm/hr):"),
             sliderInput(
               inputId = "soil_infiltration",
               label = NULL,
               min = 0,
               max = 20,
               step = 1,
               value = 10
             )
           ),
           uiOutput("designTableWithTitle")
    ),
    column(8,
           tabsetPanel(
             tabPanel("TSA Flood Mitigation Effectiveness",
                      plotOutput("timeseries_plot", height = "700px"),
                      uiOutput("floodMetricsTableWithTitle")
             ),
             tabPanel("TSA Volume to Depth Relationship",
                      plotOutput("vol_depth_plot", height = "800px")
             )
           )
    )
  )
)

# Server Code
server <- function(input, output, session) {
  
  pipe_diameter <- reactiveVal(0.1)
  
  observe({
    if (input$pipe_option == "Pipe") {
      new_diameter <- min(input$pipeDiameter, input$maxHeight)
      pipe_diameter(new_diameter)
    } else {
      pipe_diameter(NA)
    }
  })
  
  storm_df <- reactive({
    if(input$storm_type == "Qin_DF") {
      Qin_DF
    } else {
      Qin40_DF
    }
  })
  
  soil_df <- reactive({
    soil_infiltration_rate <- input$soil_infiltration
    soil_range <- as.character(soil_infiltration_rate)
    soil_rate <- soil_infiltration_rate * (1 / 1000) * (1 / 3600) * 900
    
    data.frame(soil_name = soil_range, soil_rate = soil_rate)
  })
  
  maxStorage <- reactive({
    TSA_volume.fn(input$maxHeight, TSA_dimensions)
  })
  
  simulation_result <- reactive({
    result <- TSA_model.fn(
      storm_df = storm_df(),
      CA = CA,
      maxTSA_height = input$maxHeight,
      pipe_diameter = pipe_diameter(),
      pipe_height = if(input$pipe_option == "Pipe") input$pipeHeight else NA,
      Cd = Cd,
      soil_df = soil_df(),
      soil_name = as.character(input$soil_infiltration),
      TSA_dimensions = TSA_dimensions
    )
    
    result %>%
      mutate(
        maxVol = round(TSA_volume.fn(input$maxHeight, TSA_dimensions), 0),
        maxHeight = input$maxHeight,
        pipeDiameter = pipe_diameter(),
        pipeHeight = if(input$pipe_option == "Pipe") input$pipeHeight else NA,
        soil_infiltration = input$soil_infiltration,
        scenario_id = 1
      )
  })
  
  flood_metrics_result <- reactive({
    req(simulation_result())
    
    flood_metrics.fn(
      df = simulation_result(),
      CA = CA,
      threshold = near_empty_threshold
    )
  })
  
  plot_DF <- reactive({
    req(simulation_result())
    
    aggregate_hourly(simulation_result())
  })
  
  peak_Qin_mmhr <- reactive({
    req(plot_DF())
    max(plot_DF()$Qin_mmhr, na.rm = TRUE)
  })
  
  timeseries_plot <- reactive({
    req(plot_DF())
    create_combined_plot(plot_DF(), common_theme)
  })
  
  Vol_depth_plot <- reactive({
    create_vol_depth_plot(TSA_dimensions, common_theme)
  })
  
  output$designTableWithTitle <- renderUI({
    tagList(
      h4("User Input Summary"),
      renderTable({
        data.frame(
          Parameter = c("Peak Qin (mm/hr)", "Max Height (m)", "Max Storage Capacity (m³)", 
                        "Pipe Option", "Pipe Diameter (m)", "Pipe Position",
                        "Soil Infiltration (mm/hr)"),
          Value = c(
            round(peak_Qin_mmhr(), 1),
            input$maxHeight,
            round(maxStorage(), 1),  # Added this line
            input$pipe_option,
            if(input$pipe_option == "Pipe") round(pipe_diameter(), 2) else "N/A",
            if(input$pipe_option == "Pipe") input$pipeHeight else "N/A",
            input$soil_infiltration
          )
        )
      })
    )
  })
  
  output$floodMetricsTableWithTitle <- renderUI({
    tagList(
      h4("Effectiveness Table"),
      renderTable({
        req(flood_metrics_result())
        df <- data.frame(
          "SEI" = round(flood_metrics_result()$SEI, 2),
          "Mean Retention Time (hrs)" = round(flood_metrics_result()$mean_retention_time, 1),
          "End Volume (m³)" = round(flood_metrics_result()$end_volume, 0),
          "Peak Flow Attenuation (m³/km²)" = round(flood_metrics_result()$Qp_attenuation, 0),
          "Peak Flow Reduction (%)" = round(flood_metrics_result()$Qp_reduction, 0),
          "Peak Flow Travel Time Change (hrs)" = round(flood_metrics_result()$Qp_travel_time, 1)
        )
        names(df) <- c("SEI", "Mean Retention Time (hrs)", "End Volume (m³)", 
                       "Peak Flow Attenuation (m³/km²)", "Peak Flow Reduction (%)", 
                       "Peak Flow Travel Time Change (hrs)")
        df
      }, rownames = FALSE, sanitize.text.function = function(x) x)
    )
  })
  
  output$timeseries_plot <- renderPlot({
    req(timeseries_plot())
    timeseries_plot()
  })
  
  output$vol_depth_plot <- renderPlot({
    req(Vol_depth_plot())
    Vol_depth_plot()
  })
}

# Run the application
shinyApp(ui, server)

