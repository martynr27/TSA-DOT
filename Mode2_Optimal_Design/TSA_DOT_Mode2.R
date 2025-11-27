# Temporary Storage Area Design Optimiser Tool (TSA-DOT): Mode 2
# Author: Dr Martyn Roberts (University of Aberdeen & The James Hutton Institute)
# Date: 1/11/2025

# Instructions to Run the Script:
# --------------------------------
# To run all the code in this script:
# - Highlight all the text (Ctrl + A on Windows / Command + A on Mac)
# - Then press Ctrl + Enter (Windows) or Command + Enter (Mac) to execute.

# Packages ----------------------------------------------------------------

packages <- c("tidyverse", "patchwork", "shiny", "shinyjs", "rgl")

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


# Generate scenarios ------------------------------------------------------

# Function to create all scenario combinations
create_scenarios <- function(maxHeight_range, pipeDiameter_range, pipeHeight_range, soil_range) {
  # Scenarios without a pipe
  no_pipe_scenarios <- expand.grid(
    maxHeight = maxHeight_range,
    pipeDiameter = NA,
    pipeHeight = NA,
    soil_infiltration = soil_range,
    stringsAsFactors = FALSE
  )
  
  # Scenarios with a pipe
  pipe_scenarios <- expand.grid(
    maxHeight = maxHeight_range,
    pipeDiameter = pipeDiameter_range,
    pipeHeight = pipeHeight_range,
    soil_infiltration = soil_range,
    stringsAsFactors = FALSE
  ) %>%
    # Filter out scenarios where pipe diameter exceeds max TSA height
    filter(pipeDiameter <= maxHeight)
  
  # Combine both scenarios
  scenarios <- bind_rows(
    no_pipe_scenarios, 
    pipe_scenarios
  )
  
  # Ensure distinct combinations
  scenarios <- distinct(scenarios)
  
  # Round maxHeight to 1 decimal place
  scenarios$maxHeight <- round(scenarios$maxHeight, 1)
  
  return(scenarios)
}


# Simulations -------------------------------------------------------------

simulate_scenarios <- function(scenarios, storm, CA, Cd, soil_df, TSA_dimensions) {
  lapply(1:nrow(scenarios), function(i) {
    scenario <- scenarios[i, ]
    # Apply the model to the storm dataframe
    result <- TSA_model.fn(
      storm_df = storm,
      CA = CA,
      maxTSA_height = scenario$maxHeight,
      pipe_diameter = scenario$pipeDiameter,
      pipe_height = scenario$pipeHeight,
      Cd = Cd,
      soil_df = soil_df,
      soil_name = scenario$soil_infiltration,
      TSA_dimensions = TSA_dimensions
    )
    
    # Add scenario-specific metadata to the result
    result <- result %>%
      mutate(
        maxVol = round(TSA_volume.fn(scenario$maxHeight, TSA_dimensions), -2),
        maxHeight = scenario$maxHeight,
        pipeDiameter = scenario$pipeDiameter,
        pipeHeight = scenario$pipeHeight,
        soil_infiltration = scenario$soil_infiltration,
        scenario_id = NULL  # Identifier for the scenario
      )
    
    result <- as.data.frame(result)
    
    return(result)
  }) %>%
    bind_rows()  # Use bind_rows instead of do.call(rbind)
}


# Flood mitigation metrics ------------------------------------------------

calculate_flood_metrics <- function(simulations, CA, threshold) {
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
  
  bind_rows(
    lapply(simulations, function(df) {
      flood_metrics.fn(
        df = df,
        CA = CA,
        threshold = threshold
      )
    }),
    .id = "scenario_id"
  )
}


# Scaling function --------------------------------------------------------

normalise_sei <- function(sei) {
  # Find the maximum absolute value for scaling
  max_abs_sei <- max(abs(sei))
  
  # Normalise SEI values
  normalised <- 1 - abs(sei) / max_abs_sei
  
  # Penalise negative values
  normalised[sei < 0] <- normalised[sei < 0] / 2
  
  return(normalised)
}



scaling_metrics <- function(data, config) {
  # Input validation
  required_columns <- c("SEI", "mean_retention_time", "end_volume", "Qp_attenuation", "Qp_reduction", "Qp_travel_time")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }
  
  # Normalize weights to ensure proportional contributions
  weights <- unlist(config$weights)
  weights <- weights / sum(weights)
  
  # Criterion 1 - SEI scaling (unchanged)
  data <- data %>%
    mutate(
      SEI_scaled = normalise_sei(SEI)
    )
  
  # Criterion 2 - Mean retention time and end volume scaling
  data <- data %>%
    mutate(
      retention_effectiveness = case_when(
        is.na(mean_retention_time) ~ 0,
        mean_retention_time >= config$retention_min & mean_retention_time <= config$retention_max ~ 1,
        mean_retention_time < config$retention_min ~ mean_retention_time / config$retention_min,
        !is.na(config$retention_limit) ~ 
          pmax(0, 1 - (mean_retention_time - config$retention_max) / (config$retention_limit - config$retention_max)),
        TRUE ~ pmax(0, 1 - (mean_retention_time - config$retention_max) / config$retention_max)
      ),
      end_volume_scaled = 1 - (end_volume - min(end_volume, na.rm = TRUE)) / 
        (max(end_volume, na.rm = TRUE) - min(end_volume, na.rm = TRUE)),
      retention_scaled = (retention_effectiveness + end_volume_scaled) / 2
    )
  
  # Criterion 3 - Peak Qin attenuation scaling
  data <- data %>%
    mutate(
      Qp_attenuation_scaled = if (is.na(config$Qp_attenuation_lower_limit)) {
        (Qp_attenuation - min(Qp_attenuation, na.rm = TRUE)) /
          (max(Qp_attenuation, na.rm = TRUE) - min(Qp_attenuation, na.rm = TRUE))
      } else {
        pmax(0, (Qp_attenuation - config$Qp_attenuation_lower_limit) /
               (max(Qp_attenuation, na.rm = TRUE) - config$Qp_attenuation_lower_limit))
      }
    )
  
  # Criterion 4 - Peak Qin reduction scaling
  data <- data %>%
    mutate(
      Qp_reduction_scaled = if (is.na(config$Qp_reduction_lower_limit)) {
        (Qp_reduction - min(Qp_reduction, na.rm = TRUE)) /
          (max(Qp_reduction, na.rm = TRUE) - min(Qp_reduction, na.rm = TRUE))
      } else {
        pmax(0, (Qp_reduction - config$Qp_reduction_lower_limit) /
               (max(Qp_reduction, na.rm = TRUE) - config$Qp_reduction_lower_limit))
      }
    )
  
  # Criterion 5 - Qp travel time scaling with optimal delay and limits
  data <- data %>%
    mutate(
      Qp_travel_scaled = case_when(
        is.na(Qp_travel_time) & Qp_reduction == 100 ~ 1,
        Qp_travel_time == config$Qp_travel_lower_limit ~ 0,
        Qp_travel_time > config$optimal_Qp_travel_delay ~ pmax(0,
                                                               1 - abs(Qp_travel_time - config$optimal_Qp_travel_delay) /
                                                                 max(config$optimal_Qp_travel_delay, abs(config$Qp_travel_upper_limit - config$optimal_Qp_travel_delay))),
        Qp_travel_time > config$Qp_travel_upper_limit ~ 0,
        !is.na(Qp_travel_time) ~ pmax(0,
                                      1 - abs(Qp_travel_time - config$optimal_Qp_travel_delay) /
                                        max(config$optimal_Qp_travel_delay, abs(config$Qp_travel_upper_limit - config$optimal_Qp_travel_delay)))
      )
    )
  
  # Calculate overall effectiveness using normalized weights (unchanged)
  data <- data %>%
    mutate(
      overall_effectiveness = (
        SEI_scaled * weights["SEI"] +
          Qp_attenuation_scaled * weights["Qp_attenuation"] +
          retention_scaled * weights["mean_retention_time"] +
          Qp_reduction_scaled * weights["Qp_reduction"] +
          Qp_travel_scaled * weights["Qp_travel_time"]
      )
    )
  
  # Order by overall effectiveness (unchanged)
  data <- data %>%
    arrange(desc(overall_effectiveness))
  
  return(data)
}


# Plotting functions ------------------------------------------------------

common_theme <- theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
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


# Function to aggregate data by hour and include specified columns
aggregate_hourly <- function(df, CA) {
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


# Timeseries plot
create_combined_plot <- function(plot_DF, common_theme) {
  ## Qin and Qout_quick ## 
  Qin_plot <- plot_DF %>%
    ggplot() +
    geom_line(aes(x = date_time, y = Qin_mmhr, color = "Qin", linetype = "Qin")) +
    geom_line(aes(x = date_time, y = Qout_quick_mmhr, 
                  color = "Qout_quickflow", 
                  linetype = "Qout_quickflow"
    )) +
    scale_color_manual(
      values = c("Qin" = "#0072B2", 
                 "Qout_quickflow" = "#009E73"),
      name = NULL) +
    scale_linetype_manual(
      values = c("Qin" = "solid",
                 "Qout_quickflow" = "dashed"),
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
create_vol_depth_plot <- function(TSA_dimensions, common_theme, selected_point = NULL) {
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
  
  # Add selected point if provided
  if (!is.null(selected_point)) {
    Vol_depth_plot <- Vol_depth_plot +
      geom_point(data = selected_point, aes(x = maxHeight, y = maxVol),
                 color = "red", size = 3) +
      geom_text(data = selected_point, aes(x = maxHeight, y = maxVol, label = "Selected TSA design"),
                vjust = -1, color = "red", size = 4)
  }
  
  return(Vol_depth_plot)
}


create_combined_effectiveness_plot <- function(data, x_var, y_var, fill_var,
                                               low_color = "white", high_color = "darkgreen",
                                               text_size = 12,
                                               x_label = "Max. TSA height (m)",
                                               y_label = "Outlet pipe design (dia (m) . position)",
                                               fill_label = "Effectiveness",
                                               effectiveness_title = "Overall Effectiveness",
                                               scenario_title = "TSA Design IDs",
                                               selected_id = NULL,
                                               selected_x = NULL,
                                               selected_y1 = NULL,
                                               selected_y2 = NULL) {
  
  data <- data %>%
    distinct(!!sym(x_var), !!sym(y_var[1]), !!sym(y_var[2]), .keep_all = TRUE)
  
  # Base heatmap
  effectiveness_plot <- ggplot(data, aes(x = factor(!!sym(x_var)), y = interaction(!!sym(y_var[1]), !!sym(y_var[2])), fill = !!sym(fill_var))) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = low_color, high = high_color,
                        labels = scales::label_number(accuracy = 0.1, trim = TRUE)) +
    geom_text(aes(label = sprintf("%.2f", !!sym(fill_var))), color = "black", size = text_size/.pt) +
    labs(
      title = effectiveness_title,
      x = x_label,
      y = y_label,
      fill = fill_label
    ) +
    common_theme
  
  # Base scenario plot
  data$scenario_id_num <- as.numeric(data$scenario_id)
  scenario_plot <- ggplot(data, aes(x = factor(!!sym(x_var)), y = interaction(!!sym(y_var[1]), !!sym(y_var[2])))) +
    geom_tile(aes(fill = scenario_id_num), color = "black") +
    geom_text(aes(label = scenario_id), color = "black", size = text_size/.pt) +
    scale_fill_gradient(low = "lightyellow", high = "lightblue", guide = "none") +
    labs(
      title = scenario_title,
      x = x_label,
      y = y_label
    ) +
    common_theme +
    theme(panel.grid = element_blank())
  
  # Add outline tile on effectiveness_plot if selected coordinates provided
  if (!is.null(selected_x) && !is.null(selected_y1) && !is.null(selected_y2)) {
    highlight_df <- data.frame(
      maxHeight = selected_x,
      pipeDiameter = selected_y1,
      pipeHeight = selected_y2
    )
    
    effectiveness_plot <- effectiveness_plot +
      geom_tile(data = highlight_df,
                aes(x = factor(maxHeight), y = interaction(pipeDiameter, pipeHeight)),
                fill = NA, color = "black", linewidth = 2, inherit.aes = FALSE)
    
    scenario_plot <- scenario_plot +
      geom_tile(data = highlight_df,
                aes(x = factor(maxHeight), y = interaction(pipeDiameter, pipeHeight)),
                fill = NA, color = "black", linewidth = 2, inherit.aes = FALSE)
  }
  
  combined_plot <- effectiveness_plot + scenario_plot +
    plot_layout(ncol = 2, widths = c(1.2, 1))
  
  combined_plot
}

# 3D plot
render_TSA_3D <- function(TSA_dimensions, design_id, maxHeight, maxVol, pipeDiameter, pipeHeight, upperHeight_limit, exaggerate_Z = FALSE) {
  
  # Close any existing RGL devices
  try(close3d(), silent = TRUE)
  
  # Input
  target_volume <- maxVol
  target_depth <- maxHeight
  target_area <- TSA_area.fn(target_depth, TSA_dimensions)
  target_radius <- sqrt((2 * target_area) / pi)
  
  # --- Apply depth limit if specified ---
  if (!is.null(upperHeight_limit)) {
    max_allowed_depth <- min(max(TSA_dimensions$depth), upperHeight_limit + 0.5)
    TSA_dimensions <- TSA_dimensions[TSA_dimensions$depth <= max_allowed_depth, ]
  }
  
  # Full TSA mesh
  theta <- seq(0, pi, length.out = 60)
  radii <- sqrt((2 * TSA_dimensions$area) / pi)
  x <- y <- z <- matrix(NA, nrow = length(TSA_dimensions$depth), ncol = length(theta))
  
  for (i in seq_along(TSA_dimensions$depth)) {
    r <- radii[i]
    x[i, ] <- r * cos(theta)
    y[i, ] <- r * sin(theta)
    z[i, ] <- rep(TSA_dimensions$depth[i], length(theta))
  }
  
  # Water mesh
  n_steps <- 20
  depth_steps <- seq(0, target_depth, length.out = n_steps)
  area_steps <- TSA_area.fn(depth_steps, TSA_dimensions)
  r_steps <- sqrt((2 * area_steps) / pi)
  
  x_water <- y_water <- z_water <- matrix(NA, nrow = n_steps, ncol = length(theta))
  for (i in seq_along(depth_steps)) {
    x_water[i, ] <- r_steps[i] * cos(theta)
    y_water[i, ] <- r_steps[i] * sin(theta)
    z_water[i, ] <- rep(depth_steps[i], length(theta))
  }
  
  # --- Calculate dynamic aspect ratio ---
  x_range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  y_range <- max(y, na.rm = TRUE) - min(y, na.rm = TRUE)
  z_range <- max(z, na.rm = TRUE) - min(z, na.rm = TRUE)
  
  max_range <- max(c(x_range, y_range, z_range))
  aspect_ratio <- c(x_range, y_range, z_range) / max_range
  original_aspect <- aspect_ratio
  
  # Apply exaggeration if z-aspect is too small
  if (exaggerate_Z && aspect_ratio[3] < 0.2) {
    aspect_ratio[3] <- 0.2
  }
  
  # --- 3D rendering ---
  open3d()
  bg3d("white")
  
  # Your existing plotting code...
  persp3d(x, y, z, col = "lightblue", alpha = 0.3,
          xlab = "Width (m)", ylab = "Length (m)", zlab = "Depth (m)",
          front = "lines", back = "lines", aspect = aspect_ratio)
  
  # Water volume
  persp3d(x_water, y_water, z_water, col = "dodgerblue", alpha = 0.8, add = TRUE)
  
  # Water level line
  lines3d(x = c(-target_radius, target_radius), 
          y = c(0, 0), z = rep(target_depth, 2),
          col = "navy", lwd = 2)
  
  # Label
  text3d(0, 0, target_depth + 0.4, 
         paste0(target_volume, " m3 storage at ", target_depth, " m height"),
         color = "navy", cex = 1.2)
  
  # Bund (front edge)
  bund_r <- target_radius
  bund_height <- target_depth
  bund_depth <- 2.5
  
  bx <- c(-bund_r, bund_r, bund_r, -bund_r)
  by <- c(0, 0, -bund_depth, -bund_depth)
  bz <- rep(0, 4)
  bz_top <- rep(bund_height, 4)
  
  verts_bottom <- cbind(bx, by, bz)
  verts_top <- cbind(bx, by, bz_top)
  verts <- rbind(verts_bottom, verts_top)
  
  faces <- list(
    c(1, 2, 6, 5),
    c(2, 3, 7, 6),
    c(3, 4, 8, 7),
    c(4, 1, 5, 8)
  )
  
  for (f in faces) {
    quads3d(verts[f, ], col = "saddlebrown", alpha = 1)
  }
  
  # Top of bund
  quads3d(verts_top, col = "sienna", alpha = 1)
  
  # Outlet pipe
  pipe_diameter <- pipeDiameter
  pipe_radius <- pipe_diameter / 2
  pipe_length <- bund_depth * 2
  
  pipe_height_raw <- pipeHeight
  maxTSA_height <- target_depth
  pipe_z_center <- ifelse(
    is.na(pipe_height_raw), NA,
    ifelse(pipe_height_raw == "Bottom", pipe_radius,
           ifelse(pipe_height_raw == "Middle", maxTSA_height / 2,
                  ifelse(pipe_height_raw == "Top", maxTSA_height - pipe_radius, NA)))
  )
  
  if (!is.na(pipe_z_center)) {
    pipe_y_offset <- -(bund_depth/2)
    pipe_y_vals <- seq(-pipe_length / 2, pipe_length / 2, length.out = 30) + pipe_y_offset
    pipe_theta <- seq(0, 2 * pi, length.out = 30)
    
    pipe_x <- outer(cos(pipe_theta), rep(pipe_radius, length(pipe_y_vals)))
    pipe_y <- outer(rep(1, length(pipe_theta)), pipe_y_vals)
    pipe_z <- outer(sin(pipe_theta), rep(pipe_radius, length(pipe_y_vals))) + pipe_z_center
    
    surface3d(pipe_x, pipe_y, pipe_z, col = "grey30", alpha = 1)
  }
  
  # Print to console
  cat("Original aspect ratio (x : y : z):", paste0(round(original_aspect, 2), collapse = " : "), "\n")
  cat("Used aspect ratio     (x : y : z):", paste0(round(aspect_ratio, 2), collapse = " : "), "\n")
  
  # Title and subtitle
  title3d(
    # main = paste0("3D TSA and Water Storage (Design ID: ", design_id,
    #               if (exaggerate_Z) ") - Exaggerated Z Aspect Ratio" else ") - Proportional Aspect Ratio"),
    sub = paste0("Aspect ratio (x : y : z): ", paste0(round(aspect_ratio, 2), collapse = " : ")),
    color = "black", line = 10, font = 2, cex = 1.4
  )

  scene <- scene3d()
  close3d()
  
  # Return widget with captured scene
  return(rglwidget(scene))
  
}


# Data --------------------------------------------------------------------

# TSA dimensions - Tarland
Tarland_TSA_dimensions <- data.frame(
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


# Outlet pipe discharge coefficient
Cd <- 0.65


## Flood mitigation effectiveness ##
near_empty_threshold <- 5


# Mode 2 GUI --------------------------------------------------------------

# UI Code
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("TSA-DOT: Mode 2"),
  
  fluidRow(
    column(4,
           wellPanel(
             # Main Title with Info Button
             div(style = "display: flex; align-items: center; justify-content: space-between;",
                 h3("TSA-DOT Workflow", style = "color: #007BFF; margin-bottom: 0;"),
                 actionButton("info_tool_usage", label = icon("info-circle"), class = "btn btn-info btn-sm")
             ),
             tags$hr(),
             
             # --- Section 1: Storm and Catchment Data ---
             div(style = "background-color: #F0F0F0; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                 h4("1. Storm Data & Catchment", style = "color: #444; font-weight: bold;"),
                 
                 radioButtons(
                   inputId = "storm_type",
                   label = "Select storm data:",
                   choices = list(
                     "Historical storm (Peak Qin = 1 mm/hr)" = "Qin_DF",
                     "Increased runoff scenario (Qin +40%)" = "Qin40_DF",
                     "Load custom storm data" = "custom"
                   ),
                   selected = "Qin_DF"
                 ),
                 
                 conditionalPanel(
                   condition = "input.storm_type == 'custom'",
                   fileInput("custom_storm_file", "Choose CSV File", accept = c(".csv")),
                   tags$div(
                     style = "background-color: #f8f9fa; border-left: 4px solid #007BFF; padding: 10px; margin-top: 5px;",
                     tags$p(HTML("📌 <b>Required format for custom storm data (CSV):</b>")),
                     tags$ul(
                       tags$li(HTML("<b>date_time:</b> dd/mm/yyyy hh:mm")),
                       tags$li(HTML("<b>Qin_mm:</b> Near-surface runoff")),
                       tags$li(HTML("<b>PET_mm</b>: Potential evapotranspiration (mm); use 0 or leave blank if unavailable"))
                     ),
                     tags$p(HTML("<b>Note:</b> Timestep must be consistent.")),
                     tags$p(HTML("⚠️ <span style='color: #dc3545; font-weight: bold;'>Incorrect format may prevent simulation.</span>"))
                   )
                 ),
                 
                 numericInput("contributing_area", HTML("Contributing Area (km<sup>2</sup>):"), value = 0.3, min = 0, step = 0.1)
             ),
             
             # --- Section 2: TSA Dimensions ---
             div(style = "background-color: #E9ECEF; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                 h4("2. TSA Dimensions", style = "color: #444; font-weight: bold;"),
                 
                 radioButtons("tsa_dimensions", "Select TSA dimensions:",
                              choices = list("Tarland bund dimensions" = "tarland", "Load custom dimensions" = "custom"),
                              selected = "tarland"),
                 
                 conditionalPanel(
                   condition = "input.tsa_dimensions == 'custom'",
                   fileInput("custom_tsa_file", "Choose CSV File", accept = c(".csv")),
                   tags$div(
                     style = "background-color: #f8f9fa; border-left: 4px solid #007BFF; padding: 10px; margin-top: 5px;",
                     tags$p(HTML("📌 <b>Required format for custom TSA dimensions data (CSV):</b>")),
                     tags$ul(
                       tags$li(HTML("<b>depth</b>: Height (m)")),
                       tags$li(HTML("<b>area</b>: Surface area (m²)")),
                       tags$li(HTML("<b>volume</b>: Storage volume (m³)"))
                     ),
                     tags$p(HTML("✅ <b>Requirements:</b>")),
                     tags$ul(
                       tags$li("First row: zero values"),
                       tags$li(HTML("Ensure the final row <b>depth</b> value is at least <b>0.5 m greater</b> than the upper limit set in the <b>Max. TSA Height Range</b> (below), to allow for overflow calculations.")),
                       tags$li("Area & volume must increase with depth")
                     ),
                     tags$p(HTML("⚠️ <span style='color: #dc3545; font-weight: bold;'>Incorrect format may cause errors.</span>"))
                   )
                 )
             ),
             
             # --- Section 3: TSA Height Settings ---
             div(style = "background-color: #F0F0F0; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                 h4("3. TSA Height", style = "color: #444; font-weight: bold;"),
                 sliderInput("maxHeightRange", "Max. Height Range (m):", min = 0.2, max = 2.5, value = c(0.5, 1.5), step = 0.1),
                 numericInput("maxHeightSteps", "Number of height steps:", value = 6, min = 2, max = 10, step = 1)
             ),
             
             # --- Section 4: Outlet Pipe Design ---
             div(style = "background-color: #E9ECEF; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                 h4("4. Outlet Pipe Design", style = "color: #444; font-weight: bold;"),
                 br(),
                 tags$p("Pipe diameter should not exceed TSA max height.", style = "font-style: italic; margin-top: -10px;"),
                 sliderInput("pipeDiameterRange", "Pipe Diameter Range (m):", min = 0.1, max = 2, value = c(0.1, 1), step = 0.1),
                 numericInput("pipeDiameterSteps", "Number of pipe diameter steps:", value = 6, min = 2, max = 10, step = 1),
                 checkboxGroupInput("pipeHeightSelection", "Pipe Height Position(s):", 
                                    choices = c("Bottom", "Middle", "Top"), 
                                    selected = c("Bottom", "Middle", "Top"), inline = TRUE),
                 tags$p("Simulations include both no-pipe (NA) and pipe options.", style = "font-style: italic;")
             ),
             
             # --- Section 5: Soil Infiltration ---
             div(style = "background-color: #F0F0F0; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                 h4("5. Soil Infiltration", style = "color: #444; font-weight: bold;"),
                 numericInput("soil_infiltration", "Soil Infiltration Rate (mm/hr):", value = 5, min = 0, max = 30, step = 1)
             ),
             
             # --- Run Button ---
             div(style = "text-align: center; margin-top: 10px;",
                 actionButton("run_simulation", "Run Simulations", class = "btn btn-primary btn-lg")
             ),
             
             # --- Section 6: Flood Metric Weighting ---
             div(style = "background-color: #E9ECEF; padding: 10px; margin-top: 20px; border-radius: 5px;",
                 h4("6. Flood Metric Weighting", style = "color: #444; font-weight: bold;"),
                 actionButton("info_flood_metrics", label = icon("info-circle"), class = "btn btn-info btn-sm"),
                 helpText(tags$strong("The sum of all weights should equal 1.")),
                 
                 fluidRow(
                   column(8, numericInput("weight_SEI", "Storage Efficiency Index (SEI):", 0.2, min = 0.05, max = 0.8, step = 0.05)),
                   column(4, actionButton("info_SEI", label = icon("info-circle"), class = "btn btn-info btn-sm"))
                 ),
                 fluidRow(
                   column(8, numericInput("weight_mean_retention_time", "Mean retention time:", 0.2, min = 0.05, max = 0.8, step = 0.05)),
                   column(4, actionButton("info_mean_retention_time", label = icon("info-circle"), class = "btn btn-info btn-sm"))
                 ),
                 fluidRow(
                   column(8, numericInput("weight_Qp_reduction", "Peak flow reduction:", 0.2, min = 0.05, max = 0.8, step = 0.05)),
                   column(4, actionButton("info_Qp_reduction", label = icon("info-circle"), class = "btn btn-info btn-sm"))
                 ),
                 fluidRow(
                   column(8, numericInput("weight_Qp_attenuation", "Peak flow attenuation:", 0.2, min = 0.05, max = 0.8, step = 0.05)),
                   column(4, actionButton("info_Qp_attenuation", label = icon("info-circle"), class = "btn btn-info btn-sm"))
                 ),
                 fluidRow(
                   column(8, numericInput("weight_Qp_travel_time", "Peak flow travel time:", 0.2, min = 0.05, max = 0.8, step = 0.05)),
                   column(4, actionButton("info_Qp_travel_time", label = icon("info-circle"), class = "btn btn-info btn-sm"))
                 )
             ),
             
             # --- Section 7: Explore TSA Design in Detail ---
             div(style = "background-color: #F0F0F0; padding: 10px; margin-top: 20px; border-radius: 5px;",
                 h4("7. Select TSA Design ID", style = "color: #444; font-weight: bold;"),
                 helpText("Select a TSA Design ID to explore in detail on the main panels, including storm timeseries, flood mitigation metrics, and TSA geometry."),
                 numericInput("tsa_design_id", "TSA Design ID:", value = 1, min = 1, max = 999, step = 1)
             )
           )
    ),
    
    column(8,
           tabsetPanel(
             tabPanel("TSA Design Effectiveness",
                      
                      fluidRow(
                        column(12, 
                               plotOutput("Heatmap_plot", height = "800px"),
                               br(),
                        )
                      ),
                      
                      fluidRow(
                        column(12, 
                               tags$p(
                                 HTML(
                                   "The left plot displays overall effectiveness scores for each TSA design, while the right plot shows their corresponding Design IDs. 
        To explore a specific design in more detail (i.e. TSA Design Timeseries and TSA Geometry tabs), use the 
        <b>7. Select TSA Design ID</b> option in the side-panel. The selected design will be highlighted with a black outline in both plots."
                                 ),
                                 style = "padding: 10px; border-left: 4px solid #007BFF; background-color: #F8F9FA;"
                               ),
                               br()
                        )
                      ),
                      
                      fluidRow(
                        column(12,
                               uiOutput("selected_tile_info"),
                               br(),
                        )
                      ),
                      
                      fluidRow(
                        column(12,
                               uiOutput("sum_of_weights_display"),
                               br(),
                        )
                      ),
                      
                      fluidRow(
                        column(12, 
                               downloadButton("download_flood_metrics", "Download Flood Mitigation Metrics", class = "btn-primary"),
                               downloadButton("download_scaled_results", "Download Scaled Results", class = "btn-primary")
                        )
                      )
             ),
             
             
             tabPanel("TSA Design Timeseries",
                      fluidRow(
                        column(12, 
                               h4("Selected TSA Design", style = "color: #007BFF;"),
                               tableOutput("tsa_design_info_timeseries"),
                               br()
                        )
                      ),
                      
                      fluidRow(
                        column(12, 
                               plotOutput("timeseries_plot", height = "800px"),
                               br()
                        )
                      ),
                      
                      fluidRow(
                        column(12, 
                               tags$p(
                                 HTML("This plot illustrates the flood mitigation performance of the selected TSA design during the storm event.<br><br>"),
                                 
                                 HTML("The <b>top panel</b> shows near-surface runoff entering the feature (<b>Qin</b>) and rapid TSA outflow (<b>Qout_quickflow</b>), which includes discharge through the outlet pipe and any overflow. "),
                                 
                                 "The difference between Qin and Qout_quickflow highlights the design’s attenuation effect.",
                                 
                                 HTML("The <b>middle panel</b> displays any <b>overflow</b> that occurred during the storm, while the <b>bottom panel</b> shows <b>TSA water volume</b> during the storm, with dotted lines representing maximum storage capacity.<br><br>"),
                                 
                                 "Please ensure you have selected the correct TSA design ID in the sidebar before viewing the timeseries.",
                                 
                                 style = "color: #555; background-color: #F8F9FA; padding: 10px; border-left: 4px solid #007BFF;"
                               ),
                               br()
                        )
                      ),
                      
                      fluidRow(
                        column(12,
                               h4("Flood Mitigation Metrics", style = "color: #007BFF;"),
                               tableOutput("tsa_design_flood_metrics"),
                               br()
                        )
                      ),
                      
                      fluidRow(
                        column(12, 
                               downloadButton("download_timeseries", "Download Timeseries Data", class = "btn-primary")
                        )
                      )
             ),
             
             
             tabPanel("TSA Geometry",
                      fluidRow(
                        column(12, 
                               h4("Selected TSA Design", style = "color: #007BFF;"),
                               tableOutput("tsa_design_info_geometry"),
                               br()
                        )
                      ),
                      fluidRow(
                        column(12,
                               plotOutput("vol_depth_plot", height = "500px")
                        )
                      ),
                      
                      fluidRow(
                        column(12,
                               tags$p("This plot shows how TSA storage volume varies with depth. The red circle indicates the selected TSA design, defined by its maximum storage volume and corresponding depth.",
                                      style = "color: #555; background-color: #F8F9FA; padding: 10px; border-left: 4px solid #007BFF;")
                        )
                      ),
                      br(),
                      
                      # 3D Plot - Proportional
                      fluidRow(
                        column(12,
                               rglwidgetOutput("tsa3d_scene_proportional", width = "100%", height = "800px")
                        )
                      ),
                      
                      fluidRow(
                        column(12,
                               tags$p(
                                 HTML("3D visualisation of the TSA and water storage, maintaining <b>proportional aspect ratios</b>.<br><br>"),
                                 "Use your mouse to rotate and view the TSA design from different angles. Scroll to zoom in and out. ",
                                 "The TSA footprint is shown conceptually as a semi-circle extending from the bund (front edge). ",
                                 "Water stored during the storm event is visualised in blue, with a labelled water level indicating maximum volume.",
                                 style = "color: #555; background-color: #F8F9FA; padding: 10px; border-left: 4px solid #28A745;"
                               )
                        )
                      ),
                      br(),
                      
                      # 3D Plot - Z-Exaggerated
                      fluidRow(
                        column(12,
                               rglwidgetOutput("tsa3d_scene_exaggerated", width = "100%", height = "800px")
                        )
                      ),
                      
                      fluidRow(
                        column(12,
                               tags$p(
                                 HTML("3D visualisation of the TSA and water storage, using an <b>exaggerated Z axis</b> to highlight vertical variation.<br><br>"),
                                 HTML("Use your mouse to rotate the image and scroll to zoom in/out. This helps visualise the <b>depth profile</b>, the volume of water held during the storm event, and the position and diameter of the outlet pipe.<br><br>"),
                                 "As with the proportional plot, the TSA footprint is represented as a semi-circular shape extending from the bund.",
                                 style = "color: #555; background-color: #F8F9FA; padding: 10px; border-left: 4px solid #FFC107;"
                               )
                        )
                      ),
                      br(),
                      
                      # Download Button
                      fluidRow(
                        column(12,
                               downloadButton("download_tsa_dimensions", "Download TSA Dimensions", class = "btn-primary")
                        )
                      )
             )
           )
    )
  )
) 



server <- function(input, output, session) {
  
  storm_df <- reactive({
    print("Processing storm data...")
    if (input$storm_type == "Qin_DF") {
      return(Qin_DF)
    } else if (input$storm_type == "Qin40_DF") {
      return(Qin40_DF)
    } else if (input$storm_type == "custom") {
      req(input$custom_storm_file)
      custom_data <- read.csv(input$custom_storm_file$datapath)
      
      # Validate columns
      required_columns <- c("date_time", "Qin_mm", "PET_mm")
      if (!all(required_columns %in% colnames(custom_data))) {
        showNotification("Error: Custom data must contain columns: date_time, Qin_mm, PET_mm", type = "error")
        return(NULL)
      }
      
      custom_data$date_time <- as.POSIXct(custom_data$date_time, format = "%d/%m/%Y %H:%M")
      
      if (anyNA(custom_data$date_time)) {
        showNotification("Error: Some timestamps could not be parsed", type = "error")
      }
      
      # Replace NA values in PET_mm with 0
      custom_data$PET_mm[is.na(custom_data$PET_mm)] <- 0
      
      return(custom_data)
    }
  })
  
  
  # Check time interval
  time_interval <- reactive({
    req(storm_df())
    df <- storm_df()
    df <- df[order(df$date_time), ]
    
    # Ensure first two times are valid
    if (anyNA(df$date_time[1:2])) {
      showNotification(
        "Error: first two timestamps could not be parsed.", 
        type = "error"
      )
      return(NULL)
    }
    
    interval <- as.numeric(difftime(df$date_time[2], df$date_time[1], units = "secs"))
    time_diffs <- diff(as.numeric(df$date_time))
    
    # Check uniform interval
    if (anyNA(time_diffs) || sd(time_diffs, na.rm = TRUE) > 1e-6) {
      showNotification(
        "Warning: Time intervals in custom storm data are not uniform. ",
        "Proceeding with first interval only.", 
        type = "warning"
      )
    }
    
    return(interval)
  })
  
  CA <- reactive({
    req(input$contributing_area)
    input$contributing_area * 1e6  # Convert km² to m²
  })
  
  TSA_dimensions <- reactive({
    if (input$tsa_dimensions == "tarland") {
      return(Tarland_TSA_dimensions)
    } else if (input$tsa_dimensions == "custom") {
      req(input$custom_tsa_file)
      custom_data <- read.csv(input$custom_tsa_file$datapath)
      
      required_columns <- c("depth", "area", "volume")
      if (!all(required_columns %in% colnames(custom_data))) {
        showNotification("Error: Custom TSA dimensions must contain columns: depth, area, volume", type = "error")
        return(NULL)
      }
      
      return(custom_data)
    }
    return(NULL)
  })
  
  maxHeight_range <- reactive({
    req(input$maxHeightRange, input$maxHeightSteps, TSA_dimensions())
    
    max_depth <- max(TSA_dimensions()$depth, na.rm = TRUE)
    if (input$maxHeightRange[2] >= max_depth) {
      showNotification(
        paste0("Error: The upper TSA height limit must be less than the maximum TSA dimensions depth value (", round(max_depth, 2), " m)."),
        type = "error"
      )
      return(NULL)
    }
    
    seq(input$maxHeightRange[1], input$maxHeightRange[2], length.out = input$maxHeightSteps)
  })
  
  
  soil_df <- reactive({
    req(input$soil_infiltration, time_interval())
    soil_rate <- input$soil_infiltration * (1 / 1000) * (1 / 3600) * time_interval()
    data.frame(soil_name = as.character(input$soil_infiltration), soil_rate = soil_rate)
  })
  
  pipeDiameter_range <- reactive({
    req(input$pipeDiameterRange, input$pipeDiameterSteps, input$maxHeightRange)
    
    # Check if the upper diameter limit exceeds the maxHeight upper limit
    if (input$pipeDiameterRange[2] > input$maxHeightRange[2]) {
      showNotification("Warning: Maximum pipe diameter exceeds maximum TSA height.", type = "error", duration = 8)
    }
    
    # Generate sequence, round to 1dp, then remove duplicates
    unique(round(
      seq(from = input$pipeDiameterRange[1],
          to   = input$pipeDiameterRange[2],
          length.out = input$pipeDiameterSteps), 
      1
    ))
  })
  
  
  pipeHeight_range <- reactive({
    req(input$pipeHeightSelection)
    input$pipeHeightSelection
  })
  
  
  scenarios <- reactive({
    req(maxHeight_range(), soil_df())
    create_scenarios(
      maxHeight_range = maxHeight_range(),
      pipeDiameter_range = pipeDiameter_range(),
      pipeHeight_range = pipeHeight_range(),
      soil_range = as.numeric(soil_df()$soil_name)
    )
  })
  
  observeEvent(simulation_results(), {
    showNotification("Simulations complete. You can now adjust weights and view results.", type = "message")
  })
  
  simulation_results <- eventReactive(input$run_simulation, {
    req(scenarios(), storm_df(), CA(), TSA_dimensions(), soil_df())
    
    withProgress(message = 'Running simulations...', value = 0, {
      n <- nrow(scenarios())
      results <- lapply(seq_len(n), function(i) {
        incProgress(1/n, detail = paste("Scenario", i, "of", n))
        
        # Return as dataframe with scenario metadata
        data.frame(
          scenario_id = i,
          simulate_scenarios(
            scenarios = scenarios()[i, , drop = FALSE],
            storm = storm_df(),
            CA = CA(),
            Cd = Cd,
            soil_df = soil_df(),
            TSA_dimensions = TSA_dimensions()
          ),
          stringsAsFactors = FALSE
        )
      })
    })
  })
  
  config <- reactive({
    list(
      Qp_attenuation_lower_limit = 0,
      retention_min = 10,
      retention_max = 20,
      retention_limit = 24 * 7,
      Qp_reduction_lower_limit = 0,
      optimal_Qp_travel_delay = 12,
      Qp_travel_lower_limit = 0,
      Qp_travel_upper_limit = 24,
      weights = list(
        SEI = input$weight_SEI,
        Qp_attenuation = input$weight_Qp_attenuation,
        mean_retention_time = input$weight_mean_retention_time,
        Qp_reduction = input$weight_Qp_reduction,
        Qp_travel_time = input$weight_Qp_travel_time
      )
    )
  })
  
  observe({
    weights <- c(input$weight_SEI, input$weight_Qp_attenuation, input$weight_mean_retention_time, input$weight_Qp_reduction, input$weight_Qp_travel_time)
    sum_weights <- sum(weights)
    
    output$sum_of_weights_display <- renderUI({
      color <- ifelse(abs(sum_weights - 1) < 1e-6, "green", "red")
      tags$p(
        paste("Sum of Flood Metric Weights:", round(sum_weights, 2)),
        style = paste("font-weight: bold; font-size: 18px; color:", color, ";")
      )
    })
    
    if (abs(sum_weights - 1) > 1e-6) {
      showNotification(paste("Warning: The sum of weights is", round(sum_weights, 2), ". It should equal 1."), type = "warning")
    }
  })
  
  
  flood_metrics <- reactive({
    req(simulation_results(), CA())
    calculate_flood_metrics(simulation_results(), CA(), threshold = near_empty_threshold)
  })
  
  scaled_results <- reactive({
    req(flood_metrics(), config())
    tryCatch({
      scaling_metrics(
        data = flood_metrics(),
        config = config()
      )
    }, error = function(e) {
      message("Scaling Error: ", e$message)
      showNotification(paste("Scaling Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
  timeseries_DF <- reactive({
    req(simulation_results(), input$tsa_design_id)
    
    # Extract the dataframe from the list where the first row's scenario_id matches input$tsa_design_id
    selected_df <- simulation_results() %>%
      keep(~ .x$scenario_id[1] == input$tsa_design_id) %>%
      first()
    
    if (is.null(selected_df) || nrow(selected_df) == 0) {
      showNotification("No simulation found for the selected TSA design ID", type = "warning")
      return(NULL)
    }
    
    return(selected_df)
  })
  
  tsa_design_table <- reactive({
    req(timeseries_DF())
    
    # Extract the required columns
    design_info <- timeseries_DF() %>%
      select(scenario_id, maxHeight, maxVol, pipeDiameter, pipeHeight, soil_infiltration) %>%
      distinct()
    
    # Rename the columns
    colnames(design_info) <- c("TSA design ID", "Height (m)", "Storage capacity (m³)", "Pipe diameter (m)", "Pipe position", "Soil infiltration (mm/hr)")
    
    return(design_info)
  })
  
  tsa_design_flood_metrics <- reactive({
    req(flood_metrics(), input$tsa_design_id)
    
    design_metrics <- flood_metrics() %>% 
      filter(scenario_id == input$tsa_design_id) %>% 
      select(scenario_id, SEI, mean_retention_time, end_volume, Qp_attenuation, Qp_reduction, Qp_travel_time)
    
    # Rename the columns
    colnames(design_metrics) <- c("TSA design ID", "SEI", "Mean Retention Time (hrs)", "End Volume (m³)", 
                                  "Peak Flow Attenuation (m³/km²)", "Peak Flow Reduction (%)", 
                                  "Peak Flow Travel Time Change (hrs)")
    
    return(design_metrics)
  })
  
  
  plot_DF <- reactive({
    req(timeseries_DF(), CA())  
    tryCatch({
      aggregate_hourly(timeseries_DF(), CA())
    }, error = function(e) {
      showNotification(paste("Error in aggregating data:", e$message), type = "error")
      NULL
    })
  })
  
  selected_tile <- reactiveValues(
    x = NULL,  # x-coordinate (maxHeight)
    y = NULL,  # y-coordinate (pipeDiameter.pipeHeight)
    id = NULL, # design ID
    height = NULL,
    diameter = NULL,
    position = NULL,
    effectiveness = NULL,
    storage = NULL,
    soil = NULL
  )
  
  
  observe({
    req(scaled_results())
    
    # Use current input or fallback to 1 if NULL
    sel_id <- if (!is.null(input$tsa_design_id)) input$tsa_design_id else 1
    
    data <- scaled_results()
    matching_row <- data[data$scenario_id == sel_id, ]
    
    if (nrow(matching_row) > 0) {
      selected_tile$x <- matching_row$maxHeight[1]
      selected_tile$y <- interaction(matching_row$pipeDiameter[1], matching_row$pipeHeight[1])[1]
      selected_tile$id <- matching_row$scenario_id[1]
      selected_tile$height <- matching_row$maxHeight[1]
      selected_tile$diameter <- matching_row$pipeDiameter[1]
      selected_tile$position <- matching_row$pipeHeight[1]
      selected_tile$effectiveness <- matching_row$overall_effectiveness[1]
      selected_tile$storage <- matching_row$maxVol[1]
      selected_tile$soil <- matching_row$soil_infiltration[1]
    }
  })
  
  
  Heatmap_plot <- reactive({
    req(scaled_results())
    
    # Find matching row for selected TSA Design ID
    data <- scaled_results()
    matching_row <- data[data$scenario_id == input$tsa_design_id, ]
    
    # If no matching row, set selected coords to NULL
    if (nrow(matching_row) == 0) {
      selected_x <- NULL
      selected_y1 <- NULL
      selected_y2 <- NULL
    } else {
      selected_x <- matching_row$maxHeight[1]
      selected_y1 <- matching_row$pipeDiameter[1]
      selected_y2 <- matching_row$pipeHeight[1]
    }
    
    # Create plot with selected outline
    create_combined_effectiveness_plot(
      data = data,
      x_var = "maxHeight",
      y_var = c("pipeDiameter", "pipeHeight"),
      fill_var = "overall_effectiveness",
      selected_id = input$tsa_design_id,
      selected_x = selected_x,
      selected_y1 = selected_y1,
      selected_y2 = selected_y2
    )
  })
  
  
  timeseries_plot <- reactive({
    req(plot_DF())
    tryCatch({
      create_combined_plot(plot_DF(), common_theme)
    }, error = function(e) {
      showNotification(paste("Error in creating timeseries plot:", e$message), type = "error")
      NULL
    })
  })
  
  Vol_depth_plot <- reactive({
    req(TSA_dimensions(), tsa_design_table(), input$tsa_design_id)
    
    tryCatch({
      # Extract selected row based on input$tsa_design_id
      selected_row <- tsa_design_table() %>%
        filter(`TSA design ID` == input$tsa_design_id)
      
      # Only add point if match is found
      if (nrow(selected_row) == 1) {
        selected_point <- data.frame(
          maxHeight = selected_row[["Height (m)"]],
          maxVol = selected_row[["Storage capacity (m³)"]]
        )
      } else {
        selected_point <- NULL
      }
      
      create_vol_depth_plot(TSA_dimensions(), common_theme, selected_point)
      
    }, error = function(e) {
      showNotification(paste("Error in creating volume-depth plot:", e$message), type = "error")
      NULL
    })
  })
  
  tsa3d_scene_proportional <- reactive({
    
    req(TSA_dimensions(), scaled_results(), input$tsa_design_id)
    
    sel_id <- input$tsa_design_id
    data <- scaled_results()
    matching_row <- data[data$scenario_id == sel_id, ]
    
    # If no match, return NULL
    if (nrow(matching_row) == 0) return(NULL)
    
    render_TSA_3D(
      TSA_dimensions = TSA_dimensions(),
      design_id = sel_id,
      maxHeight = matching_row$maxHeight[1],
      maxVol = matching_row$maxVol[1],
      pipeDiameter = matching_row$pipeDiameter[1],
      pipeHeight = matching_row$pipeHeight[1],
      upperHeight_limit = max(scaled_results()$maxHeight, na.rm = TRUE),
      exaggerate_Z = FALSE
    )
  })
  
  
  tsa3d_scene_exaggerated <- reactive({
    
    req(TSA_dimensions(), scaled_results(), input$tsa_design_id)
    
    sel_id <- input$tsa_design_id
    data <- scaled_results()
    matching_row <- data[data$scenario_id == sel_id, ]
    
    # If no match, return NULL
    if (nrow(matching_row) == 0) return(NULL)
    
    render_TSA_3D(
      TSA_dimensions = TSA_dimensions(),
      design_id = sel_id,
      maxHeight = matching_row$maxHeight[1],
      maxVol = matching_row$maxVol[1],
      pipeDiameter = matching_row$pipeDiameter[1],
      pipeHeight = matching_row$pipeHeight[1],
      upperHeight_limit = max(scaled_results()$maxHeight, na.rm = TRUE),
      exaggerate_Z = TRUE
    )
  })
  
  
  output$selected_tile_info <- renderUI({
    if (is.null(selected_tile$id)) {
    } else {
      tags$div(
        tags$h4("Selected Design Details", style = "margin-top: 15px;"),
        tags$table(
          class = "table table-bordered",
          tags$tr(tags$th("Design ID"), tags$td(selected_tile$id)),
          tags$tr(tags$th("Max Height (m)"), tags$td(selected_tile$height)),
          tags$tr(tags$th("Storage Capacity (m³)"), tags$td(selected_tile$storage)),
          tags$tr(tags$th("Pipe Diameter (m)"), tags$td(selected_tile$diameter)),
          tags$tr(tags$th("Pipe Position"), tags$td(selected_tile$position)),
          tags$tr(tags$th("Soil Infiltration (mm/hr)"), tags$td(selected_tile$soil)),
          tags$tr(tags$th("Effectiveness"), tags$td(sprintf("%.2f", selected_tile$effectiveness)))
        )
      )
    }
  })
  
  output$Heatmap_plot <- renderPlot({ 
    req(Heatmap_plot())
    Heatmap_plot() 
  })
  
  output$download_flood_metrics <- downloadHandler(
    filename = function() {
      paste("flood_metrics_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(flood_metrics(), file, row.names = FALSE)
    }
  )
  
  output$tsa_design_flood_metrics <- renderTable({
    tsa_design_flood_metrics()
  }, digits = 2)
  
  output$download_scaled_results <- downloadHandler(
    filename = function() {
      paste("scaled_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(scaled_results(), file, row.names = FALSE)
    }
  )
  
  
  output$timeseries_plot <- renderPlot({ req(timeseries_plot()); timeseries_plot() })
  
  output$tsa_design_info_timeseries <- renderTable({
    tsa_design_table()
  }, digits = 2)
  
  output$tsa_design_info_geometry <- renderTable({
    tsa_design_table()
  }, digits = 2)
  
  output$download_timeseries <- downloadHandler(
    filename = function() {
      paste0("timeseries_data_", input$tsa_design_id, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(timeseries_DF())
      write.csv(timeseries_DF(), file, row.names = FALSE)
    }
  )
  
  
  output$vol_depth_plot <- renderPlot({ req(Vol_depth_plot()); Vol_depth_plot() })
  
  
  output$tsa3d_scene_proportional <- renderRglwidget({
    req(tsa3d_scene_proportional())
    tsa3d_scene_proportional()
  })
  
  output$tsa3d_scene_exaggerated <- renderRglwidget({
    req(tsa3d_scene_exaggerated())
    tsa3d_scene_exaggerated()
  })
  
  output$download_tsa_dimensions <- downloadHandler(
    filename = function() {
      paste0("TSA_dimensions_", input$tsa_dimensions, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(TSA_dimensions())
      write.csv(TSA_dimensions(), file, row.names = FALSE)
    }
  )
  
  # Simulation data
  observeEvent(input$info_tool_usage, {
    showModal(modalDialog(
      title = "How to use TSA-DOT",
      HTML('
<p>This section guides you through setting up and running simulations in TSA-DOT to assess flood mitigation performance across a range of Temporary Storage Area (TSA) designs.</p>

<ol>
  <li><b>Select a storm event and runoff data (Qin)</b> – Choose from predefined events or upload custom storm data (CSV). 
      <br><b>Qin</b> represents near-surface flow (e.g. overland flow) relative to the contributing area.<br><br></li>
    
  <li><b>Enter the contributing area</b> – Define the catchment size (km²) contributing flow to the TSA.<br><br></li>
  
  <li><b>Choose TSA dimensions</b> – Use built-in Tarland bund shapes or specify custom TSA dimensions.<br><br></li>
  
  <li><b>Set TSA design range</b> – Define the lower and upper limits of maximum TSA height, and the number of steps between them.<br><br></li>
  
  <li><b>Define the outlet pipe design</b> – Set the lower and upper limits for pipe diameter, 
      the number of diameter steps, and select one or more pipe positions (bottom, middle, top).<br><br></li>
  
  <li><b>Specify the soil infiltration rate</b> – This defines the infiltration rate (mm/hr) within the TSA footprint.<br><br></li>
  
  <li><b>Click ‘Run Simulations’</b> – Simulates the response of each TSA design combination based on your selected storm event and design inputs.<br><br></li>
</ol>

<p><b>💡 Tip:</b> Once simulations are run, you can prioritise flood performance metrics in the <b>‘Flood Metric Weighting’</b> section, and explore individual TSA designs in detail using the Design ID selector below.</p>

<p><b>🔄 Note:</b> Whenever you change any simulation inputs, click <b>‘Run Simulations’</b> again to update the results. The weighting and visualisation sections update dynamically and do not require rerunning.</p>
    '),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  # Overview modal
  observeEvent(input$info_flood_metrics, {
    showModal(modalDialog(
      title = "How to use the 'Flood Metric Weighting' section",
      HTML("<b>Flood Metric Weighting:</b><br>Adjust the weight for each flood mitigation metric to prioritise it based on its importance. The sum of all weights must equal 1, and this total is displayed in the <b><i>TSA Design Effectiveness</i></b> tab.<br><br>
      Each metric is scaled from 0 (least effective) to 1 (most effective). The overall effectiveness score is then calculated as the weighted average of all five metrics, and the design with the highest score is identified as optimal for flood mitigation.<br><br>
           <b>Note:</b> You can click the info buttons next to each metric for their definitions."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Storage Efficiency Index (SEI) description
  observeEvent(input$info_SEI, {
    showModal(modalDialog(
      title = "Storage Efficiency Index (SEI)",
      HTML("The <b>SEI</b> measures how effectively the TSA utilises its storage capacity while minimising overflow."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Mean retention time description
  observeEvent(input$info_mean_retention_time, {
    showModal(modalDialog(
      title = "Mean retention time",
      HTML("<b>Mean retention time (hours)</b> represents the average duration it takes for the TSA to return to near-empty levels (less than 5 m³)."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Peak flow reduction description
  observeEvent(input$info_Qp_reduction, {
    showModal(modalDialog(
      title = "Peak flow reduction",
      HTML("<b>Peak flow reduction (%)</b> measures the difference between peak inflow (Qin) and peak quick TSA outflow (outlet discharge + overflow)."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Peak flow attenuation description
  observeEvent(input$info_Qp_attenuation, {
    showModal(modalDialog(
      title = "Peak flow attenuation",
      HTML("<b>Peak flow attenuation (m³/km²)</b> measures the additional storage capacity provided within a ±2-hour window of peak inflow (Qin)."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Peak flow travel time description
  observeEvent(input$info_Qp_travel_time, {
    showModal(modalDialog(
      title = "Peak flow travel time",
      HTML("<b>Peak flow travel time (hours)</b> assesses the delay between inflow (Qin) and quick TSA outflow (outlet discharge + overflow) peaks."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
}

shinyApp(ui, server)
