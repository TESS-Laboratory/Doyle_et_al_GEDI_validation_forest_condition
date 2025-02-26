
# ALS PREPROCESSING FUNCTIONS

# Function to set new directory, retiling preferences, and to retile LAScatalog
retile_catalog_pref <- function(catalog) {
  # Create new output directory within folder of the input LAScatalog
  # Use the first file path to get the input LAScatalog path
  first_file <- catalog@data[["filename"]][1]
  
  # Set the output directory within the same folder as the input LAScatalog
  outdir <- file.path(dirname(first_file), "retile")
  
  # Create the output folder if it doesn't exist
  if (!dir.exists(outdir)) {
    dir.create(outdir)
  }
  
  # Set options for retile
  opt_output_files(catalog) <- paste0(outdir, "/retile_{XLEFT}_{YBOTTOM}")
  opt_chunk_buffer(catalog) <- 0
  opt_laz_compression(catalog) <- TRUE
  opt_chunk_size(catalog) <- 500 # retile to 500 m
  
  # Apply retile
  retiled_catalog <- catalog_retile(catalog)
  
  return(retiled_catalog)
}


# Function to process the ALS data catalog (filters, classifies and normalises) 
process_als <- function(las) { if (is(las, "LAS"))
{
  # Filter for duplicates
  dup_las <- filter_duplicates(las)
  
  # Ground classification - Cloth Simulation Function (Zhang 2016)
  grnd_las <- classify_ground(dup_las, algorithm = csf())
  
  # Height normalization
  norm_las <- normalize_height(grnd_las, tin())
  
  return(norm_las)
}
  if (is(las, "LAScatalog"))
  {
    # Create new output directory within folder of the input LAScatalog
    # Use the first file path to get the input LAScatalog path
    first_file <- las@data[["filename"]][1]
    
    # Set the output directory within the same folder as the input LAScatalog
    outdir <- file.path(dirname(first_file), "final_norm")
    
    # Create the output folder if it doesn't exist
    if (!dir.exists(outdir)) {
      dir.create(outdir)
    }
    options <- list(
      opt_output_files(las) <- paste0(outdir, "/final_{XLEFT}_{YBOTTOM}"),
      need_output_file = TRUE,    # Error if no output template is provided
      need_buffer = TRUE)         # Error if buffer is 0
    norm_catalog <- catalog_map(las, process_als)
    return(norm_catalog)
  }
}


process_alstest <- function(las_catalog) {
  original_folder <- dirname(las_catalog@data[["filename"]][1])
  
  process_las <- function(las, step_name) {
    step_folder <- file.path(original_folder, paste0(step_name, "_", las@data$XLEFT, "_", las@data$YBOTTOM))
    
    if (!dir.exists(step_folder)) {
      dir.create(step_folder)
    }
    
    opt_output_files(las) <- paste0(step_folder, "/final_{XLEFT}_{YBOTTOM}")
    
    # Filter for duplicates
    if (step_name == "dup") {
      dup_las <- filter_duplicates(las)
      writeLAS(dup_las, paste0(step_folder, "/dup_", basename(las@data$filename)))
      return(dup_las)
    }
    
    # Ground classification - Cloth Simulation Function (Zhang 2016)
    if (step_name == "grnd") {
      grnd_las <- classify_ground(dup_las, algorithm = csf())
      writeLAS(grnd_las, paste0(step_folder, "/grnd_", basename(las@data$filename)))
      return(grnd_las)
    }
    
    # Height normalization
    if (step_name == "norm") {
      norm_las <- normalize_height(grnd_las, tin())
      writeLAS(norm_las, paste0(step_folder, "/norm_", basename(las@data$filename)))
      return(norm_las)
    }
  }
  
  # Process each step for the catalog
  for (step_name in c("dup", "grnd", "norm")) {
    las_catalog <- catalog_map(las_catalog, process_las, step_name = step_name)
  }
  
  return(las_catalog)
}


# Select classifications of ALS data and filter catalog for anomalous points
filter_als <- function(LAScatalog){
  # Filter for classification
  opt_select(LAScatalog) <- "xyzrnc"
  opt_filter(LAScatalog) <- "-drop_z_below 0"
  opt_filter(LAScatalog) <- "-drop_z_above 70"
  return(LAScatalog)
}





# GEDI FUNCTIONS


gedi2A_batch_download <- function(poly_folder_path, start_date, end_date, fgb_output_folder, als_year) {
  
  # List all shapefiles in folder
  shapefiles <- list.files(poly_folder_path, pattern = "\\.shp$", full.names = TRUE)
  
  # Process each shapefile
  for (polygon_file in shapefiles) {
    # Read the polygon from the shapefile
    polygon <- st_read(polygon_file)
    
    # Display the polygon information for large batch progress
    cat("Processing polygon:", polygon_file, "\n")
    
    # GEDI processing logic
    gedi2a_search <- find_gedi(polygon,
                               gedi_product = "2A",
                               date_start = start_date,
                               date_end = end_date)
    
     #Check if there are any GEDI files found
    if (length(gedi2a_search) == 0) {
      cat("No GEDI files found for polygon:", polygon_file, "\n")
      next  # Skip to the next iteration if no GEDI files are found
    }
    
    gedi2a_sf <- grab_gedi(gedi2a_search) |>
      filter(
        quality_flag == 1,
        degrade_flag == 0
      ) |>
      mutate(year = lubridate::year(date_time)) |>
      select(
        beam, year, solar_elevation, lat_lowestmode, lon_lowestmode,
        elev_highestreturn, elev_lowestmode, rh0, rh1, rh2, rh3, rh4, rh5, rh6, rh7, rh8, rh9, 
        rh10, rh11, rh12, rh13, rh14, rh15, rh16, rh17, rh18, rh19, rh20, rh21, rh22, rh23, rh24, 
        rh25, rh26, rh27, rh28, rh29, rh30, rh31, rh32, rh33, rh34, rh35, rh36, rh37, rh38, rh39, 
        rh40, rh41, rh42, rh43, rh44, rh45, rh46, rh47, rh48, rh49, rh50, rh51, rh52, rh53, rh54, 
        rh55, rh56, rh57, rh58, rh59, rh60, rh61, rh62, rh63, rh64, rh65, rh66, rh67, rh68, rh69, 
        rh70, rh71, rh72, rh73, rh74, rh75, rh76, rh77, rh78, rh79, rh80, rh81, rh82, rh83, rh84, 
        rh85, rh86, rh87, rh88, rh89, rh90, rh91, rh92, rh93, rh94, rh95, rh96, rh97, rh98, rh99, 
        rh100, sensitivity, shot_number, degrade_flag, num_detectedmodes, modis_treecover,
        rx_cumulative_a1_0, rx_cumulative_a1_1, rx_cumulative_a1_2, rx_cumulative_a1_3, rx_cumulative_a1_4,
        rx_cumulative_a1_5, rx_cumulative_a1_6, rx_cumulative_a1_7, rx_cumulative_a1_8, rx_cumulative_a1_9,
        rx_cumulative_a1_10, rx_cumulative_a1_11, rx_cumulative_a1_12, rx_cumulative_a1_13, rx_cumulative_a1_14,
        rx_cumulative_a1_15, rx_cumulative_a1_16, rx_cumulative_a1_17, rx_cumulative_a1_18, rx_cumulative_a1_19,
        rx_cumulative_a1_20, rx_cumulative_a1_21, rx_cumulative_a1_22, rx_cumulative_a1_23, rx_cumulative_a1_24,
        rx_cumulative_a1_25, rx_cumulative_a1_26, rx_cumulative_a1_27, rx_cumulative_a1_28, rx_cumulative_a1_29,
        rx_cumulative_a1_30, rx_cumulative_a1_31, rx_cumulative_a1_32, rx_cumulative_a1_33, rx_cumulative_a1_34,
        rx_cumulative_a1_35, rx_cumulative_a1_36, rx_cumulative_a1_37, rx_cumulative_a1_38, rx_cumulative_a1_39,
        rx_cumulative_a1_40, rx_cumulative_a1_41, rx_cumulative_a1_42, rx_cumulative_a1_43, rx_cumulative_a1_44,
        rx_cumulative_a1_45, rx_cumulative_a1_46, rx_cumulative_a1_47, rx_cumulative_a1_48, rx_cumulative_a1_49,
        rx_cumulative_a1_50, rx_cumulative_a1_51, rx_cumulative_a1_52, rx_cumulative_a1_53, rx_cumulative_a1_54,
        rx_cumulative_a1_55, rx_cumulative_a1_56, rx_cumulative_a1_57, rx_cumulative_a1_58, rx_cumulative_a1_59,
        rx_cumulative_a1_60, rx_cumulative_a1_61, rx_cumulative_a1_62, rx_cumulative_a1_63, rx_cumulative_a1_64,
        rx_cumulative_a1_65, rx_cumulative_a1_66, rx_cumulative_a1_67, rx_cumulative_a1_68, rx_cumulative_a1_69,
        rx_cumulative_a1_70, rx_cumulative_a1_71, rx_cumulative_a1_72, rx_cumulative_a1_73, rx_cumulative_a1_74,
        rx_cumulative_a1_75, rx_cumulative_a1_76, rx_cumulative_a1_77, rx_cumulative_a1_78, rx_cumulative_a1_79,
        rx_cumulative_a1_80, rx_cumulative_a1_81, rx_cumulative_a1_82, rx_cumulative_a1_83, rx_cumulative_a1_84,
        rx_cumulative_a1_85, rx_cumulative_a1_86, rx_cumulative_a1_87, rx_cumulative_a1_88, rx_cumulative_a1_89,
        rx_cumulative_a1_90, rx_cumulative_a1_91, rx_cumulative_a1_92, rx_cumulative_a1_93, rx_cumulative_a1_94,
        rx_cumulative_a1_95, rx_cumulative_a1_96, rx_cumulative_a1_97, rx_cumulative_a1_98, rx_cumulative_a1_99,
        rx_cumulative_a1_100, landsat_treecover
      ) |>
      mutate(shot_number=as.character(shot_number)) |>
             mutate(beam=as.character(beam)) |>
              rename_with(~ gsub("rx_cumulative_a1_", "rx_cum", .), starts_with("rx_cumulative_a1_")) |>

      
      collect_gedi(gedi_find = gedi2a_search)
    
    # Add a new column with polygon ALS CRS for future reference
    gedi2a_sf <- gedi2a_sf %>%
      mutate(ALS_CRS = substr(basename(polygon_file), 7, 9),
             ALS_year = als_year)  # Assign ALS_year
    
    
    # Print information about the data frame before saving
    #cat("Summary of gedi2a_sf:\n")
   # print(summary(gedi2a_sf))
    
    # Generate a unique output file name based on the shapefile name
    output_file <- file.path(fgb_output_folder, paste0(tools::file_path_sans_ext(basename(polygon_file)), "_2A.fgb"))
    
    # Save your GEDI GeoDataFrame to the output file
    sf::st_write(gedi2a_sf, output_file, delete_dsn = TRUE, overwrite = TRUE)
    
    }
}


gedi2B_batch_download <- function(poly_folder_path, start_date, end_date, fgb_output_folder) {
  
  # List all shapefiles in folder
  shapefiles <- list.files(poly_folder_path, pattern = "\\.shp$", full.names = TRUE)
  
  # Process each shapefile
  for (polygon_file in shapefiles) {
    # Read the polygon from the shapefile
    polygon <- st_read(polygon_file)
    
    # Display the polygon information for large batch progress
    cat("Processing polygon:", polygon_file, "\n")
    
    # GEDI processing logic
    gedi2b_search <- find_gedi(polygon,
                               gedi_product = "2B",
                               date_start = start_date,
                               date_end = end_date)
    
    #Check if there are any GEDI files found
    if (length(gedi2b_search) == 0) {
      cat("No GEDI files found for polygon:", polygon_file, "\n")
      next  # Skip to the next iteration if no GEDI files are found
    }
    
    gedi2b_sf <- grab_gedi(gedi2b_search) |>
      filter(
        l2b_quality_flag == 1
      ) |>
      mutate(year = lubridate::year(date_time)) |>
      select(
        lat_lowestmode, lon_lowestmode, year, shot_number, l2b_quality_flag, cover, 
        pai, fhd_normal, pgap_theta, pgap_theta_error, omega, modis_treecover, degrade_flag
      ) |>
      mutate(shot_number=as.character(shot_number)) |>
      collect_gedi(gedi_find = gedi2b_search)
    
    
    # Print information about the data frame before saving
    #cat("Summary of gedi2b_sf:\n")
    # print(summary(gedi2b_sf))
    
    # Generate a unique output file name based on the shapefile name
    output_file <- file.path(fgb_output_folder, paste0(tools::file_path_sans_ext(basename(polygon_file)), "_2B.fgb"))
    
    # Save your GEDI GeoDataFrame to the output file
    sf::st_write(gedi2b_sf, output_file, delete_dsn = TRUE, overwrite = TRUE)
    
  }
}


gedi4A_batch_download <- function(poly_folder_path, start_date, end_date, fgb_output_folder) {
  
  # List all shapefiles in folder
  shapefiles <- list.files(poly_folder_path, pattern = "\\.shp$", full.names = TRUE)
  
  # Process each shapefile
  for (polygon_file in shapefiles) {
    # Read the polygon from the shapefile
    polygon <- st_read(polygon_file)
    
    # Display the polygon information for large batch progress
    cat("Processing polygon:", polygon_file, "\n")
    
    # GEDI processing logic
    gedi4a_search <- find_gedi(polygon,
                               gedi_product = "4A",
                               date_start = start_date,
                               date_end = end_date)
    
    #Check if there are any GEDI files found
    if (length(gedi4a_search) == 0) {
      cat("No GEDI files found for polygon:", polygon_file, "\n")
      next  # Skip to the next iteration if no GEDI files are found
    }
    
    gedi4a_sf <- grab_gedi(gedi4a_search) |>
      filter(
        l2_quality_flag == 1) |>
      mutate(year = lubridate::year(date_time)) |>
      select(
        agbd, agbd_se, agbd_pi_lower, agbd_pi_upper, lat_lowestmode, lon_lowestmode, l4_quality_flag,
        shot_number, degrade_flag
      ) |>
      mutate(shot_number=as.character(shot_number)) |>
      
      
      collect_gedi(gedi_find = gedi4a_search)
    
    # Add a new column with polygon ALS CRS for future reference
    gedi4a_sf <- gedi4a_sf %>%
      mutate(ALS_CRS = substr(basename(polygon_file), 7, 9))
    
    # Print information about the data frame before saving
    #cat("Summary of gedi4a_sf:\n")
    # print(summary(gedi4a_sf))
    
    # Generate a unique output file name based on the shapefile name
    output_file <- file.path(fgb_output_folder, paste0(tools::file_path_sans_ext(basename(polygon_file)), "_4A.fgb"))
    
    # Save your GEDI GeoDataFrame to the output file
    sf::st_write(gedi4a_sf, output_file, delete_dsn = TRUE, overwrite = TRUE)
    
  }
}


# Calculate amplitude proxy from cumulative energy values in GEDI2A data
gedi_long <- function(gdf) {
  gdf |>
    sf::st_drop_geometry() |>
    dplyr::select(shot_number, ALS_year, starts_with("rh"), starts_with("rx")) |>
    tidyr::pivot_longer(
      cols = -c(shot_number, ALS_year),  # Ensure ALS_year is excluded from pivot
      names_to = c("type", "interval"),
      names_pattern = "(rh|rx_cum)(\\d+)",
      values_to = "value"
    ) |>
    dplyr::mutate(interval = as.numeric(interval)) |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = value
    ) |>
    dplyr::arrange(shot_number, interval) |>
    dplyr::group_by(shot_number, ALS_year) |>
    dplyr::mutate(
      rx = rx_cum - dplyr::lead(rx_cum),
      amp = sqrt(max(rx, na.rm = TRUE) - rx + min(rx, na.rm = TRUE))
    ) |>
    dplyr::ungroup()
}


# Filter and reproject GEDI dataset to match ALS CRS
filter_reproj_GEDI <- function(data, als_crs_value, epsg_code) {
  # Filter the data based on ALS_CRS value
  filtered_data <- data %>%
    filter(ALS_CRS == als_crs_value)
  
  # Transform the coordinate reference system
  transformed_data <- st_transform(filtered_data, epsg_code)
  
  # Return the transformed data
  return(transformed_data)
}


# Cleaning cumulative eenergy data for waveform regression statistics
gedi_cum_long <- function(gdf) {
  gdf |>
    sf::st_drop_geometry() |>
    dplyr::select(shot_number, starts_with("rh"), starts_with("rx")) |>
    tidyr::pivot_longer(
      cols = -shot_number,
      names_to = c("type", "interval"),
      names_pattern = "(rh|rx_cum)(\\d+)",
      values_to = "value"
    ) |>
    dplyr::mutate(
      interval = as.numeric(interval)
    ) |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = value
    ) |>
    dplyr::arrange(shot_number, interval) |>
    dplyr::group_by(shot_number) |>
    dplyr::mutate(
      rx = rx_cum - dplyr::lead(rx_cum),
      amp = sqrt(max(rx, na.rm = TRUE) - rx + min(rx, na.rm = TRUE))
    ) |>
    dplyr::ungroup()
}







# WAVEFORM STATISTICS

# Simple summary statistics for rh waveforms and skew/ kurtosis
wv_summary_stats <- function(row) {
  # Extract shot_number from the row
  shot_number <- row[1]
  
  # Extract the RH values
  rh_values <- as.numeric(row[-1])  # Exclude the shot_number
  
  # Calculate summary statistics
  mean_val <- mean(rh_values)
  sd_val <- sd(rh_values)
  max_val <- max(rh_values)
  min_val <- min(rh_values)
  
  # Calculate n
  n <- length(rh_values)
  
  # Handle potential zero standard deviation
  if (sd_val == 0) {
    skew <- NA
    kurt <- NA
  } else {
    # Calculate skewness
    skew <- (n * sum((rh_values - mean_val)^3)) / ((n-1) * (n-2) * sd_val^3)
    
    # Calculate kurtosis
    kurt <- ((n*(n+1) * sum((rh_values - mean_val)^4)) / ((n-1)*(n-2)*(n-3) * sd_val^4)) - (3 * (n-1)^2 / ((n-2)*(n-3)))
  }
  
  # Return the results
  return(c(shot_number, mean_val, sd_val, max_val, min_val, skew, kurt))
}

# GEDI relative height regression function for rh0 - rh100
rh_linear_regression <- function(row) {
  # Extract shot_number from the row
  shot_number <- row[1]
  
  # Extract the rh values
  rh_values <- as.numeric(row[-1])  # Exclude the shot_number
  
  # Create an index vector for the height percentiles (0-100)
  height_percentiles <- sqrt(0:100)
  
  # Fit linear regression model
  model <- lm(rh_values ~ height_percentiles)
  
  # Extract coefficients
  coefficients <- coef(model)
  
  # Calculate variance
  variance <- var(model$residuals)
  
  # Return coefficients and variance as new columns
  return(c(shot_number, coefficients, variance))
}

# Skew and kurtosis of the rh waveforms 0 - 100
calculate_skew_kurt <- function(row) {
  # Extract shot_number
  shot_number <- row[1]
  
  # Extract RH values (excluding shot_number)
  rh_values <- as.numeric(row[-1])
  
  # Calculate skewness and kurtosis
  skew <- skewness(rh_values)
  kurt <- kurtosis(rh_values)
  
  # Return shot_number along with skewness and kurtosis
  return(c(shot_number, skew, kurt))
}



# 
# # waveformlidar package - maxamp function
# safe_maxamp <- function(row) {
#   result <- tryCatch({
#     max_amp <- maxamp(as.numeric(row[-1]), smooth = TRUE, thres = 0.2, width = 3)
#     return(list(max_amp = max_amp))
#   }, error = function(e) return(list(max_amp = NA)))
#   
#   if (is.null(result)) {
#     return(list(max_amp = NA))
#   } else {
#     return(result)
#   }
# }
# 
# 
# 
# # Helper function for fslope
# safe_fslope <- function(row) {
#   result <- tryCatch({
#     fslope(as.numeric(row[-1]), smooth = TRUE, thres = 0.22, width = 5, tr = 1)
#   }, error = function(e) return(list(FS = NA, ROUGH = NA)))
#   
#   if (is.null(result)) {
#     return(list(FS = NA, ROUGH = NA))
#   } else {
#     return(result)
#   }
# }
# 
# # Helper function for integral
# safe_integral <- function(row) {
#   result <- tryCatch({
#     integral(as.numeric(row[-1]), smooth = TRUE, rescale = TRUE, thres = 0.2, width = 3, tr = 1, dis = 20)
#   }, error = function(e) return(list(ground_integral = NA, veg_integral = NA, total_integral = NA, veg_to_total = NA)))
#   
#   if (is.null(result)) {
#     return(list(ground_integral = NA, veg_integral = NA, total_integral = NA, veg_to_total = NA))
#   } else {
#     return(result)
#   }
# }
# 
# # Helper function for lpeak
# safe_lpeak <- function(row) {
#   result <- tryCatch({
#     peaks <- lpeak(as.numeric(row[-1]), span = 3)
#     return(list(peaks = peaks))
#   }, error = function(e) return(list(peaks = NA)))
#   
#   if (is.null(result)) {
#     return(list(peaks = NA))
#   } else {
#     return(result)
#   }
# }
# lpeak_results <- lapply(lpeak_results, function(x) {
#   if (!is.null(x$peaks)) {
#     # Summarize by counting the number of TRUE values
#     return(data.frame(n_peaks = sum(x$peaks, na.rm = TRUE)))
#   } else {
#     return(data.frame(n_peaks = NA))
#   }
# })
# 
# # Helper function for maxamp
# safe_maxamp <- function(row) {
#   result <- tryCatch({
#     max_amp <- maxamp(as.numeric(row[-1]), smooth = TRUE, thres = 0.2, width = 3)
#     return(list(max_amp = max_amp))
#   }, error = function(e) return(list(max_amp = NA)))
#   
#   if (is.null(result)) {
#     return(list(max_amp = NA))
#   } else {
#     return(result)
#   }
# }
# 
# # Helper function for npeaks
# safe_npeaks <- function(row) {
#   result <- tryCatch({
#     n_peaks <- npeaks(as.numeric(row[-1]), drop = c(0, 0), smooth = TRUE, threshold = 0.2)
#     return(list(n_peaks = n_peaks))
#   }, error = function(e) return(list(n_peaks = NA)))
#   
#   if (is.null(result)) {
#     return(list(n_peaks = NA))
#   } else {
#     return(result)
#   }
# }
# 
# 





# EXTRACTING METRCIS FROM ALS FUNCTIONS USING LidR

#' Standard LiDAR metrics
#'
#' This function computes a set of metrics from LiDAR point clouds. For metric
#' definitions and references, see Table 1 of Mahoney et al (2022).
#'
#' @param Z Return height for each point
#' @param ReturnNumber Return number of each point
#' @param min,max The minimum and maximum valid Z value. Returns outside of
#' this range will be discarded before metric computation.
#'
#' @references
#' Michael J Mahoney, Lucas K Johnson, Eddie Bevilacqua & Colin M Beier
#' (2022) Filtering ground noise from LiDAR returns produces inferior models of
#' forest aboveground biomass in heterogenous landscapes, GIScience & Remote
#' Sensing, 59:1, 1266-1280, DOI: 10.1080/15481603.2022.2103069
#'
#' @return A `data.frame` with 40 columns, containing various LiDAR metrics.
#'
#' @examples
#' lidar_preds(rnorm(1000, 3), sample(1:3, 1000, TRUE, c(0.7, 0.2, 0.1)))
#'
#' @export

density_preds <- function(Z, percentiles = seq(0.05, 0.95, 0.05)) {
  z_range <- range(Z)
  z_range_diff <- z_range[[2]] - z_range[[1]]
  
  lapply(
    z_range_diff * percentiles,
    \(h) mean(Z > (h + z_range[[1]]))
  ) |>
    stats::setNames(paste0("d", (percentiles * 100))) |>
    list2DF()
}
height_preds <- function(Z, percentiles = c(seq(0.05, 0.95, 0.05), 0.96, 0.97, 0.98, 0.99)) {
  stats::quantile(Z, percentiles) |>
    t() |>
    stats::setNames(paste0("rhz", (percentiles * 100))) |>
    as.data.frame()
}
l_moment_preds <- function(Z) {
  if (length(unique(Z)) == 1) {
    out <- data.frame(
      L2 = 0,
      L3 = 0,
      L4 = 0,
      L_cv = 0,
      L_skew = 0,
      L_kurt = 0
    )
  } else {
    l_moments <- lmomco::lmom.ub(Z)
    out <- data.frame(
      L2 = l_moments$L2,
      L3 = l_moments$L3,
      L4 = l_moments$L4,
      L_cv = l_moments$LCV,
      L_skew = l_moments$TAU3,
      L_kurt = l_moments$TAU4
    )
  }
  out
}

lidar_preds <- function(Z, ReturnNumber, min = 0, max = Inf) {
  density_pred_template <- lapply(
    paste0("d", seq(0.05, 0.95, 0.05) * 100),
    \(x) stats::setNames(data.frame(NA_real_), x)
  ) |>
    do.call(what = cbind)
  
  height_pred_template <- lapply(
    paste0("rhz", c(seq(0.05, 0.95, 0.05), 0.96, 0.97, 0.98, 0.99) * 100),
    \(x) stats::setNames(data.frame(NA_real_), x)
  ) |>
    do.call(what = cbind)
  
  out <- cbind(
    n = NA_integer_,
    zmean = NA_real_,
    max = NA_real_,
    min = NA_real_,
    quad_mean = NA_real_,
    cv = NA_real_,
    z_kurt = NA_real_,
    z_skew = NA_real_,
    L2 = NA_real_,
    L3 = NA_real_,
    L4 = NA_real_,
    L_cv = NA_real_,
    L_skew = NA_real_,
    L_kurt = NA_real_,
    height_pred_template,
    density_pred_template,
    cancov = NA_real_,
    quad_mean_c = NA_real_,
    zmean_c = NA_real_,
    cv_c = NA_real_,
    hvol = NA_real_,
    rpc1 = NA_real_
  )
  
  include <- Z >= min & Z <= max
  
  if (sum(include) < 10) {
    return(out)
  }
  
  Z <- Z[include]
  ReturnNumber <- ReturnNumber[include]
  
  out[["n"]] <- length(Z)
  out[["zmean"]] <- mean(Z)
  out[["max"]] <- max(Z)
  out[["min"]] <- min(Z)
  out[["quad_mean"]] <- sqrt(mean(Z^2))
  out[["cv"]] <- stats::sd(Z) / mean(Z)
  out[["z_kurt"]] <- e1071::kurtosis(Z, type = 2)
  out[["z_skew"]] <- e1071::skewness(Z, type = 2)
  l_moments <- l_moment_preds(Z)
  out[names(l_moments)] <- l_moments
  out[names(height_pred_template)] <- height_preds(Z)
  out[names(density_pred_template)] <- density_preds(Z)
  out[["cancov"]] <- mean(Z > 2)
  
  if (sum(Z > 2.5) > 1) {
    out[["quad_mean_c"]] <- sqrt(mean((Z[Z > 2.5])^2))
    out[["zmean_c"]] <- mean(Z[Z > 2.5])
    out[["cv_c"]] <- stats::sd(Z[Z > 2.5]) / out$zmean_c
  } else {
    out[["quad_mean_c"]] <- 0
    out[["zmean_c"]] <- 0
    out[["cv_c"]] <- 0
  }
  
  out$hvol <- out$cancov * out$zmean
  out$rpc1 <- mean(ReturnNumber == 1)
  out
}




# MULTISPECTRAL DATA PROCESSING FOR GEDI FOOTPRINT EXTRACTION OF DATA

# Function to pre-process multispectral secondary forest data for a given year
process_secondary_forest <- function(year) {
  
  # Set directories dynamically based on the year
  data_folder <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Secondary_forest/", year)
  output_folder <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/", year)
  
  # Create output directory if it doesn't exist
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Load shapefiles
  sites_west <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Secondary_forest/secondary_polygon_west.shp")
  sites_east <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Secondary_forest/secondary_polygon_east.shp")
  
  # List raster files
  raster_files <- list.files(data_folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Sort rasters to match naming convention
  raster_files <- sort(raster_files)
  
  # Ensure filenames are assigned correctly
  raster_mapping <- list(
    "0000000000-0000000000" = sites_west,  # First file - Crop with West
    "0000000000-0000065536" = sites_east,  # Second file - Crop with East
    "0000065536-0000065536" = sites_east   # Third file - Crop with East
  )
  
  cropped_rasters <- list()  # Store cropped raster objects
  
  for (raster_path in raster_files) {
    
    raw_raster <- raster(raster_path)
    
    # Ensure CRS matches polygons (convert if needed)
    if (!compareCRS(raw_raster, sites_west)) {  
      raw_raster <- projectRaster(raw_raster, crs = crs(sites_west))
    }
    
    # Extract only the numeric ID from the filename
    filename_id <- tools::file_path_sans_ext(basename(raster_path))
    filename_id <- sub(".*-(\\d{10}-\\d{10})$", "\\1", filename_id)  # Extract correct pattern
    
    if (filename_id %in% names(raster_mapping)) {
      crop_polygon <- raster_mapping[[filename_id]]
      cropped_raster <- crop(raw_raster, crop_polygon)
      
      # Save cropped raster
      cropped_raster_path <- paste0(output_folder, "/", filename_id, "_cropped.tif")
      writeRaster(cropped_raster, cropped_raster_path, overwrite = TRUE)
      
      # Store for merging
      cropped_rasters <- append(cropped_rasters, list(cropped_raster))
      
      # Remove objects from memory
      rm(raw_raster, cropped_raster)
      gc()
    } else {
      message("Skipping unrecognized file: ", raster_path)
    }
  }
  
  # Merge all cropped rasters using raster::merge()
  if (length(cropped_rasters) > 0) {
    merged_secondary <- do.call(merge, cropped_rasters)
    
    # Save final merged raster
    merged_raster_path <- paste0(output_folder, "/secondaryforest", year, ".tif")
    writeRaster(merged_secondary, merged_raster_path, overwrite = TRUE)
    
    final_raster <- raster(merged_raster_path)
    assign(paste0("secondary_forest_", year), final_raster, envir = .GlobalEnv)
    
    # Visualise 
    mapview(final_raster, layer.name = paste("Secondary forest age", year), na.color = "transparent")
    
  }
  
  # Clean up
  rm(cropped_rasters)
  gc()
}

# Function to process fire frequency data (uses functions from process_secondary_forest)
process_fire_frequency <- function(year) {

  # Set base directories
  data_folder <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Fire_data/", year)
  output_folder <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Fire_data/", year)
  
  # Create output directory if it doesn't exist
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Load secondary forest raster for cropping
  secondaryforest_path <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/", year, "/secondaryforest", year, ".tif")
  
  if (!file.exists(secondaryforest_path)) {
    stop("Secondary forest raster for year ", year, " not found! Ensure it has been created first.")
  }
  
  secondaryforest <- raster(secondaryforest_path)
  
  # **List only "Fire Frequency" raster files**
  raster_files <- list.files(data_folder, pattern = "Fire_Frequency_.*\\.tif$", full.names = TRUE)
  
  # Process rasters (same logic as before)
  cropped_rasters <- list()
  
  for (raster_path in raster_files) {
    raw_raster <- raster(raster_path)
    
    if (!compareCRS(raw_raster, secondaryforest)) {  
      raw_raster <- projectRaster(raw_raster, crs = crs(secondaryforest))
    }
    
    cropped_raster <- crop(raw_raster, secondaryforest)
    filename_id <- tools::file_path_sans_ext(basename(raster_path))
    
    cropped_raster_path <- paste0(output_folder, "/", filename_id, "_cropped.tif")
    writeRaster(cropped_raster, cropped_raster_path, overwrite = TRUE)
    
    cropped_rasters <- append(cropped_rasters, list(cropped_raster))
    
    rm(raw_raster, cropped_raster)
    gc()
  }
  
  if (length(cropped_rasters) > 0) {
    merged_fire_raster <- do.call(merge, cropped_rasters)
    
    merged_raster_path <- paste0(output_folder, "/fire_frequency_", year, ".tif")
    writeRaster(merged_fire_raster, merged_raster_path, overwrite = TRUE)
    
    final_raster <- raster(merged_raster_path)
    assign(paste0("fire_frequency_", year), final_raster, envir = .GlobalEnv)
    
    mapview(final_raster, layer.name = paste("Fire Frequency", year), na.color = "transparent")
  }
  
  rm(cropped_rasters)
  gc()
}

# Function to process Time Since Fire data (uses functions from process_secondary_forest)
process_time_since_fire <- function(year) {
  
  # Set base input and output directories
  data_folder <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Fire_data/", year)
  output_folder <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Fire_data/", year)
  
  # Create output directory if it doesn't exist
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Load the secondary forest raster for cropping extent
  secondaryforest_path <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/", year, "/secondaryforest", year, ".tif")
  
  if (!file.exists(secondaryforest_path)) {
    stop("Secondary forest raster for year ", year, " not found! Ensure it has been created first.")
  }
  
  secondaryforest <- raster(secondaryforest_path)
  
  # List only "Time Since Fire" raster files
  raster_files <- list.files(data_folder, pattern = "Time_Since_Last_Fire.*\\.tif$", full.names = TRUE)
  
  # Sort rasters to maintain order
  raster_files <- sort(raster_files)
  
  cropped_rasters <- list()  # Store cropped raster objects
  
  for (raster_path in raster_files) {
    
    raw_raster <- raster(raster_path)
    
    # Ensure CRS matches the secondary forest raster (convert if needed)
    if (!compareCRS(raw_raster, secondaryforest)) {  
      raw_raster <- projectRaster(raw_raster, crs = crs(secondaryforest))
    }
    
    # Crop using the secondary forest extent
    cropped_raster <- crop(raw_raster, secondaryforest)
    
    # Extract filename
    filename_id <- tools::file_path_sans_ext(basename(raster_path))
    
    # Save cropped raster
    cropped_raster_path <- paste0(output_folder, "/", filename_id, "_cropped.tif")
    writeRaster(cropped_raster, cropped_raster_path, overwrite = TRUE)
    
    # Store for merging
    cropped_rasters <- append(cropped_rasters, list(cropped_raster))
    
    # Remove objects from memory
    rm(raw_raster, cropped_raster)
    gc()
  }
  
  # Merge all cropped rasters using raster::merge()
  if (length(cropped_rasters) > 0) {
    merged_time_fire_raster <- do.call(merge, cropped_rasters)
    
    # Save final merged raster
    merged_raster_path <- paste0(output_folder, "/time_since_fire_", year, ".tif")
    writeRaster(merged_time_fire_raster, merged_raster_path, overwrite = TRUE)
    
    final_raster <- raster(merged_raster_path)
    assign(paste0("time_since_fire_", year), final_raster, envir = .GlobalEnv)
    
    # Load and visualize
    mapview(final_raster, layer.name = paste("Time Since Fire", year), na.color = "transparent")

  }
  
  # Clean up
  rm(cropped_rasters)
  gc()
}

# Function to process Forest Validation data (uses functions from process_secondary_forest)
process_forest_validation <- function(year) {
  
  # Set base directories
  input_folder <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Forest_validation/", year)
  output_folder <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Forest_Validation/", year)
  
  # Create output directory if it doesn't exist
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Load the secondary forest raster for cropping extent
  secondaryforest_path <- paste0("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/", year, "/secondaryforest", year, ".tif")
  
  if (!file.exists(secondaryforest_path)) {
    stop("Secondary forest raster for year ", year, " not found! Ensure it has been created first.")
  }
  
  secondaryforest <- raster(secondaryforest_path)
  
  # **List only "Forest Validation" raster files**
  raster_files <- list.files(input_folder, pattern = "Forest_Formation_.*\\.tif$", full.names = TRUE)
  
  # Sort rasters to maintain order
  raster_files <- sort(raster_files)
  
  cropped_rasters <- list()  # Store cropped raster objects
  
  for (raster_path in raster_files) {
    
    raw_raster <- raster(raster_path)
    
    # Ensure CRS matches the secondary forest raster (convert if needed)
    if (!compareCRS(raw_raster, secondaryforest)) {  
      raw_raster <- projectRaster(raw_raster, crs = crs(secondaryforest))
    }
    
    # Crop using the secondary forest extent
    cropped_raster <- crop(raw_raster, secondaryforest)
    
    # Extract filename
    filename_id <- tools::file_path_sans_ext(basename(raster_path))
    
    # Save cropped raster
    cropped_raster_path <- paste0(output_folder, "/", filename_id, "_cropped.tif")
    writeRaster(cropped_raster, cropped_raster_path, overwrite = TRUE)
    
    # Store for merging
    cropped_rasters <- append(cropped_rasters, list(cropped_raster))
    
    # Remove objects from memory
    rm(raw_raster, cropped_raster)
    gc()
  }
  
  # Merge all cropped rasters using raster::merge()
  if (length(cropped_rasters) > 0) {
    merged_forest_validation <- do.call(merge, cropped_rasters)
    
    # Save final merged raster
    merged_raster_path <- paste0(output_folder, "/forest_validation_", year, ".tif")
    writeRaster(merged_forest_validation, merged_raster_path, overwrite = TRUE)
    
    final_raster <- raster(merged_raster_path)
    assign(paste0("forest_validation_", year), final_raster, envir = .GlobalEnv)
    
    # Load and visualize
    mapview(final_raster, layer.name = paste("Forest Validation", year), na.color = "transparent")
  }
  
  # Clean up
  rm(cropped_rasters)
  gc()
}

# Function to extract raster values for each ALS year
process_raster_extractions <- function(df, secondary_forest_rasters, burn_frequency_rasters, time_since_fire_rasters, forest_validation_rasters) {
  
  # List of years to process
  years <- c(2018, 2021, 2023)
  
  # Split dataframe by ALS_year
  df_list <- split(df, df$ALS_year)
  
  # Initialize list to store processed dataframes
  processed_dfs <- list()
  
  for (year in years) {
    
    df_year <- df_list[[as.character(year)]]
    
    # Load corresponding raster layers
    sf_raster <- secondary_forest_rasters[[as.character(year)]]
    bf_raster <- burn_frequency_rasters[[as.character(year)]]
    tsf_raster <- time_since_fire_rasters[[as.character(year)]]
    fv_raster <- forest_validation_rasters[[as.character(year)]]
    
    # Extract values using `raster::extract()` and ensure numeric type
    df_year$secondary_age <- as.numeric(raster::extract(sf_raster, df_year, method = 'simple'))
    df_year$burn_frequency <- as.numeric(raster::extract(bf_raster, df_year, method = 'simple'))
    df_year$time_since_fire <- as.numeric(raster::extract(tsf_raster, df_year, method = 'simple'))
    df_year$forest_validation <- as.numeric(raster::extract(fv_raster, df_year, method = 'simple'))
    
    
    # Store processed dataframe
    processed_dfs[[as.character(year)]] <- df_year
  }
  
  # Merge all processed dataframes back together
  final_df <- bind_rows(processed_dfs)
  
  return(final_df)
}




# GEDI CLASSIFICATION 


# Function to calculate how many years since burn instead of year of occurrence
calculate_years_since_fire <- function(df) {
  df %>%
    mutate(
      years_since_fire = ALS_year - time_since_fire,
      years_since_fire = ifelse(time_since_fire == 0, NA, years_since_fire)
    )
}

# Function to classify GEDI pixels using secondary forest age, burn frequency,
# years since burned and forest validation layers
classify_forest_state <- function(df) {
  df %>%
    # Step 1: Create a forest type column (Primary = "P", Secondary = "S")
    mutate(
      forest_type = ifelse(secondary_age > 0, "S", "P") 
    ) %>%
    
    # Step 2: Assign Time Since Fire (TSF) - Ensure All Numeric
    mutate(
      TSF = case_when(
        forest_type == "P" & is.na(years_since_fire) ~ NA_real_,  # Numeric NA
        forest_type == "P" & !is.na(years_since_fire) ~ as.numeric(years_since_fire),  
        forest_type == "S" & burn_frequency == 0 ~ NA_real_,
        TRUE ~ as.numeric(secondary_age)
      )
    ) %>%
    
    # Step 3: Adjust Burn Frequency *ONLY* if Secondary Forest Age < (Years Since Fire - 2)
    mutate(
      burn_frequency1 = ifelse(forest_type == "S" & secondary_age < (years_since_fire - 5), 0, burn_frequency),
      burn_frequency1 = ifelse(is.na(burn_frequency1), 0, burn_frequency1),
    
    # If burn_frequency1 is now 0, set TSF to NA
    TSF = ifelse(burn_frequency1 == 0 & forest_type == "S", NA_real_, TSF)
    ) %>%
    
    # Step 4: Classify forest state using forest_type and burn frequency
    mutate(
      forest_state = case_when(
        burn_frequency1 == 0 & forest_type == "P" ~ "Intact",  
        burn_frequency1 == 0 & forest_type == "S" ~ "SU",  
        burn_frequency1 == 1 & forest_type == "P" ~ "PB1",
        burn_frequency1 == 2 & forest_type == "P" ~ "PB2",
        burn_frequency1 >= 3 & forest_type == "P" ~ "PB3+",
        burn_frequency1 == 1 & forest_type == "S" ~ "SB1",
        burn_frequency1 == 2 & forest_type == "S" ~ "SB2",
        burn_frequency1 >= 3 & forest_type == "S" ~ "SB3+",
        TRUE ~ "Unknown"  
      )
    ) %>%
    
    # Step 5: Remove primary forests where burn frequency is 1 or more and no longer has forest_validation == 1
    filter(!(forest_type == "P" & burn_frequency >= 1 & forest_validation == 0)) %>%
    
    # Step 6: Assign Age Category Based on TSF
    mutate(
      age_category = case_when(
        TSF <= 6 ~ "<7",
        TSF > 6 & TSF <= 15 ~ "7-15",
        TSF > 15 & TSF <= 25 ~ "15-25",
        TSF > 25 & TSF <= 39 ~ "25-40",
        TRUE ~ NA_character_  # Keep NA for invalid cases
      )
    )
}
  
# Function to remove additional columns from classification and project CRS

tidy_classification <- function(df, target_crs = NULL) {
  df <- df %>%
    # Tidy dataframe columns by removing unwanted columns
    select(-secondary_age, -years_since_fire, -forest_validation, -burn_frequency, -forest_type, -time_since_fire) %>%
    rename(burn_frequency = burn_frequency1)  # Rename column properly
  
  # Reproject the dataset for consistency
  if ("sf" %in% class(df) & !is.null(target_crs)) {
    df <- st_transform(df, target_crs)
  }
  
  return(df)
}

# Function to count forest_state occurrences in a dataset
count_forest_state <- function(df, dataset_name) {
  df <- st_drop_geometry(df)  # ✅ Convert to regular dataframe if it's sf
  df %>%
    count(forest_state) %>%
    mutate(dataset = dataset_name) %>%
    pivot_wider(names_from = forest_state, values_from = n, values_fill = list(n = 0))
}












# STATISTICS FUNCTIONS

# Compute the Lin's correlation concordance coefficient (CCC)
calculate_ccc <- function(data, rh_col, rhz_col, condition = NULL) {
  if (!is.null(condition)) {
    data <- data %>% filter(!!rlang::parse_expr(condition))
  }
  
  x <- data[[rh_col]]
  y <- data[[rhz_col]]
  ccc_result <- CCC(x, y, ci = "z-transform", conf.level = 0.95)
  return(paste(round(ccc_result$rho.c[1], 2)))
}

# Simplified function to compute Lin's CCC for canopy cover
# Function to calculate CCC with optional filtering condition
calculate_ccc_c <- function(data, rh_col, rhz_col, condition = NULL) {
  # If a condition is provided, filter the data
  if (!is.null(condition)) {
    data <- data %>% filter(!!rlang::parse_expr(condition))
  }
  
  # Extract the specified columns
  x <- data[[rh_col]]
  y <- data[[rhz_col]]
  
  # Calculate Lin's CCC
  ccc_result <- CCC(x, y, ci = "z-transform", conf.level = 0.95)
  
  # Return the CCC value
  return(paste(round(ccc_result$rho.c[1], 2)))
}



# Function to add a CCC result to the list
add_ccc_result <- function(condition, ccc_value, type) {
  ccc_results_list <<- append(ccc_results_list, list(data.frame(
    Condition = condition,
    Type = type,
    Lins_CCC = as.numeric(ccc_value)
  )))
}


# Function to calculate Pearson's r and test significance
calculate_pearsons_r <- function(data, rh_col, rhz_col, condition = NULL) {
  if (!is.null(condition)) {
    data <- data %>% filter(!!rlang::parse_expr(condition))
  }
  
  cor_test_result <- cor.test(data[[rh_col]], data[[rhz_col]], method = "pearson")
  
  # Extract Pearson's r and p-value
  r_value <- cor_test_result$estimate
  p_value <- cor_test_result$p.value
  
  list(r_value = r_value, p_value = p_value)
}




# Function to calculate RMSE
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

# Function to calculate Bias
calculate_bias <- function(actual, predicted) {
  mean(actual - predicted, na.rm = TRUE)
}

# Function to calculate linear model coefficient
calculate_coef <- function(data, rh_col, rhz_col, condition = NULL) {
  if (!is.null(condition)) {
    data <- data %>% filter(!!rlang::parse_expr(condition))
  }
  model <- lm(data[[rhz_col]] ~ data[[rh_col]], data = data)
  coef(model)[2]
}

# Function to calculate RMSE and Bias for given condition
calculate_stats <- function(data, rh_col, rhz_col, condition = NULL) {
  if (!is.null(condition)) {
    data <- data %>% filter(!!rlang::parse_expr(condition))
  }
  actual <- data[[rhz_col]]
  predicted <- data[[rh_col]]
  rmse <- calculate_rmse(actual, predicted)
  bias <- calculate_bias(actual, predicted)
  list(rmse = rmse, bias = bias)
}
# Function to handle 'burned' condition
handle_burned_condition <- function(data) {
  data %>% filter(grepl("Burned", forest_state, ignore.case = TRUE))
}





# Simple Function to calculate Pearson's r and p-value for canopy cover
calculate_pearsons_r_c <- function(data, rh_col, rhz_col, condition = NULL) {
  # If a condition is provided, filter the data
  if (!is.null(condition)) {
    data <- data %>% filter(!!rlang::parse_expr(condition))
  }
  
  # Perform Pearson's correlation test
  cor_test_result <- cor.test(data[[rh_col]], data[[rhz_col]], method = "pearson")
  
  # Extract Pearson's r and p-value
  r_value <- cor_test_result$estimate
  p_value <- cor_test_result$p.value
  
  # Return Pearson's r and p-value in a list
  list(r_value = r_value, p_value = p_value)
}






# Function to calculate Pearson's r for a given condition and rh-rhz pair
calculate_pearson <- function(data, condition, rh_col, rhz_col) {
  if (!is.null(condition) && condition != "All") {
    data <- data %>% filter(!!rlang::parse_expr(condition))
  }
  cor(data[[rh_col]], data[[rhz_col]], use = "complete.obs")
}









# Multinomial linear regression model


# Fit multinomial logistic regression
fit_nplot_mnlr <- function(control_var) {
  control_var <- enquo(control_var)
  # Create the formula using the symbol
  formula <- as.formula(paste("forest_state ~", quo_name(control_var)))
  mnlr <- nnet::multinom(formula, data = gpca)
  
  cli::cli_alert_info("{quo_name(control_var)} model AIC: {AIC(mnlr)}")
  
  # Predict probabilities using marginaleffects package
  preds <- as_tibble(marginaleffects::predictions(mnlr, newdata = "balanced", type = "probs"))
  
  # Plot the results
  preds |>
    ggplot() +
    aes(x = !!control_var, y = estimate, fill = group, group = group) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                color = NA,  # Remove gray outline
                alpha = 0.8   # Transparency for ribbons
    ) +
    scale_fill_manual(values = forest_state_colors, name = "Forest State") +  
    theme_linedraw() +
    theme_fancy() +
    scale_y_sqrt() +
    labs(
      x = quo_name(control_var),
      y = "Probability", 
      fill = "Forest State",  # Legend title update
      caption = glue::glue("AIC: {round(AIC(mnlr),1)}")  # Add AIC caption
    ) +
    theme(
      legend.position = "right"
    )
}





# VISUALISATIONS FUNCTIONS

# Function to compute density for smooth coloring
get_density <- function(x, y, n = 100) {
  dens <- kde2d(x, y, n = n)  # Compute 2D density with same resolution
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  density_values <- dens$z[cbind(ix, iy)]
  
  # Normalize density between 0 and 1
  return((density_values - min(density_values, na.rm = TRUE)) / 
           (max(density_values, na.rm = TRUE) - min(density_values, na.rm = TRUE)))
}

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Arial"),
      axis.text = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
      plot.title = element_text(
        size = 10,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 12, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      ),
      strip.background = element_blank(),  # Remove facet label background
      strip.text = element_text(size = 8, color = "black", face = "bold")  # Style facet label text
    )
}

#windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly

