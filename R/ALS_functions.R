
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
process_als <- function(las) 
{ if (is(las, "LAS"))
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


gedi2A_batch_download <- function(poly_folder_path, start_date, end_date, fgb_output_folder) {
  
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
        #beam, 
        year, solar_elevation, lat_lowestmode, lon_lowestmode,
        elev_highestreturn, elev_lowestmode, rh0, rh1, rh2, rh3, rh4, rh5, rh6, rh7, rh8, rh9, 
        rh10, rh11, rh12, rh13, rh14, rh15, rh16, rh17, rh18, rh19, rh20, rh21, rh22, rh23, rh24, 
        rh25, rh26, rh27, rh28, rh29, rh30, rh31, rh32, rh33, rh34, rh35, rh36, rh37, rh38, rh39, 
        rh40, rh41, rh42, rh43, rh44, rh45, rh46, rh47, rh48, rh49, rh50, rh51, rh52, rh53, rh54, 
        rh55, rh56, rh57, rh58, rh59, rh60, rh61, rh62, rh63, rh64, rh65, rh66, rh67, rh68, rh69, 
        rh70, rh71, rh72, rh73, rh74, rh75, rh76, rh77, rh78, rh79, rh80, rh81, rh82, rh83, rh84, 
        rh85, rh86, rh87, rh88, rh89, rh90, rh91, rh92, rh93, rh94, rh95, rh96, rh97, rh98, rh99, 
        rh100, sensitivity, shot_number, degrade_flag
      ) |>
      mutate(shot_number=as.character(shot_number)) |>
             #beam=as.character(beam)) 
    
      collect_gedi(gedi_find = gedi2a_search)
    
    # Add a new column with polygon ALS CRS for future reference
    gedi2a_sf <- gedi2a_sf %>%
      mutate(ALS_CRS = substr(basename(polygon_file), 7, 9))
    
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


#NEEDS WORK
gedi1B_batch_download <- function(poly_folder_path, start_date, end_date, fgb_output_folder) {
  
  # List all shapefiles in folder
  shapefiles <- list.files(poly_folder_path, pattern = "\\.shp$", full.names = TRUE)
  
  # Process each shapefile
  for (polygon_file in shapefiles) {
    # Read the polygon from the shapefile
    polygon <- st_read(polygon_file)
    
    # Display the polygon information for batch progress
    cat("Processing polygon:", polygon_file, "\n")
    
    # Search for 1B products within AOI
    gedi1b_search <- find_gedi(polygon,
                               gedi_product = "1B",
                               date_start = start_date,
                               date_end = end_date)
    
    # Download the search output; will not download if already cached
    gedi_1b_arr <- grab_gedi(gedi1b_search,
                             add_vars = list(
                               rx_energy = "rx_energy",
                               solar_elevation = "geolocation/solar_elevation") %>%
                               filter(degrade == 0))
    
    # Collect the data into a data frame
    gedi1b_sf <- collect_gedi(gedi_1b_arr, gedi1b_search)
    
    # select date_time, shot_number- point geometry fgb
    
    # Extracting all of the waveforms
    gedi1b_wvf <- extract_waveforms(gedi1b_sf)
    
    
    # save as .csv and make a new file of just shot number, point etc
    
    
    # Generate a unique output file name based on the shapefile name
    output_file <- file.path(fgb_output_folder, paste0(tools::file_path_sans_ext(basename(polygon_file)), "_1B.csv"))
    
    # Save the GEDI GeoDataFrame to the output file
    # sf::st_write(gedi1b_sf, output_file, delete_dsn = TRUE)
    
    # Save the GEDI data frame to the output file without spatial context (can join a metric to 2A table later)
    write.table(as.data.frame(gedi_1b_arr), output_file, sep = ",", row.names = FALSE)
    
  }
}

wvf_ggplot <- function(x, wf, z, .ylab = "Elevation (m)") {
  wf <- sym(wf)
  z <- sym(z)
  ggplot(x, aes(x = !!z, y = !!wf)) +
    geom_ribbon(aes(ymin = min(!!wf), ymax = !!wf),
                alpha = 0.6, fill = "#69d66975", colour = "grey30", lwd = 0.2
    ) +
    theme_light() +
    coord_flip() +
    labs(x = .ylab, y = "Waveform Amplitude")
}














# Filter andn reproject GEDI dataset to match ALS CRS
filter_reproj_GEDI <- function(data, als_crs_value, epsg_code) {
  # Filter the data based on ALS_CRS value
  filtered_data <- data %>%
    filter(ALS_CRS == als_crs_value)
  
  # Transform the coordinate reference system
  transformed_data <- st_transform(filtered_data, epsg_code)
  
  # Return the transformed data
  return(transformed_data)
}






# EXTRACTING METRCIS FROM ALS FUNCTIONS USING LidR


density_preds <- function(Z, percentiles = seq(0.1, 0.9, 0.1)) {
  z_range <- range(Z)
  z_range_diff <- z_range[[2]] - z_range[[1]]
  
  lapply(
    z_range_diff * percentiles,
    \(h) mean(Z > (h + z_range[[1]]))
  ) |>
    stats::setNames(paste0("d", (percentiles * 100))) |>
    list2DF()
}

height_preds <- function(Z, percentiles = c(seq(0.1, 0.9, 0.1), 0.25, 0.75, 0.95, 0.96, 0.97, 0.98, 0.99)) {
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
lidar_preds <- function(Z, ReturnNumber, min = 0, max = Inf) {
  density_pred_template <- lapply(
    paste0("d", seq(0.1, 0.9, 0.1) * 100),
    \(x) stats::setNames(data.frame(NA_real_), x)
  ) |>
    do.call(what = cbind)
  
  height_pred_template <- lapply(
    paste0("rhz", c(seq(0.1, 0.9, 0.1), 0.25, 0.75, 0.95, 0.96, 0.97, 0.98, 0.99) * 100),
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









#' Relative height metrics function
#' For use with lidR::plot_metrics or lidR::pixel_metrics functions. Assumes
#' the point cloud has been normalised to ground level.
#' @param Z Return height for each point
#' @param min,max The minimum and maximum valid Z value. Returns outside of
#' this range will be discarded before metric computation.
#' @return A `data.frame` with 100 columns, containing relative height metrics
#' at 1 percentile intervals.
#' @export
rh_preds <- function(Z, min = 0, max = Inf) {
  out <- lapply(
    paste0("rh", seq(0.01, 1, 0.01) * 100),
    \(x) stats::setNames(data.frame(NA_real_), x)
  ) |>
    do.call(what = cbind)
  include <- Z >= min & Z <= max
  
  if (sum(include) < 10) {
    return(out)
  }
  
  Z <- Z[include]
  Z <- Z[include]
  out[names(out)] <- height_preds(Z, percentiles = seq(0.01, 1, 0.01))
  return(out)
}


#' @rdname compute_pixel_metrics
#' @export
compute_plot_metrics <- function(
    las,
    func = lidar_preds(Z, ReturnNumber),
    geometry,
    ...,
    radius) {
  lidR::plot_metrics(las, func, geometry = geometry, ..., radius = radius)
}






























# Custom function to calculate canopy cover for circular plots within a polygonal footprint
calculate_canopy_cover <- function(las, footprint, cutoff) {
  # Create circular plots within the polygonal footprint
  plots <- plot_metrics(las, ~list(buffer_points(., radius = 12.5)), footprint)
  
  # Classify points based on first returns above cutoff
  las$vegetation <- las$Z > cutoff
  
  # Compute canopy cover for each circular plot
  canopy_cover <- plot_metrics(las, ~sum(vegetation) / point_density() * 100, plots)
  
  return(canopy_cover)
}

# Example usage:
# Assuming fin_catalog is your LAScatalog and footprints is a LAS object
cutoff <- 2  # adjust the cutoff value as needed
als_canopy_cover <- calculate_canopy_cover(fin_catalog, footprints, cutoff)














# STATISTICS FUNCTIONS

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



# VISUALISATIONS FUNCTIONS

# Create ggplot point graphs
#ggplot_point <- function(data, x_var, y_var, color_var, title, x_label, y_label) {
#  ggplot(data, aes_string(x = x_var, y = y_var, color = color_var)) +
#    geom_point() +
 #   labs(title = title, x = x_label, y = y_label) +
 #   theme_light()
#}
