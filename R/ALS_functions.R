
# ALS FUNCTIONS

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


# Select classifications of ALS data and filter catalog for anomalous points
filter_als <- function(LAScatalog){
  # Filter for classification
  opt_select(LAScatalog) <- "xyzrnc"
  opt_filter(LAScatalog) <- "-drop_z_below 0"
  opt_filter(LAScatalog) <- "-drop_z_above 70"
  return(LAScatalog)
}


# Function to move files from source folder to destination folder (for LASCatalog)
move_files <- function(source_folder, destination_folder, file_extensions) {
  # List files in the source folder
  files <- list.files(source_folder, full.names = TRUE)
  
  # Filter files with the specific file extensions
  files_to_move <- files[str_detect(files, paste0("\\.", paste(file_extensions, collapse = "|"), "$"))]
  
  # Copy files to the destination folder
  file.copy(files_to_move, destination_folder, overwrite = TRUE)
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
