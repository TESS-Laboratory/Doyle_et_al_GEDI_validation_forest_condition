
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


# GEDI functions


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
        beam, year, solar_elevation, lat_lowestmode, lon_lowestmode,
        elev_highestreturn, elev_lowestmode, rh0, rh5, rh10, rh15, rh20, rh25, rh30, rh35, rh40,
        rh45, rh50, rh55, rh60, rh65, rh70, rh75, rh80, rh85, rh90, rh95, rh97, rh98, rh99, rh100,
        sensitivity, shot_number, degrade_flag
      ) |>
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
    sf::st_write(gedi2a_sf, output_file, delete_dsn = TRUE)
    
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



