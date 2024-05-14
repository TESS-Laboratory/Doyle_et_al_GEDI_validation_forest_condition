# Test script for small piece of lidar and GEDI

# Individual piece

# Load catalog of ALS tiles (previously tiled by LAStools - look if can do this without LAStools)
catalog <- readLAScatalog('/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20S')

# Print and plot summary of ALS data
catalog
plot(catalog, mapview = TRUE, map.type = "Esri.WorldImagery")


# Validate the catalog dataset
las_check(catalog)


# Filter for duplicates
opt_output_files(catalog) <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20S/DAAC18_20Sdup_{ORIGINALFILENAME}"
dup_catalog <- catalog %>%
  filter_duplicates()


# Ground classification - Cloth Simulation Function (Zhang 2016)
opt_output_files(dup_catalog) <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20S/DAAC18_20Sgrnd_{ORIGINALFILENAME}"
grnd_catalog <- dup_catalog %>%
  classify_ground(algorithm = csf())


# Height normalization
opt_output_files(grnd_catalog) <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20S/DAAC18_20Snorm_{ORIGINALFILENAME}"
norm_catalog <- grnd_catalog %>%
  normalize_height(tin())



# Filtering for noise in catalog of data

fin_catalog <- readLAScatalog("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20S/final_catalog")


opt_select(fin_catalog) <- "xyzrnc"
opt_filter(fin_catalog) <- "-drop_z_below 0"
opt_filter(fin_catalog) <- "-drop_z_above 70"


# function version 

finfunctioncatalog <- process_alstest(catalog)

finfunctioncatalog <- filter_als(finfunctioncatalog)


#GEDI download


poly_folder_path <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20Spolys"
start_date <- "2019-01-01"
end_date <- "2020-12-31"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20Spolys"

gedi2A_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)

fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
GEDI2Atest <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI2A
GEDI2Atest <- distinct(GEDI2Atest, shot_number, .keep_all = TRUE)
sf::st_write(GEDI2Atest, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20Spolys/GEDI2Atest.fgb", delete_dsn = TRUE, overwrite = TRUE)

mapview(GEDI2Atest)



# final returns instead of first

plot(fin_catalog) + plot(GEDI2Atest, add = TRUE, col = "red")


GEDI2Atest <- st_transform(GEDI2Atest, "EPSG:31980")
st_crs(fin_catalog) <- 31980
st_crs(finfunctioncatalog) <- 31980




# Sort of works! Metric extraction (only works with step by step)
als_metrics <- plot_metrics(fin_catalog, .stdmetrics_z, GEDI2Atest, radius = 12.5)
als_metricsfunction <- plot_metrics(finfunctioncatalog, .stdmetrics_z, GEDI2Atest, radius = 12.5)




# TRYING NEW METHOD

catalog <- readLAScatalog("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20S")


# Function to filter duplicates in a lidR catalog
filter_duplicates <- function(catalog) {
  # Filter for duplicates
  opt_output_files(catalog) <- paste0(dirname(ctgpath(catalog)), "/dup_{ORIGINALFILENAME}")
  dup_catalog <- catalog %>%
    filter_duplicates()
  
  return(dup_catalog)
}





filter_duplicates <- function(catalog) {
  # Create new output directory within folder of the input LAScatalog
  first_file <- catalog@data[["filename"]][1]
  outdir <- file.path(dirname(first_file), "dupe_filter")
  
  # Create the output folder if it doesn't exist
  if (!dir.exists(outdir)) {
    dir.create(outdir)
  }
  
  # Set output files
  opt_output_files(catalog) <- paste0(outdir, "/dup_{ORIGINALFILENAME}")
  
  # Filter for duplicates
  dup_catalog <- catalog %>%
    filter_duplicates()
  
  return(dup_catalog)
}



# Function to apply ground classification to a lidR catalog
ground_classification <- function(filtered_catalog) {
  # Ground classification - Cloth Simulation Function (Zhang 2016)
  opt_output_files(filtered_catalog) <- paste0(dirname(ctgpath(filtered_catalog)), "/grnd_{ORIGINALFILENAME}")
  grnd_catalog <- filtered_catalog %>%
    classify_ground(algorithm = csf())
  
  return(grnd_catalog)
}

# Function to apply height normalization to a lidR catalog
height_normalization <- function(grnd_catalog) {
  # Height normalization
  opt_output_files(grnd_catalog) <- paste0(dirname(ctgpath(grnd_catalog)), "/norm_{ORIGINALFILENAME}")
  norm_catalog <- grnd_catalog %>%
    normalize_height()
  
  return(norm_catalog)
}



filtered_catalog <- filter_duplicates(catalog)

# Pass the filtered catalog obtained from the previous step
grnd_catalog <- ground_classification(filtered_catalog)

# Pass the ground classified catalog obtained from the previous step
norm_catalog <- height_normalization(grnd_catalog)


processed_catalog <- process_als(catalog)




# Load folder catalog of ALS tiles to retile (as control for ALS origins varying)
raw_catalog <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_20S')

retile_DAAC18_20S <- retile_catalog_pref(raw_catalog)

# Load the newly retiled folder of ALS tiles
catalog <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_20S/retile')

# Validate the catalog dataset
las_check(catalog)


# Filter for duplicates (writes new file of filtered tiles)
opt_output_files(catalog) <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20S/DAAC18_20Sdup_{ORIGINALFILENAME}"
dup_catalog <- catalog %>%
  filter_duplicates()


# Ground classification - Cloth Simulation Function (Zhang 2016) (writes new file of filtered/classified tiles)
opt_output_files(dup_catalog) <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20S/DAAC18_20Sgrnd_{ORIGINALFILENAME}"
grnd_catalog <- dup_catalog %>%
  classify_ground(algorithm = csf())


# Height normalization (writes new file of filtered/classified/normalised tiles)
opt_output_files(grnd_catalog) <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/DAAC18_20S/DAAC18_20Snorm_{ORIGINALFILENAME}"
norm_catalog <- grnd_catalog %>%
  normalize_height(tin())


# running currently for DAAC20





















