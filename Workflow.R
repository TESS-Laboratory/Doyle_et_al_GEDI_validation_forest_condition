## Script for the analysis of paper 'Doyle et al...' for the validation of the use of GEDI data 
## in degraded Amazon rainforest

# Reload all packages and functions
reload <- function() {
  # Load all packages for the project
  source("Packages.R")
  
  # Read all functions from the R directory
  function_files <- list.files("R", pattern = ".R$", full.names = TRUE)
  purrr::walk(function_files, source)

}
reload()

# Edit mapview package display options
mapviewOptions(platform = "leafgl")
options(mapviewMaxPixels = 1000000000)



# ----- Pre-Process ALS DONE -------- 
# ALS data is sourced from Sustainable Landscapes Brazil project (2018), downloaded into coordinate reference system (CRS)
# regions with the format 'DAAC_year_CRS'. Data is also sourced from Permian Global in 2023 for Rio Cautario. 
# ALS data is combined but catalogs are separated by their CRS.

# Load and retile (DAAC) catalog for consistency between various ALS sources

DAAC18_19S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_19S')
CAUT23_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/CAUT23_20S')
DAAC1821_21S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC1821_21S')

retile_DAAC18_19S <- retile_catalog_pref(DAAC18_19S)
retile_CAUT23_20S <- retile_catalog_pref(CAUT23_20S)
retile_DAAC1821_21S <- retile_catalog_pref(DAAC1821_21S)

# Check new catalog
las_check(retile_DAAC18_19S)
plot(retile_DAAC18_19S, mapview = TRUE, map.type = "Esri.WorldImagery")

# Process the retiled ALS

DAAC18_19S_norm <- process_als(retile_DAAC18_19S)
CAUT23_20S_norm <- process_als(retile_CAUT23_20S)
DAAC1821_21S_norm <- process_als(retile_DAAC1821_21S)

las_check(DAAC1821_19S_norm)

# Filter the data for anomalous results

DAAC18_19Sfinal <- filter_als(DAAC18_19S_norm)
CAUT23_20Sfinal <- filter_als(DAAC18_20S_norm)
DAAC1821_21Sfinal <- filter_als(DAAC1821_21S_norm)

# Set CRS of new catalog tiles to specified UTM

st_crs(DAAC18_19Sfinal) <- 32719
st_crs(CAUT23_20Sfinal) <- 32720
st_crs(DAAC1821_21Sfinal) <- 32721

# Create DTM for ability to determine extent of the .laz regions for GEDI overlap

dtm_DAAC18_19S <- rasterize_terrain(DAAC18_19Sfinal, 2, tin(), pkg = "terra")
dtm_CAUT23_20S <- rasterize_terrain(CAUT23_20Sfinal, 2, tin(), pkg = "terra")
dtm_DAAC1821_21S <- rasterize_terrain(DAAC1821_21Sfinal, 2, tin(), pkg = "terra")

writeRaster(dtm_DAAC18_19S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/dtm_DAAC18_19S.tif", overwrite=TRUE)
writeRaster(dtm_CAUT23_20S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/dtm_CAUT23_20S.tif", overwrite=TRUE)
writeRaster(dtm_DAAC1821_21S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/dtm_DAAC1821_21S.tif", overwrite=TRUE)

# Polygon files for the ALS extents can now be created in QGIS to use for GEDI download
# Tidy the environment
rm(DAAC18_19S, CAUT23_20S, DAAC1821_21S, retile_DAAC18_19S, retile_CAUT23_20S, 
   retile_DAAC1821_21S, DAAC18_19S_norm, CAUT23_20S_norm, DAAC1821_21S_norm, 
   dtm_DAAC18_19S, dtm_CAUT23_20S, dtm_DAAC1821_21S)


# ----------- GEDI download DONE ----------------

# GEDI files are downloaded to correspond with ALS data extent e.g. DAAC ALS 2018 = 2019-01-01 to 2019-12-31
# DAAC 2021 = 2020-06-01 to 2022-06-01 and CAUTARIO 2023 = 2022-01-01 to 2024-06-01 (year gap in 23-24 collection)
# GEDI polygon file names are in format "DAAC1821S_3.shp", with function extracting ALS and including as column in final file

# Download GEDI2A files for all polygon shapefiles in a folder, creating output geodataframe for each AOI

# Parameters for three separate folders of ALS polygon extent regions, 
# outputting to the same folder with GEDI batch function
params <- list(
  start_date = c("2019-01-01", "2020-06-01", "2022-01-01"),
  end_date = c("2019-12-31", "2022-06-01", "2024-06-01"),
  poly_folder_path = c("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/DAAC_polygons",
                       "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/DAAC_2021_polygons",
                       "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/CAUTARIO_polygons"),
  fgb_output_folder = c("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2A"))

# Function for GEDI batch download, using pmap to call the list of parameters
pmap(params, gedi2A_batch_download)

# Reading all of the 2A output .fgb files into one geodatabase

fgb_output_folder = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2A"
fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI2A <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI2A
allGEDI2A <- distinct(allGEDI2A, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI2A, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A.fgb")
mapview(allGEDI2A)




# Download GEDI2B files for all polygon shapefiles in a folder,creating output geodataframe for each AOI

# Parameters for three separate folders of ALS polygon extent regions, 
# outputting to the same folder with GEDI batch function
params <- list(
  start_date = c("2019-01-01", "2020-06-01", "2022-01-01"),
  end_date = c("2019-12-31", "2022-06-01", "2024-06-01"),
  poly_folder_path = c("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/DAAC_polygons",
                       "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/DAAC_2021_polygons",
                       "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/CAUTARIO_polygons"),
  fgb_output_folder = c("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2B"))

# Function for GEDI batch download, using pmap to call the list of parameters
pmap(params, gedi2B_batch_download)


# Reading all of the 2B output .fgb files into one geodatabase

fgb_output_folder = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2B"
fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI2B <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI2B
allGEDI2B <- distinct(allGEDI2B, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI2B, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2B.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2B <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2B.fgb")
mapview(allGEDI2B)




# Download GEDI4A files for all polygon shapefiles in a folder,creating output geodataframe for each AOI

# Parameters for three separate folders of ALS polygon extent regions, 
# outputting to the same folder with GEDI batch function
params <- list(
  start_date = c("2019-01-01", "2020-06-01", "2022-01-01"),
  end_date = c("2019-12-31", "2022-06-01", "2024-06-01"),
  poly_folder_path = c("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/DAAC_polygons",
                       "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/DAAC_2021_polygons",
                       "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/CAUTARIO_polygons"),
  fgb_output_folder = c("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI4A"))

# Function for GEDI batch download, using pmap to call the list of parameters
pmap(params, gedi4A_batch_download)

# Reading all of the 4A output .fgb files into one geodatabase

fgb_output_folder = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI4A"
fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI4A <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI4A
allGEDI4A<- distinct(allGEDI4A, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI4A, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI4A.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI4A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI4A.fgb")
mapview(allGEDI4A)



# Merge the 2A, 2B and 4A files into comprehensive dataframes

# Check the CRS is the same for all three GEDI datasets
allGEDI2B <- st_transform(allGEDI2B, st_crs(allGEDI2A))
allGEDI4A <- st_transform(allGEDI4A, st_crs(allGEDI2A))

# Remove geometries to join data by shot_number
allGEDI2A_no_geom <- st_set_geometry(allGEDI2A, NULL)
allGEDI2B_no_geom <- st_set_geometry(allGEDI2B, NULL)
allGEDI4A_no_geom <- st_set_geometry(allGEDI4A, NULL)

# Merge datasets based on shot_number to create two datasets: 2A/2B and additional 4A
merged2AB <- allGEDI2A_no_geom %>%
  left_join(allGEDI2B_no_geom, by = "shot_number") 

merged2AB4A <- merged2AB %>%
  left_join(allGEDI4A_no_geom, by = "shot_number")



# Clean data before remerging
# Remove rows with any NA values 
cleaned2AB <- drop_na(merged2AB)
cleaned2AB4A <- drop_na(merged2AB4A)

# Keep only the columns from allGEDI2B and the non-duplicated columns from the other datasets
columns_to_keep <- setdiff(names(cleaned2AB), names(allGEDI2A_no_geom))
columns_to_keep <- columns_to_keep[!grepl("\\.y$", columns_to_keep)]
columns_to_keep <- columns_to_keep[!grepl("\\.x$", columns_to_keep)]

cleaned2AB <- cleaned2AB %>%
  select(shot_number, all_of(columns_to_keep), ends_with(".x")) %>%
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))

# Keep only the columns from allGEDI4A and the non-duplicated columns from the other datasets
columns_to_keep <- setdiff(names(cleaned2AB4A), names(allGEDI2A_no_geom))
columns_to_keep <- columns_to_keep[!grepl("\\.y$", columns_to_keep)]
columns_to_keep <- columns_to_keep[!grepl("\\.x$", columns_to_keep)]

cleaned2AB4A <- cleaned2AB4A %>%
  select(shot_number, all_of(columns_to_keep), ends_with(".x")) %>%
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x")) %>%
  select(shot_number, all_of(columns_to_keep), ends_with(".x"))


# Merge the geometries back from `allGEDI2A`
allGEDI2AB <- allGEDI2A %>%
  filter(shot_number %in% cleaned2AB$shot_number) %>%
  left_join(cleaned2AB, by = "shot_number") %>%
  select(-ends_with(".y")) %>%
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))
  
allGEDI <- allGEDI2AB %>%
  filter(shot_number %in% cleaned2AB4A$shot_number) %>%
  left_join(cleaned2AB4A, by = "shot_number") %>%
  select(-year, -degrade_flag) %>%
  select(-ends_with(".x")) %>%
  rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))

sf::st_write(allGEDI2AB, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2AB <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb")
#allGEDI <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI.fgb")

# Tidy the environment
rm(allGEDI2A, allGEDI2B, allGEDI4A, allGEDI2A_no_geom, allGEDI2B_no_geom, 
   allGEDI4A_no_geom, cleaned2AB, cleaned2AB4A, merged2AB, merged2AB4A, fgb_list, params)



# ------ GEDI rh/ waveform regressions CHECK INTEGRATION ---------

# Summarise relative height rh0-100 metrics with linear regression model
# Outputs intercept, slope and variance of the 2A relative height profile

# Select only relative height data and unique identifier, transforming dataset
GEDI2AB_trans <- allGEDI2AB %>%
  as.data.frame() %>%
  select(shot_number, starts_with("rh"))

# Simple summary stats of the rh profile

summary_df <- apply(GEDI2AB_trans, 1, wv_summary_stats)

# Convert to a data frame and set column names
summary_df <- t(data.frame(summary_df))
summary_df <- as.data.frame(summary_df, stringsAsFactors = FALSE)
colnames(summary_df) <- c("shot_number", "rh_mean", "rh_sd", "rh_max", 
                          "rh_min", "rh_skew", "rh_kurt")

summary_df <- summary_df %>%
  mutate(rh_mean = as.numeric(rh_mean),
         rh_sd = as.numeric(rh_sd),
         rh_max = as.numeric(rh_max),
         rh_min = as.numeric(rh_min),
         rh_skew = as.numeric(rh_skew),
         rh_kurt = as.numeric(rh_kurt))


# Regression function to rh waveforms
result_df <- apply(GEDI2AB_trans, 1, rh_linear_regression)

# Convert the result to a dataframe and set column names
result_df <- t(data.frame(result_df))
result_df  <- as.data.frame(result_df, stringsAsFactors = FALSE)
colnames(result_df) <- c("shot_number", "Rh_intercept", "Rh_slope", "Rh_variance")

result_df <- result_df %>%
  mutate(Rh_intercept = as.numeric(Rh_intercept),
         Rh_slope = as.numeric(Rh_slope),
         Rh_variance = as.numeric(Rh_variance))

# Remerge results with original GEDI2A dataframe and name GEDI regressions

allGEDI2AB_reg <- left_join(allGEDI2AB, summary_df, by = "shot_number")
allGEDI2AB_reg <- left_join(allGEDI2AB_reg, result_df, by = "shot_number")

sf::st_write(allGEDI2AB_reg, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2AB_reg <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb")

# Tidy the environment
rm(GEDI2AB_trans, summary_df, result_df, maxamp_dt, maxamp_results, shot_numbers)






# Summarise cumulative energy proxy amplitude metrics with linear regression model
# Outputs intercept, slope and variance of the 2A waveform stats

# Select only amp data and unique identifier, transforming datbase
allGEDI2A_trans_amp <- allGEDI2A_cum %>%
  select(shot_number, interval, amp) %>%         
  pivot_wider(
    names_from = interval,                       
    names_prefix = "amp",                       
    values_from = amp 
  ) %>%
  distinct(shot_number, .keep_all = TRUE) 

# Apply regression function to cumulative waveforms
result_df_amp <- apply(allGEDI2A_trans_amp, 1, rh_linear_regression)

# Convert the result to a dataframe and set column names
result_df_amp <- t(data.frame(result_df_amp))
result_df_amp  <- as.data.frame(result_df_amp, stringsAsFactors = FALSE)
colnames(result_df_amp) <- c("shot_number", "W_intercept", "W_slope", "W_variance")

result_df_amp <- result_df_amp %>%
  mutate(W_intercept = as.numeric(W_intercept),
         W_slope = as.numeric(W_slope),
         W_variance = as.numeric(W_variance))


# Using waveform lidar package preferred format for additional summary parameters
# Convert data.frame to data.table for easier manipulation

GEDI2AB_trans_amp <- as.data.table(GEDI2AB_trans_amp)

# Separate the shot_number from the waveform data, removing shot column
shot_numbers <- GEDI2AB_trans_amp$shot_number
waveform_data <- GEDI2AB_trans_amp[, -1, with = FALSE] 

# Add an index column at the beginning ensuring the index is the first column
waveform_data[, index := .I]
setcolorder(waveform_data, c("index", setdiff(names(waveform_data), "index")))

# Apply lpeak
lpeak_results <- apply(waveform_data, 1, safe_lpeak)
lpeak_results <- lapply(lpeak_results, function(x) {
  if (!is.null(x$peaks)) {
    # Summarize by counting the number of TRUE values
    return(data.frame(n_peaks = sum(x$peaks, na.rm = TRUE)))
  } else {
    return(data.frame(n_peaks = NA))
  }
})
lpeak_dt <- rbindlist(lpeak_results, fill = TRUE)

# Apply maxamp
maxamp_results <- apply(waveform_data, 1, safe_maxamp)
maxamp_dt <- rbindlist(maxamp_results, fill = TRUE)

# Reattach shot numbers
waveform_results <- cbind(shot_number = shot_numbers, lpeak_dt, maxamp_dt)
waveform_results  <- as.data.frame(waveform_results)

# Remerge results with original GEDI2A dataframe and name GEDI regressions

allGEDI2A_amp <- left_join(allGEDI2A, result_df_amp, by = "shot_number")
allGEDI2A_amp <- left_join(allGEDI2A_amp, waveform_results, by = "shot_number")
allGEDI2A_amp <- left_join(allGEDI2A_amp, allGEDI2A_trans_amp, by = "shot_number")


allGEDI2A_amp <- allGEDI2A_amp %>%
  select(shot_number, W_intercept, W_slope, W_variance,
         max_amp, n_peaks, starts_with("amp"), matches("^rh([0-9]|[1-9][0-9]|100)$")) %>%
  select(-amp0)


sf::st_write(allGEDI2A_amp, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_amp.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2A_amp <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_amp.fgb")

rm(result_df_amp, shot_numbers, waveform_data, lpeak_dt, lpeak_results,
   maxamp_results, maxamp_dt, waveform_results)


### INTEGRATE DATAFRAMES HERE
# ALL OF THE FOLLOWING ALLGEDI2AB_REG WILL HAVE THE OLD G_INTERCEPT UP UNTIL THE PCA 

# ------ Extracting ALS metrics within GEDI footprints DONE------

# As the ALS data spans different parts of the Amazon rainforest, they are separated in folders by their
# CRS. Each ALS folder must therefore extract data from the GEDI footprints separately before being merged.

# Reload/filter and set CRS of final ALS catalogs if no longer in environment

DAAC18_19Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_19S/final_norm')
CAUT23_20Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/CAUT23_20S/final_norm')
DAAC1821_21Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC1821_21S/final_norm')

DAAC18_19Sfinal <- filter_als(DAAC18_19Sfinal)
CAUT23_20Sfinal <- filter_als(CAUT23_20Sfinal)
DAAC1821_21Sfinal <- filter_als(DAAC1821_21Sfinal)

st_crs(DAAC18_19Sfinal) <- 32719
st_crs(CAUT23_20Sfinal) <- 32720
st_crs(DAAC1821_21Sfinal) <- 32721


# Filter and reproject the GEDI dataframe to match each CRS catalog

allGEDI2AB_reg_19S <- filter_reproj_GEDI(allGEDI2AB_reg, '19S', 'EPSG:32719')
allGEDI2AB_reg_20S <- filter_reproj_GEDI(allGEDI2AB_reg, '20S', 'EPSG:32720')
allGEDI2AB_reg_21S <- filter_reproj_GEDI(allGEDI2AB_reg, '21S', 'EPSG:32721')

st_crs(allGEDI2AB_reg_19S) #, allGEDI2AB_reg_20S, allGEDI2AB_reg_21S)


# Extracting metrics of ALS within GEDI footprints in the same CRS
# Converting metrics to CRS (WGS84) associated with GEDI data to extract
# Write to file - large datasets

DAAC18_19Smetrics <- plot_metrics(DAAC18_19Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_reg_19S, radius = 12.5)
DAAC18_19Smetrics <- st_transform(DAAC18_19Smetrics , "EPSG:32643")
sf::st_write(DAAC18_19Smetrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/DAAC18_19Smetrics.fgb", delete_dsn = TRUE, overwrite = TRUE)

CAUT23_20Smetrics <- plot_metrics(CAUT23_20Sfinal1, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_reg_20S, radius = 12.5)
CAUT23_20Smetrics <- st_transform(CAUT23_20Smetrics1, "EPSG:32643")
sf::st_write(CAUT23_20Smetrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/CAUT23_20Smetrics.fgb", delete_dsn = TRUE, overwrite = TRUE)

DAAC1821_21Smetrics <- plot_metrics(DAAC1821_21Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_reg_21S, radius = 12.5)
DAAC1821_21Smetrics <- st_transform(DAAC1821_21Smetrics, "EPSG:32643")
sf::st_write(DAAC1821_21Smetrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/DAAC1821_21Smetrics.fgb", delete_dsn = TRUE, overwrite = TRUE)


# Remove duplicated rows
DAAC18_19Smetrics <- distinct(DAAC18_19Smetrics)
CAUT23_20Smetrics <- distinct(CAUT23_20Smetrics)
DAAC1821_21Smetrics <- distinct(DAAC1821_21Smetrics)

# Assuming columns are in the same order and just have different names
colnames(CAUT23_20Smetrics) <- colnames(DAAC18_19Smetrics)
colnames(DAAC1821_21Smetrics) <- colnames(DAAC18_19Smetrics)

# Combine the dataframes into one dataframe using bind_rows
merged_df <- bind_rows(DAAC18_19Smetrics, 
                       CAUT23_20Smetrics,
                       DAAC1821_21Smetrics)

# Remove rows with NAs in the 'rhz' column and filter for required columns

allGEDI2AB_ALS <- merged_df %>%
  filter(!is.na(rhz95)) %>%
  select(year, solar_elevation, elev_highestreturn, elev_lowestmode, rh0, rh5, rh10, rh15,
         rh20, rh25, rh30, rh35, rh40, rh45, rh50, rh55, rh60, rh65, rh70, rh75, rh80, rh85,
         rh90, rh95, rh96, rh97, rh98, rh99, rh100, sensitivity, shot_number, degrade_flag, ALS_CRS, l2b_quality_flag, cover,
         pai, fhd_normal, pgap_theta, pgap_theta_error, omega, modis_treecover, rh_mean,
         rh_sd, rh_max, rh_min, rh_skew, rh_kurt, G_intercept, G_slope, G_variance, max_amp,
         geometry, rhz5, rhz10, rhz15, rhz20, rhz25, rhz30, rhz35, rhz40, rhz45, rhz50, rhz55,
         rhz60, rhz65, rhz70, rhz75, rhz80, rhz85, rhz90, rhz95, rhz96, rhz97, rhz98, rhz99,
         max, cancov, z_kurt, z_skew)



sf::st_write(allGEDI2AB_ALS, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb", delete_dsn = TRUE, overwrite = TRUE)
#allGEDI2AB_ALS <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb")


# Tidy the environment
rm(merged_df, DAAC18_19Smetrics, DAAC1821_21Smetrics, CAUT23_20Smetrics, allGEDI2AB_reg_19S, 
    allGEDI2AB_reg_20S, allGEDI2AB_reg_21S, DAAC18_19Sfinal, DAAC1821_21Sfinal, CAUT23_20Sfinal)

# ------- Forest spectral classification DONE---------

# (1) Silva et al (2020) secondary forest of Brazil maps

# Set directories and file names
data_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Secondary_forest"
output_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/"
zip_file <- "secondary_forest_age_v2_2018.zip"
raster_file1 <- "secondary_forest_age_v2_2018-0000000000-0000000000.tif"
raster_file2 <- "secondary_forest_age_v2_2018-0000000000-0000065536.tif"
raster_file3 <- "secondary_forest_age_v2_2018-0000065536-0000065536.tif"

# Create directories if they don't exist
dir.create(data_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Download and unzip file -  This generates 8 .tif files. The file needed for the Rio Cautario ALS
# is '0000000000-0000000000', and files for DAAC ALS are 0000000000-0000065536 and 0000065536-0000065536

download.file('https://zenodo.org/record/3928660/files/secondary_forest_age_v2_2018.zip?download=1', 
              paste0(data_folder, "/", zip_file))
unzip(paste0(data_folder, "/", zip_file), exdir = data_folder)

# Read secondary forest rasters and polygons of the extent of ALS data (created in QGIS)
secondaryforestraw1 <- raster(paste0(data_folder, "/", raster_file1))
secondaryforestraw2 <- raster(paste0(data_folder, "/", raster_file2))
secondaryforestraw3 <- raster(paste0(data_folder, "/", raster_file3))
secondary_forest_west <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Secondary_forest/secondary_polygon_west.shp")
secondary_forest_east <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Secondary_forest/secondary_polygon_east.shp")

# Crop raster
secondaryforestwest <- crop(secondaryforestraw1, secondary_forest_west)
secondaryforesteast <- crop(secondaryforestraw2, secondary_forest_east)
secondaryforestsouth <- crop(secondaryforestraw3, secondary_forest_east)

mapview(secondaryforesteast) + mapview(secondaryforestwest) + mapview(secondaryforestsouth)

# Merge rasters
raster_list <- list(secondaryforestwest, secondaryforesteast, secondaryforestsouth)
merged_secondary <- do.call(merge, raster_list)
writeRaster(merged_secondary, paste0(output_folder, "/", "secondaryforest.tif"), overwrite=TRUE)
secondaryforest <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/secondaryforest.tif")


# Reclassify raster
secondaryforest[secondaryforest == 0] <- NA

# Add 6 years to the raster values to age secondary forest layer from 2018 to 2024 
# (when original layer was previously modelled to)

secondaryforestplus6 <- secondaryforest + 6

# Write raster
writeRaster(secondaryforestplus6, paste0(output_folder, "/", "secondaryforest2024.tif"), overwrite=TRUE)
#secondaryforest2024 <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/secondaryforest2024.tif")

mapview(secondaryforest2024, layer.name = 'Secondary forest age', na.color="transparent")


# Define spatial reference
#https://spatialreference.org/ref/epsg/32720/
sr <- "+proj=utm +zone=20 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Reproject raster
#secondaryforest2024 <- projectRaster(secondaryforest2024, crs = sr)


# (2) Burn frequency data

# Mapbiomas fire frequency layer downloaded from Google Earth Engine toolkit below (version 3.0)
# for states Rondonia, Amazonas, Para and Mato Grosso
# git clone https://earthengine.googlesource.com/users/mapbiomas/user-toolkit

MAPBIOMAS_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Fire_data"
output_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Fire_data"

# List .tif  files and create empty list to store raster objects
Burned_files <- list.files(path = MAPBIOMAS_folder, pattern = "\\.tif$", full.names = TRUE)
Burned_list <- list()

# Read each raster file and store them in the list
# MapBiomas files for Rio Cautario (2023) and DAAC areas (2018/2021) - year corresponds with ALS flights
for (file in burned_files) {
  raster_obj <- raster(file)
  Burned_list[[length(Burned_list) + 1]] <- raster_obj
}

# Merge rasters
merged_burned <- do.call(merge, Burned_list)
#merged_Burned <- projectRaster(merged_Burned, crs = sr)

# Crop raster
burnedforest <- crop(merged_Burned, secondaryforest2024)

writeRaster(Burnedforest, paste0(output_folder, "/", "MAPBIOMASfire.tif"), overwrite=TRUE)
burnedforest <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Fire_data/MAPBIOMASfire.tif")


# (3) Validation layer

# Validate if forest extent is still classified as forest after burn events in Cautario:
# Logging events do not occur in managed Extractive Reserve but fire events
# between 2019 - 2023 need to be checked using MapBiomas LULC classification data set post 2019

# Importing MapBiomas land classification layer for Rondonia for 2022 (will update to 2023 when possible)
forestclassmapbio <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Cautario_validation/mapbiomas-amazon-collection-50-rondonia-2022.tif")

# Reclassifying so forest = 1 and non forest = 0
rcl = matrix(c(0,0,0,1,6,1,7,33,0), ncol = 3, byrow = TRUE)
forestclass = reclassify(forestclassmapbio, rcl)

writeRaster(forestclass, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/forestclass.tif", format="GTiff", overwrite=TRUE)
forestclass <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/forestclass.tif")


# Tidy environment

rm(secondaryforestraw1, secondaryforestraw2, secondaryforestraw3, secondary_forest_west,
   secondary_forest_east, secondaryforest, secondaryforestplus6, Burned_list, forestclassmapbio, raster_obj,
   merged_Burned, rcl)


# ----------- GEDI classification extraction done--------------


# Extract values for forest age, burn frequency #and ALS extent#, binding new columns to GEDI data frame

forest_age <- raster::extract(secondaryforest2024, allGEDI2AB_ALS, method='simple')
allGEDI2AB_ALS <- cbind(allGEDI2AB_ALS, forest_age)

burn_freq <- terra::extract(burnedforest, allGEDI2AB_ALS, method='simple')
allGEDI2AB_ALS <- cbind(allGEDI2AB_ALS, burn_freq)

validation <- terra::extract(forestclass, allGEDI2AB_ALS, method='simple')
allGEDI2AB_ALS <- cbind(allGEDI2AB_ALS, validation)



# Segregate the secondary/ Intact forest samples of GEDI footprints

# Edit original allGEDI2AB_ALS file to have a numeric value for ageless Intact forest (n/a)
allGEDI2AB_ALS_aged <- allGEDI2AB_ALS %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age))

# Filter for Intact forest only (values of 99)
allGEDI2AB_ALS_intact <- filter(allGEDI2AB_ALS_aged, forest_age>90)

# Just degraded forest samples (classified with age by secondary forest raster)
allGEDI2AB_ALS_sec <- filter(allGEDI2AB_ALS_aged, forest_age<90)

# Mapview check of data layers
mapview(allGEDI2AB_ALS_sec, zcol = "forest_age") + mapview(secondaryforest2024) + mapview(allGEDI2AB_ALS_intact)



# To create gradient of degradation from intact to degraded/ recovering forest
# Samples spread across the Amazon sites with various soil/ distance to river/ distance to degradation

# Randomly sample 200 GEDI footprints (~20% of sample size) 
# that have not been burned prior to the secondary forest date range
# and create column highlighting them as = 1

allGEDI2AB_ALS_intact <- allGEDI2AB_ALS_intact %>%
  filter(burn_freq == 0) %>%
  filter(cover > 0.7) %>%
  mutate(intact_sample = as.integer(row_number() %in% sample(n(), size = 200)))

# Filter just the intact GEDI footprint samples
allGEDI2AB_ALS_intact_sample <- allGEDI2AB_ALS_intact %>%
  filter(intact_sample==1) 

hist(allGEDI2AB_ALS_intact_sample$rh99)

mapview(allGEDI2AB_ALS_intact_sample)


# Merge the secondary forest and ALS extent GEDI footprints data frame with new random intact samples
# Remove the data entries for Rondonia that were classified as non forest in validation dataset
# Clean dataset and extract degradation values into classified columns using process_GEDI_degradation function
# For complete gradient of GEDI samples across the site

# allGEDI2AB_ALS_intact_sample has an extra column to contend with when binding
# so will add to allGEDI2AB_ALS_sec with NA values
intact_sample <- setdiff(names(allGEDI2AB_ALS_intact_sample), names(allGEDI2AB_ALS_sec))
allGEDI2AB_ALS_sec[[intact_sample]] <- NA

# Now bind the two data frames

GEDI2AB <- rbind(allGEDI2AB_ALS_sec, allGEDI2AB_ALS_intact_sample)

GEDI2AB <- GEDI2AB %>%
  filter(validation != 0 | is.na(validation)) %>%
  filter(rh100 <= 50 | intact_sample == 1) %>%  #filter for anomalous results in degraded forest height (birdshot)
  select(-intact_sample) %>%
  process_GEDI_degradation()


sf::st_write(GEDI2AB, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2AB.fgb", delete_dsn = TRUE, overwrite = TRUE)
# GEDI2AB <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2AB.fgb")

mapview(secondaryforest2024) + mapview(Burnedforest) + mapview(GEDI2AB, zcol = "Degradation")


# Process original allGEDI2AB_ALS file and allGEDI4A to be aged/ classified for visuals also

allGEDI2AB_ALS <- allGEDI2AB_ALS_aged %>%
  process_GEDI_degradation()

# Load in 4A files to make comprehensive GEDI data frame also (sample size reduced however)
allGEDI4A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI4A.fgb")

# Remove geometry to join data by shot_number
allGEDI4A_no_geom <- st_set_geometry(allGEDI4A, NULL)

# Merge datasets based on shot_number to have GEDI4A carbon metrics also
GEDI2AB4A <- GEDI2AB %>%
  left_join(allGEDI4A_no_geom, by = "shot_number")

GEDI2AB4A <- GEDI2AB4A %>% 
  drop_na(agbd)


sf::st_write(allGEDI2AB_ALS, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(GEDI2AB4A, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2AB4A.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2AB_ALS <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb")
# GEDI2AB4A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2AB4A.fgb")


# Overwrite final datasets :
#                  allGEDI2AB_ALS (GEDI2A with regression and ALS data), 
#                  GEDI2AB (forest degradation gradient GEDI2A with regression and ALS), 
#                  GEDI2AB4A (forest degradation gradient GEDI2A and 4A with regression and ALS)


# Tidy environment

rm(allGEDI, allGEDI2AB, allGEDI2AB_reg_aged, allGEDI2AB_reg_intact, allGEDI2AB_reg_intact_sample, 
   allGEDI2AB_reg_sec, allGEDI2AB4A_reg_aged, allGEDI2AB_ALS_aged)
                  
# ------ Statistics DONE ---------

# Set rh and rhz columns for calculation (for example, rh97 and rhz97)
rh_col <- "rh25"
rhz_col <- "rhz25"

# Calculate Lin's CCC model for the entire GEDI/ ALS dataset using calculate_ccc function
ccc_mod <- calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col)
print(ccc_mod)

ccc_mod_c <- calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov")
print(ccc_mod_c)

# Subsets of forest gradient of degradation/ recovery (rh and then cover)
ccc_modint <- calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "Degradation == 'Intact'")
print(ccc_modint)

ccc_modlog <- calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "Degradation == 'Logged'")
print(ccc_modlog)

ccc_modburn <- calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "burn_freq %in% 1:10")
print(ccc_modburn)

ccc_modburnfreq1_3 <- calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "burn_freq %in% 1:3")
print(ccc_modburnfreq1_3)

ccc_modburnfreq4_6 <- calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "burn_freq %in% 4:6")
print(ccc_modburnfreq4_6)


ccc_modint_c <- calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "Degradation == 'Intact'")
print(ccc_modint_c)

ccc_modlog_c <- calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "Degradation == 'Logged'")
print(ccc_modlog_c)

ccc_modburn_c <- calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "burn_freq %in% 1:10")
print(ccc_modburn_c)

ccc_modburnfreq1_3_c <- calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "burn_freq %in% 1:3")
print(ccc_modburnfreq1_3_c)

ccc_modburnfreq4_6_c <- calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "burn_freq %in% 4:6")
print(ccc_modburnfreq4_6_c)



# RMSE calculations Inspired by Dorado et al (2021)
# Functions to calculate RMSE, Bias, fit a linear model for rh97 and do so
# for given forest conditions. Edit function for different 'rh's'


# RMSE calculations
conditions <- c("All", "Degradation == 'Intact'", "Degradation == 'Logged'", 
                "burned", "burn_freq %in% 1:3", "burn_freq %in% 4:6")

# Initialize vectors to store results
pearsons_r <- numeric(length(conditions))
p_values <- numeric(length(conditions))
rmse_m <- numeric(length(conditions))
rrmse <- numeric(length(conditions))
bias_m <- numeric(length(conditions))
relative_bias <- numeric(length(conditions))
lins_cc <- character(length(conditions))  # Lin's CCC will be stored as a character string

# Calculate mean actual height
mean_actual <- mean(allGEDI2AB_ALS[[rhz_col]], na.rm = TRUE)

for (i in seq_along(conditions)) {
  condition <- conditions[i]
  if (condition == "All") {
    burned_subset <- allGEDI2AB_ALS
  } else if (condition == "burned") {
    burned_subset <- handle_burned_condition(allGEDI2AB_ALS)
  } else {
    burned_subset <- allGEDI2AB_ALS %>% filter(!!rlang::parse_expr(condition))
  }
  
  # Calculate Pearson's r and p-value
  cor_results <- calculate_pearsons_r(burned_subset, rh_col, rhz_col)
  pearsons_r[i] <- cor_results$r_value
  p_values[i] <- cor_results$p_value
  
  stats <- calculate_stats(burned_subset, rh_col, rhz_col)
  rmse_m[i] <- stats$rmse
  rrmse[i] <- (stats$rmse / mean_actual) * 100
  bias_m[i] <- stats$bias
  relative_bias[i] <- (stats$bias / mean_actual) * 100
  lins_cc[i] <- calculate_ccc(burned_subset, rh_col, rhz_col)  # Calculate Lin's CCC
}

# Create a dataframe to store the results
stats_results <- data.frame(
  Forest_Condition = c("All", "Intact", "Logged", "Burned", "Burned 1-3", "Burned 4-6"),
  Pearsons_r = pearsons_r,
  p_value = p_values,  # Include p-value for significance testing
  RMSE_m = rmse_m,
  rRMSE = rrmse,
  Bias_m = bias_m,
  Relative_Bias = relative_bias,
  Lins_CCC = lins_cc  # Add Lin's CCC to the dataframe
)

# Format p_values with conditional notation
stats_results$p_value <- ifelse(stats_results$p_value < 0.0001, "< 0.0001", sprintf("%.4f", stats_results$p_value))

# Print the results
print(stats_results)


# Simple Pearsons and p value for canopy cover (additional to rh values)
pear_c <- calculate_pearsons_r_c(allGEDI2AB_ALS, "cover", "cancov")
print(pear_c)

pear_modint_c <- calculate_pearsons_r(allGEDI2AB_ALS, "cover", "cancov", "Degradation == 'Intact'")
print(pear_modint_c)

pear_modlog_c <- calculate_pearsons_r(allGEDI2AB_ALS, "cover", "cancov", "Degradation == 'Logged'")
print(pear_modlog_c)

pear_modburn_c <- calculate_pearsons_r(allGEDI2AB_ALS, "cover", "cancov", "burn_freq %in% 1:10")
print(pear_modburn_c)

pear_modburnfreq1_3_c <- calculate_pearsons_r(allGEDI2AB_ALS, "cover", "cancov", "burn_freq %in% 1:3")
print(pear_modburnfreq1_3_c)

pear_modburnfreq4_6_c <- calculate_pearsons_r(allGEDI2AB_ALS, "cover", "cancov", "burn_freq %in% 4:6")
print(pear_modburnfreq4_6_c)












# Calculating correlation (Pearsons R) along the waveforms for given degradation types

# Define the conditions
conditions <- c("All", "Degradation == 'Intact'", "Degradation == 'Logged'", 
               "burn_freq %in% 1:3", "burn_freq %in% 4:6")

# Define the rh and rhz columns at every 5 intervals
rh_columns <- paste0("rh", seq(5, 95, by = 5))
rhz_columns <- paste0("rhz", seq(5, 95, by = 5))

# Initialize a list to store results
results_list <- list()

# Loop through each condition and calculate Pearson's r for each rh-rhz pair
for (condition in conditions) {
  for (i in seq_along(rh_columns)) {
    rh_col <- rh_columns[i]
    rhz_col <- rhz_columns[i]
    pearson_r <- calculate_pearson(allGEDI2AB_ALS, condition, rh_col, rhz_col)
    results_list <- append(results_list, list(data.frame(
      Condition = condition,
      Pair = paste(rh_col, rhz_col, sep = "-"),
      Pearsons_r = pearson_r
    )))
  }
}

# Combine all results into a single dataframe
pearsons_results <- do.call(rbind, results_list)

pearsons_results <- pearsons_results %>%
  mutate(Condition = case_when(
    Condition == "All" ~ "All",
    Condition == "Degradation == 'Intact'" ~ "Intact",
    Condition == "Degradation == 'Logged'" ~ "Logged",
    Condition == "burn_freq %in% 1:3" ~ "Burned 1-3",
    Condition == "burn_freq %in% 4:6" ~ "Burned 4-6",
    TRUE ~ NA_character_)) %>%
  mutate(Pair = str_extract(Pair, "\\d+"))

print(pearsons_results)




















# -------- Principle Component Analysis INTEGRATION OF AMP---------


# UNTIL HAVE RUN THROUGH ALL DATA TO COMBINE NEW AMP GEDI AND DEGRADATION

# For the PCA until have updated (removed max.amp.x)

allGEDI2A_amp_PCA <- allGEDI2A_amp %>%
  select(shot_number, W_intercept, W_slope, W_variance,
         max_amp, n_peaks)

allGEDI2A_amp_PCA_nogeom <- st_set_geometry(allGEDI2A_amp_PCA, NULL)

# Merge amp dataset with overall allGEDI2AB_ALS
allGEDI2AB_ALS_amp <- allGEDI2AB_ALS %>%
  left_join(allGEDI2A_amp_PCA_nogeom, by = "shot_number") %>%
  select(-max_amp.x) %>%
  rename(max_amp = max_amp.y) %>%
  rename(Rh_intercept = G_intercept,
         Rh_slope = G_slope,
         Rh_variance = G_variance)

rm(allGEDI2A_amp_PCA, allGEDI2A_amp_PCA_nogeom)



# This is ready to run (may need editing after run through everything and edited names with amp)

# Keep the Degradation variable separate before PCA
degradation_type <- allGEDI2AB_ALS_amp$Degradation

# Remove unnecessary columns for PCA, but keep Degradation out for later use
allGEDI2ABPCA <- allGEDI2AB_ALS_amp %>%
  select(-Degradation, -solar_elevation, -l2b_quality_flag, -pgap_theta_error,
         -shot_number, -elev_highestreturn, -elev_lowestmode, 
         -sensitivity, -degrade_flag, -geometry, -Age_category,
         -Age_category2, -year, -ALS_CRS, -rh0, -matches("^rhz"),
         -max, -validation, -omega, -rh5, -rh10, -rh15,
         -rh20, -rh30, -rh35, -rh40, -rh45, -rh55, -rh60, -rh65, -rh70, 
         -rh80, -rh85, -rh90, -rh95, -rh97, -rh98, -z_kurt, -z_skew,
         -modis_treecover, -cancov, -forest_age, -burn_freq, -max_amp)


allGEDI2ABPCA <- allGEDI2ABPCA %>%
  select(-rh99, -rh100, -rh_max, -rh25, -n_peaks, -rh_mean, -rh50, - rh_min, -rh_kurt, -rh_skew,
           -rh_sd, -W_variance, -W_intercept, -pai, -W_slope, -Rh_intercept, -Rh_variance, Rh_slope)

# Remove geometry
allGEDI2ABPCA <- allGEDI2ABPCA %>%
  st_drop_geometry()

# Convert character and integer columns to numeric
#allGEDI2ABPCA <- allGEDI2ABPCA %>%
#  mutate(
   # W_intercept = as.numeric(W_intercept),
    #W_slope = as.numeric(W_slope),
    #W_variance = as.numeric(W_variance),
    #Rh_intercept = as.numeric(Rh_intercept),
    #Rh_slope = as.numeric(Rh_slope)
  # Rh_variance = as.numeric(Rh_variance)
 # )

# Standardize the data for PCA
scaled_data <- scale(allGEDI2ABPCA)

# Run PCA on the numeric data
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

summary(pca_result)




# Sort loadings to see potentially too similar loadings
loadings_pca <- pca_result$rotation[, c(1, 2)]  # PC1 is the first column, PC2 is the second
loadings_df <- as.data.frame(loadings_pca)
loadings_df$Variable <- rownames(loadings_pca)

# Sort loadings by PC1 in descending order
sorted_PC1 <- loadings_df %>%
  select(Variable, PC1) %>%
  arrange(desc(PC1))

# Sort loadings by PC2 in descending order
sorted_PC2 <- loadings_df %>%
  select(Variable, PC2) %>%
  arrange(desc(PC2))

print(sorted_PC1)
print(sorted_PC2)



# See explaination of variance of top contributing variables
loadings <- pca_result$rotation
pc1_loadings <- loadings[, 1]
pc2_loadings <- loadings[, 2]

# Sort PC loadings by absolute value
sorted_pc1_loadings <- sort(abs(pc1_loadings), decreasing = TRUE)
sorted_pc2_loadings <- sort(abs(pc2_loadings), decreasing = TRUE)

sorted_pc1_loadings
sorted_pc2_loadings


# View outputs
# Convert PCA components to a data frame 
components <- as.data.frame(pca_result$x)

# Add the degradation type back to the components dataframe
components$Degradation <- as.factor(degradation_type)

# Set color palette for degradation types
colors <- c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")

# Reverse PC2 for visual consistency
components$PC2 <- -components$PC2

# Plot 1: Full Biplot with All Loadings
scale <- 5 # Adjust arrow length scale
biplot_full <- ggplot(data = components, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Degradation), size = 1, shape = 19, alpha = 0.8) +
  geom_segment(data = as.data.frame(pca_result$rotation), 
               aes(x = 0, y = 0, xend = PC1 * scale, yend = PC2 * scale), 
               arrow = arrow(length = unit(0.3, "cm"), type = "open", angle = 25), 
               size = 0.8, color = "darkblue") +
  geom_text_repel(data = as.data.frame(pca_result$rotation), 
                  aes(x = PC1 * scale, y = PC2 * scale, label = rownames(pca_result$rotation)), 
                  size = 4, color = "black", max.overlaps = 5, 
                  box.padding = 0.35, point.padding = 0.5) +
  scale_color_manual(values = colors) +
  labs(title = "Biplot - PCA", x = "PC1", y = "PC2", color = "Degradation Type") +
  theme_minimal()

# Display the full biplot
print(biplot_full)



# Graphs for PC1 axis only 
pca_result$x[,1]
pca_result$x[,1][which(degradation_type=="Burned 1-3")]

pc1_intact <- hist(pca_result$x[,1][which(degradation_type == "Intact")], 
  main = "PC1 Intact",       
  xlab = "PC1 Values",                                    
  ylab = "Frequency",                                     
  col = "#92c5de",                                      
  border = "black",                                      
  breaks = 20                                            
)


# Define common x and y axis limits
x_limits <- c(-10, 5)

# Extract the principal components data into a dataframe
pca_data <- as.data.frame(pca_result$x)

# Create histograms using ggplot2 for each degradation type
pc1_intact <- ggplot(pca_data[degradation_type == "Intact", ], aes(x = PC1)) +
  geom_histogram(binwidth = 0.5, fill = "#92c5de") +
  labs(x = "PC1 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

pc1_logged <- ggplot(pca_data[degradation_type == "Logged", ], aes(x = PC1)) +
  geom_histogram(binwidth = 0.5, fill = "#0073e6") +
  labs(x = "PC1 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

pc1_burn1_3 <- ggplot(pca_data[degradation_type == "Burned 1-3", ], aes(x = PC1)) +
  geom_histogram(binwidth = 0.5, fill = "#e9a2b4") +
  labs(x = "PC1 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

pc1_burn4_6 <- ggplot(pca_data[degradation_type == "Burned 4+", ], aes(x = PC1)) +
  geom_histogram(binwidth = 0.5, fill = "#ca0020") +
  labs(x = "PC1 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

# Combine all plots into a single panel using patchwork
fig_4 <- (pc1_burn4_6 | pc1_burn1_3 | pc1_logged | pc1_intact)

# Display the combined plot
print(fig_4)

    
pc2_intact <- ggplot(pca_data[degradation_type == "Intact", ], aes(x = PC2)) +
  geom_histogram(binwidth = 0.5, fill = "#92c5de") +
  labs(x = "PC2 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

pc2_logged <- ggplot(pca_data[degradation_type == "Logged", ], aes(x = PC2)) +
  geom_histogram(binwidth = 0.5, fill = "#0073e6") +
  labs(x = "PC2 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

pc2_burn1_3 <- ggplot(pca_data[degradation_type == "Burned 1-3", ], aes(x = PC2)) +
  geom_histogram(binwidth = 0.5, fill = "#e9a2b4") +
  labs(x = "PC2 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

pc2_burn4_6 <- ggplot(pca_data[degradation_type == "Burned 4+", ], aes(x = PC2)) +
  geom_histogram(binwidth = 0.5, fill = "#ca0020") +
  labs(x = "PC2 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 


# Combine all plots into a single panel using patchwork
fig_5 <- (pc2_burn4_6 | pc2_burn1_3 | pc2_logged | pc2_intact)

# Display the combined plot
print(fig_5)

fig_6 <- (fig_4) / (fig_5)
print(fig_6)

pc3_intact <- ggplot(pca_data[degradation_type == "Intact", ], aes(x = PC3)) +
  geom_histogram(binwidth = 0.5, fill = "#92c5de") +
  labs(x = "PC3 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

pc3_logged <- ggplot(pca_data[degradation_type == "Logged", ], aes(x = PC3)) +
  geom_histogram(binwidth = 0.5, fill = "#0073e6") +
  labs(x = "PC3 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

pc3_burn1_3 <- ggplot(pca_data[degradation_type == "Burned 1-3", ], aes(x = PC3)) +
  geom_histogram(binwidth = 0.5, fill = "#e9a2b4") +
  labs(x = "PC3 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

pc3_burn4_6 <- ggplot(pca_data[degradation_type == "Burned 4+", ], aes(x = PC3)) +
  geom_histogram(binwidth = 0.5, fill = "#ca0020") +
  labs(x = "PC3 Values", y = "Frequency") +
  xlim(x_limits) +
  theme_bw() 

fig_7 <- (pc3_burn4_6 | pc3_burn1_3 | pc3_logged | pc3_intact)


fig_8 <- (fig_4) / (fig_5) / (fig_7)
print(fig_8)







#####---- TEST FOR GEDI SMALLER SAMPLE------

# Keep the Degradation variable separate before PCA
degradation_type <- GEDI2AB$Degradation

# Merge
GEDI2ABPCA <- GEDI2A_no_geom %>%
  left_join(allGEDI2AB_ALS_amp_clean, by = "shot_number")

## For smaller/ more evenly distributed dataset
GEDI2A_no_geom <- st_drop_geometry(GEDI2AB)
allGEDI2AB_ALS_amp_no_geom <- st_drop_geometry(allGEDI2AB_ALS_amp)

# Identify common columns between the two datasets
common_columns <- intersect(names(GEDI2A_no_geom), names(allGEDI2AB_ALS_amp_no_geom))
common_columns <- setdiff(common_columns, "shot_number")  # Exclude 'shot_number' from removal

# Remove the duplicate columns from the larger dataset
allGEDI2AB_ALS_amp_clean <- allGEDI2AB_ALS_amp_no_geom %>%
  select(-all_of(common_columns))

str(GEDI2ABPCA)
rm(allGEDI2A_no_geom, allGEDI2AB_ALS_amp_no_geom, common_columns)



GEDI2ABPCA <- GEDI2ABPCA %>%
  select(-Degradation, -solar_elevation, -l2b_quality_flag, -pgap_theta_error,
         -shot_number, -elev_highestreturn, -elev_lowestmode, 
         -sensitivity, -degrade_flag, -Age_category,
         -Age_category2, -year, -ALS_CRS, -rh0, -matches("^rhz"),
        -max, -validation, -omega, -rh5, -rh10, -rh15,
         -rh20, -rh30, -rh35, -rh40, -rh45, -rh55, -rh60, -rh65, -rh70, 
         -rh80, -rh85, -rh90, -rh95, -rh97, -rh98,
         -z_kurt, -z_skew, -rh0, -rh5,-rh10, -rh15, -rh20, -rh30, -rh35,  
         -rh40, -rh45, -rh55, -rh60, -rh65, -rh70, -rh80, -rh85, 
         -rh90,  -rh97, -rh98, -rh99, -rh_min, -rh_max,
         -modis_treecover, -cancov, -forest_age,
         -burn_freq, -max_amp,
        -rh25, -rh100, -n_peaks, -rh_mean, -rh50, -W_variance)


# Convert character and integer columns to numeric
GEDI2ABPCA <- GEDI2ABPCA %>%
  mutate(
    W_intercept = as.numeric(W_intercept),
    W_slope = as.numeric(W_slope),
    #W_variance = as.numeric(W_variance),
    Rh_intercept = as.numeric(Rh_intercept),
    Rh_slope = as.numeric(Rh_slope),
    Rh_variance = as.numeric(Rh_variance)
  )

# Standardize the data for PCA
scaled_data_1 <- scale(GEDI2ABPCA)

# Run PCA on the numeric data
pca_result_1 <- prcomp(scaled_data_1, center = TRUE, scale. = TRUE)

summary(pca_result_1)




# Sort loadings to see potentially too similar loadings
loadings_pca_1 <- pca_result_1$rotation[, c(1, 2)]  # PC1 is the first column, PC2 is the second
loadings_df_1 <- as.data.frame(loadings_pca_1)
loadings_df_1$Variable <- rownames(loadings_pca_1)

# Sort loadings by PC1 in descending order
sorted_PC1_1 <- loadings_df_1 %>%
  select(Variable, PC1) %>%
  arrange(desc(PC1))

# Sort loadings by PC2 in descending order
sorted_PC2_1 <- loadings_df_1 %>%
  select(Variable, PC2) %>%
  arrange(desc(PC2))

print(sorted_PC1_1)
print(sorted_PC2_1)



# See explaination of variance of top contributing variables
loadings_1 <- pca_result_1$rotation
pc1_loadings_1 <- loadings[, 1]
pc2_loadings_1 <- loadings[, 2]

# Sort PC loadings by absolute value
sorted_pc1_loadings_1 <- sort(abs(pc1_loadings_1), decreasing = TRUE)
sorted_pc2_loadings_1 <- sort(abs(pc2_loadings_1), decreasing = TRUE)

sorted_pc1_loadings_1
sorted_pc2_loadings_1


# View outputs
# Convert PCA components to a data frame 
components <- as.data.frame(pca_result_1$x)

# Add the degradation type back to the components dataframe
components$Degradation <- as.factor(degradation_type)

# Set color palette for degradation types
colors <- c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")

# Reverse PC2 for visual consistency
components$PC2 <- -components$PC2

# Plot 1: Full Biplot with All Loadings
scale <- 20  # Adjust arrow length scale
biplot_full <- ggplot(data = components, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Degradation), size = 1, shape = 19, alpha = 0.8) +
  geom_segment(data = as.data.frame(pca_result$rotation), 
               aes(x = 0, y = 0, xend = PC1 * scale, yend = PC2 * scale), 
               arrow = arrow(length = unit(0.3, "cm"), type = "open", angle = 25), 
               size = 0.8, color = "darkblue") +
  geom_text_repel(data = as.data.frame(pca_result$rotation), 
                  aes(x = PC1 * scale, y = PC2 * scale, label = rownames(pca_result$rotation)), 
                  size = 4, color = "black", max.overlaps = 5, 
                  box.padding = 0.35, point.padding = 0.5) +
  scale_color_manual(values = colors) +
  labs(title = "Biplot - PCA", x = "PC1", y = "PC2", color = "Degradation Type") +
  theme_minimal()

# Display the full biplot
print(biplot_full)






# Summary Plot for PC1 (perhaps for smaller sample)

# Extract explained variance
explained_variance_ratio <- summary(pca_result)[["importance"]]['Proportion of Variance',]
explained_variance_ratio <- 100 * explained_variance_ratio

# Define axis formatting (no lines and grid customization)
axis <- list(showline=FALSE,
             zeroline=FALSE,
             gridcolor='#ffff',
             ticklen=4,
             titlefont=list(size=13))

# Create the scatterplot matrix (SPLOM) using plotly
fig <- components %>%
  plot_ly() %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label=paste('PC 1 (', toString(round(explained_variance_ratio[1], 1)), '%)', sep = ''), values=~PC1),
      list(label=paste('PC 2 (', toString(round(explained_variance_ratio[2], 1)), '%)', sep = ''), values=~PC2),
      list(label=paste('PC 3 (', toString(round(explained_variance_ratio[3], 1)), '%)', sep = ''), values=~PC3),
      list(label=paste('PC 4 (', toString(round(explained_variance_ratio[4], 1)), '%)', sep = ''), values=~PC4)
    ),
    color = ~Degradation,  # Color based on degradation type
    colors = c("#e9a2b4", "#ca0020","#92c5de", "#0073e6"),
    marker = list(size = 4, opacity = 0.6)  # Smaller points, with transparency
  ) %>%
  style(diagonal = list(visible = FALSE)) %>%
  layout(
    legend = list(title = list(text = 'Degradation Type')),
    hovermode = 'closest',
    dragmode = 'select',
    plot_bgcolor = 'rgba(240,240,240, 0.95)',  # Light grey background
    xaxis = list(domain = NULL, showline = F, zeroline = F, gridcolor = '#ffff', ticklen = 4),
    yaxis = list(domain = NULL, showline = F, zeroline = F, gridcolor = '#ffff', ticklen = 4),
    xaxis2 = axis,  # Corrected comma and syntax
    yaxis2 = axis   # Add yaxis2 for formatting
  )

# Display the figure
fig














# Subset data for each degradation type
Burned_data4_6 <- allGEDI2ABPCA[allGEDI2ABPCA$Degradation_numeric == 3, ]
Burned_data1_3 <- allGEDI2ABPCA[allGEDI2ABPCA$Degradation_numeric == 2, ]
Logged_data <- allGEDI2ABPCA[allGEDI2ABPCA$Degradation_numeric == 1, ]
Intact_data <- allGEDI2ABPCA[allGEDI2ABPCA$Degradation_numeric == 0, ]

# Remove columns with zero variance (excluding Degradation_numeric)
Burned_data4_6 <- Burned_data4_6[, apply(Burned_data4_6[, -which(names(Burned_data4_6) == "Degradation_numeric")], 2, function(x) var(x) != 0)]
Burned_data1_3 <- Burned_data1_3[, apply(Burned_data1_3[, -which(names(Burned_data1_3) == "Degradation_numeric")], 2, function(x) var(x) != 0)]
Logged_data <- Logged_data[, apply(Logged_data[, -which(names(Logged_data) == "Degradation_numeric")], 2, function(x) var(x) != 0)]
Intact_data <- Intact_data[, apply(Intact_data[, -which(names(Intact_data) == "Degradation_numeric")], 2, function(x) var(x) != 0)]

# Perform PCA for Burned data
pca_Burned4_6 <- prcomp(select(Burned_data4_6, -Degradation_numeric), center = TRUE, scale. = TRUE)
pca_Burned1_3 <- prcomp(select(Burned_data1_3, -Degradation_numeric), center = TRUE, scale. = TRUE)
pca_Logged <- prcomp(select(Logged_data, -Degradation_numeric), center = TRUE, scale. = TRUE)
pca_Intact <- prcomp(select(Intact_data, -Degradation_numeric), center = TRUE, scale. = TRUE)

# Analyze PCA results for each subset
summary(pca_Burned4_6)
summary(pca_Burned1_3)
summary(pca_Logged)
summary(pca_Intact)

# Extract loadings for the first few PCs for Burned data
loadings_Burned4_6 <- pca_Burned4_6$rotation[, 1:3] 
loadings_Burned1_3 <- pca_Burned1_3$rotation[, 1:3] # Adjust the number (e.g., 1:3) for the desired number of PCs
loadings_Logged <- pca_Logged$rotation[, 1:3]
loadings_Intact <- pca_Intact$rotation[, 1:3]

print(loadings_Burned4_6)
print(loadings_Burned1_3)
print(loadings_Logged)
print(loadings_Intact)














# ------- Graphs/ visualisations CHECK-------


# PANEL ONE DONE - JUST NEED TO REMOVE CAPTIONS

## FIGURE 1 - GEDI validation / correspondance

# Plot 1 : 4 panels - Correspondance between GEDI and ALS heights
# Filter the reshaped height data to keep only the pairs of ALS and GEDI columns
height_rhz25 <- allGEDI2AB_ALS_height_long %>% filter(ALS == "rhz25")
height_rhz50 <- allGEDI2AB_ALS_height_long %>% filter(ALS == "rhz50")
height_rhz75 <- allGEDI2AB_ALS_height_long %>% filter(ALS == "rhz75")
height_rhz97 <- allGEDI2AB_ALS_height_long %>% filter(ALS == "rhz97")

# Define the limits for both x and y axes for the rhz plots
common_limits <- c(0, 60)

# Create individual plots for each height metric with the same scales and aspect ratio
# Suppressing legends on rhz plots
plot_rhz25 <- ggplot(height_rhz25, aes(x = ALS_value, y = GEDI_value, color = Degradation)) +
  geom_point(size = 0.5) +
  geom_text(data = correlations_height %>% filter(ALS == "rhz25"), 
            aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  labs(x = "ALS Relative Height (m)", y = "GEDI Relative Height (m)", title = "rhz25") +
  theme_bw() +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  coord_fixed(ratio = 1, xlim = common_limits, ylim = common_limits) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")  # Suppress the legend

plot_rhz50 <- ggplot(height_rhz50, aes(x = ALS_value, y = GEDI_value, color = Degradation)) +
  geom_point(size = 0.5) +
  geom_text(data = correlations_height %>% filter(ALS == "rhz50"), 
            aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  labs(x = "ALS Relative Height (m)", y = "GEDI Relative Height (m)", title = "rhz50") +
  theme_bw() +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  coord_fixed(ratio = 1, xlim = common_limits, ylim = common_limits) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")  # Suppress the legend

plot_rhz75 <- ggplot(height_rhz75, aes(x = ALS_value, y = GEDI_value, color = Degradation)) +
  geom_point(size = 0.5) +
  geom_text(data = correlations_height %>% filter(ALS == "rhz75"), 
            aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  labs(x = "ALS Relative Height (m)", y = "GEDI Relative Height (m)", title = "rhz75") +
  theme_bw() +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  coord_fixed(ratio = 1, xlim = common_limits, ylim = common_limits) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")  # Suppress the legend

plot_rhz97 <- ggplot(height_rhz97, aes(x = ALS_value, y = GEDI_value, color = Degradation)) +
  geom_point(size = 0.5) +
  geom_text(data = correlations_height %>% filter(ALS == "rhz97"), 
            aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  labs(x = "ALS Relative Height (m)", y = "GEDI Relative Height (m)", title = "rhz97") +
  theme_bw() +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  coord_fixed(ratio = 1, xlim = common_limits, ylim = common_limits) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")  # Suppress the legend


# Plot 2 - Correspondance between canopy cover
# Reshaping the data for canopy cover metrics
allGEDI2AB_ALS_cover_long <- allGEDI2AB_ALS %>%
  pivot_longer(cols = starts_with("can"), names_to = "ALS", values_to = "ALS_value") %>%
  pivot_longer(cols = "cover", names_to = "GEDI", values_to = "GEDI_value") %>%
  filter(ALS == "cancov")

# Converting columns to character type to avoid type mismatch
allGEDI2AB_ALS_cover_long <- allGEDI2AB_ALS_cover_long %>%
  mutate(across(c(ALS, GEDI), as.character))

# Calculate correlation for canopy cover
correlations_cover <- allGEDI2AB_ALS_cover_long %>%
  group_by(GEDI) %>%
  summarise(correlation = cor(ALS_value, GEDI_value, use = "complete.obs")) %>%
  mutate(annotation = paste0("R² = ", round(correlation^2, 2)))

# Create the panel plot
panel_plot2 <- allGEDI2AB_ALS_cover_long %>%
  ggplot(aes(x = ALS_value, y = GEDI_value, color = Degradation)) +
  geom_point(size = 0.5) +  
  geom_text(data = correlations_cover, aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  labs(x = "ALS Canopy Cover", y = "GEDI Canopy Cover", title = "Canopy Cover") +
  theme_bw() +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  coord_fixed(ratio = 1) +  # Ensures square aspect ratio
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

print(panel_plot2)


# Combine validation plots (height and cover)
figure1 <- (plot_rhz25 + plot_rhz50 + panel_plot2 + plot_rhz75 + plot_rhz96)

# Display the combined plot
figure1







## FIGURE 2 - Degradation specific and pair wise correlations


# Plot 3 - Correspondance for degraadtion types
# Calculate correlation and create annotation text for degradation types at rh97
correlations_degradation <- allGEDI2AB_ALS %>%
  filter(Degradation %in% c("Intact", "Logged", "Burned 1-3", "Burned 4+")) %>%
  group_by(Degradation) %>%
  summarise(correlation = cor(rhz97, rh97, use = "complete.obs")) %>%
  mutate(annotation = paste0("R² = ", round(correlation^2, 2)))

panel_plot3 <- allGEDI2AB_ALS %>%
  filter(Degradation %in% c("Intact", "Logged", "Burned 1-3", "Burned 4+")) %>%
  ggplot(aes(x = rhz97, y = rh97, color = Degradation)) +
  geom_point(size = 0.5) +
  geom_text(data = correlations_degradation, aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  labs(x = "ALS Relative height (m)", y = "GEDI Relative height (m)") +
  theme_bw() +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  facet_wrap(~ Degradation) +  # Remove scales = "free" to keep consistent scales across facets
  xlim(0, 60) +  # Set x-axis limits for all facets
  ylim(0, 60) +  # Set y-axis limits for all facets
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Print the panel plot
print(panel_plot3)


# Plot 4 - Correlations for complete waveform for each degradation type
# Ensure the Pair column is ordered correctly
pearsons_results$Pair <- factor(pearsons_results$Pair, levels = as.character(seq(5, 95, 5)))

# Filter the dataset to include only the specified conditions
pearsons_results_data_filtered <- pearsons_results %>%
  filter(Condition %in% c("Intact", "Logged", "Burned 1-3", "Burned 4-6"))

# Find the maximum Pearson's r value for each condition
max_pearsons_r <- pearsons_results_data_filtered %>%
  group_by(Condition) %>%
  slice_max(order_by = Pearsons_r, n = 1)

# Create the panel plot
panel_plot4 <- pearsons_results_data_filtered %>%
  ggplot(aes(x = Pair, y = Pearsons_r, color = Condition)) +
  geom_point(size = 1) +  # Points for each Pearson's r value
  geom_line(aes(group = Condition), size = 0.5) +  # Line connecting the points for each condition
  geom_point(data = max_pearsons_r, aes(x = Pair, y = Pearsons_r), size = 4, shape = 18, show.legend = FALSE) +  # Highlight the max points
  labs(x = "Pair (GEDI rh- ALS rhz)", y = "Pearson's r") +
  theme_bw() +
  scale_color_manual(values = c("Intact" = "#92c5de", "Logged" = "#0073e6", "Burned 1-3" = "#e9a2b4", "Burned 4-6" = "#ca0020")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    strip.background = element_blank(),  # Remove grey background from facet labels
    strip.text = element_text(face = "bold"),  # Make facet labels bold
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, face = "italic")  # Rotate x-axis labels for better readability and make them italic
   )

# Print the panel plot
print(panel_plot4)


# Combine the plots using the patchwork package
figure2 <- (panel_plot3 + panel_plot4) +
  plot_layout(guides = "collect")

# Print the combined plot
print(figure2)












## FIGURE 3 - GEDI degradation data across Amazonia 

# Ordering of age within plots
age_order <- c("<7", "7-15", "15-25", "25-40", ">40")
age_order2 <- c("<10", "10-20", "20-30", "30-40", ">40")
allGEDI2AB_ALS <- mutate(allGEDI2AB_ALS, Age_category = factor(Age_category, levels = age_order))
allGEDI2AB_ALS <- mutate(allGEDI2AB_ALS, Age_category2 = factor(Age_category2, levels = age_order2))
GEDI2AB <- mutate(GEDI2AB, Age_category = factor(Age_category, levels = age_order))
GEDI2AB <- mutate(GEDI2AB, Age_category2 = factor(Age_category2, levels = age_order2))


# Plot 1 - GEDI height/ age/ degradation
# WITH IMAGE BACKGROUND
# Load your image
background_image <- jpeg::readJPEG("/Users/emilydoyle/Desktop/vector-tropical-rainforest.jpeg")  # Use readPNG() if it's a PNG file

# Create the plot
panel_plot5 <- GEDI2AB %>%
  ggplot() +
  #annotation_custom(rasterGrob(background_image, width = unit(1,"npc"), height = unit(1,"npc"), 
                             #  gp = gpar(alpha = 0.5))) +  # Add background image with opacity
  geom_point(aes(x = Age_category, y = rh97, color = Degradation), size = 1, position = position_jitter(width = 0.3)) +
  labs(title = "Canopy Height", x = "Forest Age", y = "Canopy Height (m)") +
  theme_bw() +
  ylim(0, 45) +  # Set the y-axis limit
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6"))
  
# Display the plot
plot(panel_plot5)


# Box plot of the data instead
panel_plot6 <- GEDI2AB %>%
  ggplot(aes(x = Age_category, y = rh97, color = Degradation, fill = Degradation)) +
  geom_boxplot(outlier.size = 1, outlier.shape = 16, position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.5), 
               shape = 23, size = 2, color = "black", fill = "white", aes(group = Degradation)) +
  stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 0.5), 
               aes(label = round(..y.., 1), group = Degradation), vjust = -1.5, size = 3) +
  labs(x = "Forest Age", y = "Canopy Height (m)") +
  theme_bw() +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  scale_fill_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6"))

  
# Display the plot
plot(panel_plot6)



# GEDI CANOPY COVER AND AGE
# Define breaks for age categories

panel_plot7 <- GEDI2AB1 %>%
  ggplot() +
  geom_point(aes(x = Age_category, y = cover, color = Degradation), size = 1, alpha = 0.9, position = position_jitter(width = 0.3)) +
  labs(title = "Canopy Cover", x = "Forest age", y = "Canopy cover (%)") +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  theme_bw()

plot(panel_plot7)


# Box plot of the data instead
panel_plot8 <- GEDI2AB %>%
  ggplot(aes(x = Age_category, y = cover, color = Degradation, fill = Degradation)) +
  geom_boxplot(outlier.size = 1, outlier.shape = 16, position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.5), 
               shape = 23, size = 2, color = "black", fill = "white", aes(group = Degradation)) +
  stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 0.5), 
               aes(label = round(..y.., 1), group = Degradation), vjust = -1.5, size = 3) +
  labs(x = "Forest Age", y = "Canopy cover (%)") +
  theme_bw() +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  scale_fill_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6"))

# Display the plot
plot(panel_plot8)


# GEDI AGBD


GEDI2AB4A <- GEDI2AB4A %>%
  filter(l4_quality_flag == 1)

panel_plot9 <- GEDI2AB4A %>%
  ggplot() +
  geom_point(aes(x = Age_category, y = agbd, color = Degradation), size = 1, alpha = 0.9, position = position_jitter(width = 0.3)) +
  labs(title = "AGBD", x = "Forest age", y = "AGBD") +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  theme_bw()

plot(panel_plot9)


panel_plot10 <- GEDI2AB4A %>%
  ggplot(aes(x = Age_category, y = agbd, color = Degradation, fill = Degradation)) +
  geom_boxplot(outlier.size = 1, outlier.shape = 16, position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.5), 
               shape = 23, size = 2, color = "black", fill = "white", aes(group = Degradation)) +
  stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 0.5), 
               aes(label = round(..y.., 1), group = Degradation), vjust = -1.5, size = 3) +
  labs(x = "Forest Age", y = "AGBD (ppm)") +
  theme_bw() +
  scale_colour_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6")) +
  scale_fill_manual(values = c("#e9a2b4", "#ca0020", "#92c5de", "#0073e6"))

# Print the plot
print(panel_plot10)

# Combine the plots using the patchwork package
figure3 <- (panel_plot5 + panel_plot6) /
                   (panel_plot7 + panel_plot8) /
                   (panel_plot9 + panel_plot10)

# Print the combined plot
print(figure3)






## FIGURE 4 - Average relative height and waveform 

# Transpose the data and select only relevant rows for rh shape

allGEDI2AB_trans <- allGEDI2AB_ALS %>%
  as.data.frame() %>%
  select(shot_number, geometry, Degradation, starts_with("rh"))

# Reshape the data to long format
allGEDI2AB_trans_plot <- allGEDI2AB_trans %>%
  pivot_longer(cols = starts_with("rh"), 
               names_to = "rh_value", 
               values_to = "rh_shape") %>%
  mutate(rh_value = as.numeric(gsub("rh", "", rh_value)))


# Calculate the average rh_shape for each degradation type
avg_rh_shape <- allGEDI2AB_trans_plot %>%
  group_by(Degradation, rh_value) %>%
  summarize(mean_rh_shape = mean(rh_shape, na.rm = TRUE))

# Plot the average rh_shape by degradation type
panel_plot12 <- ggplot(avg_rh_shape, aes(x = rh_value, y = mean_rh_shape, color = Degradation)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Average Relative Height", 
       x = "Relative Height (rh)", 
       y = "Elevation (m)") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Burned 1-3" = "#e9a2b4", 
                                "Burned 4+" = "#ca0020", 
                                "Intact" = "#92c5de", 
                                "Logged" = "#0073e6")) +
  theme(legend.position = "none")

# Print the plot
print(panel_plot12)


# DATA FROM GEDI_AMP SCRIPT

panel_plot13 <- ggplot(combined_data, aes(x = avg_rh, y = avg_amplitude, color = Degradation, group = Degradation)) +
  geom_line(size = 1) +
  labs(title = "Average Waveform Amplitude",
       x = "Elevation (m)",
       y = "Waveform Amplitude") +
  theme_minimal() +
  scale_color_manual(values = c("Burned 1-3" = "#e9a2b4", 
                                "Burned 4+" = "#ca0020", 
                                "Intact" = "#92c5de", 
                                "Logged" = "#0073e6")) +
  coord_flip() +  # Flip axes
  scale_y_reverse() # NEED TO CHECK AMP VALUES (CUMULATIVE DECREASING ETC)

print(panel_plot13)

figure3 <- (panel_plot12 + panel_plot13) +
  plot_layout(guides = "collect")

figure3



























##FIGURE 5 - Violin plots for loadings


wave_slope <- allGEDI2AB_ALS_amp %>%
  ggplot(aes(x=Degradation, y=W_slope, fill=Degradation)) +
  geom_violin(color = "black", width = 1.3, alpha = 0.9) +  
  #geom_jitter(width = 0.1, size = 0.1, alpha = 0.1) +
  labs(title = "Gradient slope of waveform profile", x = "Degradation type", y = "Slope") +
  theme_bw() +
  scale_fill_manual(values = c("Burned 1-3" = "#e9a2b4", 
                                "Burned 4+" = "#ca0020", 
                                "Intact" = "#92c5de", 
                                "Logged" = "#0073e6")) +  # Custom colors
     theme(
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
        legend.position = "none"
       )

plot(wave_slope)


GEDI2A_amp_deg <- GEDI2A_amp %>%
  mutate(Degradation_Group = case_when(
    str_detect(Degradation, "Burned") ~ "Burned",
    Degradation == "Logged" ~ "Logged",
    Degradation == "Intact" ~ "Intact"
  ))


wave_slope2 <- GEDI2A_amp_deg %>%
  ggplot(aes(x = Degradation_Group, y = W_slope, fill = Degradation_Group)) +
  geom_violin(color = "black", width = 0.9, alpha = 0.9) +  
  labs(title = "Gradient slope of waveform profile", x = "Degradation Group", y = "Slope") +
  theme_bw() +
  scale_fill_manual(values = c("Burned" = "#cc3333", 
                               "Logged" = "#0073e6", 
                               "Intact" = "#92c5de")) +  # Adjust colors for the new groups
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

plot(wave_slope2)









rh96_violin <- GEDI2A_amp_deg %>%
  ggplot(aes(x = Degradation, y = rh96, fill = Degradation)) +
  geom_violin(color = "black", width = 1.3, alpha = 0.9, scale = "width") +  
  labs(title = "Relative height across degradation", x = "Degradation type", y = "Reelative height (m)") +
  theme_bw() +
  scale_fill_manual(values = c("Burned 1-3" = "#e9a2b4", 
                               "Burned 4+" = "#ca0020", 
                               "Intact" = "#92c5de", 
                               "Logged" = "#0073e6")) +  # Custom colors
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

plot(rh96_violin)





wave_slope <- allGEDI2AB_ALS_amp %>%
  ggplot(aes(x = Degradation, y = W_slope, fill = Degradation)) +
  geom_violin(color = "black", width = 1.3, alpha = 0.9, scale = "width") +  # Adjust the scale
  labs(title = "Gradient slope of waveform profile", x = "Degradation type", y = "Slope") +
  theme_bw() +
  scale_fill_manual(values = c("Burned 1-3" = "#e9a2b4", 
                               "Burned 4+" = "#ca0020", 
                               "Intact" = "#92c5de", 
                               "Logged" = "#0073e6")) +  # Custom colors
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

plot(wave_slope)









# Violin plot for GEDI top height across degradation
GEDIrh99_degradation <- GEDI2AB %>%
  ggplot(aes(x = Degradation, y = rh97, fill = Degradation)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_jitter(width = 0.5, size = 1, alpha = 0.5) +
  labs(title = "GEDI Canopy height with various degradation type", x = "Degradation type", y = "Relative height top of canopy (m) (rh99)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
plot(GEDIrh99_degradation)






# NEED TO LOOK AT PLOTTING THE VARIANCES

#GEDI2AB$G_slope <- as.numeric(GEDI2AB$G_slope)
#GEDI2AB$G_variance <- as.numeric(GEDI2AB$G_variance)
#GEDI2AB$G_intercept <- as.numeric(GEDI2AB$G_intercept)


 



# Violin plot for GEDI top height across degradation

allGEDI2AB_ALStest <- allGEDI2AB_ALS %>%
  mutate(status = ifelse(Degradation == "Logged" | Degradation == "Burned", "Degraded", "Intact"))

GEDI_condition_test <- allGEDI2AB_ALStest %>%
  ggplot(aes(x=status, y=G_slope, fill=status)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  labs(title = "Gradient slope of relative height profile", x = "Degradation type", y = "G_slope") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
plot(GEDI_condition_test)






# ------ Extras? --------




#make column for forest extent and under 4m (maybe 5 for FAO definition) in height for non-forest

test <- filter(allGEDI2AB_reg, rh100<4)




# MAYBE DONT NEED
# Repeat for GEDI4A dataframe
{
  
  # Load in 4A files to make comprehensive GEDI data frame also (sample size reduced however)
  allGEDI4A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI4A.fgb")
  
  # Remove geometry to join data by shot_number
  allGEDI4A_no_geom <- st_set_geometry(allGEDI4A, NULL)
  
  # Merge datasets based on shot_number to have GEDI4A carbon metrics also
  allGEDI2AB4A_reg <- allGEDI2AB_reg %>%
    left_join(allGEDI4A_no_geom, by = "shot_number")
  
  allGEDI2AB4A_reg <- allGEDI2AB4A_reg %>% 
    drop_na(agbd)
  
  sf::st_write(allGEDI2AB4A_reg, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB4A_reg.fgb", delete_dsn = TRUE, overwrite = TRUE)
  #allGEDI2AB4A_reg <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB4A_reg.fgb")
  
}



# REVIEW THIS 
# Repeat degradation extraction for all datasets
{
  
  # Mutate allGEDI2AB_ALS, allGEDI2AB_ALS and allGEDI2AB4A_reg data set to also reflect degradation type
  # Overwrite files for final data frames for analysis/ summaries
  
  allGEDI2AB_ALS <- allGEDI2AB_ALS_aged %>%
    process_GEDI_degradation()
  
  
  # Load in 4A files to make comprehensive GEDI data frame also (sample size reduced however)
  allGEDI4A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI4A.fgb")
  
  # Remove geometry to join data by shot_number
  allGEDI4A_no_geom <- st_set_geometry(allGEDI4A, NULL)
  
  # Merge datasets based on shot_number to have GEDI4A carbon metrics also
  GEDI2AB4A <- GEDI2AB %>%
    left_join(allGEDI4A_no_geom, by = "shot_number")
  
  GEDI2AB4A <- GEDI2AB4A %>% 
    drop_na(agbd)
  
  
  
  # Overwrite final datasets : allGEDI2AB_ALS (GEDI2A with regression), 
  #                  allGEDI2AB4A_reg (GEDI2A and 4A with regression), 
  #                  allGEDI2AB_ALS (GEDI2A with regression and ALS data), 
  #                  GEDI2AB (forest degradation gradient GEDI2A with regression and ALS), 
  #                  GEDI2AB4A (forest degradation gradient GEDI2A and 4A with regression and ALS)
  # Sample sizes become smaller when including GEDI 4A and ALS data so retaining original samples also
  
  sf::st_write(allGEDI2AB_ALS, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb", delete_dsn = TRUE, overwrite = TRUE)
  sf::st_write(allGEDI2AB_reg, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb", delete_dsn = TRUE, overwrite = TRUE)
  sf::st_write(allGEDI2AB4A_reg, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB4A_reg.fgb", delete_dsn = TRUE, overwrite = TRUE)
  sf::st_write(GEDI2AB4A, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2AB4A.fgb", delete_dsn = TRUE, overwrite = TRUE)
  
  # allGEDI2AB_reg <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb")
  # allGEDI2AB_ALS <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb")
  # allGEDI2AB4A_reg <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB4A_reg.fgb")
  # GEDI2AB4A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2AB4A.fgb")
  
  
  
}







