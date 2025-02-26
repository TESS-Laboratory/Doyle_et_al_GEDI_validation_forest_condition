## Script for the analysis of paper 'Doyle et al., 2024' for the validation of the use of GEDI data 
## in degraded Amazon rainforest to investigate forest condition

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

# ----- Pre-Process ALS -------- 
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

# ----------- GEDI download ----------------

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
  fgb_output_folder = c("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2A"),
  als_year = c(2018, 2021, 2023))  # Define ALS_year for each folder

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


# Download GEDI2B files for all polygon shapefiles in a folder,creating output geodata frame for each AOI

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

# Check the CRS is the same for all three GEDI data sets
allGEDI2B <- st_transform(allGEDI2B, st_crs(allGEDI2A))
allGEDI4A <- st_transform(allGEDI4A, st_crs(allGEDI2A))

# Remove geometries to join data by shot_number
allGEDI2A_no_geom <- st_set_geometry(allGEDI2A, NULL)
allGEDI2B_no_geom <- st_set_geometry(allGEDI2B, NULL)
allGEDI4A_no_geom <- st_set_geometry(allGEDI4A, NULL)

# Merge data sets based on shot_number to create two data sets: 2A/2B and additional 4A
merged2AB <- allGEDI2A_no_geom %>%
  left_join(allGEDI2B_no_geom, by = "shot_number") 

merged2AB4A <- merged2AB %>%
  left_join(allGEDI4A_no_geom, by = "shot_number")


# Clean data before remerging
# Remove rows with any NA values 
cleaned2AB <- drop_na(merged2AB)
cleaned2AB4A <- drop_na(merged2AB4A)

# Keep only the columns from allGEDI2B and the non-duplicated columns from the other data sets
columns_to_keep <- setdiff(names(cleaned2AB), names(allGEDI2A_no_geom))
columns_to_keep <- columns_to_keep[!grepl("\\.y$", columns_to_keep)]
columns_to_keep <- columns_to_keep[!grepl("\\.x$", columns_to_keep)]

cleaned2AB <- cleaned2AB %>%
  dplyr::select(shot_number, all_of(columns_to_keep), ends_with(".x")) %>%
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))

# Keep only the columns from allGEDI4A and the non-duplicated columns from the other data sets
columns_to_keep <- setdiff(names(cleaned2AB4A), names(allGEDI2A_no_geom))
columns_to_keep <- columns_to_keep[!grepl("\\.y$", columns_to_keep)]
columns_to_keep <- columns_to_keep[!grepl("\\.x$", columns_to_keep)]

cleaned2AB4A <- cleaned2AB4A %>%
  dplyr::select(shot_number, all_of(columns_to_keep), ends_with(".x")) %>%
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x")) %>%
  dplyr::select(shot_number, all_of(columns_to_keep), ends_with(".x"))


# Merge the geometries back from `allGEDI2A`
allGEDI2AB <- allGEDI2A %>%
  filter(shot_number %in% cleaned2AB$shot_number) %>%
  left_join(cleaned2AB, by = "shot_number") %>%
  dplyr::select(-ends_with(".y")) %>%
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))

allGEDI <- allGEDI2AB %>%
  filter(shot_number %in% cleaned2AB4A$shot_number) %>%
  left_join(cleaned2AB4A, by = "shot_number") %>%
  dplyr::select(-year, -degrade_flag) %>%
  dplyr::select(-ends_with(".x")) %>%
  rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))

# Filter the data by sensitivity metric of 0.95 (sample decreases from 9161 to 7915)
allGEDI2AB <- allGEDI2AB %>%
  filter(sensitivity > 0.95)


sf::st_write(allGEDI2AB, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2AB <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb")
#allGEDI <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI.fgb")

# Calculate amplitude proxy from cumulative energy values in GEDI2A data

allGEDI2A_long <- gedi_long(allGEDI2A)

allGEDI2A_amp <- allGEDI2A_long %>%
  dplyr::select(shot_number, interval, amp, ALS_year) %>%
  pivot_wider(
    names_from = interval,
    values_from = amp,
    names_prefix = "amp_"
  )

allGEDI2AB_amp <- allGEDI2AB %>%
  left_join(allGEDI2A_amp, by = "shot_number") %>%
  dplyr::select(-starts_with("rx_cum"))


# Tidy the environment
rm(allGEDI2A_no_geom, allGEDI2B_no_geom, allGEDI4A_no_geom, cleaned2AB, cleaned2AB4A,
   merged2AB, merged2AB4A, fgb_list, params) #allGEDI2A, allGEDI2B, allGEDI4A, allGEDI2A_amp)


# ------ GEDI rh/ waveform regressions ---------

# Summarise relative height rh0-100 metrics with linear regression model
# Outputs intercept, slope and variance of the 2A relative height profile

# dplyr::select only relative height data and unique identifier, transforming data set
GEDI2AB_trans <- allGEDI2AB %>%
  as.data.frame() %>%
  dplyr::select(shot_number, starts_with("rh"))

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

# Remerge results with original GEDI2AB dataframe and name GEDI regressions

allGEDI2AB_reg <- left_join(allGEDI2AB, summary_df, by = "shot_number")
allGEDI2AB_reg <- left_join(allGEDI2AB_reg, result_df, by = "shot_number")

sf::st_write(allGEDI2AB_reg, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb", delete_dsn = TRUE, overwrite = TRUE)
#allGEDI2AB_reg <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb")

# Tidy the environment
rm(GEDI2AB_trans, summary_df, result_df)



# Summarise cumulative energy proxy amplitude metrics with linear regression model
# Outputs intercept, slope and variance of the 2A waveform stats

# dplyr::select only amp data and unique identifier, transforming data base
allGEDI2AB_trans_amp <- allGEDI2AB_amp %>%
  as.data.frame() %>%
  dplyr::select(shot_number, starts_with("amp"))

# Apply regression function to cumulative waveforms
result_df_amp <- apply(allGEDI2AB_trans_amp, 1, rh_linear_regression)

# Convert the result to a dataframe and set column names
result_df_amp <- t(data.frame(result_df_amp))
result_df_amp  <- as.data.frame(result_df_amp, stringsAsFactors = FALSE)
colnames(result_df_amp) <- c("shot_number", "W_intercept", "W_slope", "W_variance")

result_df_amp <- result_df_amp %>%
  mutate(W_intercept = as.numeric(W_intercept),
         W_slope = as.numeric(W_slope),
         W_variance = as.numeric(W_variance))

# Re-merge results and amplitudes with original GEDI2A data frame and name GEDI regressions

allGEDI2AB_reg <- left_join(allGEDI2AB_reg, result_df_amp, by = "shot_number")
allGEDI2AB_reg <- left_join(allGEDI2AB_reg, allGEDI2AB_trans_amp, by = "shot_number")

# Tidy the environment
rm(allGEDI2AB_trans_amp, result_df_amp)





# Create column called max_amp, identifying the maximum value from columns containing "amp"
allGEDI2AB_amp <- allGEDI2AB_amp %>%
  mutate(max_amp = apply(dplyr::select(., starts_with("amp")) %>% st_drop_geometry() %>% as.matrix(), 
                          1, max, na.rm = TRUE))

# Extract and format max_amp from allGEDI2AB_amp
result_df_amp <- allGEDI2AB_amp %>%
  st_drop_geometry() %>%
  dplyr::select(shot_number, max_amp) %>%
  mutate(max_amp = as.numeric(max_amp),
    shot_number = as.character(shot_number))

# Re-merge max_amp results with original allGEDI2A data frame
allGEDI2AB_reg <- allGEDI2AB_reg %>%
  left_join(result_df_amp, by = "shot_number")


# Finalise data sets for methods

allGEDI2AB_amp <- allGEDI2AB_reg %>%
  dplyr::select(shot_number, ALS_year, W_intercept, W_slope, W_variance,
         max_amp, starts_with("amp"), matches("^rh([0-9]|[1-9][0-9]|100)$"))

allGEDI2AB <- allGEDI2AB_reg %>%
  dplyr::select(-starts_with("rx_cum"), -starts_with("amp"))


sf::st_write(allGEDI2AB, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2AB_amp, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_amp.fgb", delete_dsn = TRUE, overwrite = TRUE)
#allGEDI2AB_amp <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_amp.fgb")

rm(result_df_amp, shot_numbers, allGEDI2AB_reg, allGEDI2A_long)

# ------ Extracting ALS metrics within GEDI footprints------

# As the ALS data spans different parts of the Amazon rainforest, they are separated in folders by their
# CRS. Each ALS folder must therefore extract data from the GEDI footprints separately before being merged.
# Rio Cautario (CAUT) pre-processed files were split into two folders for processing efficiency (1&2)

# Reload/filter and set CRS of final ALS catalogs if no longer in environment

DAAC18_19Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_19S/final_norm')
CAUT23_20Sfinal1 <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/CAUT23_20S/final_norm1')
CAUT23_20Sfinal2 <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/CAUT23_20S/final_norm2')
DAAC1821_21Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC1821_21S/final_norm')

DAAC18_19Sfinal <- filter_als(DAAC18_19Sfinal)
CAUT23_20Sfinal1 <- filter_als(CAUT23_20Sfinal1)
CAUT23_20Sfinal2 <- filter_als(CAUT23_20Sfinal2)
DAAC1821_21Sfinal <- filter_als(DAAC1821_21Sfinal)

st_crs(DAAC18_19Sfinal) <- 32719
st_crs(CAUT23_20Sfinal1) <- 32720
st_crs(CAUT23_20Sfinal2) <- 32720
st_crs(DAAC1821_21Sfinal) <- 32721


# Filter and reproject the GEDI dataframe to match each CRS catalog

allGEDI2AB_19S <- filter_reproj_GEDI(allGEDI2AB, '19S', 'EPSG:32719')
allGEDI2AB_20S <- filter_reproj_GEDI(allGEDI2AB, '20S', 'EPSG:32720')
allGEDI2AB_21S <- filter_reproj_GEDI(allGEDI2AB, '21S', 'EPSG:32721')

st_crs(allGEDI2AB_19S) #, allGEDI2AB_20S, allGEDI2AB_21S)


# Extracting metrics of ALS within GEDI footprints in the same CRS
# Converting metrics to CRS (WGS84) associated with GEDI data to extract
# Write to file - large data sets

DAAC18_19Smetrics <- plot_metrics(DAAC18_19Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_19S, radius = 12.5)
DAAC18_19Smetrics <- st_transform(DAAC18_19Smetrics , "EPSG:32643")
sf::st_write(DAAC18_19Smetrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/DAAC18_19Smetrics.fgb", delete_dsn = TRUE, overwrite = TRUE)

CAUT23_20Smetrics1 <- plot_metrics(CAUT23_20Sfinal1, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_20S, radius = 12.5)
CAUT23_20Smetrics1 <- st_transform(CAUT23_20Smetrics1, "EPSG:32643")
CAUT23_20Smetrics1 <- CAUT23_20Smetrics1 %>%
  filter(!is.na(rhz95))
sf::st_write(CAUT23_20Smetrics1, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/CAUT23_20Smetrics1.fgb", delete_dsn = TRUE, overwrite = TRUE)

CAUT23_20Smetrics2 <- plot_metrics(CAUT23_20Sfinal2, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_20S, radius = 12.5)
CAUT23_20Smetrics2 <- st_transform(CAUT23_20Smetrics2, "EPSG:32643")
CAUT23_20Smetrics2 <- CAUT23_20Smetrics2 %>%
  filter(!is.na(rhz95))
sf::st_write(CAUT23_20Smetrics2, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/CAUT23_20Smetrics2.fgb", delete_dsn = TRUE, overwrite = TRUE)

DAAC1821_21Smetrics <- plot_metrics(DAAC1821_21Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_21S, radius = 12.5)
DAAC1821_21Smetrics <- st_transform(DAAC1821_21Smetrics, "EPSG:32643")
sf::st_write(DAAC1821_21Smetrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/DAAC1821_21Smetrics.fgb", delete_dsn = TRUE, overwrite = TRUE)


#DAAC18_19Smetrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/DAAC18_19Smetrics.fgb")
#CAUT23_20Smetrics1 <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/CAUT23_20Smetrics1.fgb")
#CAUT23_20Smetrics2 <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/CAUT23_20Smetrics2.fgb")
#DAAC1821_21Smetrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/DAAC1821_21Smetrics.fgb")


# Remove duplicated rows
DAAC18_19Smetrics <- distinct(DAAC18_19Smetrics)
CAUT23_20Smetrics1 <- distinct(CAUT23_20Smetrics1)
CAUT23_20Smetrics2 <- distinct(CAUT23_20Smetrics2)
DAAC1821_21Smetrics <- distinct(DAAC1821_21Smetrics)


# Assuming columns are in the same order and just have different names
colnames(CAUT23_20Smetrics1) <- colnames(DAAC18_19Smetrics)
colnames(CAUT23_20Smetrics2) <- colnames(DAAC18_19Smetrics)
colnames(DAAC1821_21Smetrics) <- colnames(DAAC18_19Smetrics)


# Combine the dataframes into one dataframe using bind_rows
merged_df <- bind_rows(DAAC18_19Smetrics, 
                       CAUT23_20Smetrics1,
                       CAUT23_20Smetrics2,
                       DAAC1821_21Smetrics)

# Remove rows with NAs in the 'rhz' column and filter for required columns

allGEDI2AB_ALS <- merged_df %>%
  filter(!is.na(rhz95)) %>%
  dplyr::select(year, solar_elevation, elev_highestreturn, elev_lowestmode, rh0, rh5, rh10, rh15,
         rh20, rh25, rh30, rh35, rh40, rh45, rh50, rh55, rh60, rh65, rh70, rh75, rh80, rh85,
         rh90, rh95, rh96, rh97, rh98, rh99, rh100, sensitivity, shot_number, degrade_flag, ALS_CRS, ALS_year,
         cover, pai, fhd_normal, pgap_theta, pgap_theta_error, omega, modis_treecover, geometry, rhz5, rhz10, rhz15, rhz20, 
         rhz25, rhz30, rhz35, rhz40, rhz45, rhz50, rhz55, rhz60, rhz65, rhz70, rhz75, rhz80, rhz85, 
         rhz90, rhz95, rhz96, rhz97, rhz98, rhz99, max, cancov, z_kurt, z_skew)


sf::st_write(allGEDI2AB_ALS, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb", delete_dsn = TRUE, overwrite = TRUE)
#allGEDI2AB_ALS <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb")


# Tidy the environment
rm(merged_df, DAAC18_19Smetrics, DAAC1821_21Smetrics, CAUT23_20Smetrics1, CAUT23_20Smetrics2, 
   allGEDI2AB_19S, allGEDI2AB_20S, allGEDI2AB_21S, DAAC18_19Sfinal, DAAC1821_21Sfinal, CAUT23_20Sfinal1,
   CAUT23_20Sfinal2)

# ------- Forest spectral classification UNFINISHED GEE REFERENCE ---------

# ##### Need to put in file structure here - maybe list at the bottom of the Google Earth Engine code??? 
#ALSO CHECK THE TITLES OF SECTIONS

# (1) MapBiomas version 9 secondary forest of Brazil maps

# Run function to pre-process secondary forest multispectral data from files for each year of ALS collection
process_secondary_forest(2023) 

process_secondary_forest(2021) 

process_secondary_forest(2018) 



# (2) Fire data

# Mapbiomas fire frequency layer downloaded from Google Earth Engine toolkit below (version 3.0)
# for states Rondonia, Amazonas, Para and Mato Grosso
# git clone https://earthengine.googlesource.com/users/mapbiomas/user-toolkit
# Updated link: https://earthengine.googlesource.com/users/mapbiomas/user-toolkit/+/59f6cf84a1c91fb9fe116c939910bb8453302e60/mapbiomas-user-toolkit-download-mosaics.js


# Run function to pre-process fire frequency multispectral data (uses functions from process_secondary_forest)
process_fire_frequency(2023)

process_fire_frequency(2021)

process_fire_frequency(2018)


# Run function to pre-process TSF (time since fire) multispectral data (uses functions from process_secondary_forest)
process_time_since_fire(2023) 

process_time_since_fire(2021) 

process_time_since_fire(2018) 



# (3) Forest validation layer

# Validate if forest extent is still classified as forest after burn events

# Run function to pre-process land cover (forest) multispectral data (uses functions from process_secondary_forest)
process_forest_validation(2023)

process_forest_validation(2021)

process_forest_validation(2018)




# ----------- GEDI classification extraction EDIT 5 IN WRITE TO FILE --------------

# Using a function that extracts raster values for each ALS year per GEDI point:
# Define raster file paths
raster_base_path <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data"

# Load rasters into lists for easy access
secondary_forest_rasters <- list(
  "2018" = raster(paste0(raster_base_path, "/Secondary_forest_classification/2018/secondaryforest2018.tif")),
  "2021" = raster(paste0(raster_base_path, "/Secondary_forest_classification/2021/secondaryforest2021.tif")),
  "2023" = raster(paste0(raster_base_path, "/Secondary_forest_classification/2023/secondaryforest2023.tif"))
)

burn_frequency_rasters <- list(
  "2018" = raster(paste0(raster_base_path, "/Fire_data/2018/fire_frequency_2018.tif")),
  "2021" = raster(paste0(raster_base_path, "/Fire_data/2021/fire_frequency_2021.tif")),
  "2023" = raster(paste0(raster_base_path, "/Fire_data/2023/fire_frequency_2023.tif"))
)

time_since_fire_rasters <- list(
  "2018" = raster(paste0(raster_base_path, "/Fire_data/2018/time_since_fire_2018.tif")),
  "2021" = raster(paste0(raster_base_path, "/Fire_data/2021/time_since_fire_2021.tif")),
  "2023" = raster(paste0(raster_base_path, "/Fire_data/2023/time_since_fire_2023.tif"))
)

forest_validation_rasters <- list(
  "2018" = raster(paste0(raster_base_path, "/Forest_Validation/2018/forest_validation_2018.tif")),
  "2021" = raster(paste0(raster_base_path, "/Forest_Validation/2021/forest_validation_2021.tif")),
  "2023" = raster(paste0(raster_base_path, "/Forest_Validation/2023/forest_validation_2023.tif"))
)


# Run multispectral data point extractions for all GEDI dataframes

allGEDI2A_extraction <- process_raster_extractions(allGEDI2A, secondary_forest_rasters, burn_frequency_rasters, time_since_fire_rasters, forest_validation_rasters)
allGEDI2AB_ALS_extraction <- process_raster_extractions(allGEDI2AB_ALS, secondary_forest_rasters, burn_frequency_rasters, time_since_fire_rasters, forest_validation_rasters)
allGEDI_extraction <- process_raster_extractions(allGEDI, secondary_forest_rasters, burn_frequency_rasters, time_since_fire_rasters, forest_validation_rasters)
allGEDI2AB_amp_extraction <- process_raster_extractions(allGEDI2AB_amp, secondary_forest_rasters, burn_frequency_rasters, time_since_fire_rasters, forest_validation_rasters)
allGEDI2AB_extraction <- process_raster_extractions(allGEDI2AB, secondary_forest_rasters, burn_frequency_rasters, time_since_fire_rasters, forest_validation_rasters)


# Function that creates a column that uses time since burned and ALS collection year to 
# age how many years since fire

allGEDI2A_extraction <- calculate_years_since_fire(allGEDI2A_extraction)
allGEDI2AB_ALS_extraction <- calculate_years_since_fire(allGEDI2AB_ALS_extraction)
allGEDI_extraction <- calculate_years_since_fire(allGEDI_extraction)
allGEDI2AB_amp_extraction <- calculate_years_since_fire(allGEDI2AB_amp_extraction)
allGEDI2AB_extraction <- calculate_years_since_fire(allGEDI2AB_extraction)


# Function to classify forest condition based upon decision tree for processed multi-spectral data conditions

allGEDI2A_extraction <- classify_forest_state(allGEDI2A_extraction)
allGEDI2AB_ALS_extraction <- classify_forest_state(allGEDI2AB_ALS_extraction)
allGEDI_extraction <- classify_forest_state(allGEDI_extraction)
allGEDI2AB_amp_extraction <- classify_forest_state(allGEDI2AB_amp_extraction)
allGEDI2AB_extraction <- classify_forest_state(allGEDI2AB_extraction)

# Function to tidy datasets

# Define target CRS (EPSG:4326)
common_crs <- st_crs(allGEDI)

allGEDI2A <- tidy_classification(allGEDI2A_extraction, common_crs)
allGEDI2AB_ALS <- tidy_classification(allGEDI2AB_ALS_extraction, common_crs)
allGEDI <- tidy_classification(allGEDI_extraction, common_crs)
allGEDI2AB_amp <- tidy_classification(allGEDI2AB_amp_extraction, common_crs)
allGEDI2AB <- tidy_classification(allGEDI2AB_extraction, common_crs)


# Function to check forest_state counts per class

allGEDI_summary <- count_forest_state(allGEDI, "allGEDI")
allGEDI2A_summary <- count_forest_state(allGEDI2A, "allGEDI2A")
allGEDI2AB_summary <- count_forest_state(allGEDI2AB, "allGEDI2AB")
allGEDI2AB_ALS_summary <- count_forest_state(allGEDI2AB_ALS, "allGEDI2AB_ALS")
allGEDI2AB_amp_summary <- count_forest_state(allGEDI2AB_amp, "allGEDI2AB_amp")

# Combine summaries into table
forest_state_summaries <- bind_rows(
  allGEDI_summary, 
  allGEDI2A_summary, 
  allGEDI2AB_summary, 
  allGEDI2AB_ALS_summary, 
  allGEDI2AB_amp_summary
)



# Write to file
sf::st_write(allGEDI2A, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2AB_ALS, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2AB_amp, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_amp.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2AB, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb", delete_dsn = TRUE, overwrite = TRUE)


#allGEDI2A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A5.fgb")
#allGEDI2AB <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB5.fgb")
#allGEDI2AB_ALS <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS5.fgb")
#allGEDI <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI5.fgb")
#allGEDI2AB_amp <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_amp5.fgb")


# Tidy environment
rm(list = ls(pattern = "_extraction$|_2018$|_2021$|2023$|summary$"))
rm(secondary_forest_rasters, burn_frequency_rasters, time_since_fire_rasters, forest_validation_rasters, fgb_list)
rm(forest_state_summary_transposed, forest_state_summary_wide, common_crs)


# ------ Statistics (Aim 1)  ---------

# Accuracy assessment: calculate correspondence/ correlation between GEDI and ALS

# Set rh and rhz columns for calculation (for example, rh25 and rhz25)
# Run repeatedly along the GEDI profile e.g. rh25, rh50, rh75, rh95
rh_col <- "rh95"
rhz_col <- "rhz95"

# Initialize an empty list to store results
ccc_results_list <- list()

# Height Calculations
add_ccc_result("All", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col), "Height")
add_ccc_result("PU", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "forest_state == 'PU'"), "Height")
add_ccc_result("SU", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "forest_state == 'SU'"), "Height")
add_ccc_result("PB1", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "forest_state == 'PB1'"), "Height")
add_ccc_result("PB2", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "forest_state == 'PB2'"), "Height")
add_ccc_result("PB3+", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "forest_state == 'PB3+'"), "Height")
add_ccc_result("SB1", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "forest_state == 'SB1'"), "Height")
add_ccc_result("SB2", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "forest_state == 'SB2'"), "Height")
add_ccc_result("SB3+", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "forest_state == 'SB3+'"), "Height")

# Canopy Cover Calculations (fixed, not based upon rh value)
add_ccc_result("All", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov"), "Canopy Cover")
add_ccc_result("PU", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "forest_state == 'PU'"), "Canopy Cover")
add_ccc_result("SU", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "forest_state == 'SU'"), "Canopy Cover")
add_ccc_result("PB1", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "forest_state == 'PB1'"), "Canopy Cover")
add_ccc_result("PB2", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "forest_state == 'PB2'"), "Canopy Cover")
add_ccc_result("PB3+", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "forest_state == 'PB3+'"), "Canopy Cover")
add_ccc_result("SB1", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "forest_state == 'SB1'"), "Canopy Cover")
add_ccc_result("SB2", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "forest_state == 'SB2'"), "Canopy Cover")
add_ccc_result("SB3+", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "forest_state == 'SB3+'"), "Canopy Cover")

# Combine all results into a single dataframe
ccc_results <- do.call(rbind, ccc_results_list)

# Print the results as a table
print(ccc_results)



# RMSE calculations Inspired by Dorado et al (2021)
# Functions to calculate RMSE, Bias, fit a linear model for rh97 for given forest conditions. 
# Edit function for different 'rh's'


# RMSE calculations
conditions <- c(
  "All",  "forest_state == 'PU'",  
  "forest_state == 'PB1'", 
  "forest_state == 'PB2'", 
  "forest_state == 'PB3+'",
  "forest_state == 'SU'", 
  "forest_state == 'SB1'", 
  "forest_state == 'SB2'", 
  "forest_state == 'SB3+'" )

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
  
  stats <- calculate_stats(burned_subset, rhz_col, rh_col)
  rmse_m[i] <- stats$rmse
  rrmse[i] <- (stats$rmse / mean_actual) * 100
  bias_m[i] <- stats$bias
  relative_bias[i] <- (stats$bias / mean_actual) * 100
  lins_cc[i] <- calculate_ccc(burned_subset, rh_col, rhz_col)  # Calculate Lin's CCC
}

# Create a dataframe to store the results
stats_results <- data.frame(
  Forest_Condition = c("All", "PU", "PB1", "PB2", "PB3+", "SU","SB1", "SB2", "SB3+"),
  Pearsons_r = pearsons_r,
  p_value = p_values,  # Include p-value for significance testing
  RMSE_m = rmse_m,
  rRMSE = rrmse,
  Bias_m = bias_m,
  Relative_Bias = relative_bias,
  Lins_CCC = lins_cc  # Add Lin's CCC to the dataframe
)

# Format all numeric columns except p_value to one decimal place
stats_results <- stats_results %>%
  mutate(across(where(is.numeric), ~ sprintf("%.1f", .)))  # Format to 1 decimal place

# Ensure p_value keeps its conditional notation
stats_results$p_value <- ifelse(stats_results$p_value < 0.0001, 
                                "< 0.0001", 
                                sprintf("%.4f", as.numeric(stats_results$p_value)))
# Print the results
print(stats_results)

#Tidy dataset
rm(ccc_results_list, stats, burned_subset)


## FIGURE: Relative height correspondence

# Define Forest Class categories and color mapping
forest_state_categories <- c("PU", "PB1", "PB2", "PB3+", "SU", "SB1", "SB2", "SB3+")
forest_state_colors <- c(
  "All" = "midnightblue",
  "PU" = "#1F65CC",
  "PB1" = "#4AA4DE", 
  "PB2" = "#9ECAE1",
  "PB3+" = "pink",
  "SU" ="palevioletred1",
  "SB1" =  "firebrick1",
  "SB2" = "red3",
  "SB3+" = "red4"
)

# Set up the Lin's CCC calculations for each pair of ALS and GEDI columns
allGEDI2AB_ALS_height_long <- allGEDI2AB_ALS %>%
  pivot_longer(cols = starts_with("rhz"), names_to = "ALS", values_to = "ALS_value") %>%
  pivot_longer(cols = starts_with("rh"), names_to = "GEDI", values_to = "GEDI_value") %>%
  filter((ALS == "rhz25" & GEDI == "rh25") | 
           (ALS == "rhz50" & GEDI == "rh50") |
           (ALS == "rhz75" & GEDI == "rh75") |
           (ALS == "rhz95" & GEDI == "rh95"))


# Order the forest_state types for output
allGEDI2AB_ALS_height_long <- allGEDI2AB_ALS_height_long %>%
  mutate(forest_state = factor(forest_state, levels = forest_state_categories))

# Define the limits for both x and y axes for the rhz plots
common_limits <- c(0, 60)

# Calculate Lin's CCC for each height pair
correlations_height_ccc <- allGEDI2AB_ALS_height_long %>%
  group_by(ALS, GEDI) %>%
  summarise(ccc_value = as.numeric(calculate_ccc(cur_data(), "ALS_value", "GEDI_value")), .groups = 'drop') %>%
  mutate(annotation = paste0("CCC = ", round(ccc_value, 2)))

# Combine all height pairs for a single plot with facets
rh_correspondence <- allGEDI2AB_ALS_height_long %>%
  ggplot(aes(x = ALS_value, y = GEDI_value, color = forest_state)) +
  geom_point(size = 0.4, alpha = 0.7) +
  geom_text(data = correlations_height_ccc, aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed", size = 0.5) +
  labs(x = "ALS Relative Height (m)", y = "GEDI Relative Height (m)", color = "Forest Class") +
  theme_fancy() +
  scale_colour_manual(
    values = forest_state_colors,
    breaks = names(forest_state_colors)  # Ensure order and mapping remain correct
  ) +
  coord_fixed(ratio = 1, xlim = common_limits, ylim = common_limits) +
  facet_wrap(~ ALS, scales = "fixed") +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

rh_correspondence <- rh_correspondence + theme(legend.position = "bottom")


rh_correspondence


# FIGURE: Calculating pairwise correspondence (Lin's CCC) along the 
# Relative height profile for given forest_state types

# Define the rh and rhz columns at every 5 intervals
rh_columns <- paste0("rh", seq(5, 95, by = 5))
rhz_columns <- paste0("rhz", seq(5, 95, by = 5))

# Initialize a list to store results
results_list <- list()

# Loop through each condition and calculate Lin's CCC for each rh-rhz pair
for (condition in conditions) {
  if (condition == "All") {
    # For "All", use the full data set without filtering
    data_subset <- allGEDI2AB_ALS
  } else {
    # For other conditions, filter the data set
    data_subset <- allGEDI2AB_ALS %>% filter(eval(parse(text = condition)))
  }
  
  # Calculate Lin's CCC for each rh-rhz pair in this subset
  for (i in seq_along(rh_columns)) {
    rh_col <- rh_columns[i]
    rhz_col <- rhz_columns[i]
    
    # Calculate Lin's CCC for the specified subset
    lins_ccc <- calculate_ccc(data_subset, rh_col, rhz_col)
    
    # Store results
    results_list <- append(results_list, list(data.frame(
      Condition = ifelse(condition == "All", "All", condition),
      Pair = paste(rh_col, rhz_col, sep = "-"),
      Lins_CCC = as.numeric(lins_ccc)  # Convert to numeric if needed
    )))
  }
}

# Combine all results into a single dataframe
ccc_pair_results <- do.call(rbind, results_list)

# Clean up the Condition and Pair columns for consistent labeling
ccc_pair_results <- ccc_pair_results %>%
  mutate(Condition = case_when(
    Condition == "All" ~ "All",
    Condition == "forest_state == 'PU'" ~ "PU",
    Condition == "forest_state == 'PB1'" ~ "PB1",
    Condition == "forest_state == 'PB2'" ~ "PB2",
    Condition == "forest_state == 'PB3+'" ~ "PB3+",
    Condition == "forest_state == 'SU'" ~ "SU",
    Condition == "forest_state == 'SB1'" ~ "SB1",
    Condition == "forest_state == 'SB2'" ~ "SB2",
    Condition == "forest_state == 'SB3+'" ~ "SB3+",
    TRUE ~ NA_character_  # Set unknown/missing values as NA
  )) %>%
  mutate(Pair = str_extract(Pair, "\\d+"))

# Print the cleaned data to verify
print(ccc_pair_results)






# FIGURE: Correlations for complete relative height profile for each forest_state type


# Ensure the Pair column in ccc_pair_results is ordered correctly
ccc_pair_results$Pair <- factor(ccc_pair_results$Pair, levels = as.character(seq(5, 95, 5)))

# Include all forest classes including "All"
ccc_results_data_filtered <- ccc_pair_results %>%
  filter(Condition %in% c("All", forest_state_categories)) %>%
  mutate(Condition = factor(Condition, levels = c("All", forest_state_categories)))

# Verify that the filtered data has content
print(ccc_results_data_filtered)

# Find the maximum Lin's CCC value for each condition
max_lins_ccc <- ccc_results_data_filtered %>%
  group_by(Condition) %>%
  slice_max(order_by = Lins_CCC, n = 1)


forest_state_categories <- c("PU", "PB1", "PB2", "PB3+", "SU", "SB1", "SB2", "SB3+")
forest_state_colors <- c(
  "All" = "midnightblue",
  "PU" = "#1F65CC",
  "PB1" = "#4AA4DE", 
  "PB2" = "#9ECAE1",
  "PB3+" = "pink",
  "SU" ="palevioletred1",
  "SB1" =  "firebrick1",
  "SB2" = "red3",
  "SB3+" = "red4"
)

# Create the plot using Lin's CCC values
plot_pairwise <- ccc_results_data_filtered %>%
  ggplot(aes(x = Pair, y = Lins_CCC, color = Condition)) +
  geom_point(size = 1) +
  geom_line(aes(group = Condition), size = 0.5) +
  geom_point(data = max_lins_ccc, aes(x = Pair, y = Lins_CCC), size = 4, shape = 18, alpha = 0.7, show.legend = FALSE) +
  labs(x = "Pair (GEDI rh - ALS rhz)", y = "Lin's CCC") +
  theme_fancy() +
  scale_color_manual(values = forest_state_colors) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  guides(color = guide_legend(title = "Forest Class", title.position = "top", title.hjust = 0.5))


# Legend position for plot_pairwise 
figure1 <- plot_pairwise + theme(legend.position = "bottom")

figure1

# Save the figure
ggsave(figure1,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/Correspondence_height.png",
       width = 16, height = 13, units = "cm"
)




# FIGURE: Correspondence for relative height and canopy cover per degradation type


# Ensure the forest_state column is a factor with the desired levels
allGEDI2AB_ALS <- allGEDI2AB_ALS %>%
  mutate(forest_state = factor(forest_state, levels = forest_state_categories))

# Calculate Lin's CCC for relative height
ccc_forest_state_height <- allGEDI2AB_ALS %>%
  group_by(forest_state) %>%
  summarise(ccc_value = as.numeric(calculate_ccc(cur_data(), "rh95", "rhz95")), .groups = 'drop') %>%
  mutate(annotation = paste0("CCC = ", round(ccc_value, 2)))

# Canopy Cover Data
allGEDI2AB_ALS_cover_long <- allGEDI2AB_ALS %>%
  pivot_longer(cols = starts_with("can"), names_to = "ALS", values_to = "ALS_value") %>%
  pivot_longer(cols = "cover", names_to = "GEDI", values_to = "GEDI_value") %>%
  filter(ALS == "cancov")

# Ensure correct order
allGEDI2AB_ALS_cover_long <- allGEDI2AB_ALS_cover_long %>%
  mutate(forest_state = factor(forest_state, levels = forest_state_categories))

# Calculate Lin's CCC for canopy cover
ccc_forest_state_cover <- allGEDI2AB_ALS_cover_long %>%
  group_by(forest_state) %>%
  summarise(ccc_value = as.numeric(calculate_ccc_c(cur_data(), "ALS_value", "GEDI_value")), .groups = 'drop') %>%
  mutate(annotation = paste0("CCC = ", round(ccc_value, 2)))



# Compute density for both datasets with normalized values
allGEDI2AB_ALS$density <- get_density(allGEDI2AB_ALS$rhz95, allGEDI2AB_ALS$rh95)
allGEDI2AB_ALS_cover_long$density <- get_density(allGEDI2AB_ALS_cover_long$ALS_value, allGEDI2AB_ALS_cover_long$GEDI_value)

# Find global min/max density across both datasets (after normalization)
global_min_density <- min(c(allGEDI2AB_ALS$density, allGEDI2AB_ALS_cover_long$density), na.rm = TRUE)
global_max_density <- max(c(allGEDI2AB_ALS$density, allGEDI2AB_ALS_cover_long$density), na.rm = TRUE)

# Define shared color scale with fixed limits
shared_color_scale <- scale_color_viridis_c(
  option = "A",
  limits = c(global_min_density, global_max_density),  # Ensure same density scale
  guide = guide_colorbar(barwidth = 6, barheight = 0.3)  # Smaller legend
)

# Relative height plot
ccc_height_deg <- allGEDI2AB_ALS %>%
  ggplot(aes(x = rhz95, y = rh95, color = density)) +
  geom_point(size = 0.7, alpha = 0.7) +  # Larger points, semi-transparent
  shared_color_scale +  
  geom_abline(slope = 1, intercept = 0, color = "gray70", linetype = "dashed", size = 0.7) +
  geom_text(data = ccc_forest_state_height, aes(x = 30, y = 60, label = annotation),  
            size = 2.5, color = "black") +  
  labs(x = "ALS Relative Height (m)", y = "GEDI Relative Height (m)", color = "Density") + 
  theme_bw() +
  facet_wrap(~ forest_state, ncol = 8) +  
  coord_cartesian(ylim = c(0, 60)) +
  theme_fancy() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "none"
  )

# Canopy cover plot
ccc_cover_deg <- allGEDI2AB_ALS_cover_long %>%
  ggplot(aes(x = ALS_value, y = GEDI_value, color = density)) +
  geom_point(size = 0.7, alpha = 0.7) +  
  shared_color_scale +  
  geom_abline(slope = 1, intercept = 0, color = "gray70", linetype = "dashed", size = 0.7) +
  geom_text(data = ccc_forest_state_cover, aes(x = 0.5, y = 1.01, label = annotation),  
            size = 2.5, color = "black") +  
  labs(x = "ALS Canopy Cover", y = "GEDI Canopy Cover", color = "Density") + 
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +  # Custom x-axis breaks
  theme_bw() +  
  coord_cartesian(ylim = c(0, 1)) +  
  facet_wrap(~ forest_state, ncol = 8) +  # Faceting is separate from theme()
  theme_fancy() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "none"
  )

# Combine with one shared legend (smaller and below)
figure2 <- (ccc_height_deg / ccc_cover_deg) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") #&  
  #theme(
   # legend.position = "bottom",
    #legend.text = element_text(size = 8),
    #legend.title = element_text(size = 9)
 # )

# Display the figure
print(figure2)


# Save the figure
ggsave(figure2,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/Correspondence_forest_state_density.png",
       width = 19, height = 12, units = "cm"
)



# Tidy environemnt
rm(allGEDI2AB_ALS_height_long, allGEDI2AB_ALS_cover_long, ccc_results_list, 
  cor_results, ccc_pair_results, ccc_results_data_filtered, ccc_forest_state_height, 
  ccc_forest_state_cover, rh_correspondence, plot_pairwise, ccc_height_deg, 
  ccc_cover_deg, conditions, rh_columns, rhz_columns, results_list, common_limits,
  max_lins_ccc, data_subset, burned_subset, correlations_height_ccc)


# ------- GEDI Gradient (Aim 2) -------


## FIGURE 3 - GEDI Forest Class data across Amazonia 

# Ordering of age within plots
age_order <- c("<7", "7-15", "15-25", "25-40", "n/a")

allGEDI2AB <- allGEDI2AB %>%
  mutate(
    age_category = factor(age_category, levels = age_order))
   

allGEDI <- allGEDI %>%
  mutate(
    age_category = factor(age_category, levels = age_order))
  

allGEDI <- allGEDI %>%
  filter(l4_quality_flag == 1) %>%
  filter(agbd < 800)
 

# Order the Forest Class types for output
allGEDI2AB <- allGEDI2AB %>%
  mutate(forest_state = factor(forest_state, levels = forest_state_categories))

allGEDI <- allGEDI %>%
  mutate(forest_state = factor(forest_state, levels = forest_state_categories))


# Violin plot for canopy height
filtered_data1 <- allGEDI2AB %>%
  group_by(age_category, forest_state) %>%
  filter(var(rh95, na.rm = TRUE) > 0)

height_violin <- filtered_data1 %>%
  ggplot(aes(x = age_category, y = rh95, color = forest_state, fill = forest_state)) +
  geom_violin(width = 0.9, position = position_dodge(width = 0.7), scale = "width", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.7), 
               shape = 23, size = 1, color = "black", fill = "white", aes(group = forest_state)) +
  stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 1), 
               aes(label = round(..y.., 1), group = forest_state), vjust = -1.5, size = 2.5) +
  labs(y = "Canopy Height (m)", x = NULL, fill = "Forest Class", color = "Forest Class") + 
  theme_bw() +
  scale_colour_manual(values = forest_state_colors) +
  scale_fill_manual(values = forest_state_colors) +
  ylim(0, 50) +
  theme_fancy() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.1, 0, 0.1, 0), "cm") 
  )

# Violin plot for canopy cover

# Filter out groups where the violin plot would not be visible
filtered_data2 <- allGEDI2AB %>%
  group_by(age_category, forest_state) %>%
  filter(var(cover, na.rm = TRUE) > 0)  # Keep groups with non-zero variance

cover_violin <- filtered_data2 %>%
  ggplot(aes(x = age_category, y = cover, color = forest_state, fill = forest_state)) +
  geom_violin(width = 0.9, position = position_dodge(width = 0.7), scale = "width", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.7), 
               shape = 23, size = 1, color = "black", fill = "white", aes(group = forest_state)) +
  stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 1), 
               aes(label = round(..y.., 1), group = forest_state), vjust = -1.5, size = 2.5) +
  labs(y = "Canopy Cover", fill = "Forest Class", color = "Forest Class") +  
  theme_bw() +
  scale_colour_manual(values = forest_state_colors) +
  scale_fill_manual(values = forest_state_colors) +
  theme_fancy() +
  theme(
    axis.title.x = element_blank(),  
    axis.text.x = element_blank(),   
    axis.ticks.x = element_blank(),  
    plot.margin = unit(c(0.1, 0, 0.1, 0), "cm")
  )

# Violin plot for AGBD

filtered_data3 <- allGEDI %>%
  group_by(age_category, forest_state) %>%
  filter(var(agbd, na.rm = TRUE) > 0)

agbd_violin <- filtered_data3 %>%
  ggplot(aes(x = age_category, y = agbd, color = forest_state, fill = forest_state)) +
  geom_violin(width = 0.9, position = position_dodge(width = 0.7), scale = "width", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.7), 
               shape = 23, size = 1, color = "black", fill = "white", aes(group = forest_state)) +
  stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 1), 
               aes(label = round(..y.., 1), group = forest_state), vjust = -1.5, size = 2.5) +
  labs(y = "AGBD (Mg/ha)", x = "Years Since Fire", fill = "Forest Class", color = "Forest Class") + 
  theme_bw() +
  scale_colour_manual(values = forest_state_colors) +
  scale_fill_manual(values = forest_state_colors) +
  ylim(0, 600) +
  theme_fancy() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.1, 0, 0.1, 0), "cm")
  )

# Legend position for plot
cover_violin <- cover_violin +
  theme(
    legend.position = "right",
    legend.margin = margin(-1, 0, 0, 0, unit = "cm"),  # Moves legend slightly up
    legend.box.margin = margin(0, 0, 0, 0, unit = "cm")  # Keeps spacing clean
  )

# Combine the plots using patchwork and reduce space between plots
figure3 <- (height_violin / cover_violin / agbd_violin) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.margin = unit(c(0.1, 0, 0.1, 0), "cm"))


# Print the combined plot
print(figure3)

ggsave(figure3,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/GEDI_gradient.png",
       width = 18, height = 14, units = "cm"
)



# # Violin plot for PAI
# pai_violin <- allGEDI %>%
#   ggplot(aes(x = age_category, y = pai, color = forest_state, fill = forest_state)) +
#   geom_violin(width = 0.9, position = position_dodge(width = 0.7), scale = "width", alpha = 0.7) +
#   stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.7), 
#                shape = 23, size = 1, color = "black", fill = "white", aes(group = forest_state)) +
#   stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 1), 
#                aes(label = round(..y.., 1), group = forest_state), vjust = -1.5, size = 3) +
#   labs(y = "AGBD (Mg/ha)", x = "Forest Age") +
#   theme_bw() +
#   scale_colour_manual(values = forest_state_colors) +
#   scale_fill_manual(values = forest_state_colors) +
#   ylim(0, 8) +
#   theme(
#     legend.position = "none",
#     plot.margin = unit(c(0.1, 0, 0.1, 0), "cm")  # Minimize horizontal spacing
#   ) &
#   labs(color = "Condition")  
# 
# print(pai_violin)


# Tidy environment
rm(filtered_data1, filtered_data2, filtered_data3, age_order, height_violin, cover_violin, agbd_violin)


# -------- Principle Component Analysis/ ML model (Aim 3) ---------

# PCA for mixture of GEDI variables and height/ waveform summaries to assess forest condition

# Keep the Forest Class variable separate before PCA
forest_state_type <- allGEDI2AB$forest_state

# Create PCA dataset, removing unnecessary columns for PCA
allGEDI2ABPCA <- allGEDI2AB %>%
  dplyr::select(-starts_with("rh"), rh_min, Rh_slope, Rh_variance, Rh_intercept, rh_sd, rh_skew, rh_kurt, rh_mean,
         rh_max, rh100, rh99, rh95, rh75, rh50, rh25, max_amp, W_variance, W_slope, W_intercept, pgap_theta,
         fhd_normal, pai, cover, num_detectedmodes, -beam, -year, -solar_elevation, -elev_highestreturn,
         -elev_lowestmode,-sensitivity, -shot_number, -degrade_flag, -modis_treecover, -landsat_treecover,
         -ALS_CRS, -ALS_year,-l2b_quality_flag, -omega, -TSF, -forest_state, -age_category,
         -burn_frequency, -pgap_theta_error, -geometry)


# Removing variables that ranked lowest in the explanation of variance for both PC's
allGEDI2ABPCA <- allGEDI2ABPCA %>%
  dplyr::select(-rh99, -rh100, -rh_max, -rh25, -rh50, -rh_min, -rh_kurt, rh_skew,
         -rh_sd, -W_variance, -pai, -W_slope, -max_amp, -num_detectedmodes)

allGEDI2ABPCA <- allGEDI2ABPCA %>%
  dplyr::select(cover, fhd_normal, pgap_theta, -W_intercept, Rh_slope, Rh_variance,
                Rh_intercept, -rh_skew, rh_mean, rh95, rh75)

# Final removal of variables after correlation analysis/ reflection of explained variance improvements

allGEDI2ABPCA <- allGEDI2ABPCA %>%
  dplyr::select(cover, fhd_normal, pgap_theta, Rh_slope, -Rh_variance,
                -Rh_intercept, -rh_mean, rh95, rh75)



# # Remove unnecessary columns for PCA, but keep forest_state out for later use (De Conta run)
# allGEDI2ABPCA <- allGEDI %>%
#   dplyr::select(rh98, agbd, fhd_normal, cover, pai)
# 

# Remove geometry
allGEDI2ABPCA <- allGEDI2ABPCA %>%
  st_drop_geometry()

# Ensure all columns are numeric
allGEDI2ABPCA <- allGEDI2ABPCA %>%
  mutate(across(everything(), as.numeric))

allGEDI2ABPCA <- allGEDI2ABPCA %>%
  drop_na()

# Standardize the data for PCA
scaled_data <- scale(allGEDI2ABPCA)

# Run PCA on the numeric data
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Flip the first principal component if needed
pca_result$x[,1] <- -pca_result$x[,1]
pca_result$rotation[,1] <- -pca_result$rotation[,1]

summary(pca_result)




# Sort loadings to see potentially too similar loadings
loadings_pca <- pca_result$rotation[, c(1, 2, 3)]  # PC1 is the first column, PC2 is the second
loadings_df <- as.data.frame(loadings_pca)
loadings_df$Variable <- rownames(loadings_pca)

# Sort loadings by PC1 in descending order
sorted_PC1 <- loadings_df %>%
  dplyr::select(Variable, PC1) %>%
  arrange(desc(PC1))

# Sort loadings by PC2 in descending order
sorted_PC2 <- loadings_df %>%
  dplyr::select(Variable, PC2) %>%
  arrange(desc(PC2))

# Sort loadings by PC3 in descending order
sorted_PC3 <- loadings_df %>%
  dplyr::select(Variable, PC3) %>%
  arrange(desc(PC3))

print(sorted_PC1)
print(sorted_PC2)
print(sorted_PC3)


# See explaination of variance of top contributing variables
loadings <- pca_result$rotation
pc1_loadings <- loadings[, 1]
pc2_loadings <- loadings[, 2]

# Sort PC loadings by absolute value
sorted_pc1_loadings <- sort(abs(pc1_loadings), decreasing = TRUE)
sorted_pc2_loadings <- sort(abs(pc2_loadings), decreasing = TRUE)

sorted_pc1_loadings
sorted_pc2_loadings


# FIGURE: PCA biplot
# Convert PCA components to a data frame 
components <- as.data.frame(pca_result$x)

# Add the forest_state type back to the components dataframe
components$forest_state <- as.factor(forest_state_type)

# Order the forest_state types for output
components <- components %>%
  mutate(forest_state = factor(forest_state, levels = forest_state_categories))

# Reverse PC2 for visual consistency
components$PC2 <- -components$PC2

# Plot 1: Full Biplot with All Loadings
scale <- 5 # Adjust arrow length scale
biplot_full <- ggplot(data = components, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = forest_state, alpha = ifelse(forest_state == "PU", 0.4, 1)), 
             size = 1.2, shape = 16) +  # Shape 16 removes outlines, size increases visibility
  scale_alpha_identity() +  # Ensures manual alpha settings are applied
  scale_color_manual(values = forest_state_colors, guide = guide_legend(override.aes = list(alpha = 1))) + 
  geom_segment(data = as.data.frame(pca_result$rotation), 
               aes(x = 0, y = 0, xend = PC1 * scale, yend = PC2 * scale), 
               arrow = arrow(length = unit(0.3, "cm"), type = "open", angle = 25), 
               size = 0.8, color = "darkblue") +
  geom_text_repel(data = as.data.frame(pca_result$rotation), 
                  aes(x = PC1 * scale, y = PC2 * scale, label = rownames(pca_result$rotation)), 
                  size = 3, fontface = "bold", color = "black", max.overlaps = 5, 
                  box.padding = 0.35, point.padding = 0.5) +  
  labs(title = "Biplot - PCA", x = "PC1", y = "PC2", color = "Forest Class") +
  theme_minimal() +
  theme_fancy() +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("center", "bottom")
  )


# Display the full biplot
print(biplot_full)

# FIGURE: Squashed PCA axis plots

# Define common x and y axis limits
x_limits <- c(-6, 6)

# Add the forest_state type from forest_state_type to pca_data
pca_data <- as.data.frame(pca_result$x)
pca_data$forest_state <- forest_state_type
pca_data$forest_state <- factor(pca_data$forest_state, levels = c("SB3+", "SB2","SB1", "SU", "PB3+", "PB2", 
                                                                  "PB1", "PU"))


# Compute sample sizes for each forest_state
forest_state_summaries_2AB <- allGEDI2AB %>%
  count(forest_state)

# Create a named vector for facet labels
facet_labels <- setNames(
  paste0(forest_state_summaries_2AB$forest_state, "\n(n=", forest_state_summaries_2AB$n, ")"),  
  forest_state_summaries_2AB$forest_state
)

# Create PC1 plot
fig_pc1 <- ggplot(pca_data, aes(x = PC1, fill = forest_state)) +  
  geom_histogram(binwidth = 0.5, color = "white", size = 0.1, show.legend = FALSE) +  
  scale_fill_manual(values = forest_state_colors) +
  labs(x = "PC1 Values", y = "Frequency") +
  xlim(x_limits) +
  facet_wrap(~ forest_state, nrow = 1, scales = "free_y", 
             labeller = labeller(forest_state = facet_labels)) +  
  theme_minimal() +  
  theme_fancy() &
  theme(
    strip.text = element_text(size = 8, face = "bold"),  # Keep text readable
    axis.text.x = element_text(size = 8)
  )

# Create PC2 plot
fig_pc2 <- ggplot(pca_data, aes(x = PC2, fill = forest_state)) +  
  geom_histogram(binwidth = 0.5, color = "white", size = 0.1, show.legend = FALSE) +  
  scale_fill_manual(values = forest_state_colors) +
  labs(x = "PC2 Values", y = "Frequency") +
  xlim(x_limits) +
  facet_wrap(~ forest_state, nrow = 1, scales = "free_y", 
             labeller = labeller(forest_state = facet_labels)) +  
  theme_minimal() +   
  theme_fancy() &
  theme(
    strip.text = element_text(size = 8, face = "bold"),  
    axis.text.x = element_text(size = 8)
  )



# Combine both PC1 and PC2 panels
fig_pcs <- (fig_pc1) / (fig_pc2)
fig_pcs

# Figure with both panels
figure4 <- (biplot_full) / (fig_pcs) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 8, face = "bold"),
    plot.tag.position = c(0, 1)
  )

print(figure4)

ggsave(figure4,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/PCA.png",
       width = 17, height = 20, units = "cm"
)

# 
# # FIGURE: PC1 Density plot
# 
# fig_5 <- ggplot(pca_data, aes(x = PC1, color = forest_state_type)) +
#   geom_density(size = 1) +  # Set line size for better visibility
#   labs(x = "PC1 Values", y = "Density") +
#   scale_color_manual(values = c(
#     "PU" = "#92c5de",
#     "Logged" = "#0073e6",
#     "Burned 1" = "pink",
#     "Burned 2" = "palevioletred1",
#     "Burned 3" = "palevioletred3",
#     "Burned 4+" = "#ca0020")) +
#   theme_bw() +
#   theme(
#     legend.position = "right",  # Position legend to the right
#     legend.title = element_blank()  # Remove legend title for a cleaner look
#   ) &
#   labs(color = "Condition")  
# 
# print(fig_5)
# 
# # Extracting probability
# 
# burned_4pc1_den <- density(pca_data$PC1[which(pca_data$forest_state == "Burned 4+")], from = -6, to = 6, n = 13)
# nonburned_4pc1_den <- density(pca_data$PC1[which(pca_data$forest_state != "Burned 4+")], from = -6, to = 6, n = 13)
# 
# plot(burned_4pc1_den$x, 100*burned_4pc1_den$y/(burned_4pc1_den$y+nonburned_4pc1_den$y), type = "l",
#      xlab= "PC1 values", ylab = "Probabilty of being Burned", xaxt = "n")
# 
# axis(side=1, at=c(-7:7))



## Multinomial logistic regression model using nnet package to classify probability for each forest condition
# Multiple runs of PCA combinations to test for most suitable forest condition metric

# Add row ID
allGEDI2ABPCA <- allGEDI2ABPCA |>
  dplyr::mutate(rowid = dplyr::row_number())

options(pillar.width = 200)

# Define color palette for Forest Class types
forest_state_colors <- c(
  "PU" = "#1F65CC",
  "PB1" = "#4AA4DE", 
  "PB2" = "#9ECAE1",
  "PB3+" = "pink",
  "SU" ="palevioletred1",
  "SB1" =  "firebrick1",
  "SB2" = "red3",
  "SB3+" = "red4"
)

# Set the factor/ scale levels
rescale_2 <- function(x, to = c(0, 1)) {
  scales::rescale(x, to = to)
}
gpca <- pca_data |>
  dplyr::mutate(
    forest_state = forcats::fct_relevel(
      forest_state,
      "PU", "PB1", "PB2", "PB3+", "SU", "SB1", "SB2", "SB3+"
    ),
    pc_sum = rescale_2(PC1) + rescale_2(PC2),
    pc_ratio = rescale_2(log(rescale_2(PC1) / rescale_2(PC2))),
    pc_ratio = dplyr::case_when(!is.finite(pc_ratio) ~ NA_real_, TRUE ~ pc_ratio),
    pc1_x_pc2 = rescale_2(PC1) * rescale_2(PC2),
    pc1 = rescale_2(PC1),
    pc2 = rescale_2(PC2),
    rowid = dplyr::row_number()
  ) |>
  dplyr::left_join(st_drop_geometry(allGEDI2ABPCA), by = "rowid")
# Multi-performance metrics
mod_perf_tab <- list(
  as.formula(forest_state ~ PC1 + PC2),
  as.formula(forest_state ~ PC1 * PC2),
  as.formula(forest_state ~ PC1),
  as.formula(forest_state ~ PC2),
  as.formula(forest_state ~ pc_sum),
  as.formula(forest_state ~ pc_ratio),
  as.formula(forest_state ~ rh95) # Use rh95 (relative height at 95%) instead of rh99 for consistency
) |>
  purrr::map(~ nnet::multinom(.x, data = gpca)) |>
  purrr::map(performance::model_performance) |>
  purrr::set_names(
    c("PC1 + PC2", "PC1 * PC2", "PC1", "PC2", "PC1_add_PC2", "PC_ratio", "rh95")
  ) |>
  purrr::imap(~ dplyr::mutate(as_tibble(.x), model = .y)) |>
  purrr::list_rbind() |>
  dplyr::relocate(model) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))
# Display model performance metrics
kableExtra::kable(mod_perf_tab, rownames = FALSE)
# Inspect the effects of the PC ratio model
pc_ratio1 <- nnet::multinom(forest_state ~ pc_ratio, data = gpca)
summary(pc_ratio1)
performance::model_performance(pc_ratio1)
gge <- ggeffects::ggemmeans(pc_ratio1, terms = "pc_ratio [all]")
print(gge)
plot(gge)

fit_nplot_mnlr(pc_ratio)


# Stack plots in a 2x2 grid with pc_ratio spanning the full width below
plot_model_sup <- ((fit_nplot_mnlr(pc1) | fit_nplot_mnlr(pc2)) /
                     (fit_nplot_mnlr(pc_sum) | fit_nplot_mnlr(rh95)) /
                     (fit_nplot_mnlr(pc_ratio) | plot_spacer())) +
  plot_layout(guides = "collect", heights = c(1, 1, 1))

plot_model_sup

# Individual PC ratio model plot
plot_model <- fit_nplot_mnlr(pc_ratio) +
  labs(x = "PC1/PC2 ratio (PCR)")
plot_model

# Save the plot
ggsave(
  plot_model,
  filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/model.png",
  width = 16, height = 14, units = "cm"
)

# Save the plot
ggsave(
  plot_model_sup,
  filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/model_supplementary.png",
  width = 16, height = 18, units = "cm"
)




# ---- Supplementary/ extras ------

## Relative height and waveform amplitude profiles (Figure 6)

#  Reshape relative height data to long format and filter by forest_state categories
rh_long <- allGEDI2AB_amp %>%
  st_set_geometry(NULL) %>%  # Remove geometry if present
  filter(forest_state %in% forest_state_categories) %>%  # Filter only the specified forest_state types
  dplyr::select(forest_state, starts_with("rh")) %>%  # dplyr::select relative height columns
  pivot_longer(cols = starts_with("rh"), names_to = "rh_index", values_to = "relative_height") %>%
  mutate(rh_index = as.numeric(gsub("rh", "", rh_index)))  # Extract numeric index

# Calculate the average relative height for each forest_state type and height level
avg_rh_profile <- rh_long %>%
  group_by(forest_state, rh_index) %>%
  summarise(avg_relative_height = mean(relative_height, na.rm = TRUE)) %>%
  ungroup()


# Order the forest_state types for output
avg_rh_profile <- avg_rh_profile %>%
  mutate(forest_state = factor(forest_state, levels = forest_state_categories))

# Plot the average relative height profile by forest_state type
avg_rh <- ggplot(avg_rh_profile, aes(x = rh_index, y = avg_relative_height, color = forest_state, group = forest_state)) +
  geom_line(size = 1) +
  labs(
    x = "Relative Height Percentile",  # Add x-axis label
    y = "Relative Height (m)"     # Shared y-axis label
  ) +
  scale_color_manual(values = forest_state_colors) +
  theme_bw() +
  theme_fancy() +
  theme(
    legend.position = "none") +
  ylim(-5, 30)  # Limit y-axis to 30



# Drop geometry from selected_waveforms to avoid sf-related issues
selected_waveforms <- allGEDI2AB_amp %>%
  filter(forest_state %in% forest_state_categories) %>%
  group_by(forest_state) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  st_set_geometry(NULL)  # Remove geometry to avoid sf issues

# Reshape amplitude data to long format for selected shots
amp_long <- selected_waveforms %>%
  pivot_longer(cols = starts_with("amp_"), names_to = "index", values_to = "amplitude") %>%
  mutate(index = as.numeric(gsub("amp_", "", index)))  # Convert 'amp_' index to numeric

# Reshape relative height data to long format for selected shots
rh_long <- selected_waveforms %>%
  pivot_longer(cols = starts_with("rh"), names_to = "index", values_to = "relative_height") %>%
  mutate(index = as.numeric(gsub("rh", "", index)))  # Convert 'rh' index to numeric

# Combine amplitude and relative height data by shot_number, forest_state, and index
combined_waveform <- inner_join(amp_long, rh_long, by = c("shot_number", "forest_state", "index"))

combined_waveform <- combined_waveform %>%
  mutate(forest_state = factor(forest_state, levels = forest_state_categories))

# Plot the waveform amplitude vs. relative height for each forest_state type (Plot B)
rand_amp <- ggplot(combined_waveform, aes(x = relative_height, y = amplitude, color = forest_state, group = forest_state)) +
  geom_line(size = 1) +
  coord_flip() +  # Flip axes for profile orientation
  scale_color_manual(values = forest_state_colors) +
  labs(
    x = "Amplitude",  # Keep individual x-axis label
    y = NULL          # Remove redundant y-axis label (shared y-axis from avg_rh)
  ) +
  theme_bw() +
  theme_fancy() +
  theme(
    legend.position = "right",  # Place legend on the right for Plot B
    axis.title.y = element_blank(),  # Suppress redundant y-axis title
    axis.text.y = element_blank(),   # Suppress y-axis text
    axis.ticks.y = element_blank()   # Suppress y-axis ticks
  ) +
  labs(color = "Forest Class")

# Combine the two plots side by side
figure6 <- (avg_rh | rand_amp) +
  plot_annotation(
    tag_levels = 'A',          # Add subplot annotations
    caption = "Amplitude"      # Add a caption below the plot
  ) &
  theme(
    plot.tag = element_text(size = 8, face = "bold"),        # Style for subplot tags
    plot.caption = element_text(size = 10, hjust = 0.7, margin = margin(t = -20))
  )

# Display the combined plot
print(figure6)


ggsave(figure6,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/Rh_amp3.png",
       width = 16, height = 12, units = "cm"
)








## (Figure S2) Correlation analysis used to aid informing the removal of highly correlated 
# loadings in PCA (run before the PCA is applied)

correlation_df <- melt(correlation_matrix)

# Plot the heatmap with a custom color gradient
correlation <- ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Create the heatmap tiles
  scale_fill_gradientn(colors = c("royalblue4", "royalblue1", "white", "firebrick2", "firebrick4"),
                       limits = c(-1, 1),  # Ensure gradient spans -1 to 1
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), size = 1.5) +  # Add correlation values
  theme_fancy() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()) +
   theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0, vjust = 1),  # Adjust x-axis text
    axis.text.y = element_text(hjust = 1),
    axis.title = element_blank()
  ) +
  scale_x_discrete(position = "top") + # Move x-axis to the top
  theme(legend.position = "right")
correlation 

ggsave(correlation,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/correlation.png",
       width = 16, height = 12, units = "cm"
)





## PCA run with De Conto et al 2024 GEDI variables

# Keep the forest_state variable separate before PCA
forest_state_type <- allGEDI$forest_state

# Remove unnecessary columns for PCA, but keep forest_state out for later use
allGEDIBPCA <- allGEDI %>%
  dplyr::select(agbd, cover, fhd_normal, rh98, pai)


# Rename to slot into code
allGEDI2ABPCA <- allGEDIBPCA






## (Table S4) Impact of sensitivity filtering GEDI

# Ensure the original data is PU by creating separate filtered datasets
allGEDI2ABfilter1 <- allGEDI2AB %>%
  filter(sensitivity > 0.98)

allGEDI2ABfilter2 <- allGEDI2AB %>%
  filter(sensitivity > 0.98 & solar_elevation < 0)

# Create combined data with an indicator for each filter
forest_state_combined <- bind_rows(
  allGEDI2AB %>%
    mutate(Filter = "Original"),
  allGEDI2ABfilter1 %>%
    mutate(Filter = "Sensitivity > 0.98"),
  allGEDI2ABfilter2 %>%
    mutate(Filter = "Sensitivity > 0.98 & Solar Elevation < 0")
)

# Count samples by forest_state type and Filter
forest_state_counts_combined <- forest_state_combined %>%
  group_by(Filter, forest_state) %>%
  summarise(Sample_Count = n(), .groups = "drop") %>%
  mutate(forest_state = factor(forest_state, levels = c("PU", "Logged", "Burned 1", "Burned 2", "Burned 3", "Burned 4+")))

# Check the output to ensure counts are correct
print(forest_state_counts_combined)

# Plot using facet_wrap to display each filter condition in separate panels, side by side
figure7 <- ggplot(forest_state_counts_combined, aes(x = forest_state, y = Sample_Count, fill = forest_state)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Sample_Count), vjust = -0.5, size = 4, color = "black") +
  labs(x = "Forest Condition", y = "Sample Count") +
  theme_minimal() +
  scale_fill_manual(name = "Condition", values = forest_state_colors) +
  theme_fancy() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 8)  # Adjust facet labels
  ) +
  facet_wrap(~ Filter, ncol = 3) +  # Arrange plots side by side (3 columns)
theme(
  legend.position = c(0.9, 0.5),  # Adjusts the position (x, y) relative to the plot
  legend.justification = c("center", "bottom")  # Anchors the legend to its new position
)


figure7 <- figure7 + 
  plot_annotation(tag_levels = 'A')  # Add A, B, C labels to facets

# Plot the combined figure
plot(figure7)

# Save the figure
ggsave(figure7,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/Sample_count.png",
       width = 20, height = 14, units = "cm"
)



# Accuracy assessment RERUN FOR FILTERED GEDI CLASSES: calculate correspondence/ correlation between GEDI and ALS

allGEDI2AB_ALSfilter1 <- allGEDI2AB_ALS %>%
  filter(sensitivity > 0.98)

allGEDI2AB_ALSfilter2 <- allGEDI2AB_ALS %>%
  filter(sensitivity > 0.98 & solar_elevation < 0)


# Set rh and rhz columns for calculation (for example, rh25 and rhz25)
# Run repeatedly along the GEDI profile e.g. rh25, rh50, rh75, rh95
rh_col <- "rh25"
rhz_col <- "rhz25"

# Initialize an empty list to store results
ccc_results_list <- list()

# Height Calculations
add_ccc_result("All", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col), "Height")
add_ccc_result("PU", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "forest_state == 'PU'"), "Height")
add_ccc_result("SU", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "forest_state == 'SU'"), "Height")
add_ccc_result("PB1", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "forest_state == 'PB1'"), "Height")
add_ccc_result("PB2", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "forest_state == 'PB2'"), "Height")
add_ccc_result("PB3+", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "forest_state == 'PB3+'"), "Height")
add_ccc_result("SB1", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "forest_state == 'SB1'"), "Height")
add_ccc_result("SB2", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "forest_state == 'SB2'"), "Height")
add_ccc_result("SB3+", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "forest_state == 'SB3+'"), "Height")

# Canopy Cover Calculations (fixed, not based upon rh value)
add_ccc_result("All", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov"), "Canopy Cover")
add_ccc_result("PU", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "forest_state == 'PU'"), "Canopy Cover")
add_ccc_result("SU", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "forest_state == 'SU'"), "Canopy Cover")
add_ccc_result("PB1", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "forest_state == 'PB1'"), "Canopy Cover")
add_ccc_result("PB2", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "forest_state == 'PB2'"), "Canopy Cover")
add_ccc_result("PB3+", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "forest_state == 'PB3+'"), "Canopy Cover")
add_ccc_result("SB1", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "forest_state == 'SB1'"), "Canopy Cover")
add_ccc_result("SB2", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "forest_state == 'SB2'"), "Canopy Cover")
add_ccc_result("SB3+", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "forest_state == 'SB3+'"), "Canopy Cover")

# Combine all results into a single dataframe
ccc_results_filter1 <- do.call(rbind, ccc_results_list)

# Print the results as a table
print(ccc_results_filter1)


# Set rh and rhz columns for calculation (for example, rh25 and rhz25)
# Run repeatedly along the GEDI profile e.g. rh25, rh50, rh75, rh95
rh_col <- "rh25"
rhz_col <- "rhz25"

# Initialize an empty list to store results
ccc_results_list <- list()

# Height Calculations
add_ccc_result("All", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col), "Height")
add_ccc_result("PU", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "forest_state == 'PU'"), "Height")
add_ccc_result("SU", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "forest_state == 'SU'"), "Height")
add_ccc_result("PB1", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "forest_state == 'PB1'"), "Height")
add_ccc_result("PB2", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "forest_state == 'PB2'"), "Height")
add_ccc_result("PB3+", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "forest_state == 'PB3+'"), "Height")
add_ccc_result("SB1", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "forest_state == 'SB1'"), "Height")
add_ccc_result("SB2", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "forest_state == 'SB2'"), "Height")
add_ccc_result("SB3+", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "forest_state == 'SB3+'"), "Height")

# Canopy Cover Calculations (fixed, not based upon rh value)
add_ccc_result("All", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov"), "Canopy Cover")
add_ccc_result("PU", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "forest_state == 'PU'"), "Canopy Cover")
add_ccc_result("SU", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "forest_state == 'SU'"), "Canopy Cover")
add_ccc_result("PB1", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "forest_state == 'PB1'"), "Canopy Cover")
add_ccc_result("PB2", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "forest_state == 'PB2'"), "Canopy Cover")
add_ccc_result("PB3+", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "forest_state == 'PB3+'"), "Canopy Cover")
add_ccc_result("SB1", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "forest_state == 'SB1'"), "Canopy Cover")
add_ccc_result("SB2", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "forest_state == 'SB2'"), "Canopy Cover")
add_ccc_result("SB3+", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "forest_state == 'SB3+'"), "Canopy Cover")

# Combine all results into a single dataframe
ccc_results_filter2 <- do.call(rbind, ccc_results_list)

# Print the results as a table
print(ccc_results_filter2)








## (Figure S3) Original PCA graph with 25 loadings

# Plot 1: Full Biplot with All Loadings

# Plot 1: Full Biplot with All Loadings

# Manually format labels to ensure n= is below the Forest Class
facet_labels <- setNames(
  paste0(forest_state_summaries_2AB$forest_state, "\n(n=", forest_state_summaries_2AB$n, ")"),  
  forest_state_summaries_2AB$forest_state
)

# Ensure no missing values in PCA Data & Set Legend & Panel Orders Separately
pca_data <- as.data.frame(pca_result$x) %>%
  mutate(
    forest_state = factor(forest_state_type, levels = c("SB3+", "SB2", "SB1", "SU", "PB3+", "PB2", "PB1", "PU")),
    forest_state_legend = factor(forest_state_type, levels = c("PU", "PB1", "PB2", "PB3+", "SU", "SB1", "SB2", "SB3+"))
  ) %>%
  drop_na(PC1, PC2)

# Flip PC1 and PC2 for consistency
pca_data$PC1 <- -pca_data$PC1
pca_data$PC2 <- -pca_data$PC2

# Flip PCA loadings (arrows)
pca_rotation <- as.data.frame(pca_result$rotation)
pca_rotation$PC1 <- -pca_rotation$PC1
pca_rotation$PC2 <- -pca_rotation$PC2

# Use the defined forest_state_colors
biplot_full <- ggplot(data = pca_data, aes(x = PC1, y = PC2, color = forest_state_legend)) +
  geom_point(aes(alpha = ifelse(forest_state_legend == "PU", 0.4, 1)), 
             size = 1.2, shape = 16) +  # Shape 16 removes outlines
  scale_alpha_identity() +  # Ensures alpha values are correctly applied
  scale_color_manual(values = forest_state_colors, guide = guide_legend(override.aes = list(alpha = 1))) + 
  geom_segment(data = pca_rotation,  
               aes(x = 0, y = 0, xend = PC1 * 24, yend = PC2 * 24), 
               arrow = arrow(length = unit(0.1, "cm"), type = "open", angle = 25), 
               size = 0.8, color = "darkblue") +
  geom_text_repel(data = pca_rotation, 
                  aes(x = PC1 * 24, y = PC2 * 24, label = rownames(pca_rotation)), 
                  size = 2.5, fontface = "bold", color = "black", max.overlaps = 50,  
                  box.padding = 0.35, point.padding = 0.5) +  
  labs(title = "Biplot - PCA", x = "PC1", y = "PC2", color = "Forest Class") +
  theme_minimal() +
  theme_fancy() +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  theme(
    legend.position = c(0.98, 0),
    legend.justification = c("center", "bottom")
  )

# PC1 Histogram
fig_pc1 <- ggplot(pca_data, aes(x = PC1, fill = forest_state)) +  
  geom_histogram(binwidth = 0.5, color = "white", size = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = forest_state_colors) +
  labs(x = "PC1 Values", y = "Frequency") +
  xlim(x_limits) +
  facet_wrap(~ forest_state, nrow = 1, scales = "free_y", 
             labeller = labeller(forest_state = facet_labels)) +  
  theme_minimal() +
  theme_fancy() &
  theme(
    strip.text = element_text(size = 8, face = "bold"),  # Bolder facet labels
    axis.text.x = element_text(size = 8)
  )

# PC2 Histogram
fig_pc2 <- ggplot(pca_data, aes(x = PC2, fill = forest_state)) +  
  geom_histogram(binwidth = 0.5, color = "white", size = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = forest_state_colors) +
  labs(x = "PC2 Values", y = "Frequency") +
  xlim(x_limits) +
  facet_wrap(~ forest_state, nrow = 1, scales = "free_y", 
             labeller = labeller(forest_state = facet_labels)) +  
  theme_minimal() +   
  theme_fancy() &
  theme(
    strip.text = element_text(size = 8, face = "bold"),  
    axis.text.x = element_text(size = 8)
  )

# Combine PC1 & PC2 Histograms
fig_pcs <- (fig_pc1 / fig_pc2)

# Combine All Plots
figure4 <- (biplot_full / fig_pcs) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 8, face = "bold"),
    plot.tag.position = c(0, 1)
  )

print(figure4)

ggsave(figure4,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/PCA_25loadings.png",
       width = 17, height = 20, units = "cm"
)







