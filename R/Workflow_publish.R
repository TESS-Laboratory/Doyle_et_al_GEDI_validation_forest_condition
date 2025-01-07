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
  select(shot_number, all_of(columns_to_keep), ends_with(".x")) %>%
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))

# Keep only the columns from allGEDI4A and the non-duplicated columns from the other data sets
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

# Filter the data by senitivity metric of 0.95 (sample decreases from 9161 to 7915)
allGEDI2AB <- allGEDI2AB %>%
  filter(sensitivity >0.95)


sf::st_write(allGEDI2AB, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2AB <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb")
#allGEDI <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI.fgb")

# Calculate amplitude proxy from cumulative energy values in GEDI2A data

allGEDI2A_long <- gedi_long(allGEDI2AB)

allGEDI2A_amp <- allGEDI2A_long %>%
  select(shot_number, interval, amp) %>%
  pivot_wider(
    names_from = interval,
    values_from = amp,
    names_prefix = "amp_"
  )

allGEDI2AB_amp <- allGEDI2AB %>%
  left_join(allGEDI2A_amp, by = "shot_number") %>%
  select(-starts_with("rx_cum"))


# Tidy the environment
rm(allGEDI2A, allGEDI2B, allGEDI4A, allGEDI2A_no_geom, allGEDI2B_no_geom, 
   allGEDI4A_no_geom, cleaned2AB, cleaned2AB4A, merged2AB, merged2AB4A, fgb_list, params,
   allGEDI2A_amp)


# ------ GEDI rh/ waveform regressions ---------

# Summarise relative height rh0-100 metrics with linear regression model
# Outputs intercept, slope and variance of the 2A relative height profile

# Select only relative height data and unique identifier, transforming data set
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

# Remerge results with original GEDI2AB dataframe and name GEDI regressions

allGEDI2AB_reg <- left_join(allGEDI2AB, summary_df, by = "shot_number")
allGEDI2AB_reg <- left_join(allGEDI2AB_reg, result_df, by = "shot_number")

sf::st_write(allGEDI2AB_reg, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2AB_reg <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb")

# Tidy the environment
rm(GEDI2AB_trans, summary_df, result_df)



# Summarise cumulative energy proxy amplitude metrics with linear regression model
# Outputs intercept, slope and variance of the 2A waveform stats

# Select only amp data and unique identifier, transforming data base
allGEDI2AB_trans_amp <- allGEDI2AB_amp %>%
  as.data.frame() %>%
  select(shot_number, starts_with("amp"))

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
  mutate(max_amp = apply(select(., starts_with("amp")) %>% st_drop_geometry() %>% as.matrix(), 
                          1, max, na.rm = TRUE))

# Extract and format max_amp from allGEDI2AB_amp
result_df_amp <- allGEDI2AB_amp %>%
  st_drop_geometry() %>%
  select(shot_number, max_amp) %>%
  mutate(max_amp = as.numeric(max_amp),
    shot_number = as.character(shot_number))

# Re-merge max_amp results with original allGEDI2A data frame
allGEDI2AB_reg <- allGEDI2AB_reg %>%
  left_join(result_df_amp, by = "shot_number")


# Finalise data sets for methods

allGEDI2AB_amp <- allGEDI2AB_reg %>%
  select(shot_number, W_intercept, W_slope, W_variance,
         max_amp, starts_with("amp"), matches("^rh([0-9]|[1-9][0-9]|100)$"))

allGEDI2AB <- allGEDI2AB_reg %>%
  select(-starts_with("rx_cum"), -starts_with("amp"))


sf::st_write(allGEDI2AB, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2AB_amp, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_amp.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2A_amp <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_amp.fgb")

rm(result_df_amp, shot_numbers, allGEDI2AB_reg)

# ------ Extracting ALS metrics within GEDI footprints------

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

CAUT23_20Smetrics <- plot_metrics(CAUT23_20Sfinal1, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_20S, radius = 12.5)
CAUT23_20Smetrics <- st_transform(CAUT23_20Smetrics1, "EPSG:32643")
sf::st_write(CAUT23_20Smetrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/CAUT23_20Smetrics.fgb", delete_dsn = TRUE, overwrite = TRUE)

DAAC1821_21Smetrics <- plot_metrics(DAAC1821_21Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_21S, radius = 12.5)
DAAC1821_21Smetrics <- st_transform(DAAC1821_21Smetrics, "EPSG:32643")
sf::st_write(DAAC1821_21Smetrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/DAAC1821_21Smetrics.fgb", delete_dsn = TRUE, overwrite = TRUE)


#DAAC18_19Smetrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/DAAC18_19Smetrics.fgb")
#CAUT23_20Smetrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/CAUT23_20Smetrics.fgb")
#DAAC1821_21Smetrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/DAAC1821_21Smetrics.fgb")


# Remove duplicated rows
DAAC18_19Smetrics <- distinct(DAAC18_19Smetrics)
CAUT23_20Smetrics <- distinct(CAUT23_20Smetrics)
DAAC1821_21Smetrics <- distinct(DAAC1821_21Smetrics)

# Assuming columns are in the same order and just have different names
colnames(CAUT23_20Smetrics1) <- colnames(DAAC18_19Smetrics)
colnames(DAAC1821_21Smetrics) <- colnames(DAAC18_19Smetrics)


# Combine the dataframes into one dataframe using bind_rows
merged_df <- bind_rows(DAAC18_19Smetrics, 
                       CAUT23_20Smetrics1,
                       CAUT23_20Smetrics2,
                       DAAC1821_21Smetrics)

# Remove rows with NAs in the 'rhz' column and filter for required columns

allGEDI2AB_ALS <- merged_df %>%
  filter(!is.na(rhz95)) %>%
  select(year, solar_elevation, elev_highestreturn, elev_lowestmode, rh0, rh5, rh10, rh15,
         rh20, rh25, rh30, rh35, rh40, rh45, rh50, rh55, rh60, rh65, rh70, rh75, rh80, rh85,
         rh90, rh95, rh96, rh97, rh98, rh99, rh100, sensitivity, shot_number, degrade_flag, ALS_CRS,
         cover, pai, fhd_normal, pgap_theta, pgap_theta_error, omega, modis_treecover, landsat_treecover,
         rh_mean, rh_sd, rh_max, rh_min, rh_skew, rh_kurt, Rh_intercept, Rh_slope, Rh_variance, W_intercept,
         W_slope, W_variance, max_amp, num_detectedmodes, geometry, rhz5, rhz10, rhz15, rhz20, 
         rhz25, rhz30, rhz35, rhz40, rhz45, rhz50, rhz55, rhz60, rhz65, rhz70, rhz75, rhz80, rhz85, 
         rhz90, rhz95, rhz96, rhz97, rhz98, rhz99, max, cancov, z_kurt, z_skew)


sf::st_write(allGEDI2AB_ALS, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb", delete_dsn = TRUE, overwrite = TRUE)
#allGEDI2AB_ALS <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb")


# Tidy the environment
rm(merged_df, DAAC18_19Smetrics, DAAC1821_21Smetrics, CAUT23_20Smetrics, allGEDI2AB_19S, 
   allGEDI2AB_20S, allGEDI2AB_21S, DAAC18_19Sfinal, DAAC1821_21Sfinal, CAUT23_20Sfinal)

# ------- Forest spectral classification ---------

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
# Updated link: https://earthengine.googlesource.com/users/mapbiomas/user-toolkit/+/59f6cf84a1c91fb9fe116c939910bb8453302e60/mapbiomas-user-toolkit-download-mosaics.js

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
#burnedforest <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Fire_data/MAPBIOMASfire.tif")


# (3) Validation layer

# Validate if forest extent is still classified as forest after burn events in Cautario:
# Logging events do not occur in managed Extractive Reserve 
# but fire events between 2019 - 2023 need to be checked using MapBiomas LULC classification data set post 2019

# Importing MapBiomas land classification layer for Rondonia for 2022 (2023 unavailable still)
forestclassmapbio <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Cautario_validation/mapbiomas-amazon-collection-50-rondonia-2022.tif")

# Reclassifying so forest = 1 and non forest = 0
rcl = matrix(c(0,0,0,1,6,1,7,33,0), ncol = 3, byrow = TRUE)
forestclass = reclassify(forestclassmapbio, rcl)

writeRaster(forestclass, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/forestclass.tif", format="GTiff", overwrite=TRUE)
#forestclass <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/forestclass.tif")


# Tidy environment

rm(secondaryforestraw1, secondaryforestraw2, secondaryforestraw3, secondary_forest_west,
   secondary_forest_east, secondaryforest, secondaryforestplus6, Burned_list, forestclassmapbio, raster_obj,
   merged_Burned, rcl)


# ----------- GEDI classification extraction--------------

#Classify data frames for various analyses

forest_age <- raster::extract(secondaryforest2024, allGEDI2A, method='simple')
allGEDI2A <- cbind(allGEDI2A, forest_age)
forest_age <- raster::extract(secondaryforest2024, allGEDI2AB_ALS, method='simple')
allGEDI2AB_ALS <- cbind(allGEDI2AB_ALS, forest_age)
forest_age <- raster::extract(secondaryforest2024, allGEDI, method='simple')
allGEDI <- cbind(allGEDI, forest_age)
forest_age <- raster::extract(secondaryforest2024, allGEDI2AB_amp, method='simple')
allGEDI2AB_amp <- cbind(allGEDI2AB_amp, forest_age)
forest_age <- raster::extract(secondaryforest2024, allGEDI2AB, method='simple')
allGEDI2AB <- cbind(allGEDI2AB, forest_age)


burn_freq <- terra::extract(burnedforest, allGEDI2A, method='simple')
allGEDI2A <- cbind(allGEDI2A, burn_freq)
burn_freq <- terra::extract(burnedforest, allGEDI2AB_ALS, method='simple')
allGEDI2AB_ALS <- cbind(allGEDI2AB_ALS, burn_freq)
burn_freq <- terra::extract(burnedforest, allGEDI, method='simple')
allGEDI <- cbind(allGEDI, burn_freq)
burn_freq <- terra::extract(burnedforest, allGEDI2AB_amp, method='simple')
allGEDI2AB_amp <- cbind(allGEDI2AB_amp, burn_freq)
burn_freq <- terra::extract(burnedforest, allGEDI2AB, method='simple')
allGEDI2AB <- cbind(allGEDI2AB, burn_freq)


validation <- terra::extract(forestclass, allGEDI2A, method='simple')
allGEDI2A <- cbind(allGEDI2A, validation)
validation <- terra::extract(forestclass, allGEDI2AB_ALS, method='simple')
allGEDI2AB_ALS <- cbind(allGEDI2AB_ALS, validation)
validation <- terra::extract(forestclass, allGEDI, method='simple')
allGEDI <- cbind(allGEDI, validation)
validation <- terra::extract(forestclass, allGEDI2AB_amp, method='simple')
allGEDI2AB_amp <- cbind(allGEDI2AB_amp, validation)
validation <- terra::extract(forestclass, allGEDI2AB, method='simple')
allGEDI2AB <- cbind(allGEDI2AB, validation)


# Edit GEDI files to have a numeric value for ageless Intact forest (n/a)
allGEDI2A_aged <- allGEDI2A %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age)) %>%
  mutate(validation = ifelse(is.na(validation), 2, validation)) %>%
  filter(validation != 0) %>%
  select(-validation)

allGEDI2AB_ALS_aged <- allGEDI2AB_ALS %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age)) %>%
  mutate(validation = ifelse(is.na(validation), 2, validation)) %>%
  filter(validation != 0) %>%
  select(-validation)

allGEDI_aged <- allGEDI %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age)) %>%
  mutate(validation = ifelse(is.na(validation), 2, validation)) %>%
  filter(validation != 0) %>%
  select(-validation)

allGEDI2AB_amp_aged <- allGEDI2AB_amp %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age)) %>%
  mutate(validation = ifelse(is.na(validation), 2, validation)) %>%
  filter(validation != 0) %>%
  select(-validation)

allGEDI2AB_aged <- allGEDI2AB %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age)) %>%
  mutate(validation = ifelse(is.na(validation), 2, validation)) %>%
  filter(validation != 0) %>%
  select(-validation)


# Process GEDI files to be aged/ classified by Degradation type

allGEDI2A <- allGEDI2A_aged %>%
  process_GEDI_degradation()

allGEDI2AB_ALS <- allGEDI2AB_ALS_aged %>%
  process_GEDI_degradation()

allGEDI <- allGEDI_aged %>%
  process_GEDI_degradation()

allGEDI2AB_amp <- allGEDI2AB_amp_aged %>%
  process_GEDI_degradation()

allGEDI2AB <- allGEDI2AB_aged %>%
  process_GEDI_degradation()


# Write to file
sf::st_write(allGEDI2A, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2AB_ALS, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2AB_amp, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_amp.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2AB, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2AB <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB.fgb")
#allGEDI2AB_ALS <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb")
#allGEDI <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI.fgb")
#allGEDI2AB_amp <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_amp.fgb")


# Tidy environment
rm(allGEDI2A_aged, allGEDI2AB_ALS_aged, allGEDI_aged, allGEDI2AB_amp_aged, allGEDI2AB_aged,
   burnedforest, forestclass, secondaryforest2024, burned_subset)

# ------ Statistics (Aim 1)  ---------

# Accuracy assessment: calculate correspondence/ correlation between GEDI and ALS

# Set rh and rhz columns for calculation (for example, rh25 and rhz25)
# Run repeatedly along the GEDI profile e.g. rh25, rh50, rh75, rh96
rh_col <- "rh96"
rhz_col <- "rhz96"

# Initialize an empty list to store results
ccc_results_list <- list()

# Height Calculations
add_ccc_result("All", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col), "Height")
add_ccc_result("Intact", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "Degradation == 'Intact'"), "Height")
add_ccc_result("Logged", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "Degradation == 'Logged'"), "Height")
add_ccc_result("Burned 1-10", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "burn_freq %in% 1:10"), "Height")
add_ccc_result("Burned 1", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "burn_freq == 1"), "Height")
add_ccc_result("Burned 2", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "burn_freq == 2"), "Height")
add_ccc_result("Burned 3", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "burn_freq == 3"), "Height")
add_ccc_result("Burned 4-6", calculate_ccc(allGEDI2AB_ALS, rh_col, rhz_col, "burn_freq %in% 4:6"), "Height")

# Canopy Cover Calculations (fixed, not based upon rh value)
add_ccc_result("All", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov"), "Canopy Cover")
add_ccc_result("Intact", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "Degradation == 'Intact'"), "Canopy Cover")
add_ccc_result("Logged", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "Degradation == 'Logged'"), "Canopy Cover")
add_ccc_result("Burned 1-10", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "burn_freq %in% 1:10"), "Canopy Cover")
add_ccc_result("Burned 1", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "burn_freq == 1"), "Canopy Cover")
add_ccc_result("Burned 2", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "burn_freq == 2"), "Canopy Cover")
add_ccc_result("Burned 3", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "burn_freq == 3"), "Canopy Cover")
add_ccc_result("Burned 4-6", calculate_ccc_c(allGEDI2AB_ALS, "cover", "cancov", "burn_freq %in% 4:6"), "Canopy Cover")

# Combine all results into a single dataframe
ccc_results <- do.call(rbind, ccc_results_list)

# Print the results as a table
print(ccc_results)



# RMSE calculations Inspired by Dorado et al (2021)
# Functions to calculate RMSE, Bias, fit a linear model for rh97 for given forest conditions. 
# Edit function for different 'rh's'


# RMSE calculations
conditions <- c("All", "Degradation == 'Intact'", "Degradation == 'Logged'", "burned",
                "burn_freq == 1", "burn_freq == 2", "burn_freq == 3", "burn_freq %in% 4:6")

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
  Forest_Condition = c("All", "Intact", "Logged", "Burned", "Burned 1", "Burned 2", "Burned 3", "Burned 4-6"),
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






## FIGURE: Relative height correspondence

# Define degradation categories and color mapping
degradation_categories <- c("Intact", "Logged", "Burned 1", "Burned 2", "Burned 3", "Burned 4+")
degradation_colors <- c("Intact" = "#92c5de", "Logged" = "#0073e6", "Burned 1" = "pink", 
                        "Burned 2" = "palevioletred1", "Burned 3" = "palevioletred3", "Burned 4+" = "#ca0020")

# Set up the Lin's CCC calculations for each pair of ALS and GEDI columns
allGEDI2AB_ALS_height_long <- allGEDI2AB_ALS %>%
  pivot_longer(cols = starts_with("rhz"), names_to = "ALS", values_to = "ALS_value") %>%
  pivot_longer(cols = starts_with("rh"), names_to = "GEDI", values_to = "GEDI_value") %>%
  filter((ALS == "rhz25" & GEDI == "rh25") | 
           (ALS == "rhz50" & GEDI == "rh50") |
           (ALS == "rhz75" & GEDI == "rh75") |
           (ALS == "rhz96" & GEDI == "rh96"))


# Order the degradation types for output
allGEDI2AB_ALS_height_long <- allGEDI2AB_ALS_height_long %>%
  mutate(Degradation = factor(Degradation, levels = degradation_categories))

# Define the limits for both x and y axes for the rhz plots
common_limits <- c(0, 60)

# Calculate Lin's CCC for each height pair
correlations_height_ccc <- allGEDI2AB_ALS_height_long %>%
  group_by(ALS, GEDI) %>%
  summarise(ccc_value = as.numeric(calculate_ccc(cur_data(), "ALS_value", "GEDI_value")), .groups = 'drop') %>%
  mutate(annotation = paste0("CCC = ", round(ccc_value, 2)))

# Combine all height pairs for a single plot with facets
rh_correspondence <- allGEDI2AB_ALS_height_long %>%
  ggplot(aes(x = ALS_value, y = GEDI_value, color = Degradation)) +
  geom_point(size = 0.4, alpha = 0.7) +
  geom_text(data = correlations_height_ccc, aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed", size = 0.5) +
  labs(x = "ALS Relative Height (m)", y = "GEDI Relative Height (m)", color = "Condition") +  # Change legend title here
  theme_fancy() +
  scale_colour_manual(values = degradation_colors) +
  coord_fixed(ratio = 1, xlim = common_limits, ylim = common_limits) +
  facet_wrap(~ ALS, scales = "fixed") +  # Create facets for each ALS column
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) 


# FIGURE: Calculating pairwise correspondence (Lin's CCC) along the 
# Relative height profile for given degradation types

# Define the conditions
conditions <- c("All", 
                "Degradation == 'Intact'", 
                "Degradation == 'Logged'", 
                "burn_freq == 1", 
                "burn_freq == 2", 
                "burn_freq == 3", 
                "burn_freq >= 4")

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
    Condition == "Degradation == 'Intact'" ~ "Intact",
    Condition == "Degradation == 'Logged'" ~ "Logged",
    Condition == "burn_freq == 1" ~ "Burned 1",
    Condition == "burn_freq == 2" ~ "Burned 2",
    Condition == "burn_freq == 3" ~ "Burned 3",
    Condition == "burn_freq >= 4" ~ "Burned 4+",
    TRUE ~ NA_character_
  )) %>%
  mutate(Pair = str_extract(Pair, "\\d+"))

# Print the cleaned data to verify
print(ccc_pair_results)






# FIGURE: Correlations for complete relative height profile for each degradation type

# Degradation colors for plotting
degradation_colors <- c("All" = "darkblue", "Intact" = "#92c5de", "Logged" = "#0073e6", 
                        "Burned 1" = "pink", "Burned 2" = "palevioletred1", 
                        "Burned 3" = "palevioletred3", "Burned 4+" = "#ca0020")

# Ensure the Pair column in ccc_pair_results is ordered correctly
ccc_pair_results$Pair <- factor(ccc_pair_results$Pair, levels = as.character(seq(5, 95, 5)))

# Filter the data set to include only the specified conditions, including "All"
ccc_results_data_filtered <- ccc_pair_results %>%
  filter(Condition %in% c("All", "Intact", "Logged", "Burned 1", "Burned 2", "Burned 3", "Burned 4+")) %>%
  mutate(Condition = factor(Condition, levels = c("All", degradation_categories)))

# Verify that the filtered data has content
print(ccc_results_data_filtered)

# Find the maximum Lin's CCC value for each condition
max_lins_ccc <- ccc_results_data_filtered %>%
  group_by(Condition) %>%
  slice_max(order_by = Lins_CCC, n = 1)

# Create the plot using Lin's CCC values
plot_pairwise <- ccc_results_data_filtered %>%
  ggplot(aes(x = Pair, y = Lins_CCC, color = Condition)) +
  geom_point(size = 1) +  # Points for each Lin's CCC value
  geom_line(aes(group = Condition), size = 0.5) +  # Line connecting points for each condition
  geom_point(data = max_lins_ccc, aes(x = Pair, y = Lins_CCC), size = 3, shape = 18, show.legend = FALSE) +  # Highlight max points
  labs(x = "Pair (GEDI rh - ALS rhz)", y = "Lin's CCC") +
  theme_fancy() +
  scale_color_manual(values = degradation_colors) +  # Updated degradation colors
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, face = "italic")
  )

# Legend position for rh_correspondence
rh_correspondence <- rh_correspondence +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(legend.title = element_text(face = "bold"))

# Legend position for plot_pairwise 
plot_pairwise <- plot_pairwise +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(legend.title = element_text(face = "bold"))

# Combine the plots
figure1 <- (rh_correspondence | plot_pairwise) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(widths = c(1, 1)) 

# Display the figure
print(figure1)

# Save the figure
ggsave(figure1,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/Correspondence_height.png",
       width = 20, height = 13, units = "cm"
)




# FIGURE: Correspondence for relative height per degradation type

# Define degradation types and colors
degradation_categories <- c("Intact", "Logged", "Burned 1", "Burned 2", "Burned 3", "Burned 4+")
degradation_colors <- c("Intact" = "#92c5de", "Logged" = "#0073e6", 
                        "Burned 1" = "pink", "Burned 2" = "palevioletred1", 
                        "Burned 3" = "palevioletred3", "Burned 4+" = "#ca0020")

# Ensure the Degradation column is a factor with the desired levels
allGEDI2AB_ALS <- allGEDI2AB_ALS %>%
  mutate(Degradation = factor(Degradation, levels = degradation_categories))

# Calculate Lin's CCC for relative height at rh96-rhz96 for each degradation condition
ccc_degradation_height <- allGEDI2AB_ALS %>%
  filter(Degradation %in% degradation_categories) %>%
  group_by(Degradation) %>%
  summarise(ccc_value = as.numeric(calculate_ccc(cur_data(), "rh96", "rhz96"))) %>%
  mutate(annotation = paste0("CCC = ", round(ccc_value, 2)))

# Create relative height plot with separate facets
ccc_height_deg <- allGEDI2AB_ALS %>%
  filter(Degradation %in% degradation_categories) %>%
  ggplot(aes(x = rhz96, y = rh96, color = Degradation)) +
  geom_point(size = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "gray70", linetype = "dashed", size = 0.5) +
  geom_text(data = ccc_degradation_height, aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  labs(x = "ALS Relative Height (m)", y = "GEDI Relative Height (m)") +
  theme_bw() +
  scale_colour_manual(values = degradation_colors) +
  facet_wrap(~ Degradation, ncol = 6) +  # Display each degradation in its own facet
  xlim(0, 60) + ylim(0, 60) +
  theme_fancy() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Adjust x-axis text
    legend.position = "none"
  )


# FIGURE: Correspondence for canopy cover per degaradtion type

# Reshape the data for canopy cover
allGEDI2AB_ALS_cover_long <- allGEDI2AB_ALS %>%
  pivot_longer(cols = starts_with("can"), names_to = "ALS", values_to = "ALS_value") %>%
  pivot_longer(cols = "cover", names_to = "GEDI", values_to = "GEDI_value") %>%
  filter(ALS == "cancov")

# Reorder the data for canopy cover
allGEDI2AB_ALS_cover_long <- allGEDI2AB_ALS_cover_long %>%
  mutate(Degradation = factor(Degradation, levels = degradation_categories))

# Calculate Lin's CCC for canopy cover across each degradation type
ccc_degradation_cover <- allGEDI2AB_ALS_cover_long %>%
  group_by(Degradation) %>%
  summarise(ccc_value = as.numeric(calculate_ccc_c(cur_data(), "ALS_value", "GEDI_value")), .groups = 'drop') %>%
  mutate(annotation = paste0("CCC = ", round(ccc_value, 2)))

# Create the canopy cover plot with separate facets
ccc_cover_deg <- allGEDI2AB_ALS_cover_long %>%
  filter(Degradation %in% degradation_categories) %>%
  ggplot(aes(x = ALS_value, y = GEDI_value, color = Degradation)) +
  geom_point(size = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "gray70", linetype = "dashed", size = 0.5) +
  geom_text(data = ccc_degradation_cover, aes(x = Inf, y = Inf, label = annotation), 
            hjust = 1.1, vjust = 2, size = 3, color = "black") +
  labs(x = "ALS Canopy Cover", y = "GEDI Canopy Cover") +
  theme_bw() +
  scale_colour_manual(values = degradation_colors) +
  facet_wrap(~ Degradation, ncol = 6) +  # Display each degradation in its own facet
  coord_fixed(ratio = 1) +  # Square aspect ratio without xlim and ylim
  theme_fancy() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Adjust x-axis text
    legend.position = "none"
  )

# Combine both panels in a two-row layout using patchwork and set legend title globally
figure2 <- (ccc_height_deg / ccc_cover_deg) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") 


# Display the combined figure
print(figure2)


# Save the figure
ggsave(figure2,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/Correspondence_degradation.png",
       width = 20, height = 12, units = "cm"
)


# Tidy environemnt
rm(allGEDI2AB_ALS_height_long, allGEDI2AB_ALS_cover_long, ccc_results_list, 
  ccc_results, ccc_pair_results, ccc_results_data_filtered, ccc_degradation_height, 
  ccc_degradation_cover, rh_correspondence, plot_pairwise, ccc_height_deg, 
  ccc_cover_deg, conditions, rh_columns, rhz_columns, results_list, common_limits)


# ------- GEDI Gradient (Aim 2) -------


## FIGURE 3 - GEDI degradation data across Amazonia 

# Ordering of age within plots
age_order <- c("<7", "7-15", "15-25", "25-40", ">40")
age_order2 <- c("<10", "10-20", "20-30", "30-40", ">40")

allGEDI2AB <- allGEDI2AB %>%
  mutate(
    Age_category = factor(Age_category, levels = age_order),
    Age_category2 = factor(Age_category2, levels = age_order2))

allGEDI <- allGEDI %>%
  mutate(
    Age_category = factor(Age_category, levels = age_order),
    Age_category2 = factor(Age_category2, levels = age_order2))

allGEDI <- allGEDI %>%
  filter(l4_quality_flag == 1) %>%
  filter(agbd < 800)
 
# Define the color scale

# Define degradation categories and color mapping
degradation_colors <- c("Intact" = "#92c5de", "Logged" = "#0073e6", "Burned 1" = "pink", 
                        "Burned 2" = "palevioletred1", "Burned 3" = "palevioletred3", "Burned 4+" = "#ca0020")

# Order the degradation types for output
allGEDI2AB <- allGEDI2AB %>%
  mutate(Degradation = factor(Degradation, levels = degradation_categories))

allGEDI <- allGEDI %>%
  mutate(Degradation = factor(Degradation, levels = degradation_categories))


# Violin plot for canopy height
filtered_data1 <- allGEDI2AB %>%
  group_by(Age_category, Degradation) %>%
  filter(var(rh97, na.rm = TRUE) > 0)

height_violin <- filtered_data1 %>%
  ggplot(aes(x = Age_category, y = rh97, color = Degradation, fill = Degradation)) +
  geom_violin(width = 0.9, position = position_dodge(width = 0.7), scale = "width", alpha = 0.7)+
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.7), 
               shape = 23, size = 1, color = "black", fill = "white", aes(group = Degradation)) +
  stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 1), 
               aes(label = round(..y.., 1), group = Degradation), vjust = -1.5, size = 3) +
  labs(y = "Canopy Height (m)", x = NULL) +
  theme_bw() +
  scale_colour_manual(values = degradation_colors) +
  scale_fill_manual(values = degradation_colors) +
  ylim(0, 50) +
  theme_fancy() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.1, 0, 0.1, 0), "cm") 
  ) &
  labs(color = "Condition")  +
  theme(legend.position = "none")

# Violin plot for canopy cover

# Filter out groups where the violin plot would not be visible
filtered_data2 <- allGEDI2AB %>%
  group_by(Age_category, Degradation) %>%
  filter(var(cover, na.rm = TRUE) > 0)  # Keep groups with non-zero variance

cover_violin <- filtered_data2 %>%
  ggplot(aes(x = Age_category, y = cover, color = Degradation, fill = Degradation)) +
  geom_violin(width = 0.9, position = position_dodge(width = 0.7), scale = "width", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.7), 
               shape = 23, size = 1, color = "black", fill = "white", aes(group = Degradation)) +
  stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 1), 
               aes(label = round(..y.., 1), group = Degradation), vjust = -1.5, size = 3) +
  labs(y = "Canopy Cover", fill = "Condition", color = "Condition") +
  theme_bw() +
  scale_colour_manual(values = degradation_colors) +
  scale_fill_manual(values = degradation_colors) +
  theme_fancy() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    plot.margin = unit(c(0.1, 0, 0.1, 0), "cm")  # Adjust margins for alignment
  )

# Violin plot for AGBD

filtered_data3 <- allGEDI %>%
  group_by(Age_category, Degradation) %>%
  filter(var(agbd, na.rm = TRUE) > 0)

agbd_violin <- filtered_data3 %>%
  ggplot(aes(x = Age_category, y = agbd, color = Degradation, fill = Degradation)) +
  geom_violin(width = 0.9, position = position_dodge(width = 0.7), scale = "width", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.7), 
               shape = 23, size = 1, color = "black", fill = "white", aes(group = Degradation)) +
  stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 1), 
               aes(label = round(..y.., 1), group = Degradation), vjust = -1.5, size = 3) +
  labs(y = "AGBD (Mg/ha)", x = "Forest Age") +
  theme_bw() +
  scale_colour_manual(values = degradation_colors) +
  scale_fill_manual(values = degradation_colors) +
  ylim(0, 600) +
  theme_fancy() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.1, 0, 0.1, 0), "cm") 
  ) &
  labs(color = "Condition") +
  theme(legend.position = "none")

# Legend position for plot_pairwise 
cover_violin <- cover_violin +
  theme(legend.position = "right") 

# Combine the plots using patchwork and reduce space between plots
figure3 <- (height_violin / cover_violin / agbd_violin) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.margin = unit(c(0.1, 0, 0.1, 0), "cm"))


# Print the combined plot
print(figure3)

ggsave(figure3,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/GEDI_gradient.png",
       width = 20, height = 14, units = "cm"
)



# # Violin plot for AGBD
# pai_violin <- allGEDI %>%
#   ggplot(aes(x = Age_category, y = pai, color = Degradation, fill = Degradation)) +
#   geom_violin(width = 0.9, position = position_dodge(width = 0.7), scale = "width", alpha = 0.7) +
#   stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.7), 
#                shape = 23, size = 1, color = "black", fill = "white", aes(group = Degradation)) +
#   stat_summary(fun = mean, geom = "text", color = "black", position = position_dodge(width = 1), 
#                aes(label = round(..y.., 1), group = Degradation), vjust = -1.5, size = 3) +
#   labs(y = "AGBD (Mg/ha)", x = "Forest Age") +
#   theme_bw() +
#   scale_colour_manual(values = degradation_colors) +
#   scale_fill_manual(values = degradation_colors) +
#   ylim(0, 8) +
#   theme(
#     legend.position = "none",
#     plot.margin = unit(c(0.1, 0, 0.1, 0), "cm")  # Minimize horizontal spacing
#   ) &
#   labs(color = "Condition")  
# 
# print(pai_violin)




# -------- Principle Component Analysis/ ML model (Aim 3) ---------

# PCA for mixture of GEDI variables and height/ waveform summaries to assess forest condition

# Keep the Degradation variable separate before PCA
degradation_type <- allGEDI2AB$Degradation

# Remove unnecessary columns for PCA, but keep Degradation out for later use



allGEDI2ABPCA <- allGEDI2ABPCA %>%
  select(-rh99, -rh100, -rh_max, -rh25, rh_mean, -rh50, -rh_min, -rh_kurt, -rh_skew,
         -rh_sd, -W_variance, -W_intercept, -pai, -W_slope, -Rh_intercept, -Rh_variance, Rh_slope, 
         -max_amp, -num_detectedmodes, -beam, cover, fhd_normal, -n_peaks)



# # Remove unnecessary columns for PCA, but keep Degradation out for later use (De Conta run)
# allGEDI2ABPCA <- allGEDI%>%
#   select(rh98, agbd, fhd_normal, cover, pai)
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
  select(Variable, PC1) %>%
  arrange(desc(PC1))

# Sort loadings by PC2 in descending order
sorted_PC2 <- loadings_df %>%
  select(Variable, PC2) %>%
  arrange(desc(PC2))

# Sort loadings by PC3 in descending order
sorted_PC3 <- loadings_df %>%
  select(Variable, PC3) %>%
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

# Add the degradation type back to the components dataframe
components$Degradation <- as.factor(degradation_type)

# Set color palette for degradation types
colors <- c("Intact" = "#92c5de", "Logged" = "#0073e6", "Burned 1" = "pink", 
                        "Burned 2" = "palevioletred1", "Burned 3" = "palevioletred3", "Burned 4+" = "#ca0020")

# Order the degradation types for output
components <- components %>%
  mutate(Degradation = factor(Degradation, levels = degradation_categories))



# Reverse PC2 for visual consistency
components$PC2 <- -components$PC2

# Plot 1: Full Biplot with All Loadings
scale <- 5 # Adjust arrow length scale
biplot_full <- ggplot(data = components, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Degradation), size = 0.7, shape = 19, alpha = 0.8) +
  geom_segment(data = as.data.frame(pca_result$rotation), 
               aes(x = 0, y = 0, xend = PC1 * scale, yend = PC2 * scale), 
               arrow = arrow(length = unit(0.3, "cm"), type = "open", angle = 25), 
               size = 0.8, color = "darkblue") +
  geom_text_repel(data = as.data.frame(pca_result$rotation), 
                  aes(x = PC1 * scale, y = PC2 * scale, label = rownames(pca_result$rotation)), 
                  size = 3, color = "black", max.overlaps = 5, 
                  box.padding = 0.35, point.padding = 0.5) +
  scale_color_manual(values = colors) +
  labs(title = "Biplot - PCA", x = "PC1", y = "PC2", color = "Condition") +
  theme_minimal() +
  theme_fancy() +
  theme(
    legend.position = c(0.95, 0.05),  # Adjusts the position (x, y) relative to the plot
    legend.justification = c("center", "bottom")  # Anchors the legend to its new position
  )

# Display the full biplot
print(biplot_full)

# FIGURE: Squashed PCA axis plots

# Define common x and y axis limits
x_limits <- c(-6, 6)

# Add the Degradation type from degradation_type to pca_data
pca_data <- as.data.frame(pca_result$x)
pca_data$Degradation <- degradation_type
pca_data$Degradation <- factor(pca_data$Degradation, levels = c("Burned 4+", "Burned 3", 
                                                                "Burned 2", "Burned 1", "Logged", "Intact"))


# Create PC1 plot with facets
fig_pc1 <- ggplot(pca_data, aes(x = PC1, fill = Degradation)) +  
  geom_histogram(binwidth = 0.5, color = "white", size = 0.1, show.legend = FALSE) +  # Thin white outline on bins
  scale_fill_manual(values = degradation_colors) +
  labs(x = "PC1 Values", y = "Frequency") +
  xlim(x_limits) +
  facet_wrap(~ Degradation, nrow = 1, scales = "free_y") +  # One row with independent y-scales
  theme_minimal() +  
  theme_fancy() &
  labs(color = "Condition")  

# Create PC2 plot with facets
fig_pc2 <- ggplot(pca_data, aes(x = PC2, fill = Degradation)) +  
  geom_histogram(binwidth = 0.5, color = "white", size = 0.1, show.legend = FALSE) +  # Thin white outline on bins
  scale_fill_manual(values = degradation_colors) +
  labs(x = "PC2 Values", y = "Frequency") +
  xlim(x_limits) +
  facet_wrap(~ Degradation, nrow = 1, scales = "free_y") +  # One row with independent y-scales
  theme_minimal() +   
  theme_fancy() &
  labs(color = "Condition")  


# Combine both PC1 and PC2 panels
fig_pcs <- (fig_pc1) / (fig_pc2)
fig_pcs

# Figure with both panels

figure4 <- (biplot_full) / (fig_pcs) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 8, face= "bold"),
    plot.tag.position = c(0, 1)  # Position the tags, (0, 1) is top-left
  )

print(figure4)

ggsave(figure4,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/PCA.png",
       width = 18, height = 20, units = "cm"
)

# 
# # FIGURE: PC1 Density plot
# 
# fig_5 <- ggplot(pca_data, aes(x = PC1, color = degradation_type)) +
#   geom_density(size = 1) +  # Set line size for better visibility
#   labs(x = "PC1 Values", y = "Density") +
#   scale_color_manual(values = c(
#     "Intact" = "#92c5de",
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
# burned_4pc1_den <- density(pca_data$PC1[which(pca_data$Degradation == "Burned 4+")], from = -6, to = 6, n = 13)
# nonburned_4pc1_den <- density(pca_data$PC1[which(pca_data$Degradation != "Burned 4+")], from = -6, to = 6, n = 13)
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

# Set color palette for degradation types
colors <- c("Intact" = "#92c5de", "Logged" = "#0073e6", "Burned 1" = "pink", 
            "Burned 2" = "palevioletred1", "Burned 3" = "palevioletred3", "Burned 4+" = "#ca0020")


# Set the factor levels
gpca <- pca_data |>
  dplyr::mutate(
    Degradation = forcats::fct_relevel(
      Degradation,
      "Intact", "Logged", "Burned 1", "Burned 2", "Burned 3", "Burned 4+"
    ),
    pc_sum = rescale(PC1) + rescale(PC2),
    pc_ratio = rescale(log(rescale(PC1) / rescale(PC2))),
    pc_ratio = dplyr::case_when(!is.finite(pc_ratio) ~ NA_real_, TRUE ~ pc_ratio),
    pc1_x_pc2 = rescale(PC1) * rescale(PC2),
    pc1 = rescale(PC1),
    rowid = dplyr::row_number()
  ) |>
  dplyr::left_join(st_drop_geometry(allGEDI2ABPCA), by = "rowid")


# multi-performance metrics
mod_perf_tab <- list(
  as.formula(Degradation ~ PC1 + PC2),
  as.formula(Degradation ~ PC1 * PC2),
  as.formula(Degradation ~ PC1),
  as.formula(Degradation ~ pc_sum),
  as.formula(Degradation ~ pc_ratio),
  as.formula(Degradation ~ rh99)
) |>
  purrr::map(~ nnet::multinom(.x, data = gpca)) |>
  purrr::map(performance::model_performance) |>
  purrr::set_names(
    c("PC1 + PC2", "PC1 * PC2", "PC1", "PC1_add_PC2", "PC_ratio", "RH99")
  ) |>
  purrr::imap(~ dplyr::mutate(as_tibble(.x), model = .y)) |>
  purrr::list_rbind() |>
  dplyr::relocate(model) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))

kableExtra::kable(mod_perf_tab, rownames = FALSE)

# Inspect the effects of the PC ratio model
# PC ratio:
pc_ratio1 <- nnet::multinom(Degradation ~ pc_ratio, data = gpca)
summary(pc_ratio1)
performance::model_performance(pc_ratio1)
gge <- ggeffects::ggemmeans(pc_ratio1, terms = "pc_ratio [all]")
print(gge)
plot(gge)

# Stack plots to compare
plot_model_sup <- (fit_nplot_mnlr(pc_ratio) | fit_nplot_mnlr(pc_sum)) /
  (fit_nplot_mnlr(pc1) | fit_nplot_mnlr(rh99)) +
  plot_layout(guides = "collect")
plot_model_sup

plot_model <- fit_nplot_mnlr(pc_ratio)
plot_model <- plot_model +
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
  width = 16, height = 12, units = "cm"
)




# ---- Supplementary/ extras ------

## Relative height and waveform amplitude profiles (Figures)

# Reshape relative height data to long format and filter by degradation categories
rh_long <- allGEDI2AB_amp %>%
  st_set_geometry(NULL) %>%  # Remove geometry if present
  filter(Degradation %in% degradation_categories) %>%  # Filter only the specified degradation types
  select(Degradation, starts_with("rh")) %>%  # Select relative height columns
  pivot_longer(cols = starts_with("rh"), names_to = "rh_index", values_to = "relative_height") %>%
  mutate(rh_index = as.numeric(gsub("rh", "", rh_index)))  # Extract numeric index

# Calculate the average relative height for each degradation type and height level
avg_rh_profile <- rh_long %>%
  group_by(Degradation, rh_index) %>%
  summarise(avg_relative_height = mean(relative_height, na.rm = TRUE)) %>%
  ungroup()


# Order the degradation types for output
avg_rh_profile <- avg_rh_profile %>%
  mutate(Degradation = factor(Degradation, levels = degradation_categories))

# Plot the average relative height profile by degradation type
avg_rh <- ggplot(avg_rh_profile, aes(x = rh_index, y = avg_relative_height, color = Degradation, group = Degradation)) +
  geom_line(size = 1) +
  labs(
    x = "Relative Height Percentile",  # Add x-axis label
    y = "Relative Height (m)"     # Shared y-axis label
  ) +
  scale_color_manual(values = degradation_colors) +
  theme_bw() +
  theme_fancy() +
  theme(
    legend.position = "none") +
  ylim(-5, 30)  # Limit y-axis to 30



# Drop geometry from selected_waveforms to avoid sf-related issues
selected_waveforms <- allGEDI2AB_amp %>%
  filter(Degradation %in% degradation_categories) %>%
  group_by(Degradation) %>%
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

# Combine amplitude and relative height data by shot_number, Degradation, and index
combined_waveform <- inner_join(amp_long, rh_long, by = c("shot_number", "Degradation", "index"))

combined_waveform <- combined_waveform %>%
  mutate(Degradation = factor(Degradation, levels = degradation_categories))

# Plot the waveform amplitude vs. relative height for each degradation type (Plot B)
rand_amp <- ggplot(combined_waveform, aes(x = relative_height, y = amplitude, color = Degradation, group = Degradation)) +
  geom_line(size = 1) +
  coord_flip() +  # Flip axes for profile orientation
  scale_color_manual(values = degradation_colors) +
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
  labs(color = "Condition")

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
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/Rh_amp.png",
       width = 20, height = 12, units = "cm"
)






## Correlation analysis used to aid informing the removal of highly correlated loadings in PCA (run before the PCA is applied)
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

# Keep the Degradation variable separate before PCA
degradation_type <- allGEDI$Degradation

# Remove unnecessary columns for PCA, but keep Degradation out for later use
allGEDIBPCA <- allGEDI%>%
  select(-Degradation, -solar_elevation, -l2b_quality_flag, -pgap_theta_error,
         -shot_number, -elev_highestreturn, -elev_lowestmode, -sensitivity, -geometry, 
         -Age_category, -beam, -l4_quality_flag, -Age_category2, -ALS_CRS, -matches("^rhz"),
         -agbd_se, -agbd_pi_lower, -omega, -starts_with("rh"), -modis_treecover, -pgap_theta,
         -agbd_pi_upper, -forest_age, -burn_freq, -starts_with("rx"), -num_detectedmodes,
         -forest_age.1, -validation.1, -burn_freq.1, agbd, cover, fhd_normal, rh98, pai)

# Rename to slot into code
allGEDI2ABPCA <- allGEDIBPCA






## Distribution of the data plots for supplementary per degradtion type

# Ensure the original data is intact by creating separate filtered datasets
allGEDI2ABfilter1 <- allGEDI2AB %>%
  filter(sensitivity > 0.98)

allGEDI2ABfilter2 <- allGEDI2AB %>%
  filter(sensitivity > 0.98 & solar_elevation < 0)

# Create combined data with an indicator for each filter
degradation_combined <- bind_rows(
  allGEDI2AB %>%
    mutate(Filter = "Original"),
  allGEDI2ABfilter1 %>%
    mutate(Filter = "Sensitivity > 0.98"),
  allGEDI2ABfilter2 %>%
    mutate(Filter = "Sensitivity > 0.98 & Solar Elevation < 0")
)

# Count samples by Degradation type and Filter
degradation_counts_combined <- degradation_combined %>%
  group_by(Filter, Degradation) %>%
  summarise(Sample_Count = n(), .groups = "drop") %>%
  mutate(Degradation = factor(Degradation, levels = c("Intact", "Logged", "Burned 1", "Burned 2", "Burned 3", "Burned 4+")))

# Check the output to ensure counts are correct
print(degradation_counts_combined)

# Plot using facet_wrap to display each filter condition in separate panels, side by side
figure7 <- ggplot(degradation_counts_combined, aes(x = Degradation, y = Sample_Count, fill = Degradation)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Sample_Count), vjust = -0.5, size = 4, color = "black") +
  labs(x = "Forest Condition", y = "Sample Count") +
  theme_minimal() +
  scale_fill_manual(name = "Condition", values = degradation_colors) +
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
# Run repeatedly along the GEDI profile e.g. rh25, rh50, rh75, rh96
rh_col <- "rh25"
rhz_col <- "rhz25"

# Initialize an empty list to store results
ccc_results_list <- list()

# Height Calculations
add_ccc_result("All", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col), "Height")
add_ccc_result("Intact", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "Degradation == 'Intact'"), "Height")
add_ccc_result("Logged", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "Degradation == 'Logged'"), "Height")
add_ccc_result("Burned 1-10", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "burn_freq %in% 1:10"), "Height")
add_ccc_result("Burned 1", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "burn_freq == 1"), "Height")
add_ccc_result("Burned 2", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "burn_freq == 2"), "Height")
add_ccc_result("Burned 3", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "burn_freq == 3"), "Height")
add_ccc_result("Burned 4-6", calculate_ccc(allGEDI2AB_ALSfilter1, rh_col, rhz_col, "burn_freq %in% 4:6"), "Height")

# Canopy Cover Calculations (fixed, not based upon rh value)
add_ccc_result("All", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov"), "Canopy Cover")
add_ccc_result("Intact", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "Degradation == 'Intact'"), "Canopy Cover")
add_ccc_result("Logged", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "Degradation == 'Logged'"), "Canopy Cover")
add_ccc_result("Burned 1-10", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "burn_freq %in% 1:10"), "Canopy Cover")
add_ccc_result("Burned 1", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "burn_freq == 1"), "Canopy Cover")
add_ccc_result("Burned 2", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "burn_freq == 2"), "Canopy Cover")
add_ccc_result("Burned 3", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "burn_freq == 3"), "Canopy Cover")
add_ccc_result("Burned 4-6", calculate_ccc_c(allGEDI2AB_ALSfilter1, "cover", "cancov", "burn_freq %in% 4:6"), "Canopy Cover")

# Combine all results into a single dataframe
ccc_results <- do.call(rbind, ccc_results_list)

# Print the results as a table
print(ccc_results)


# Set rh and rhz columns for calculation (for example, rh25 and rhz25)
# Run repeatedly along the GEDI profile e.g. rh25, rh50, rh75, rh96
rh_col <- "rh25"
rhz_col <- "rhz25"

# Initialize an empty list to store results
ccc_results_list <- list()

# Height Calculations
add_ccc_result("All", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col), "Height")
add_ccc_result("Intact", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "Degradation == 'Intact'"), "Height")
add_ccc_result("Logged", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "Degradation == 'Logged'"), "Height")
add_ccc_result("Burned 1-10", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "burn_freq %in% 1:10"), "Height")
add_ccc_result("Burned 1", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "burn_freq == 1"), "Height")
add_ccc_result("Burned 2", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "burn_freq == 2"), "Height")
add_ccc_result("Burned 3", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "burn_freq == 3"), "Height")
add_ccc_result("Burned 4-6", calculate_ccc(allGEDI2AB_ALSfilter2, rh_col, rhz_col, "burn_freq %in% 4:6"), "Height")

# Canopy Cover Calculations (fixed, not based upon rh value)
add_ccc_result("All", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov"), "Canopy Cover")
add_ccc_result("Intact", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "Degradation == 'Intact'"), "Canopy Cover")
add_ccc_result("Logged", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "Degradation == 'Logged'"), "Canopy Cover")
add_ccc_result("Burned 1-10", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "burn_freq %in% 1:10"), "Canopy Cover")
add_ccc_result("Burned 1", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "burn_freq == 1"), "Canopy Cover")
add_ccc_result("Burned 2", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "burn_freq == 2"), "Canopy Cover")
add_ccc_result("Burned 3", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "burn_freq == 3"), "Canopy Cover")
add_ccc_result("Burned 4-6", calculate_ccc_c(allGEDI2AB_ALSfilter2, "cover", "cancov", "burn_freq %in% 4:6"), "Canopy Cover")

# Combine all results into a single dataframe
ccc_results <- do.call(rbind, ccc_results_list)

# Print the results as a table
print(ccc_results)








## Original PCA graph with 27 loadings

# Plot 1: Full Biplot with All Loadings
scale <- 24  # Extend arrow length
biplot_full <- ggplot(data = components, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Degradation), size = 0.7, shape = 19, alpha = 0.8) +
  geom_segment(data = as.data.frame(pca_result$rotation), 
               aes(x = 0, y = 0, xend = PC1 * scale, yend = PC2 * scale), 
               arrow = arrow(length = unit(0.1, "cm"), type = "open", angle = 25), 
               size = 0.8, color = "darkblue") +
  geom_text_repel(data = as.data.frame(pca_result$rotation), 
                  aes(x = PC1 * scale, y = PC2 * scale, label = rownames(pca_result$rotation)), 
                  size = 2.5, color = "black", max.overlaps = 10, 
                  box.padding = 0.35, point.padding = 0.5) +
  scale_color_manual(values = colors) +
  labs(title = "Biplot - PCA", x = "PC1", y = "PC2", color = "Condition") +
  theme_minimal() +
  theme_fancy() +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +  # Adjust axis limits
  theme(
    legend.position = c(0.95, 0.05),  # Adjust legend position
    legend.justification = c("center", "bottom")  # Anchor legend
  )

# Display the full biplot
print(biplot_full)

# FIGURE: Squashed PCA axis plots

# Define common x and y axis limits
x_limits <- c(-10, 10)

# Add the Degradation type from degradation_type to pca_data
pca_data <- as.data.frame(pca_result$x)
pca_data$Degradation <- degradation_type
pca_data$Degradation <- factor(pca_data$Degradation, levels = c("Burned 4+", "Burned 3", 
                                                                "Burned 2", "Burned 1", "Logged", "Intact"))


# Create PC1 plot with facets
fig_pc1 <- ggplot(pca_data, aes(x = PC1, fill = Degradation)) +  
  geom_histogram(binwidth = 0.5, color = "white", size = 0.1, show.legend = FALSE) +  # Thin white outline on bins
  scale_fill_manual(values = degradation_colors) +
  labs(x = "PC1 Values", y = "Frequency") +
  xlim(x_limits) +
  facet_wrap(~ Degradation, nrow = 1, scales = "free_y") +  # One row with independent y-scales
  theme_minimal() +  
  theme_fancy() &
  labs(color = "Condition")  

# Create PC2 plot with facets
fig_pc2 <- ggplot(pca_data, aes(x = PC2, fill = Degradation)) +  
  geom_histogram(binwidth = 0.5, color = "white", size = 0.1, show.legend = FALSE) +  # Thin white outline on bins
  scale_fill_manual(values = degradation_colors) +
  labs(x = "PC2 Values", y = "Frequency") +
  xlim(x_limits) +
  facet_wrap(~ Degradation, nrow = 1, scales = "free_y") +  # One row with independent y-scales
  theme_minimal() +   
  theme_fancy() &
  labs(color = "Condition")  


# Combine both PC1 and PC2 panels
fig_pcs <- (fig_pc1) / (fig_pc2)
fig_pcs

# Figure with both panels

figure4 <- (biplot_full) / (fig_pcs) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 8, face= "bold"),
    plot.tag.position = c(0, 1)  # Position the tags, (0, 1) is top-left
  )

print(figure4)

ggsave(figure4,
       filename = "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Publication_outputs/PCA_27loadings.png",
       width = 18, height = 20, units = "cm"
)












# 
# # Validation extra
# 
# allGEDI2AB <- allGEDI2AB %>%
#       left_join(landsat_treecover_df, by = "shot_number")
# 
# allGEDI2AB_ALS <- allGEDI2AB_ALS %>%
#      left_join(landsat_treecover_df, by = "shot_number")
#  burnedover40 <- excluded_shots <- allGEDI2AB %>%
#       filter(Age_category == ">40", 
#           str_detect(Degradation, "Burned"))


# # Extra validation step: removing unclassified burned forest below 0.3 cover (predominantly misclassified farmland)
# 
# # Using cover metrics from allGEDI2AB, isolate the shot_number values that meet filtering criteria
# excluded_shots <- allGEDI2AB %>%
#   filter(Age_category == ">40", 
#          str_detect(Degradation, "Burned"), 
#          cover > 0.3) %>%
#   pull(shot_number)
# 
# # Remove rows with those `shot_number` values from allGEDI2AB
# 
# allGEDI <- allGEDI %>%
#   filter(!shot_number %in% excluded_shots)
# 
# allGEDI2A <- allGEDI2A %>%
#   filter(!shot_number %in% excluded_shots)
# 
# allGEDI2AB <- allGEDI2AB %>%
#   filter(!shot_number %in% excluded_shots)
# 
# allGEDI2AB_ALS <- allGEDI2AB_ALS%>%
#   filter(!shot_number %in% excluded_shots)
# 
# allGEDI2AB_amp <- allGEDI2AB_amp %>%
#   filter(!shot_number %in% excluded_shots)








