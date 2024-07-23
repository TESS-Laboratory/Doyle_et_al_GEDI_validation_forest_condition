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



# ----- Pre-Process ALS done -------- 
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


# ----------- GEDI download done ----------------

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



# ------ GEDI regressions NEED WAVEFORMLIDAR ADD KURTOSIS SKEW ETC ---------

# Summarise relative height rh0-100 metrics with linear regression model 
# Outputs intercept, slope and variance of the 2A waveform
# USE THE WAVEFORMLIDAR VIGNETTE TO EXTRACT MORE VARIABLES

GEDI2AB_trans <- allGEDI2AB %>%
  as.data.frame() %>%
  select(shot_number, starts_with("rh"))

# Apply regression function to each row of the dataframe
result_df <- apply(GEDI2AB_trans, 1, rh_linear_regression)

# Convert the result to a dataframe and set column names
result_df <- t(data.frame(result_df))
colnames(result_df) <- c("shot_number", "G_intercept", "G_slope", "G_variance")
result_df <- as.data.frame(result_df) %>%
  as.numeric(G_intercept, G_slope, G_variance)

library(waveformlidar)

#waveform lidar bits here






# Remerge with original GEDI2A dataframe

allGEDI2AB_reg <- left_join(allGEDI2AB, result_df, by = "shot_number", copy = TRUE)

sf::st_write(allGEDI2AB_reg, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2AB_reg <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb")


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

# Tidy the environment
rm(GEDI2AB_trans, result_df, allGEDI4A_no_geom)

# ------ Extracting ALS metrics within GEDI footprints ------

# As the ALS data spans different parts of the Amazon rainforest, they are separated in folders by their
# CRS. Each ALS folder must therefore extract data from the GEDI footprints separately before being merged.

# Reload/filter and set CRS of final ALS catalogs if no longer in environment

DAAC18_19Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_19S/final_norm')
CAUT23_20Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/CAUT23_20S/final_norm')
DAAC1821_21Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC1821_21S/final_norm')

DAAC18_19Sfinal <- filter_als(DAAC18_19Sfinal)
CAUT23_20Sfinal <- filter_als(CAUT23_20Sfinal1)
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

DAAC18_19Smetrics <- plot_metrics(DAAC18_19Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_reg_19S, radius = 12.5)
CAUT23_20Smetrics <- plot_metrics(CAUT23_20Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_reg_20S, radius = 12.5)
DAAC1821_21Smetrics <- plot_metrics(DAAC1821_21Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2AB_reg_21S, radius = 12.5)

# Merging metrics and using control CRS (WGS84) associated with GEDI data now extraction is complete

DAAC18_19Smetrics <- st_transform(DAAC18_19Smetrics , "EPSG:32643")
CAUT23_20Smetrics1 <- st_transform(CAUT23_20Smetrics1, "EPSG:32643")
CAUT23_20Smetrics2 <- st_transform(CAUT23_20Smetrics2, "EPSG:32643")
DAAC21_21Smetrics <- st_transform(DAAC21_21Smetrics, "EPSG:32643")

st_crs(DAAC18_19Smetrics)



# Remove duplicated rows
DAAC18_19Smetrics <- distinct(DAAC18_19Smetrics)
CAUT23_20Smetrics <- distinct(CAUT23_20Smetrics)
DAAC1821_21Smetrics <- distinct(DAAC121_21Smetrics)

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
  select(year, solar_elevation, elev_highestreturn, elev_lowestmode,
         sensitivity, shot_number, degrade_flag, ALS_CRS, rh0, rh10, rh25, rh50, rh75, rh90, rh95, 
         rh96, rh97, rh98, rh99, rh100, G_intercept, G_slope, G_variance, cover, pai, fhd_normal,
         pgap_theta, modis_treecover, geometry, rhz10, rhz25, rhz50, rhz75, rhz90, rhz95, 
         rhz96, rhz97, rhz98, rhz99, max, cancov, z_kurt, z_skew)


sf::st_write(allGEDI2AB_ALS, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb", delete_dsn = TRUE, overwrite = TRUE)
#allGEDI2AB_ALS <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_ALS.fgb")


# Tidy the environment
rm(merged_df, DAAC18_19Smetrics, DAAC21_21Smetrics, CAUT23_20Smetrics, allGEDI2AB_reg_19S, 
    allGEDI2AB_reg_20S, allGEDI2AB_reg_21S, DAAC18_19Sfinal, DAAC1821_21Sfinal, CAUT23_20Sfinal)

# ------- Forest spectral classification done---------

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
for (file in Burned_files) {
  raster_obj <- raster(file)
  Burned_list[[length(Burned_list) + 1]] <- raster_obj
}

# Merge rasters
merged_Burned <- do.call(merge, Burned_list)
#merged_Burned <- projectRaster(merged_Burned, crs = sr)

# Crop raster
Burnedforest <- crop(merged_Burned, secondaryforest2024)

writeRaster(Burnedforest, paste0(output_folder, "/", "MAPBIOMASfire.tif"), overwrite=TRUE)
Burnedforest <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Fire_data/MAPBIOMASfire.tif")


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


# ----------- GEDI classification extraction done --------------

# Extract values for forest age, burn frequency #and ALS extent#, binding new columns to GEDI data frame

forest_age <- raster::extract(secondaryforest2024, allGEDI2AB_reg, method='simple')
allGEDI2AB_reg <- cbind(allGEDI2AB_reg, forest_age)

burn_freq <- terra::extract(Burnedforest, allGEDI2AB_reg, method='simple')
allGEDI2AB_reg <- cbind(allGEDI2AB_reg, burn_freq)

validation <- terra::extract(forestclass, allGEDI2AB_reg, method='simple')
allGEDI2AB_reg <- cbind(allGEDI2AB_reg, validation)


# Segregate the secondary/ Intact forest samples of GEDI footprints

# Edit original allGEDI2AB_reg file to have a numeric value for ageless Intact forest (n/a)
allGEDI2AB_reg_aged <- allGEDI2AB_reg %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age))

# Filter for Intact forest only (values of 99)
allGEDI2AB_reg_Intact <- filter(allGEDI2AB_reg_aged, forest_age>90)

# Just degraded forest samples (classified with age by secondary forest raster)
allGEDI2AB_reg_sec <- filter(allGEDI2AB_reg_aged, forest_age<90)

# Mapview check of data layers
mapview(allGEDI2AB_reg_sec, zcol = "forest_age") + mapview(secondaryforest2024) + mapview(allGEDI2AB_reg_Intact)



# To create gradient of degradation from Intact to degraded/ recovering forest
# Samples spread across the Amazon sites with various soil/ distance to river/ distance to degradation

# Randomly sample 150 GEDI footprints (~20% of sample size) that meet above criteria and create column highlighting them as =1
allGEDI2AB_reg_Intact <- allGEDI2AB_reg_Intact %>%
  mutate(Intact_sample = as.integer(row_number() %in% sample(n(), size = 150)))

# Filter just the Intact GEDI footprint samples
allGEDI2AB_reg_Intact_sample <- allGEDI2AB_reg_Intact %>%
  filter(Intact_sample==1) %>%
  dplyr::select(-Intact_sample) 

mapview(allGEDI2AB_reg_Intact_sample)



# Merge the secondary forest and ALS extent GEDI footprints data frame with new random Intact samples
# Remove the data entries for Rondonia that were classified as non forest in validation dataset
# Clean dataset and extract degradation values into classified columns using process_GEDI_degradation function
# For complete gradient of GEDI samples across the site

GEDI2AB <- rbind(allGEDI2AB_reg_sec, allGEDI2AB_reg_Intact_sample)

GEDI2AB <- GEDI2AB %>%
  filter(validation != 0 | is.na(validation)) %>%
  process_GEDI_degradation()


sf::st_write(GEDI2AB, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2AB.fgb", delete_dsn = TRUE, overwrite = TRUE)
# GEDI2AB <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2AB.fgb")

mapview(secondaryforest2024) + mapview(Burnedforest) + mapview(GEDI2AB, zcol = "Degradation")


# Repeat degradation extraction for all datasets
{

# Mutate allGEDI2AB_reg, allGEDI2AB_ALS and allGEDI2AB4A_reg data set to also reflect degradation type
# Overwrite files for final data frames for analysis/ summaries

allGEDI2AB_reg <- allGEDI2AB_reg_aged %>%
  process_GEDI_degradation()

allGEDI2AB4A_reg_aged <- allGEDI2AB4A_reg %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age))

allGEDI2AB4A_reg <- allGEDI2AB4A_reg_aged %>%
  process_GEDI_degradation()

allGEDI2AB_ALS_aged <- allGEDI2AB_ALS %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age))

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

# Overwrite final datasets : allGEDI2AB_reg (GEDI2A with regression), 
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


# Tidy environment

rm(allGEDI, allGEDI2AB, allGEDI2AB_reg_aged, allGEDI2AB_reg_Intact, allGEDI2AB_reg_Intact_sample, 
   allGEDI2AB_reg_sec, allGEDI2AB4A_reg_aged, allGEDI2AB_ALS_aged)
                  
# ------ Statistics CHECK ---------

# Calculate Lin's CCC model for entire GEDI/ ALS dataset at rh97 using calculate_ccc function
ccc_mod <- calculate_ccc(allGEDI2AB_ALS)
print(ccc_mod)

# Subsets of forest gradient of degradation/ recovery
ccc_modint <- calculate_ccc(allGEDI2AB_ALS, "Degradation == 'Intact'")
print(ccc_modint)

ccc_modlog <- calculate_ccc(allGEDI2AB_ALS, "Degradation == 'Logged'")
print(ccc_modlog)

ccc_modburn <- calculate_ccc(allGEDI2AB_ALS, "Degradation == 'Burned' | Degradation == 'Burned 3+'")
print(ccc_modburn)

ccc_modburnfreq1_3 <- calculate_ccc(allGEDI2AB_ALS, "burn_freq %in% 1:3")
print(ccc_modburnfreq1_3)

ccc_modburnfreq4_6 <- calculate_ccc(allGEDI2AB_ALS, "burn_freq %in% 4:6")
print(ccc_modburnfreq4_6)




# RMSE calculations Inspired by Dorado et al (2021)

# Define linear regression models for different conditions and burn frequencies
mod <- lm(rhz95 ~ rh95, data = allGEDI2AB_ALS)
modint <- lm(rhz95[Degradation == 'Intact'] ~ rh95[Degradation == 'Intact'], data = allGEDI2AB_ALS)
modlog <- lm(rhz95[Degradation == 'Logged'] ~ rh95[Degradation == 'Logged'], data = allGEDI2AB_ALS)
modburn <- lm(rhz95[Degradation == 'Burned'] ~ rh95[Degradation == 'Burned'], data = allGEDI2AB_ALS)
modburnfreq1_3 <- lm(rhz95[burn_freq %in% 1:3] ~ rh95[burn_freq %in% 1:3], data = allGEDI2AB_ALS)
modburnfreq4_6 <- lm(rhz95[burn_freq %in% 4:6] ~ rh95[burn_freq %in% 4:6], data = allGEDI2AB_ALS)


# Compute RMSE
# Difference between rhz and rh predicted values - positive or negative not identified
# Error as opposed to fitting a modeled error to predict hypothetical error
rmse <- sqrt(sum((allGEDI2AB_ALS$rhz95 - allGEDI2AB_ALS$rh95)^2, na.rm = TRUE) / length(which(!is.na(allGEDI2AB_ALS$rhz95))))
rmse_int <- sqrt(sum((allGEDI2AB_ALS$rhz95[which(allGEDI2AB_ALS$Degradation == 'Intact')] - allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$Degradation == 'Intact')])^2, na.rm = TRUE) /length(which(!is.na(allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$Degradation == 'Intact')]))))
rmse_log <- sqrt(sum((allGEDI2AB_ALS$rhz95[which(allGEDI2AB_ALS$Degradation == 'Logged')] - allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$Degradation == 'Logged')])^2, na.rm = TRUE)/length(which(!is.na(allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$Degradation == 'Logged')]))))
rmse_burn <- sqrt(sum((allGEDI2AB_ALS$rhz95[which(allGEDI2AB_ALS$Degradation == 'Burned')] - allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$Degradation == 'Burned')])^2, na.rm = TRUE)/length(which(!is.na(allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$Degradation == 'Burned')]))))
rmse_burnfreq1_3 <- sqrt(sum((allGEDI2AB_ALS$rhz95[which(allGEDI2AB_ALS$burn_freq %in% 1:3)] - allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$burn_freq %in% 1:3)])^2, na.rm = TRUE) / length(which(!is.na(allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$burn_freq %in% 1:3)]))))
rmse_burnfreq4_6 <- sqrt(sum((allGEDI2AB_ALS$rhz95[which(allGEDI2AB_ALS$burn_freq %in% 4:6)] - allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$burn_freq %in% 4:6)])^2, na.rm = TRUE) / length(which(!is.na(allGEDI2AB_ALS$rh95[which(allGEDI2AB_ALS$burn_freq %in% 4:6)]))))

# Compute mean ALS height
mean_actual <- sum(mean(allGEDI2AB_ALS$rhz95))

# Compute relative rmse
rrmse <- sum(rmse/mean_actual)*100
rrmse_int <- sum(rmse_int/mean_actual)*100
rrmse_log <- sum(rmse_log/mean_actual)*100
rrmse_burn <- sum(rmse_burn/mean_actual)*100
rrmse_burnfreq1_3 <- sum(rmse_burnfreq1_3/mean_actual)*100
rrmse_burnfreq4_6 <- sum(rmse_burnfreq4_6/mean_actual)*100


# Compute bias:  Average difference between rhz and rh (in this case hyp is -0.7m lower than rh)
bias <- sum((allGEDI2AB_ALS$rhz95 - allGEDI2AB_ALS$rh95), na.rm = TRUE)/length(which(!is.na(allGEDI2AB_ALS$rh95)))
bias_int <- sum(allGEDI2AB_ALS$rhz95[allGEDI2AB_ALS$Degradation == 'Intact'] - allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$Degradation == 'Intact'], na.rm = TRUE)/length(which(!is.na(allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$Degradation == 'Intact'])))
bias_log <- sum(allGEDI2AB_ALS$rhz95[allGEDI2AB_ALS$Degradation == 'Logged'] - allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$Degradation == 'Logged'], na.rm = TRUE)/length(which(!is.na(allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$Degradation == 'Logged'])))
bias_burn <- sum(allGEDI2AB_ALS$rhz95[allGEDI2AB_ALS$Degradation == 'Burned'] - allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$Degradation == 'Burned'], na.rm = TRUE)/length(which(!is.na(allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$Degradation == 'Burned'])))
bias_burnfreq1_3 <- sum(allGEDI2AB_ALS$rhz95[allGEDI2AB_ALS$burn_freq %in% 1:3] - allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$burn_freq %in% 1:3], na.rm = TRUE) / length(which(!is.na(allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$burn_freq %in% 1:3])))
bias_burnfreq4_6 <- sum(allGEDI2AB_ALS$rhz95[allGEDI2AB_ALS$burn_freq %in% 4:6] - allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$burn_freq %in% 4:6], na.rm = TRUE) / length(which(!is.na(allGEDI2AB_ALS$rh95[allGEDI2AB_ALS$burn_freq %in% 4:6])))


# Compute relative bias : rbias (y = mean height of the hypsometer) % error relative to height of category
rbias <- sum(bias/mean_actual)*100
rbias_int <- sum(bias_int/mean_actual)*100
rbias_log <- sum(bias_log/mean_actual)*100
rbias_burn <- sum(bias_burn/mean_actual)*100
rbias_burnfreq1_3 <- sum(bias_burnfreq1_3/mean_actual)*100
rbias_burnfreq4_6 <- sum(bias_burnfreq4_6/mean_actual)*100


# Create a dataframe to store the results
stats_results <- data.frame(
  Forest_Condition = c("All", "Intact", "Logged", "Burned", "Burned 1-3", "Burned 4-6"),
  Pearsons_r = c(coef(mod)[2], coef(modint)[2], coef(modlog)[2], coef(modburn)[2], coef(modburnfreq1_3)[2], coef(modburnfreq4_6)[2]),
  RMSE_m = c(rmse, rmse_int, rmse_log, rmse_burn, rmse_burnfreq1_3, rmse_burnfreq4_6),
  rRMSE = c(rrmse, rrmse_int, rrmse_log, rrmse_burn, rrmse_burnfreq1_3, rrmse_burnfreq4_6),
  Bias_m = c(bias, bias_int, bias_log, bias_burn, bias_burnfreq1_3, bias_burnfreq4_6),
  Relative_Bias = c(rbias, rbias_int, rbias_log, rbias_burn, rbias_burnfreq1_3, rbias_burnfreq4_6)
)










conditions <- c("All", "Degradation == 'Intact'", "Degradation == 'Logged'", 
                "burned", "burn_freq %in% 1:3", "burn_freq %in% 4:6")

# Initialize vectors to store results
pearsons_r <- numeric(length(conditions))
rmse_m <- numeric(length(conditions))
rrmse <- numeric(length(conditions))
bias_m <- numeric(length(conditions))
relative_bias <- numeric(length(conditions))

# Calculate mean actual height
mean_actual <- mean(allGEDI2AB_ALS$rhz95, na.rm = TRUE)

for (i in seq_along(conditions)) {
  condition <- conditions[i]
  if (condition == "All") {
    data_subset <- allGEDI2AB_ALS
  } else if (condition == "burned") {
    data_subset <- handle_burned_condition(allGEDI2AB_ALS)
  } else {
    data_subset <- allGEDI2AB_ALS %>% filter(!!rlang::parse_expr(condition))
  }
  pearsons_r[i] <- calculate_coef(data_subset)
  stats <- calculate_stats(data_subset)
  rmse_m[i] <- stats$rmse
  rrmse[i] <- (stats$rmse / mean_actual) * 100
  bias_m[i] <- stats$bias
  relative_bias[i] <- (stats$bias / mean_actual) * 100
}

# Create a dataframe to store the results
stats_results <- data.frame(
  Forest_Condition = c("All", "Intact", "Logged", "Burned", "Burned 1-3", "Burned 4-6"),
  Pearsons_r = pearsons_r,
  RMSE_m = rmse_m,
  rRMSE = rrmse,
  Bias_m = bias_m,
  Relative_Bias = relative_bias
)

print(stats_results)






















# TESTING FOR THE PCA


# Eventually need this to inlcude all the ALS waveform stats, GEDI rh intervals
# canopy cover metrics etc
allGEDI2APCA <- GEDI2AB_ALS %>%
  mutate(Degradation_numeric = case_when(
    Degradation == 'Burned' ~ "2",
    Degradation == 'Logged' ~ "1",
    Degradation == 'Intact' ~ "0",
    TRUE ~ NA_character_)) %>% 
      select(-solar_elevation, -lat_lowestmode, -lon_lowestmode, 
         -shot_number, -elev_highestreturn, -elev_lowestmode, 
         -sensitivity, -degrade_flag, -geometry, -Age_category,
         -Age_category2, -year, -ALS_CRS, -Degradation2, 
         -Degradation, -rh0) #- forest_age, -burn_freq)

allGEDI2APCA <- st_drop_geometry(allGEDI2APCA)
         
allGEDI2APCA$G_intercept <- as.numeric(allGEDI2APCA$G_intercept)
allGEDI2APCA$G_slope <- as.numeric(allGEDI2APCA$G_slope)
allGEDI2APCA$G_variance <- as.numeric(allGEDI2APCA$G_variance)
allGEDI2APCA$Degradation_numeric <- as.numeric(allGEDI2APCA$Degradation_numeric)

str(allGEDI2APCA)


# Standardize data and run PCA analysis
scaled_data <- scale(allGEDI2APCA)
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

#  Summary of the variance explained by each principal component
summary(pca_result)

# Extract loadings for the first few PCs for Burned data
loadings_pca <- pca_result$rotation[, 1:3]  # Adjust the number (e.g., 1:3) for the desired number of PCs
print(loadings_pca)

# Loadings of variables on each principal component
loadings(pca_result)

# Scree plot to visualize variance explained
screeplot(pca_result, type = "line")

# Biplot to visualize scores and loadings
biplot(pca_result)

# Scatterplot of scores on the first two principal components
ggplot(as.data.frame(pca_result$x), aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(x = "Principal Component 1",
       y = "Principal Component 2",
       title = "PCA: Scores Plot")



# Plotly graphs to show degradation type with overall PCA results
components <- as.data.frame(pca_result$x)
loadings <- (pca_result)$rotation

# Extract degradation types
degradation_type <- allGEDI2APCA$Degradation_numeric

# Create a color palette for degradation types
colors <- c("chartreuse3", "deepskyblue", "red")  # You may need to adjust the colors

# Reverse PC2 to match the example
components$PC2 <- -components$PC2

# Create the plot
fig <- plot_ly(components, x = ~PC1, y = ~PC2, color = ~degradation_type, 
               colors = colors, type = 'scatter', mode = 'markers') %>%
  layout(
    plot_bgcolor = "white",  # Set background color to white
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2")
  )

# Add loadings
features <- colnames(allGEDI2APCA)
for (i in seq_along(features)) {
  fig <- fig %>%
    add_segments(x = 0, xend = loadings[i, 1], y = 0, yend = loadings[i, 2], 
                 line = list(color = 'black'), inherit = FALSE, showlegend = FALSE) %>%
    add_annotations(x = loadings[i, 1], y = loadings[i, 2], ax = 0, ay = 0, 
                    text = features[i], xanchor = 'center', yanchor = 'bottom', 
                    showarrow = FALSE, showlegend = FALSE)
}

fig





# Subset data for each degradation type
Burned_data <- allGEDI2APCA[allGEDI2APCA$Degradation_numeric == 2, ]
Logged_data <- allGEDI2APCA[allGEDI2APCA$Degradation_numeric == 1, ]
Intact_data <- allGEDI2APCA[allGEDI2APCA$Degradation_numeric == 0, ]

# Remove columns with zero variance (excluding Degradation_numeric)
Burned_data <- Burned_data[, apply(Burned_data[, -which(names(Burned_data) == "Degradation_numeric")], 2, function(x) var(x) != 0)]
Logged_data <- Logged_data[, apply(Logged_data[, -which(names(Logged_data) == "Degradation_numeric")], 2, function(x) var(x) != 0)]
Intact_data <- Intact_data[, apply(Intact_data[, -which(names(Intact_data) == "Degradation_numeric")], 2, function(x) var(x) != 0)]

# Perform PCA for Burned data
pca_Burned <- prcomp(select(Burned_data, -Degradation_numeric), center = TRUE, scale. = TRUE)

# Perform PCA for Logged data
pca_Logged <- prcomp(select(Logged_data, -Degradation_numeric), center = TRUE, scale. = TRUE)

# Perform PCA for Intact data
pca_Intact <- prcomp(select(Intact_data, -Degradation_numeric), center = TRUE, scale. = TRUE)

# Analyze PCA results for each subset
summary(pca_Burned)
summary(pca_Logged)
summary(pca_Intact)

# Extract loadings for the first few PCs for Burned data
loadings_Burned <- pca_Burned$rotation[, 1:3]  # Adjust the number (e.g., 1:3) for the desired number of PCs

# Extract loadings for the first few PCs for Logged data
loadings_Logged <- pca_Logged$rotation[, 1:3]

# Extract loadings for the first few PCs for Intact data
loadings_Intact <- pca_Intact$rotation[, 1:3]

# Print the loadings
print(loadings_Burned)
print(loadings_Logged)
print(loadings_Intact)

# Plot PCA results for each subset
plot(pca_Burned, main = "PCA for Burned Data")
plot(pca_Logged, main = "PCA for Logged Data")
plot(pca_Intact, main = "PCA for Intact Data")
# Plot PCA results for each subset
plot(pca_Burned, main = "PCA for Burned Data")
plot(pca_Logged, main = "PCA for Logged Data")
plot(pca_Intact, main = "PCA for Intact Data")










# ------- Graphs/ visulisations CHECK-------


allGEDI2AB_ALS <- allGEDI2AB_ALS %>%
  mutate(Degradation = case_when(
    burn_freq == 2 ~ "Burned",
    burn_freq == 1 ~ "Burned",
    burn_freq > 2 ~ "Burned 3+",
    forst_g < 50 ~ "Logged",
    forst_g > 50 ~ "Intact",
    TRUE ~ NA_character_))

# Violin plot for GEDI top height across degradation
GEDIrh99_degradation <- allGEDI2AB_ALS %>%
  ggplot(aes(x = Degradation, y = rh99, fill = Degradation)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  labs(title = "GEDI Canopy height with various degradation type", x = "Degradation type", y = "Relative height top of canopy (m) (rh99)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
plot(GEDIrh99_degradation)

# Violin plot for GEDI top height across degradation
GEDIcover_violin <- allGEDI_gradient %>%
  ggplot(aes(x = Degradation, y = cover, fill = Degradation)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  labs(title = "GEDI Canopy cover with various degradation type", x = "Degradation type", y = "Canopy cover (%)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
plot(GEDIcover_violin)


# Nice to have one for canopy cover / AGB too


# GEDI DEGRADATION AGE
age_order <- c("<6", "6-15", "15-25", "25-40", ">40")
GEDI2AB_ALS <- mutate(GEDI2AB_ALS, Age_category = factor(Age_category, levels = age_order))

GEDIheight_age <- GEDI2AB_ALS %>%
  ggplot() +
  geom_point(aes(x = Age_category, y = rh99, color = Degradation2)) +
  labs(title = "GEDI canopy height by degradation", x = "Forest age", y = "Canopy height (m)") +
  theme_bw() +
  scale_colour_manual(values = c("orange", "red", "chartreuse3","deepskyblue" ))

plot(GEDIheight_age)




# GEDI CANOPY COVER AND AGE
# Define breaks for age categories
breaks <- c(0, 6, 15, 25, 40, 99)
labels <- c("<6", "6-15", "15-25", "25-40", "<40")
allGEDI_gradient <- mutate(allGEDI_gradient, Age_category = cut(forest_age, breaks = breaks, labels = labels))

GEDIcover_age <- allGEDI_gradient %>%
  ggplot() +
  geom_point(aes(x = Age_category, y = cover, color = Degradation)) +
  labs(title = "GEDI canopy cover", x = "Forest age", y = "Canopy cover (%)") +
  #scale_color_continuous(name = "AGBD", low = "greenyellow", high = "darkgreen") +  # Set color for the legend bar
  theme_bw()

plot(GEDIcover_age)


# GEDI CANOPY COVER AND DEGRADATION
# Define breaks for age categories
breaks <- c(0, 6, 15, 25, 40, 99)
labels <- c("<6", "6-15", "15-25", "25-40", "<40")
allGEDI_gradient <- mutate(allGEDI_gradient, Age_category = cut(forest_age, breaks = breaks, labels = labels))

GEDIcover_degradation <- allGEDI_gradient %>%
  ggplot() +
  geom_point(aes(x = Degradation, y = cover, color = Degradation)) +
  labs(title = "GEDI canopy cover", x = "Forest age", y = "Canopy cover (%)") +
  #scale_color_continuous(name = "AGBD", low = "greenyellow", high = "darkgreen") +  # Set color for the legend bar
  theme_bw()

plot(GEDIcover_degradation)

# GEDI CANOPY COVER AND CARBON
# Define breaks for age categories
breaks <- c(0, 6, 15, 25, 40, 99)
labels <- c("<6", "6-15", "15-25", "25-40", "<40")
allGEDI_gradient <- mutate(allGEDI_gradient, Age_category = cut(forest_age, breaks = breaks, labels = labels))

GEDIcover_agbd <- allGEDI_gradient %>%
  ggplot() +
  geom_point(aes(x = Age_category, y = cover, color = agbd)) +
  labs(title = "GEDI canopy cover", x = "Forest age", y = "Canopy cover (%)") +
  scale_color_continuous(name = "AGBD", low = "greenyellow", high = "darkgreen") +  # Set color for the legend bar
  theme_bw()

plot(GEDIcover_agbd)



# Graph for correspondence between ALS95 and GEDI95
ALSGEDIrh95 <- allGEDI2AB_ALS %>%
  ggplot(aes(x=rhz95, y=rh95, color=Degradation)) +
  geom_point() +
  labs(title = "Correspondance between ALS and GEDI 95%", x = "ALS", y = "GEDI") +
  theme_bw() +
  scale_colour_manual(values = c("orange", "red", "chartreuse3","deepskyblue" ))
plot(ALSGEDIrh95)


# Graph for correspondence between ALS95 and GEDI95
ALSGEDIrh75 <- allGEDI2AB_ALS %>%
  ggplot(aes(x=rhz75, y=rh75, color=Degradation)) +
  geom_point() +
  labs(title = "Correspondance between ALS and GEDI 75%", x = "ALS", y = "GEDI") +
  theme_bw() 
#scale_colour_manual(values = c("darkorange", "darkolivegreen3", "Black"))
plot(ALSGEDIrh75)

# Graph for correspondence between ALS95 and GEDI95
ALSGEDIrh95 <- allGEDI2AB_ALS %>%
  ggplot(aes(x=rhz95, y=rh95, color=Degradation)) +
  geom_point() +
  labs(title = "Correspondance between ALS and GEDI 95%", x = "ALS", y = "GEDI") +
  theme_bw() 
#scale_colour_manual(values = c("darkorange", "darkolivegreen3", "Black"))
plot(ALSGEDIrh95)


# Graph for correspondence between ALS95 and GEDI95
ALSGEDIrh50 <- allGEDI2AB_ALS %>%
  ggplot(aes(x=rhz75, y=rh50, color=Degradation)) +
  geom_point() +
  labs(title = "Correspondance between ALS and GEDI 50%", x = "ALS", y = "GEDI") +
  theme_bw() 
#scale_colour_manual(values = c("darkorange", "darkolivegreen3", "Black"))
plot(ALSGEDIrh50)



# NEED TO LOOK AT PLOTTING THE VARIANCES

GEDI2AB_ALS$G_slope <- as.numeric(GEDI2AB_ALS$G_slope)
GEDI2AB_ALS$G_variance <- as.numeric(GEDI2AB_ALS$G_variance)
GEDI2AB_ALS$G_intercept <- as.numeric(GEDI2AB_ALS$G_intercept)

GEDI_slope <- GEDI2AB_ALS %>%
  ggplot(aes(x=Degradation, y=G_variance, color=Degradation)) +
  geom_point() +
  labs(title = "G_slope", x = "height", y = "G_slope") +
  theme_bw() 
#scale_colour_manual(values = c("darkorange", "darkolivegreen3", "Black"))
plot(GEDI_slope)

# Violin plot for GEDI top height across degradation
GEDI_slope <- GEDI2AB_ALS %>%
  ggplot(aes(x=Degradation, y=G_slope, fill=Degradation)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  labs(title = "Gradient slope of relative height profile", x = "Degradation type", y = "Slope") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
plot(GEDI_slope)

# Violin plot for GEDI top height across degradation

GEDI2AB_ALStest <- GEDI2AB_ALS %>%
  mutate(status = ifelse(Degradation == "Logged" | Degradation == "Burned", "degraded", "Intact"))

GEDI_condition_test <- GEDI2AB_ALStest %>%
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







#make column for forest extent and under 4m (maybe 5 for FAO definition) in height for non-forest

test <- filter(allGEDI2AB_reg, rh100<4)










