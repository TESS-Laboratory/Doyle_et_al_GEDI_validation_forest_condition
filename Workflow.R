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


# MISSED OFF GEDI FOR DAAC22S SO RERUN WHOLE PROJECT WITH THOSE INCLUDED
# RERUN ALS 20S/ CAUTARIO SO OUTPUT IS .LAZ FILE
# MAKE TABLES/ GRAPHS IN R?


# ----- PRE-PROCESS ALS --------
# ALS data is sourced from Sustainable Landscapes Brazil project (2018), downloaded into coordinate reference system (CRS)
# regions with the format 'DAAC_year_CRS'. Data is also sourced from Permian Global in 2023 for Rio Cautario. 
# ALS data is combined but catalogs are separated by their CRS.

# Load and retile (DAAC) catalog for consistency between various ALS sources

DAAC18_19S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_19S')
DAAC18CAUT23_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18CAUT23_20S')
DAAC18_21S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_21S')
DAAC18_22S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_22S')

retile_DAAC18_19S <- retile_catalog_pref(DAAC18_19S)
retile_DAAC18CAUT23_20S <- retile_catalog_pref(DAAC18CAUT23_20S)
retile_DAAC18_21S <- retile_catalog_pref(DAAC18_21S)
retile_DAAC18_22S <- retile_catalog_pref(DAAC18_22S)

# Check new catalog
las_check(retile_DAAC18_19S)
plot(retile_DAAC18_19S, mapview = TRUE, map.type = "Esri.WorldImagery")

# Process the retiled ALS

DAAC18_19S_norm <- process_als(retile_DAAC18_19S)
DAAC18CAUT23_20S_norm <- process_als(retile_DAAC18CAUT23_20S)
DAAC18_21S_norm <- process_als(retile_DAAC18_21S)
DAAC18_22S_norm <- process_als(retile_DAAC18_22S)

las_check(DAAC18_19S_norm)

# SOME OF THIS NEEDS TO GO DOWN TO ALS/GEDI EXTRACTION
# Filter the data for anomalous results

DAAC18_19Sfinal <- filter_als(DAAC18_19S_norm)
DAAC18CAUT23_20Sfinal <- filter_als(DAAC18_20S_norm)
DAAC18_21Sfinal <- filter_als(DAAC18_21S_norm)
DAAC18_22Sfinal<- filter_als(DAAC18_22S_norm)

# Set CRS of new catalog tiles to specified UTM

st_crs(DAAC18_19Sfinal) <- 32719
st_crs(DAAC18CAUT23_20Sfinal) <- 32720
st_crs(DAAC18_21Sfinal) <- 32721
st_crs(DAAC18_22Sfinal) <- 32722

# Create DTM for ability to determine extent of the .laz regions for GEDI overlap

dtm_DAAC18_19S <- rasterize_terrain(DAAC18_19Sfinal, 2, tin(), pkg = "terra")
dtm_DAAC18CAUT23_20S <- rasterize_terrain(DAAC18CAUT23_20Sfinal, 2, tin(), pkg = "terra")
dtm_DAAC18_21S <- rasterize_terrain(DAAC18_21Sfinal, 2, tin(), pkg = "terra")
dtm_DAAC18_22S <- rasterize_terrain(DAAC18_22Sfinal, 2, tin(), pkg = "terra")

writeRaster(dtm_DAAC18_19S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/dtm_DAAC18_19S.tif", overwrite=TRUE)
writeRaster(dtm_DAAC18CAUT23_20S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/dtm_DAAC18CAUT23_20S.tif", overwrite=TRUE)
writeRaster(dtm_DAAC18_21S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/dtm_DAAC18_21S.tif", overwrite=TRUE)
writeRaster(dtm_DAAC18_22S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/dtm_DAAC18_22S.tif", overwrite=TRUE)

# Polygon files for the ALS extents can now be created in QGIS to use for GEDI download
# Remove ALS catalogs from the environment for now
rm(DAAC18_19S, DAAC18CAUT23_20S, DAAC18_21S, DAAC18_22S, retile_DAAC18_19S, retile_DAAC18CAUT23_20S, 
   retile_DAAC18_21S, retile_DAAC18_22S, DAAC18_19S_norm, DAAC18CAUT23_20S_norm, DAAC18_21S_norm, 
   DAAC18_22S_norm, dtm_DAAC18_19S, dtm_DAAC18CAUT23_20S, dtm_DAAC18_21S, dtm_DAAC18_22S)


# ----------- GEDI download ERRORS NEED FIXING ----------------

#THIS NEEDS SOME WORK - SOME ERRORS in 2B DOWNLOAD WITH SOME OF THE CAUTARIO FILES

# Download GEDI2A files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI
# Reading all of the 2A output .fgb files into one geodatabase

poly_folder_path <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/DAAC_polygons"
start_date <- "2019-01-01"
end_date <- "2019-12-31"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2A"

gedi2A_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)

fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI2A <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI2A
allGEDI2A <- distinct(allGEDI2A, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI2A, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A.fgb")
mapview(allGEDI2A) + mapview(secondaryforest2023)


# Download GEDI2B files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI
# then reading all of the 2B output .fgb files into one geodatabase

poly_folder_path <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/CAUTARIO_polygons"
start_date <- "2022-01-01"
end_date <- "2023-12-31"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2B"

gedi2B_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)

#oneGEDI2B <- st_read("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI2B/DAAC1819S_1_2B.fgb")

fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI2B <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI2B
allGEDI2B <- distinct(allGEDI2B, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI2B, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2B.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2B <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2B.fgb")
mapview(allGEDI2B)


# Download GEDI4A files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI
# then reading all of the 2B output .fgb files into one geodatabase

poly_folder_path <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/CAUTARIO_polygons"
start_date <- "2019-01-01"
end_date <- "2020-01-01"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI4A"

gedi4A_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)

fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI4A <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI4A
allGEDI4A<- distinct(allGEDI4A, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI4A, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI4A.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI4A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI4A.fgb")
mapview(allGEDI4A)


#1B GEDI download

#poly_folder_path <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/DAAC_polygons"
#start_date <- "2019-01-01"
#end_date <- "2019-12-31"
#fgb_output_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/GEDI1B"
#gedi1B <- gedi1B_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)



# Merge the 2A, 2B and 4A files by shot_number

#This will produce in an incomplete file but workflow complete (not all 2B/4A files worked) ASK HUGH FOR HELP ON THIS

#THIS NEEDS WORK? GOES FROM 6702 to 3515
allGEDI <- st_join(allGEDI2A, allGEDI2B) %>%
  filter(!is.na(shot_number.y)) %>%
  st_join(allGEDI4A)%>%
  filter(!is.na(shot_number)) 


allGEDI2Anosf <- st_drop_geometry(allGEDI2A)
allGEDI2Bnosf <- st_drop_geometry(allGEDI2B)
allGEDI4Anosf <- st_drop_geometry(allGEDI4A)

GEDI2A2B <- merge(allGEDI2Anosf, allGEDI2Bnosf, by = "shot_number")

allGEDInosf <- merge(GEDI2A2B, allGEDI4Anosf, by = "shot_number")

allGEDIdata <- st_sf(allGEDInosf, geometry = geometry_sf)

# NOW HAS NO GEOMETRY, NEED TO FIX





# ------ GEDI regressions SHOULD ADD KURTOSIS SKEW ETC ---------

# Summarise relative height rh0-100 metrics with linear regression model 
# Outputs intercept, slope and variance of the 2A waveform
# USE THE WAVEFORMLIDAR VIGNETTE TO EXTRACT MORE VARIABLES

# USE ALLGEDI EVENTUALLY

GEDI2A_trans <- allGEDI2A %>%
  as.data.frame() %>%
  select(shot_number, starts_with("rh"))

# Apply regression function to each row of the dataframe
result_df <- apply(GEDI2A_trans, 1, rh_linear_regression)

# Convert the result to a dataframe and set column names
result_df <- t(data.frame(result_df))
colnames(result_df) <- c("shot_number", "G_intercept", "G_slope", "G_variance")
result_df <- as.data.frame(result_df) %>%
  as.numeric(G_intercept, G_slope, G_variance)

library(waveformlidar)

waveform lidar bits here






# Remerge with original GEDI2A dataframe and filter for specific relative height entries

allGEDI2A <- left_join(allGEDI2A, result_df, by = "shot_number")

allGEDI2A_reg <- allGEDI2A %>%
  select(year, solar_elevation, lat_lowestmode, lon_lowestmode, elev_highestreturn, elev_lowestmode,
         sensitivity, shot_number, degrade_flag, ALS_CRS, rh0, rh10, rh25, rh50, rh75, rh90, rh95, 
         rh96, rh97, rh98, rh99, rh100,G_intercept, G_slope, G_variance,geometry)
#cover, pai, fhd_normal, pgap_theta, modis_treecover, agbd, agbd_se, agbd_pi_lower, agbd_pi_upper

sf::st_write(allGEDI2A_reg, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_regressions.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_regressions.fgb")



# ------- Forest spectral classification MODIS AND SECONDARY FOREST---------

# DOWNLOAD/MANIPULATION OF SECONDARY FOREST DATASET
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

# Read rasters and polygons
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

# Add 5 years to the raster values to age secondary forest layer from 2018 to 2023 
# (when original layer was previously modeled to)
# Continued forest extent from 2018 - 2023 will be checked with GEDI MODIS tree cover metrics
# CHECK THIS PART???? ONLY CAUTARIO IS AN ISSUE AND KNOW THIS SITE?

secondaryforestplus4 <- secondaryforest + 5

# Write raster
writeRaster(secondaryforestplus4, paste0(output_folder, "/", "secondaryforest2023.tif"), overwrite=TRUE)
secondaryforest2023 <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Secondary_forest_classification/secondaryforest2023.tif")

mapview(secondaryforest2023, layer.name = 'Secondary forest age', na.color="transparent")


# Define spatial reference
#https://spatialreference.org/ref/epsg/32720/
#sr <- "+proj=utm +zone=20 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Reproject raster
#secondaryforest1 <- projectRaster(secondaryforestraw1, crs = sr)
#secondaryforest2 <- projectRaster(secondaryforestraw2, crs = sr)



# COROBORATE MODIS AND SECONDARY FOREST






# BURNED AREA DATA

MAPBIOMAS_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Fire_data"
output_folder <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Fire_data"

# List .tif  files and create empty list to store raster objects
burned_files <- list.files(path = MAPBIOMAS_folder, pattern = "\\.tif$", full.names = TRUE)
burned_list <- list()

# Read each raster file and store them in the list
# MAPBIOMAS files for Rio Cautario date til 2022 and DAAC areas date until 2018 (corresponds with ALS flights)
for (file in burned_files) {
  raster_obj <- raster(file)
  burned_list[[length(burned_list) + 1]] <- raster_obj
}

# Merge rasters
merged_burned <- do.call(merge, burned_list)

# Crop raster
burnedforest <- crop(merged_burned, secondaryforest)

writeRaster(burnedforest, paste0(output_folder, "/", "MAPBIOMASfire.tif"), overwrite=TRUE)
burnedforest <- raster("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/Fire_data/MAPBIOMASfire.tif")




# ----------- GEDI classification extraction NEEDS ALLGEDI EDITS --------------

# Burned data 1985 - 2019 for DAAC and 1985 - 2022 for Cautario
# Extract values for forest age, burn frequency #and ALS extent#, binding new column to GEDI data frame

# MAKE THIS A FUNCTION TO PLAY WITH WHATEVER GEDI FILE INPUT 
# EVENTUALLY USE JUST THE ALLGEDI FILE

forest_age <- raster::extract(secondaryforest2023, allGEDI2A, method='simple')
allGEDI2A <- cbind(allGEDI2A, forest_age)

burn_freq <- terra::extract(burnedforest, allGEDI2A, method='simple')
allGEDI2A <- cbind(allGEDI2A, burn_freq)

#forest_age <- raster::extract(secondaryforest2023, allGEDI, method='simple')
#allGEDI <- cbind(allGEDI, forest_age)

#burn_freq <- terra::extract(burnedforest, allGEDI, method='simple')
#allGEDI<- cbind(allGEDI, burn_freq)



# Segregate the secondary/ intact forest samples of GEDI footprints

# Edit original allGEDI2A file to have a numeric value for ageless intact forest (n/a)
allGEDI2A_aged <- allGEDI2A %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age))

# Filter for intact forest only (values of 99)
allGEDI2A_intact <- filter(allGEDI2A_aged, forest_age>90)

# Just degraded forest samples (classified with age by secondary forest raster)
allGEDI2A_sec <- filter(allGEDI2A_aged, forest_age<90)


# Edit original allGEDI2A file to have a numeric value for ageless intact forest (n/a)
#allGEDI_aged <- allGEDI %>%
 # mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age))

# Filter for intact forest only (values of 99)
#allGEDI_intact <- filter(allGEDI_aged, forest_age>90)

# Just degraded forest samples (classified with age by secondary forest raster)
#allGEDI_sec <- filter(allGEDI_aged, forest_age<90)

secondaryforestRio <- raster("/Users/emilydoyle/Documents/workspace/Doyle_development_data/Secondary_forest_classification/secondaryforest2023.tif")

mapview(allGEDI2A_sec, zcol = "forest_age") + mapview(secondaryforest2023) + mapview(allGEDI2A_intact)

mapview(secondaryforestRio) + mapview(burnedforest) + mapview(allGEDI2A_gradient, zcol = "Degradation")

# Randomly sample 150 GEDI footprints (20% of sample size) that meet above criteria and create column highlighting them as =1
allGEDI2A_intact <- allGEDI2A_intact %>%
  mutate(intact_sample = as.integer(row_number() %in% sample(n(), size = 150)))

# Filter just the intact GEDI footprint samples
allGEDI2A_intact_sample <- allGEDI2A_intact %>%
  filter(intact_sample==1) %>%
  dplyr::select(-intact_sample) 

mapview(allGEDI_intact_sample)


# Randomly sample 150 GEDI footprints (20% of sample size) that meet above criteria and create column highlighting them as =1
#allGEDI_intact <- allGEDI_intact %>%
 # mutate(intact_sample = as.integer(row_number() %in% sample(n(), size = 150)))

# Filter just the intact GEDI footprint samples
#allGEDI_intact_sample <- allGEDI_intact %>%
#  filter(intact_sample==1) %>%
 # dplyr::select(-intact_sample) 

# samples spread across the amazon with various soil/ distance to river/ distance to degradation



# Merge the secondary forest and ALS extent GEDI footprints data frame with new random intact samples
# For complete gradient of GEDI samples across the site

allGEDI2A_gradient <- rbind(allGEDI2A_sec, allGEDI2A_intact_sample)

#allGEDI_gradient <- rbind(allGEDI_sec, allGEDI_intact_sample)

allGEDI2A_gradient <- allGEDI2A_gradient %>%
  mutate(Degrdtn = case_when(
    burn_freq > 0 ~ "Burned",
    forest_age < 50 ~ "Logged",
    forest_age > 50 ~ "Intact",
    TRUE ~ NA_character_)) %>%
  mutate(Degradation = case_when(
    burn_freq == 2 ~ "Burned",
    burn_freq == 1 ~ "Burned",
    burn_freq > 2 ~ "Burned 3+",
    forest_age < 50 ~ "Logged",
    forest_age > 50 ~ "Intact",
    TRUE ~ NA_character_)) %>%
  mutate(Age_category = cut(forest_age, breaks=c(-Inf, 6, 15, 25, 40, Inf), 
                            labels=c("<6", "6-15", "15-25", "25-40", ">40"))) %>%
  mutate(Age_category2 = cut(forest_age, breaks=c(-Inf, 10, 20, 30, 40, Inf), 
                             labels=c("<10", "10-20", "20-30", "30-40", ">40")))


#allGEDI_gradient <- allGEDI_gradient %>%
 # select(year.x, solar_elevation, lat_lowestmode, lon_lowestmode, elev_highestreturn, elev_lowestmode,
        # sensitivity, shot_number, degrade_flag,rh0, rh10, rh25, rh50, rh75, rh90, rh95, 
        # rh96, rh97, rh98, rh99, rh100, geometry, forest_age, burn_freq, cover,
        # pai, fhd_normal, pgap_theta, modis_treecover, agbd, agbd_se, agbd_pi_lower, agbd_pi_upper)

# TEMP FILES SO DONT NEED TO RUN ALL AGAIN 

sf::st_write(allGEDI2A_gradient, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_gradient.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI_gradient, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI_gradient.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2A_gradient <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_gradient.fgb")
#allGEDI_gradient <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI_gradient_test.fgb")

rm(allGEDI2A, allGEDI2A_aged, allGEDI2A_intact, allGEDI2A_intact_sample, allGEDI2A_sec)

# ------ Extracting ALS metrics within GEDI footprints ------

# As the ALS data spans different parts of the Amazon rainforest, they are separated in folders by their
# CRS. Each ALS folder must therefore extract data from the GEDI footprints separately before being merged.

# Reaload/filter and set CRS of final ALS catalogs if no longer in environment

# DAAC18_19Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_19S/final_norm')
# DAAC18CAUT23_20Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18CAUT23_20S/final_norm')
# DAAC18_21Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_21S/final_norm')
# DAAC18_22Sfinal <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/DAAC18_22S/final_norm')
# 
# DAAC18_19Sfinal <- filter_als(DAAC18_19Sfinal)
# DAAC18CAUT23_20Sfinal <- filter_als(DAAC18CAUT23_20Sfinal)
# DAAC18_21Sfinal <- filter_als(DAAC18_21Sfinal)
# DAAC18_22Sfinal <- filter_als(DAAC18_22Sfinal)
# 
# st_crs(DAAC18_19Sfinal) <- 32719
# st_crs(DAAC18CAUT23_20Sfinal) <- 32720
# st_crs(DAAC18_21Sfinal) <- 32721
# st_crs(DAAC18_22Sfinal) <- 32722


# Filter and reproject the GEDI dataframe to match each CRS catalog

allGEDI2A_19S <- filter_reproj_GEDI(allGEDI2A, '19S', 'EPSG:32719')
allGEDI2A_20S <- filter_reproj_GEDI(allGEDI2A, '20S', 'EPSG:32720')
allGEDI2A_21S <- filter_reproj_GEDI(allGEDI2A, '21S', 'EPSG:32721')
allGEDI2A_22S <- filter_reproj_GEDI(allGEDI2A, '22S', 'EPSG:32722')
st_crs(allGEDI2A_19S) #, allGEDI2A_20S, allGEDI2A_21S, allGEDI2A_22S)


 # Extracting metrics of ALS within GEDI footprints in the same CRS

DAAC18_19Smetrics <- plot_metrics(DAAC18_19Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2A_19S, radius = 12.5)
DAAC18CAUT23_20Smetrics <- plot_metrics(DAAC18CAUT23_20Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2A_20S, radius = 12.5)
DAAC18_21Smetrics <- plot_metrics(DAAC18_21Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2A_21S, radius = 12.5)
DAAC18_22Smetrics <- plot_metrics(DAAC18_22Sfinal, ~lidar_preds(Z, ReturnNumber, min = 0, max = Inf), allGEDI2A_22S, radius = 12.5)


# Merging metrics and using control CRS (WGS84) associated with GEDI data now extraction is complete

DAAC18_19Smetrics <- st_transform(allGEDI2A19S_metrics, "EPSG:32643")
DAAC18CAUT23_20Smetrics <- st_transform(allGEDI2A20S_metrics, "EPSG:32643")
DAAC18_21Smetrics <- st_transform(allGEDI2A21S_metrics, "EPSG:32643")
DAAC18_22Smetrics <- st_transform(allGEDI2ACAUT20S_metrics, "EPSG:32643")

st_crs(allGEDI2A19S_metrics)

# Remove duplicated rows
DAAC18_19Smetrics <- distinct(DAAC18_19Smetrics)
DAAC18CAUT23_20Smetrics <- distinct(DAAC18CAUT23_20Smetrics)
DAAC18_21Smetrics <- distinct(DAAC18_21Smetrics)
DAAC18_22Smetrics <- distinct(DAAC18_22Smetrics)

# Assuming columns are in the same order and just have different names
colnames(DAAC18CAUT23_20Smetrics) <- colnames(DAAC18_19Smetrics)
colnames(DAAC18_21Smetrics) <- colnames(DAAC18_19Smetrics)
colnames(DAAC18_22Smetrics) <- colnames(DAAC18_19Smetrics)

# Combine the dataframes into one dataframe using bind_rows
merged_df <- bind_rows(DAAC18_19Smetrics, 
                       DAAC18CAUT23_20Smetrics,
                       DAAC18_21Smetrics,
                       DAAC18_22Smetrics)


# WORK ON THIS BIT
# Remove rows with NAs in the 'year' column
allheight <- merged_df %>%
  filter(!is.na(rhz95))

sf::st_write(allheight, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allheight_draft.fgb", delete_dsn = TRUE, overwrite = TRUE)
allheight <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allheight_draft.fgb")


# MERGE GEDI GRADIENT AND PICK THE USEFUL BITS?


rm(allGEDI2A_19S, allGEDI2A_20S, allGEDI2A_21S, allGEDI2A_22S, DAAC18_19Sfinal, 
   DAAC18_20Sfinal, DAAC18_21Sfinal, DAAC18_22Sfinal, DAAC18_19Smetrics, DAAC18_20Smetrics,
   DAAC18_21Smetrics, DAAC18_22Smetrics)

# ------ Statistics CHECK ---------

# EXAMPLE CODE OF A LINS CCC
# Plot Tramway Runscomparison of resampling

x <- as.vector(MeanFwdSeqresampNDVI)
y <- as.vector(MeanFwdMREresampNDVI)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")



library(DescTools)

# Compute the Lin's correlation concordance coefficient

#mod
x <- allheight$rh95
y <- allheight$zq95
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc_mod <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
ccc_mod

#modint
attach(allheight)
x <- allheight$rh95[Degrdtn == 'intact']
y <- allheight$zq95[Degrdtn == 'intact']
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc_modint <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
ccc_modint

#modlog
attach(allheight)
x <- allheight$rh95[Degrdtn == 'logged']
y <- allheight$zq95[Degrdtn == 'logged']
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc_modlog <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
ccc_modlog

#modburn
attach(allheight)
x <- allheight$rh95[Degrdtn == 'burned']
y <- allheight$zq95[Degrdtn == 'burned']
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc_modburn <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
ccc_modburn

#modburnfreq1_3
attach(allheight)
x <- allheight$rh95[brn_frq %in% 1:3]
y <- allheight$zq95[brn_frq %in% 1:3]
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc_modburnfreq1_3 <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
ccc_modburnfreq1_3

#modburnfreq4_6
attach(allheight)
x <- allheight$rh95[brn_frq %in% 4:6]
y <- allheight$zq95[brn_frq %in% 4:6]
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc_modburnfreq4_6 <- paste("CCC = ", round(ccc_result$rho.c[1], 2))



# Create panel in paper of rh25 zq25 upwards

# Inspired by Dorado et al 2021

# Define linear regression models for different conditions and burn frequencies
mod <- lm(zq95 ~ rh95, data = allheight)
modint <- lm(zq95[Degrdtn == 'intact'] ~ rh95[Degrdtn == 'intact'], data = allheight)
modlog <- lm(zq95[Degrdtn == 'logged'] ~ rh95[Degrdtn == 'logged'], data = allheight)
modburn <- lm(zq95[Degrdtn == 'burned'] ~ rh95[Degrdtn == 'burned'], data = allheight)
modburnfreq1_3 <- lm(zq95[brn_frq %in% 1:3] ~ rh95[brn_frq %in% 1:3], data = allheight)
modburnfreq4_6 <- lm(zq95[brn_frq %in% 4:6] ~ rh95[brn_frq %in% 4:6], data = allheight)


# Compute RMSE
# Difference between zq and rh predicted values - positive or negative not identified
# Error as opposed to fitting a modeled error to predict hypothetical error
rmse <- sqrt(sum((allheight$zq95 - allheight$rh95)^2, na.rm = TRUE) / length(which(!is.na(allheight$zq95))))
rmse_int <- sqrt(sum((allheight$zq95[which(allheight$Degrdtn == 'intact')] - allheight$rh95[which(allheight$Degrdtn == 'intact')])^2, na.rm = TRUE) /length(which(!is.na(allheight$rh95[which(allheight$Degrdtn == 'intact')]))))
rmse_log <- sqrt(sum((allheight$zq95[which(allheight$Degrdtn == 'logged')] - allheight$rh95[which(allheight$Degrdtn == 'logged')])^2, na.rm = TRUE)/length(which(!is.na(allheight$rh95[which(allheight$Degrdtn == 'logged')]))))
rmse_burn <- sqrt(sum((allheight$zq95[which(allheight$Degrdtn == 'burned')] - allheight$rh95[which(allheight$Degrdtn == 'burned')])^2, na.rm = TRUE)/length(which(!is.na(allheight$rh95[which(allheight$Degrdtn == 'burned')]))))
rmse_burnfreq1_3 <- sqrt(sum((allheight$zq95[which(allheight$brn_frq %in% 1:3)] - allheight$rh95[which(allheight$brn_frq %in% 1:3)])^2, na.rm = TRUE) / length(which(!is.na(allheight$rh95[which(allheight$brn_frq %in% 1:3)]))))
rmse_burnfreq4_6 <- sqrt(sum((allheight$zq95[which(allheight$brn_frq %in% 4:6)] - allheight$rh95[which(allheight$brn_frq %in% 4:6)])^2, na.rm = TRUE) / length(which(!is.na(allheight$rh95[which(allheight$brn_frq %in% 4:6)]))))

# Compute mean ALS height
mean_actual <- sum(mean(allheight$zq95))

# Compute relative rmse
rrmse <- sum(rmse/mean_actual)*100
rrmse_int <- sum(rmse_int/mean_actual)*100
rrmse_log <- sum(rmse_log/mean_actual)*100
rrmse_burn <- sum(rmse_burn/mean_actual)*100
rrmse_burnfreq1_3 <- sum(rmse_burnfreq1_3/mean_actual)*100
rrmse_burnfreq4_6 <- sum(rmse_burnfreq4_6/mean_actual)*100


# Compute bias:  Average difference between zq and rh (in this case hyp is -0.7m lower than rh)
bias <- sum((allheight$zq95 - allheight$rh95), na.rm = TRUE)/length(which(!is.na(allheight$rh95)))
bias_int <- sum(allheight$zq95[allheight$Degrdtn == 'intact'] - allheight$rh95[allheight$Degrdtn == 'intact'], na.rm = TRUE)/length(which(!is.na(allheight$rh95[allheight$Degrdtn == 'intact'])))
bias_log <- sum(allheight$zq95[allheight$Degrdtn == 'logged'] - allheight$rh95[allheight$Degrdtn == 'logged'], na.rm = TRUE)/length(which(!is.na(allheight$rh95[allheight$Degrdtn == 'logged'])))
bias_burn <- sum(allheight$zq95[allheight$Degrdtn == 'burned'] - allheight$rh95[allheight$Degrdtn == 'burned'], na.rm = TRUE)/length(which(!is.na(allheight$rh95[allheight$Degrdtn == 'burned'])))
bias_burnfreq1_3 <- sum(allheight$zq95[allheight$brn_frq %in% 1:3] - allheight$rh95[allheight$brn_frq %in% 1:3], na.rm = TRUE) / length(which(!is.na(allheight$rh95[allheight$brn_frq %in% 1:3])))
bias_burnfreq4_6 <- sum(allheight$zq95[allheight$brn_frq %in% 4:6] - allheight$rh95[allheight$brn_frq %in% 4:6], na.rm = TRUE) / length(which(!is.na(allheight$rh95[allheight$brn_frq %in% 4:6])))


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












# TESTING FOR THE PCA


# Eventually need this to inlcude all the ALS waveform stats, GEDI rh intervals
# canopy cover metrics etc
allGEDI2APCA <- allGEDI2A_gradient %>%
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

# Extract loadings for the first few PCs for burned data
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
burned_data <- allGEDI2APCA[allGEDI2APCA$Degradation_numeric == 2, ]
logged_data <- allGEDI2APCA[allGEDI2APCA$Degradation_numeric == 1, ]
intact_data <- allGEDI2APCA[allGEDI2APCA$Degradation_numeric == 0, ]

# Remove columns with zero variance (excluding Degradation_numeric)
burned_data <- burned_data[, apply(burned_data[, -which(names(burned_data) == "Degradation_numeric")], 2, function(x) var(x) != 0)]
logged_data <- logged_data[, apply(logged_data[, -which(names(logged_data) == "Degradation_numeric")], 2, function(x) var(x) != 0)]
intact_data <- intact_data[, apply(intact_data[, -which(names(intact_data) == "Degradation_numeric")], 2, function(x) var(x) != 0)]

# Perform PCA for burned data
pca_burned <- prcomp(select(burned_data, -Degradation_numeric), center = TRUE, scale. = TRUE)

# Perform PCA for logged data
pca_logged <- prcomp(select(logged_data, -Degradation_numeric), center = TRUE, scale. = TRUE)

# Perform PCA for intact data
pca_intact <- prcomp(select(intact_data, -Degradation_numeric), center = TRUE, scale. = TRUE)

# Analyze PCA results for each subset
summary(pca_burned)
summary(pca_logged)
summary(pca_intact)

# Extract loadings for the first few PCs for burned data
loadings_burned <- pca_burned$rotation[, 1:3]  # Adjust the number (e.g., 1:3) for the desired number of PCs

# Extract loadings for the first few PCs for logged data
loadings_logged <- pca_logged$rotation[, 1:3]

# Extract loadings for the first few PCs for intact data
loadings_intact <- pca_intact$rotation[, 1:3]

# Print the loadings
print(loadings_burned)
print(loadings_logged)
print(loadings_intact)

# Plot PCA results for each subset
plot(pca_burned, main = "PCA for Burned Data")
plot(pca_logged, main = "PCA for Logged Data")
plot(pca_intact, main = "PCA for Intact Data")
# Plot PCA results for each subset
plot(pca_burned, main = "PCA for Burned Data")
plot(pca_logged, main = "PCA for Logged Data")
plot(pca_intact, main = "PCA for Intact Data")










# ------- Graphs/ visulisations CHECK-------


allheight <- allheight %>%
  mutate(Degradation = case_when(
    brn_frq == 2 ~ "Burned",
    brn_frq == 1 ~ "Burned",
    brn_frq > 2 ~ "Burned 3+",
    forst_g < 50 ~ "Logged",
    forst_g > 50 ~ "Intact",
    TRUE ~ NA_character_))

# Violin plot for GEDI top height across degradation
GEDIrh99_degradation <- allheight %>%
  ggplot(aes(x = Degrdtn, y = rh99, fill = Degrdtn)) +
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
  ggplot(aes(x = Degrdtn, y = cover, fill = Degrdtn)) +
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
allGEDI2A_gradient <- mutate(allGEDI2A_gradient, Age_category = factor(Age_category, levels = age_order))

GEDIheight_age <- allGEDI2A_gradient %>%
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
  geom_point(aes(x = Age_category, y = cover, color = Degrdtn)) +
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
  geom_point(aes(x = Degrdtn, y = cover, color = Degrdtn)) +
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
ALSGEDIrh95 <- allheight %>%
  ggplot(aes(x=zq95, y=rh95, color=Degradation)) +
  geom_point() +
  labs(title = "Correspondance between ALS and GEDI 95%", x = "ALS", y = "GEDI") +
  theme_bw() +
  scale_colour_manual(values = c("orange", "red", "chartreuse3","deepskyblue" ))
plot(ALSGEDIrh95)


# Graph for correspondence between ALS95 and GEDI95
ALSGEDIrh75 <- allheight %>%
  ggplot(aes(x=zq75, y=rh75, color=Degrdtn)) +
  geom_point() +
  labs(title = "Correspondance between ALS and GEDI 75%", x = "ALS", y = "GEDI") +
  theme_bw() 
#scale_colour_manual(values = c("darkorange", "darkolivegreen3", "Black"))
plot(ALSGEDIrh75)

# Graph for correspondence between ALS95 and GEDI95
ALSGEDIrh95 <- allheight %>%
  ggplot(aes(x=zq95, y=rh95, color=Degrdtn)) +
  geom_point() +
  labs(title = "Correspondance between ALS and GEDI 95%", x = "ALS", y = "GEDI") +
  theme_bw() 
#scale_colour_manual(values = c("darkorange", "darkolivegreen3", "Black"))
plot(ALSGEDIrh95)


# Graph for correspondence between ALS95 and GEDI95
ALSGEDIrh50 <- allheight %>%
  ggplot(aes(x=zq75, y=rh50, color=Degrdtn)) +
  geom_point() +
  labs(title = "Correspondance between ALS and GEDI 50%", x = "ALS", y = "GEDI") +
  theme_bw() 
#scale_colour_manual(values = c("darkorange", "darkolivegreen3", "Black"))
plot(ALSGEDIrh50)



# NEED TO LOOK AT PLOTTING THE VARIANCES

allGEDI2A_gradient$G_slope <- as.numeric(allGEDI2A_gradient$G_slope)
allGEDI2A_gradient$G_variance <- as.numeric(allGEDI2A_gradient$G_variance)
allGEDI2A_gradient$G_intercept <- as.numeric(allGEDI2A_gradient$G_intercept)

GEDI_slope <- allGEDI2A_gradient %>%
  ggplot(aes(x=Degradation, y=G_variance, color=Degradation)) +
  geom_point() +
  labs(title = "G_slope", x = "height", y = "G_slope") +
  theme_bw() 
#scale_colour_manual(values = c("darkorange", "darkolivegreen3", "Black"))
plot(GEDI_slope)

# Violin plot for GEDI top height across degradation
GEDI_slope <- allGEDI2A_gradient %>%
  ggplot(aes(x=Degrdtn, y=G_slope, fill=Degrdtn)) +
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

allGEDI2A_gradienttest <- allGEDI2A_gradient %>%
  mutate(status = ifelse(Degradation == "logged" | Degradation == "burned", "degraded", "intact"))

GEDI_condition_test <- allGEDI2A_gradienttest %>%
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




