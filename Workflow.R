## Script for the analysis of paper 'Doyle et al...' 

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


# ----- PRE-PROCESS ALS --------
# Load and retile (DAAC) catalog for consistency between various ALS sources (NEED TO ADD CAUATRIO)
# WILL NEED TO RERUN ALL ONCE FIX ERRORS WITH CAUATRIO DOWNLOAD

CAUT23_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/Permian_ALS/saibrasil_permian-global-rio-cautario_2023-10-20_0450/LiDAR')
DAAC18_19S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_19S')
DAAC18_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_20S')
DAAC18_21S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_21S')
DAAC18_22S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_22S')

retile_CAUT23_20S <- retile_catalog_pref(CAUT23_20S)
retile_DAAC18_19S <- retile_catalog_pref(DAAC18_19S)
retile_DAAC18_20S <- retile_catalog_pref(DAAC18_20S)
retile_DAAC18_21S <- retile_catalog_pref(DAAC18_21S)
retile_DAAC18_22S <- retile_catalog_pref(DAAC18_22S)

# Check new catalog
las_check(retile_DAAC18_19S)
plot(retile_DAAC18_19S, mapview = TRUE, map.type = "Esri.WorldImagery")

# Process the retiled ALS

CAUT23_20S_norm <- process_als(retile_CAUT23_20S)
DAAC18_19S_norm <- process_als(retile_DAAC18_19S)
DAAC18_20S_norm <- process_als(retile_DAAC18_20S)
DAAC18_21S_norm <- process_als(retile_DAAC18_21S)
DAAC18_22S_norm <- process_als(retile_DAAC18_22S)

las_check(DAAC18_19S_norm)

# Filter the data for anomalous results

CAUT23_20S_norm <- filter_als(CAUT23_20S_norm)
DAAC18_19S_norm <- filter_als(DAAC18_19S_norm)
DAAC18_20S_norm <- filter_als(DAAC18_20S_norm)
DAAC18_21S_norm <- filter_als(DAAC18_21S_norm)
DAAC18_22S_norm <- filter_als(DAAC18_22S_norm)

# Set CRS of new catalog tiles to specified UTM

st_crs(CAUT23_20S_norm) <- 32720
st_crs(DAAC18_19S_norm) <- 32719
st_crs(DAAC18_20S_norm) <- 32720
st_crs(DAAC18_21S_norm) <- 32721
st_crs(DAAC18_22S_norm) <- 32722

# Create DTM for ability to determine extent of the .laz regions for GEDI overlap
# Already have DTM for CAUT2320S

dtm_DAAC18_19S <- rasterize_terrain(DAAC18_19S_norm, 2, tin(), pkg = "terra")
dtm_DAAC18_20S <- rasterize_terrain(DAAC18_20S_norm, 2, tin(), pkg = "terra")
dtm_DAAC18_21S <- rasterize_terrain(DAAC18_21S_norm, 2, tin(), pkg = "terra")
dtm_DAAC18_22S <- rasterize_terrain(DAAC18_22S_norm, 2, tin(), pkg = "terra")

writeRaster(dtm_DAAC18_19S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)
writeRaster(dtm_DAAC18_20S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)
writeRaster(dtm_DAAC18_21S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)
writeRaster(dtm_DAAC18_22S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)


# RUN THIS WHEN HAVE TIME TO CHECK SAME AS SERVER RUN - PRETTY SURE IT IS BUT CHECK
CAUT23_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/Permian_ALS/saibrasil_permian-global-rio-cautario_2023-10-20_0450/LiDAR')
retile_CAUT23_20S <- retile_catalog_pref(CAUT23_20S)
CAUT23_20S_norm <- process_als(retile_CAUT23_20S)
CAUT23_20S_norm <- filter_als(CAUT23_20S_norm)
st_crs(CAUT23_20S_norm) <- 32720


# NEED TO REDO THIS WITH THE NEW CAUTARIO FILES - REGET THE FILES FROM THE SERVER
# Move the final files into single folder for final catalog import

source_folders <- c("/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_19S/final_norm",
                    "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_20S/final_norm",
                    "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_21S/final_norm",
                    "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_22S/final_norm",
                    "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/CAUTARIO_20S/final_tiles")
destination_folder <- "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/final_ALS_Chap1"
file_extensions <- c("las", "laz")

# Use function 'move_files' to move files
map(source_folders, ~ move_files(.x, destination_folder, file_extensions))


# ----------- GEDI download ----------------

#THIS NEEDS SOME WORL - SOME ERRORS in 2B DOWNLOAD WITH SOME OF THE CAUTARIO FILES

# Download GEDI2A files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI
# Reading all of the 2A output .fgb files into one geodatabase

poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/CAUTARIO_polygons"
start_date <- "2022-01-01"
end_date <- "2024-01-01"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI2A"

gedi2A_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)

fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI2A <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI2A
allGEDI2A <- distinct(allGEDI2A, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI2A, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2A <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A.fgb")
mapview(allGEDI2A)

# Download GEDI2B files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI
# then reading all of the 2B output .fgb files into one geodatabase

poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/DAAC_polygons"
start_date <- "2019-01-01"
end_date <- "2020-01-01"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI2B"

gedi2B_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)

#oneGEDI2B <- st_read("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI2B/DAAC1819S_1_2B.fgb")

fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI2B <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI2B
allGEDI2B <- distinct(allGEDI2B, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI2B, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2B.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2B <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2B.fgb")
mapview(allGEDI2B)


# Download GEDI4A files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI
# then reading all of the 2B output .fgb files into one geodatabase

poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/DAAC_polygons"
start_date <- "2019-01-01"
end_date <- "2020-01-01"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI4A"

gedi4A_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)

fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI4A <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI4A
allGEDI4A<- distinct(allGEDI4A, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI4A, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI4A.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI4A <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI4A.fgb")
mapview(allGEDI4A)


#1B GEDI download

#poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/DAAC_polygons"
#start_date <- "2019-01-01"
#end_date <- "2019-12-31"
#fgb_output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI1B"

#gedi1B <- gedi1B_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)


# Merge the 2A, 2B and 4A files by shot_number

#This will produce in an incomplete file but workflow complete (not all 2B/4A files worked) ASK HUGH FOR HELP ON THIS

allGEDI <- st_join(allGEDI2A, allGEDI2B) %>%
  filter(!is.na(shot_number.y)) %>%
  st_join(allGEDI4A)%>%
  filter(!is.na(shot_number)) 


# ------ GEDI regressions ---------

# Summarise relative height rh0-100 metrics with linear regression model 
# Outputs intercept, slope and variance of the 2A waveform

# USE ALLGEDI EVENTUALLY

GEDI2A_trans <- allGEDI2A %>%
  as.data.frame() %>%----
  select(shot_number, starts_with("rh"))

# Apply regression function to each row of the dataframe
result_df <- apply(GEDI2A_trans, 1, rh_linear_regression)

# Convert the result to a dataframe and set column names
result_df <- t(data.frame(result_df))
colnames(result_df) <- c("shot_number", "G_intercept", "G_slope", "G_variance")
result_df <- as.data.frame(result_df) %>%
  as.numeric(G_intercept, G_slope, G_variance)

# Remerge with original GEDI2A dataframe and filter for specific relative height entries

allGEDI2A <- left_join(allGEDI2A, result_df, by = "shot_number")

allGEDI2A_reg <- allGEDI2A %>%
  select(year, solar_elevation, lat_lowestmode, lon_lowestmode, elev_highestreturn, elev_lowestmode,
         sensitivity, shot_number, degrade_flag, ALS_CRS, rh0, rh10, rh25, rh50, rh75, rh90, rh95, 
         rh96, rh97, rh98, rh99, rh100,G_intercept, G_slope, G_variance,geometry)
#cover, pai, fhd_normal, pgap_theta, modis_treecover, agbd, agbd_se, agbd_pi_lower, agbd_pi_upper

sf::st_write(allGEDI2A_reg, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A_regressions.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2A <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A_regressions.fgb")



# ------- Forest spectral classification ---------

# DOWNLOAD/MANIPULATION OF SECONDARY FOREST DATASET
# Set directories and file names
data_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/Secondary_forest"
output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/Secondary_forest_classification/"
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
secondary_forest_west <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/Secondary_forest/secondary_polygon_west.shp")
secondary_forest_east <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/Secondary_forest/secondary_polygon_east.shp")


# Crop raster
secondaryforestwest <- crop(secondaryforestraw1, secondary_forest_west)
secondaryforesteast <- crop(secondaryforestraw2, secondary_forest_east)
secondaryforestsouth <- crop(secondaryforestraw3, secondary_forest_east)

mapview(secondaryforesteast) + mapview(secondaryforestwest) + mapview(secondaryforestsouth)

# Merge rasters
raster_list <- list(secondaryforestwest, secondaryforesteast, secondaryforestsouth)
merged_secondary <- do.call(merge, raster_list)
writeRaster(merged_secondary, paste0(output_folder, "/", "secondaryforest.tif"), overwrite=TRUE)
secondaryforest <- raster("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/Secondary_forest_classification/secondaryforest.tif")


# Reclassify raster
secondaryforest[secondaryforest == 0] <- NA

# Add 5 years to the raster values to age secondary forest layer from 2018 to 2023 
# (when original layer was previously modeled to)
# Continued forest extent from 2018 - 2023 will be checked with GEDI MODIS tree cover metrics

secondaryforestplus4 <- secondaryforest + 5

# Write raster
writeRaster(secondaryforestplus4, paste0(output_folder, "/", "secondaryforest2023.tif"), overwrite=TRUE)
secondaryforest2023 <- raster("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/Secondary_forest_classification/secondaryforest2023.tif")

mapview(secondaryforest2023, layer.name = 'Secondary forest age', na.color="transparent")


# Define spatial reference
#https://spatialreference.org/ref/epsg/32720/
#sr <- "+proj=utm +zone=20 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Reproject raster
#secondaryforest1 <- projectRaster(secondaryforestraw1, crs = sr)
#secondaryforest2 <- projectRaster(secondaryforestraw2, crs = sr)



# COROBORATE MODIS AND SECONDARY FOREST



# BURNED AREA DATA

MAPBIOMAS_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/Fire_data"
output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/Fire_data"

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
burnedforest <- raster("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/Fire_data/MAPBIOMASfire.tif")




# ----------- GEDI extents/processing NEEDS ALLGEDI EDITS --------------

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

mapview(allGEDI2A_sec, zcol = "forest_age") + mapview(secondaryforest2023) + mapview(allGEDI2A_intact)



# Randomly sample 150 GEDI footprints (20% of sample size) that meet above criteria and create column highlighting them as =1
allGEDI2A_intact <- allGEDI2A_intact %>%
  mutate(intact_sample = as.integer(row_number() %in% sample(n(), size = 150)))

# Filter just the intact GEDI footprint samples
allGEDI2A_intact_sample <- allGEDI2A_intact %>%
  filter(intact_sample==1) %>%
  dplyr::select(-intact_sample) 

mapview(allGEDI_intact_sample)


# Randomly sample 150 GEDI footprints (20% of sample size) that meet above criteria and create column highlighting them as =1
allGEDI_intact <- allGEDI_intact %>%
  mutate(intact_sample = as.integer(row_number() %in% sample(n(), size = 150)))

# Filter just the intact GEDI footprint samples
allGEDI_intact_sample <- allGEDI_intact %>%
  filter(intact_sample==1) %>%
  dplyr::select(-intact_sample) 

# samples spread across the amazon with various soil/ distance to river/ distance to degradation



# Merge the secondary forest and ALS extent GEDI footprints data frame with new random intact samples
# For complete gradient of GEDI samples across the site

allGEDI2A_gradient <- rbind(allGEDI2A_sec, allGEDI2A_intact_sample)

allGEDI_gradient <- rbind(allGEDI_sec, allGEDI_intact_sample)


allGEDI_gradient <- allGEDI_gradient %>%
  mutate(Degradation = case_when(
    burn_freq > 0 ~ "burned",
    forest_age < 50 ~ "logged",
    forest_age > 50 ~ "intact",
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

sf::st_write(allGEDI2A_gradient, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A_gradient_test.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI_gradient, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI_gradient_test.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2A_gradient <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A_gradient_test.fgb")
#allGEDI_gradient <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A_gradient_test.fgb")




# ------ Extracting metrics and canopy cover from ALS within GEDI footprints ------


# Load the ALS catalog of the final processed .laz files

final_ALS <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/final_ALS_Chap1')


# Sort of works! Metric extraction (doesn't work with above catalog processing function)
# Currently using both allGEDI2A and allGEDI as 2A has height and regressions 
# and allGEDI has canopy and carbon
als_metrics <- plot_metrics(fin_catalog, .stdmetrics_z, allGEDI2A_gradient, radius = 12.5)


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


# NEED TO WRITE FUNCTIONS TO EXTRACT FROM ALS

#*stats between GEDI and ALS
#*random forest model



# ------ Statistics ---------

# Inspired by Dorado et al 2021

# Function to perform linear regression
perform_regression <- function(data, formula) {
  lm(formula = formula, data = data)
}

# Function to perform step-wise model to find optimal fit, reintroducing variables for best fit (pulls out ones that don't explain)
perform_stepwise_selection <- function(model) {
  step(model, direction = 'both')
}

# Function to compute RMSE
compute_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

# Function to compute bias
compute_bias <- function(actual, predicted) {
  mean(actual - predicted, na.rm = TRUE)
}

# Function to compute relative bias (rbias)
compute_relative_bias <- function(bias, mean_actual) {
  (sum(bias) / mean_actual) * 100
}

# Example usage:
# Assuming "all_height_intersect" is your dataframe

# Full model
mod <- perform_regression(all_height_intersect, hyp.mean.tot.height ~ rh99 + rh95 + rh100)

# Step-wise model :to find optimal fit, reintroducing variables for best fit (pulls out ones that don't explain)
mod2 <- perform_stepwise_selection(mod)

# Compute RMSE : # Difference between hyp and rh predicted values - positive or negative not identified
# Error as opposed to fitting a modeled error to predict hypothetical error
rmse <- compute_rmse(all_height_intersect$hyp.mean.tot.height, predict(mod, all_height_intersect))
rmse_log <- compute_rmse(all_height_intersect$hyp.mean.tot.height[all_height_intersect$Degradation == 'logged'], predict(modlog, all_height_intersect[all_height_intersect$Degradation == 'logged', ]))
rmse_burn <- compute_rmse(all_height_intersect$hyp.mean.tot.height[all_height_intersect$Degradation == 'burned'], predict(modburn, all_height_intersect[all_height_intersect$Degradation == 'burned', ]))

# Compute bias : Average difference between hyp and rh (in this case hyp is -0.7m lower than rh)
bias <- compute_bias(all_height_intersect$hyp.mean.tot.height, predict(mod, all_height_intersect))
bias_log <- compute_bias(all_height_intersect$hyp.mean.tot.height[all_height_intersect$Degradation == 'logged'], predict(modlog, all_height_intersect[all_height_intersect$Degradation == 'logged', ]))
bias_burn <- compute_bias(all_height_intersect$hyp.mean.tot.height[all_height_intersect$Degradation == 'burned'], predict(modburn, all_height_intersect[all_height_intersect$Degradation == 'burned', ]))

# Compute mean hyp height : for % error relative to mean height of category
mean_actual <- mean(all_height_intersect$hyp.mean.tot.height)

# Compute relative bias (rbias) 
rbias <- compute_relative_bias(bias, mean_actual)
rbias_log <- compute_relative_bias(bias_log, mean_actual)
rbias_burn <- compute_relative_bias(bias_burn, mean_actual)





# Create a tibble with the results
results <- tibble(
  Variable = c("Canopy Height", "Carbon", "Canopy Cover"),
  Full_Model_RMSE = c(full_mod_rmse, carbon_rmse, canopy_cover_rmse),
  Full_Model_Bias = c(full_mod_bias, carbon_bias, canopy_cover_bias),
  Relative_Bias = c(full_mod_relative_bias, rbias_carbon, rbias_canopy_cover)
)

# Create a function to format results
format_results <- function(x) {
  format(x, digits = 2)
}

# Apply formatting to numeric columns
results_formatted <- results %>%
  mutate(across(where(is.numeric), format_results))

# Print the table
results_formatted









# ------- Graphs/ visulisations -------


GEDI_gradient_ALSplot <-GEDI_degrade0_2021 %>%
  ggplot(aes(x=Age_category, y=rh99, color=Degradation)) +
  geom_point() +
  labs(title = "GEDI Canopy height", x = "Age of forest", y = "Relative height top of canopy (m) (rh99)") +
  theme_bw()
plot(GEDI_gradient_ALSplot)


# Example usage of the function
data <- dataframe %>%
  ggplot_point(data, "x_var", "y_var", "color_var", "title", "x_label", "y_label")
print(plot)

