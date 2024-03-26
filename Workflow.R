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


# SEE WHERE TO INSERT THIS????
# Checking if ALS lidar and GEDI dataframe are the same CRS
st_distance(als, GEDI_gradient_ALS)
st_crs(als)

# Reprojecting the GEDI dataframe
GEDI_gradient_ALScrs = st_transform(GEDI_gradient_ALS, "EPSG:31980")
GEDI_gradient_ALScrs = st_set_crs(GEDI_gradient_ALS, "EPSG:31980")
st_crs(GEDI_gradient_ALScrs)



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


# ----------- GEDI download  ERRORS NEED FIXING ----------------

#THIS NEEDS SOME WORL - SOME ERRORS in 2B DOWNLOAD WITH SOME OF THE CAUTARIO FILES

# Download GEDI2A files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI
# Reading all of the 2A output .fgb files into one geodatabase

poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/CAUTARIO_polygons"
start_date <- "2022-01-01"
end_date <- "2023-12-31"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI2A"

gedi2A_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)

fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI2A <- do.call(rbind, fgb_list)
# Remove duplicates from allGEDI2A
allGEDI2A <- distinct(allGEDI2A, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI2A, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A.fgb", delete_dsn = TRUE, overwrite = TRUE)

#allGEDI2A <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A.fgb")
mapview(allGEDI2A) + mapview(secondaryforest2023)

# Download GEDI2B files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI
# then reading all of the 2B output .fgb files into one geodatabase

poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/CAUTARIO_polygons"
start_date <- "2022-01-01"
end_date <- "2023-12-31"
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

poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/CAUTARIO_polygons"
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
  mutate(Degrdtn = case_when(
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
#allGEDI_gradient <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI_gradient_test.fgb")


# ------ Extracting metrics and canopy cover from ALS within GEDI footprints ------


# THIS NEEDS WORK, IT IS GOING TO HAVE TO BE DONE MANUALLY FOR EACH CRS

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

# TEMPORARILY MERGING CATALOGS (600ish points)

allGEDI2A_19S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/allGEDI2A_19S_metrics.fgb")
allGEDI2A_20S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/allGEDI2A_20S_metrics.fgb")
allGEDI2A_21S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/allGEDI2A_21S_metrics.fgb")
allGEDI2A_CAUT20S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/allGEDI2A_CAUT20S_metrics.fgb")

allGEDI2A_19S_metrics <- st_transform(allGEDI2A_19S_metrics, "EPSG:32643")
allGEDI2A_20S_metrics <- st_transform(allGEDI2A_20S_metrics, "EPSG:32643")
allGEDI2A_21S_metrics <- st_transform(allGEDI2A_21S_metrics, "EPSG:32643")
allGEDI2A_CAUT20S_metrics <- st_transform(allGEDI2A_CAUT20S_metrics, "EPSG:32643")

st_crs(allGEDI2A_19S_metrics)

# Remove duplicated rows
allGEDI2A_19S_metrics <- distinct(allGEDI2A_19S_metrics)
allGEDI2A_20S_metrics <- distinct(allGEDI2A_20S_metrics)
allGEDI2A_21S_metrics <- distinct(allGEDI2A_21S_metrics)
allGEDI2A_CAUT20S_metrics <- distinct(allGEDI2A_CAUT20S_metrics)

# Example: Standardizing column names
# Assuming columns are in the same order and just have different names
colnames(allGEDI2A_20S_metrics) <- colnames(allGEDI2A_19S_metrics)
colnames(allGEDI2A_21S_metrics) <- colnames(allGEDI2A_19S_metrics)
colnames(allGEDI2A_CAUT20S_metrics) <- colnames(allGEDI2A_19S_metrics)

# Combine the dataframes into one dataframe using bind_rows
merged_df <- bind_rows(allGEDI2A_19S_metrics, 
                       allGEDI2A_20S_metrics,
                       allGEDI2A_21S_metrics,
                       allGEDI2A_CAUT20S_metrics)

# Remove rows with NAs in the 'year' column
allheight <- merged_df %>%
  filter(!is.na(zq95))

sf::st_write(allheight, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allheight_draft.fgb", delete_dsn = TRUE, overwrite = TRUE)
allheight <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allheight_draft.fgb")





# ------ Statistics ---------

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




# ------- Graphs/ visulisations -------

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
plot(GEDI_degradation)

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
  geom_point(aes(x = Age_category, y = rh99, color = Degradation)) +
  labs(title = "GEDI canopy height by degradation", x = "Forest age", y = "Canopy height (m)") +
  theme_bw()

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
  ggplot(aes(x=zq95, y=rh95, color=Degrdtn)) +
  geom_point() +
  labs(title = "Correspondance between ALS and GEDI 95%", x = "ALS", y = "GEDI") +
  theme_bw() 
#scale_colour_manual(values = c("darkorange", "darkolivegreen3", "Black"))
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
  ggplot(aes(x=Degradation, y=G_intercept, fill=Degradation)) +
  geom_violin(color = "black", alpha = 0.8) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  labs(title = "Gradient intercept of relative height profile", x = "Degradation type", y = "G_intercept") +
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



