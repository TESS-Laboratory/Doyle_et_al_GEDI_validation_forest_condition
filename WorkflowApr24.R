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
# WORK ON THE WORKFLOW< WRIE IT ALL OUT

CAUT23_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/CAUTARIO_20S')
DAAC18_19S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_19S')
DAAC18_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_20S')
DAAC18_21S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_21S')
DAAC18_22S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_22S')

retile_CAUT23_20S <- retile_catalog_pref(CAUT23_20S)
retile_DAAC18_19S <- retile_catalog_pref(DAAC18_19S)
retile_DAAC18_20S <- retile_catalog_pref(DAAC18_20S)
retile_DAAC18_21S <- retile_catalog_pref(DAAC18_21S)
retile_DAAC18_22S <- retile_catalog_pref(DAAC18_22S)


# Load the newly retiled folder of ALS tiles (must do for each catalog of DAAC/ CAUTARIO data)
catalog <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_20S/retile')

# Validate the catalog dataset
las_check(catalog)
plot(catalog, mapview = TRUE, map.type = "Esri.WorldImagery")

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

# ----------- GEDI download ERRORS NEED FIXING ----------------

#THIS NEEDS SOME WORL - SOME ERRORS in 2B DOWNLOAD WITH SOME OF THE CAUTARIO FILES

# Download GEDI2A files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI
# Reading all of the 2A output .fgb files into one geodatabase

poly_folder_path <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/CAUTARIO_polygons"
start_date <- "2022-01-01"
end_date <- "2023-12-31"
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

sf::st_write(allGEDI2A_reg, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_regressions.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2A <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_regressions.fgb")



# ------- Forest spectral classification ---------



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



# ------ Extracting metrics and canopy cover from ALS within GEDI footprints ------

# THIS NEEDS TO BE DONE WITH THE FINAL EXTRACTION CODE E>G> CANOPY COVER AND ALL OF THE RH METRICS
# BUT FOR EGU AND TO TEST THIS IS FINE88888888888iu   
# Have to bring down from onedrive each CRS and do the extraction individually (takes time)
# Read in processed ALS catalogs (can mix CAUTARIO and DAAC20S?)

DAAC18_19S_norm <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/finalEGU19S')
DAAC18_20S_norm <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/finalEGU20S')
DAAC18_21S_norm <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/finalEGU21S')
DAAC18_22S_norm <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/finalEGU22S')
CAUT23_20S_norm <- readLAScatalog('/raid/home/eld223/Documents/final_ALS/finalCAUT20S')


# Set CRS of new catalog tiles to specified UTM

st_crs(CAUT23_20S_norm) <- 32720
st_crs(DAAC18_19S_norm) <- 32719
st_crs(DAAC18_20S_norm) <- 32720
st_crs(DAAC18_21S_norm) <- 32721
st_crs(DAAC18_22S_norm) <- 32722


# DAAC18_19S
# april 2024 - 8 of 8 worked
las_check(DAAC18_19S_norm)

allGEDI2A_19S <- allGEDI2A_gradient %>%
  filter(ALS_CRS == '19S')

allGEDI2A_19S = st_transform(allGEDI2A_19S, "EPSG:32719")
allGEDI2A_19S = st_set_crs(allGEDI2A_19S, "EPSG:32719")
st_crs(allGEDI2A_19S)

allGEDI2A_19S_metrics <- plot_metrics(DAAC18_19S_norm, .stdmetrics_z, allGEDI2A_19S, radius = 12.5)

#DAAC18_20S - ONLY 2 WORKED OF 383
# april 2024 only one worked
las_check(DAAC18_20S_norm)

allGEDI2A_20S <- allGEDI2A_gradient %>%
  filter(ALS_CRS == '20S')

allGEDI2A_20S = st_transform(allGEDI2A_20S, "EPSG:32720")
allGEDI2A_20S = st_set_crs(allGEDI2A_20S, "EPSG:32720")
st_crs(allGEDI2A_20S)

allGEDI2A_20S_metrics <- plot_metrics(DAAC18_20S_norm, .stdmetrics_z, allGEDI2A_20S, radius = 12.5)


#DAAC18_21S - ONLY 244 WORKED OF 260
las_check(DAAC18_21S_norm)

allGEDI2A_21S <- allGEDI2A_gradient %>%
  filter(ALS_CRS == '21S')

allGEDI2A_21S = st_transform(allGEDI2A_21S, "EPSG:32721")
allGEDI2A_21S = st_set_crs(allGEDI2A_21S, "EPSG:32721")
st_crs(allGEDI2A_21S)

allGEDI2A_21S_metrics <- plot_metrics(DAAC18_21S_norm, .stdmetrics_z, allGEDI2A_21S, radius = 12.5)


#DAAC18_22S - 0
las_check(DAAC18_22S_norm)

allGEDI2A_22S <- allGEDI2A_gradient %>%
  filter(ALS_CRS == '22S')

allGEDI2A_22S = st_transform(allGEDI2A_22S, "EPSG:32722")
allGEDI2A_22S = st_set_crs(allGEDI2A_22S, "EPSG:32722")
st_crs(allGEDI2A_22S)

allGEDI2A_22S_metrics <- plot_metrics(DAAC18_22S_norm, .stdmetrics_z, allGEDI2A_22S, radius = 12.5)


#CAUTARIO_20S_test - 376 WORKED OF 383
#round 2 april 2024 - 378 worked of 385 - this one has to be done on server
las_check(CAUT23_20S_norm)
st_crs(CAUT23_20S_norm) <- 32720


allGEDI2A_20S <- allGEDI2A %>%
  filter(ALS_CRS == '20S')

allGEDI2A_20S = st_transform(allGEDI2A_20S, "EPSG:32720")
allGEDI2A_20S = st_set_crs(allGEDI2A_20S, "EPSG:32720")
st_crs(allGEDI2A_20S)

allGEDI2A_CAUT20S_metrics <- plot_metrics(CAUT23_20S_norm, .stdmetrics_z, allGEDI2A_20S, radius = 12.5)

# only 19, 21 and cautario provide the data, 20 provides 1 bit (remove?)

sf::st_write(allGEDI2A_19S_metrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_19S_metrics_step.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2A_20S_metrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_19S_metrics_step.fgb", delete_dsn = TRUE, overwrite = TRUE)
sf::st_write(allGEDI2A_21S_metrics, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_19S_metrics_step.fgb", delete_dsn = TRUE, overwrite = TRUE)














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
#*
#*
#*# THE GEDI DATA HAS THE CRS OM IT< MAY HAVE TO SEPARATE THESE OUT INTO 
#*5 SEPARATE EXTRACTIONS WITH GEDI BEING CRS TO THE CRS OF THE  ALS THEN MERGED

# TEMPORARILY MERGING CATALOGS (600ish points)

allGEDI2A_19S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/allGEDI2A_19S_metrics.fgb")
allGEDI2A_20S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/allGEDI2A_20S_metrics.fgb")
allGEDI2A_21S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/allGEDI2A_21S_metrics.fgb")
allGEDI2A_CAUT20S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/allGEDI2A_CAUT20S_metrics.fgb")

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

sf::st_write(allheight, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allheight_draft.fgb", delete_dsn = TRUE, overwrite = TRUE)
allheight <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allheight_draft.fgb")




