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


#*classification

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


CAUT23_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/Permian_ALS/saibrasil_permian-global-rio-cautario_2023-10-20_0450/LiDAR')
retile_CAUT23_20S <- retile_catalog_pref(CAUT23_20S)
CAUT23_20S_norm <- process_als(retile_CAUT23_20S)
CAUT23_20S_norm <- filter_als(CAUT23_20S_norm)
st_crs(CAUT23_20S_norm) <- 32720



#import the final_tiles and filter them


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

#allGEDI2A_read <- read_sf("/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A.fgb")
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
# Remove duplicates from allGEDI2A
allGEDI2B <- distinct(allGEDI2B, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI2B, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2B.fgb", delete_dsn = TRUE, overwrite = TRUE)

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
# Remove duplicates from allGEDI2A
allGEDI4A <- distinct(allGEDI4A, shot_number, .keep_all = TRUE)
sf::st_write(allGEDI4A, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI4A.fgb", delete_dsn = TRUE, overwrite = TRUE)

mapview(allGEDI4A)


#1B GEDI download

#poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/DAAC_polygons"
#start_date <- "2019-01-01"
#end_date <- "2019-12-31"
#fgb_output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI1B"

#gedi1B <- gedi1B_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)


# Merge the 2A, 2B and 4A files by shot_number

#This will produce in an incomplete file but workflow complete (not all 2B/4A files worked)

allGEDI <- st_join(allGEDI2A, allGEDI2B) %>%
  filter(!is.na(shot_number.y)) %>%
  st_join(allGEDI4A)%>%
  filter(!is.na(shot_number)) 


# ------ GEDI regressions ---------

# Summarise relative height rh0-100 metrics with linear regression model 
# Outputs intercept, slope and variance of the 2A waveform

GEDI2A_trans <- allGEDI2A %>%
  as.data.frame() %>%----
  select(shot_number, starts_with("rh"))

# Apply regression function to each row of the dataframe
result_df <- apply(GEDI2A_trans, 1, rh_linear_regression)

# Convert the result to a dataframe and set column names
result_df <- t(data.frame(result_df))
colnames(result_df) <- c("shot_number", "G_intercept", "G_slope", "G_variance")
result_df <- as.data.frame(result_df) 

# Remerge with original GEDI2A dataframe and filter for specific relative height entries

allGEDI2A <- left_join(allGEDI2A, result_df, by = "shot_number")

allGEDI2A_reg <- allGEDI2A %>%
  select(year, solar_elevation, lat_lowestmode, lon_lowestmode, elev_highestreturn, elev_lowestmode,
         sensitivity, shot_number, degrade_flag, ALS_CRS, rh0, rh10, rh25, rh50, rh75, rh90, rh95, 
         rh96, rh97, rh98, rh99, rh100,G_intercept, G_slope, G_variance,geometry)

sf::st_write(allGEDI2A_reg, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/allGEDI2A_regressions.fgb", delete_dsn = TRUE, overwrite = TRUE)



# ----------- GEDI processing --------------

# ------ Extracting metrics and canopy cover from ALS within GEDI footprints ------
# WORKING PROGRESS

library(lidR)

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



#*stats between GEDI and ALS
#*random forest model