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

# ----- PROCESS ALS --------
# Load and retile (DAAC) catalog for consistency between various ALS sources

cautario_ALS
DAAC18_19S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_19S')
DAAC18_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_20S')
DAAC18_21S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_21S')
DAAC18_22S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_22S')

retile_DAAC18_19S <- retile_catalog_pref(DAAC18_19S)
retile_DAAC18_20S <- retile_catalog_pref(DAAC18_20S)
retile_DAAC18_21S <- retile_catalog_pref(DAAC18_21S)
retile_DAAC18_22S <- retile_catalog_pref(DAAC18_22S)

# Check new catalog
las_check(retile_DAAC18_19S)
plot(retile_DAAC18_19S, mapview = TRUE, map.type = "Esri.WorldImagery")

# Process the retiled ALS

DAAC18_19S_norm <- process_als(retile_DAAC18_19S)
DAAC18_20S_norm <- process_als(retile_DAAC18_20S)
DAAC18_21S_norm <- process_als(retile_DAAC18_21S)
DAAC18_22S_norm <- process_als(retile_DAAC18_22S)

las_check(DAAC18_19S_norm)

# Filter the data for anomalous results

DAAC18_19S_norm <- filter_als(DAAC18_19S_norm)
DAAC18_20S_norm <- filter_als(DAAC18_20S_norm)
DAAC18_21S_norm <- filter_als(DAAC18_21S_norm)
DAAC18_22S_norm <- filter_als(DAAC18_22S_norm)

# Set CRS of new catalog tiles to specified UTM
st_crs(DAAC18_19S_norm) <- 32719
st_crs(DAAC18_20S_norm) <- 32720
st_crs(DAAC18_21S_norm) <- 32721
st_crs(DAAC18_22S_norm) <- 32722

# Create DTM for ability to determine extent of the .laz regions for GEDI overlap

dtm_DAAC18_19S <- rasterize_terrain(DAAC18_19S_norm, 2, tin(), pkg = "terra")
dtm_DAAC18_20S <- rasterize_terrain(DAAC18_20S_norm, 2, tin(), pkg = "terra")
dtm_DAAC18_21S <- rasterize_terrain(DAAC18_21S_norm, 2, tin(), pkg = "terra")
dtm_DAAC18_22S <- rasterize_terrain(DAAC18_22S_norm, 2, tin(), pkg = "terra")

writeRaster(dtm_DAAC18_19S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)
writeRaster(dtm_DAAC18_20S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)
writeRaster(dtm_DAAC18_21S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)
writeRaster(dtm_DAAC18_22S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)



# ------ extracting metrics and canopy cover from footprints ------
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





# ----------- GEDI download ----------------

# Download GEDI2A files for all polygon shapefiles in a folder,
# creating output geodataframe for each AOI

poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/CAUTARIO_polygons"
start_date <- "2022-01-01"
end_date <- "2024-02-01"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI2A"

gedi2A_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)


# Reading all of the 2A output .fgb files into one geodatabase

fgb_files <- list.files(path = fgb_output_folder, pattern = "\\.fgb$", full.names = TRUE)
fgb_list <- lapply(fgb_files, st_read)
allGEDI2A <- do.call(rbind, fgb_list)
sf::st_write(allGEDI2A, "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI2A/allGEDI2A.fgb", delete_dsn = TRUE)

mapview(allGEDI2A)





DAAC1819S_1_2A <- read_sf('/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI2A/DAAC1819S_1_2A.fgb') %>%
  distinct(shot_number, .keep_all = TRUE)

mapview(daac_test)






daac_test1 <- read.csv('/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI1B/DAAC1819S_1_1B.fgb') %>%
  
  
  
  distinct(shot_number, .keep_all = TRUE)

gedi1b_sf <-gedi1b_sf  %>%
  distinct(shot_number, .keep_all = TRUE)



# this needs to run for the plotting


poly_folder_path <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/DAAC_polygons"
start_date <- "2019-01-01"
end_date <- "2019-12-31"
fgb_output_folder <- "/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Output_data/GEDI1B"

gedi1B_test <- gedi1B_batch_download(poly_folder_path, start_date, end_date, fgb_output_folder)


#fp <- function(x, dat.path="/Users/emilydoyle/Documents/workspace/Doyle_Chap1_data/Input_data/DAAC_polygons"){
#file.path(dat.path, x)
#}
#polygon <- fp("DAAC1819S_1.shp") %>%
#  read_sf








# Extracting all of the waveforms
gedi1b_wvf <- extract_waveforms(gedi1b_sf)

print(gedi1b_wvf)

# plot them using `ggplot2`...
wvf_ggplot(gedi1b_wvf, "rxwaveform", "rxelevation") +
  facet_wrap(~shot_number, ncol = 5, scales = "free")











# ----------- GEDI processing --------------
#*stats between GEDI and ALS
#*random forest model