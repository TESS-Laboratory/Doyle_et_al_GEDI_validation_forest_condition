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

# ----- PROCESS ALS --------
# Load and retile (DAAC) catalog for consistency between various ALS sources

DAAC18_19S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_19S')
DAAC18_20S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_20S')
DAAC18_21S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_21S')
DAAC18_22S <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar/DAAC18_22S')

retile_DAAC18_19S <- retile_catalog_pref(DAAC18_19S)
retile_DAAC18_20S <- retile_catalog_pref(DAAC18_20S)
retile_DAAC18_21S <- retile_catalog_pref(DAAC18_21S)
retile_DAAC18_22S <- retile_catalog_pref(DAAC18_22S)

# Set CRS of new catalog tiles to specified UTM
st_crs(retile_DAAC18_19S) <- 32719
st_crs(retile_DAAC18_20S) <- 32720
st_crs(retile_DAAC18_21S) <- 32721
st_crs(retile_DAAC18_22S) <- 32722

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

# Create DTM for ability to determiine extent of the .laz regions for GEDI overlap

dtm_DAAC18_19S <- rasterize_terrain(DAAC18_19S_norm, 2, tin(), pkg = "terra")
dtm_DAAC18_20S <- rasterize_terrain(DAAC18_20S_norm, 2, tin(), pkg = "terra")
dtm_DAAC18_21S <- rasterize_terrain(DAAC18_21S_norm, 2, tin(), pkg = "terra")
dtm_DAAC18_22S <- rasterize_terrain(DAAC18_22S_norm, 2, tin(), pkg = "terra")

writeRaster(dtm_DAAC18_19S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)
writeRaster(dtm_DAAC18_20S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)
writeRaster(dtm_DAAC18_21S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)
writeRaster(dtm_DAAC18_22S, "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/DAAC_lidar", overwrite=TRUE)





