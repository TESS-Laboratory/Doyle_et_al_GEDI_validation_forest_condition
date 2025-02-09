# Load necessary libraries
library(sf)
library(dplyr)

# Set the folder path where your polygon files are stored
folder_path <- "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Input_data/Supplementary_polygons"

# List all polygon files (assuming shapefiles for this example)
polygon_files <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)

# Initialize an empty data frame to store results
centroids_table <- data.frame()

for (file in polygon_files) {
  # Read the polygon file
  polygons <- st_read(file)
  
  # Reproject to WGS84
  polygons <- st_transform(polygons, crs = 4326)
  
  # Compute the centroids
  centroids <- st_centroid(polygons)
  
  # Extract coordinates
  centroids_coords <- st_coordinates(centroids)
  
  # Create a simplified and formatted data frame
  centroids_df <- data.frame(
    X = round(centroids_coords[, "X"], 6), # Longitude
    Y = round(centroids_coords[, "Y"], 6), # Latitude
    FileName = basename(file)              # File name
  )
  
  # Combine with the results table
  centroids_table <- bind_rows(centroids_table, centroids_df)
}

# Save the results as a CSV file or view them
write.csv(centroids_table, "centroids_table.csv", row.names = FALSE)
print(centroids_table)

# Filter out the specific row where X = -54.69990 and Y = -3.115570
centroids_df <- centroids_table %>%
  filter(!(X == -54.69990 & Y == -3.115570))

# Check the updated data frame
print(centroids_table)

# Convert the data frame to an sf object
centroids_sf <- st_as_sf(centroids_df, coords = c("X", "Y"), crs = 4326)

# Save as a shapefile
st_write(centroids_sf, "site_centroids.shp")