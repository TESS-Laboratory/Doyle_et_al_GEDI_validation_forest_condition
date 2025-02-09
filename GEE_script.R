# Google Earth Engine script to download various MapBiomas multispectral data to be used to classify
# GEDI forest condition. Edit the years for 2018, 2021 and 2023 to correspond to ALS data collection


// Load the MapBiomas Secondary Vegetation Age Layer
var secondaryVegetationAge = ee.Image('projects/mapbiomas-public/assets/brazil/lulc/collection9/mapbiomas_collection90_secondary_vegetation_age_v1');

// Select a specific year (e.g., 2018)
var ageBand = secondaryVegetationAge.select('secondary_vegetation_age_2018');

// Define visualization parameters for the Secondary Vegetation Age Layer
var ageVisParams = {
  min: 0,    // Minimum age
  max: 33,   // Maximum age (e.g., for 1986–2018)
  palette: ['ffffcc', 'c2e699', '78c679', '31a354', '006837'] // Younger to older vegetation
};

// Add the layers to the map
Map.addLayer(ageBand, ageVisParams, 'Secondary Vegetation Age (2018)');

// Center the map on Brazil
Map.setCenter(-55.491477, -13.430146, 4);  // Longitude, Latitude, Zoom Level

// Print available bands for debugging
print('Secondary Vegetation Age Bands:', secondaryVegetationAge.bandNames());

// Define export parameters
Export.image.toDrive({
  image: ageBand,                 // The image to export
  description: 'Secondary_Vegetation_Age_2018', // Task description
  folder: 'EarthEngineExports',   // Folder in your Google Drive (optional)
  fileNamePrefix: 'Secondary_Vegetation_Age_2018', // File name
  region: ee.Geometry.Rectangle([-74, -34, -30, 5]), // Define the export region (Brazil's bounding box as an example)
  scale: 30,                      // Resolution in meters
  crs: 'EPSG:4326',               // Coordinate Reference System (WGS 84)
  maxPixels: 1e13                 // Maximum number of pixels to export
});






// Load the MapBiomas Fire Frequency Layer
var fireFrequency = ee.Image('projects/mapbiomas-public/assets/brazil/fire/collection3/mapbiomas_fire_collection3_fire_frequency_v1');

// Load the MapBiomas Time Since Last Fire Layer
var timeSinceLastFire = ee.Image('projects/mapbiomas-public/assets/brazil/fire/collection3/mapbiomas_fire_collection3_year_last_fire_v1');

// Select the appropriate band for Fire Frequency since 1985 and for 2023
var fireFrequencyBand = fireFrequency.select('fire_frequency_1985_2018');

// Select the appropriate classification band for Time Since Last Fire for 2023
var timeSinceLastFireBand = timeSinceLastFire.select('classification_2018');

// Define visualization parameters for Fire Frequency
var fireFrequencyVisParams = {
  min: 0,    // Minimum fire frequency
  max: 20,   // Maximum fire frequency (adjust as needed based on data range)
  palette: ['ffffff', 'ffeda0', 'feb24c', 'f03b20', 'bd0026'] // Low to high fire frequency
};

// Define visualization parameters for Time Since Last Fire
var timeSinceLastFireVisParams = {
  min: 1985, // Minimum year (since 1985)
  max: 2018, // Maximum year (2023)
  palette: ['ffffcc', 'c2e699', '78c679', '31a354', '006837'] // Older to more recent fires
};

// Add the layers to the map
Map.addLayer(fireFrequencyBand, fireFrequencyVisParams, 'Fire Frequency (1985-2018)');
Map.addLayer(timeSinceLastFireBand, timeSinceLastFireVisParams, 'Time Since Last Fire (2018)');

// Center the map on Brazil
Map.setCenter(-55.491477, -13.430146, 4); // Longitude, Latitude, Zoom Level

// Export Fire Frequency to Google Drive
Export.image.toDrive({
  image: fireFrequencyBand,          // The image to export
  description: 'Fire_Frequency_1985_2018', // Task description
  folder: 'EarthEngineExports',      // Folder in your Google Drive (optional)
  fileNamePrefix: 'Fire_Frequency_1985_2018',  // File name
  region: ee.Geometry.Rectangle([-74, -34, -30, 5]), // Define the export region (Brazil's bounding box as an example)
  scale: 30,                         // Resolution in meters
  crs: 'EPSG:4326',                  // Coordinate Reference System (WGS 84)
  maxPixels: 1e13                    // Maximum number of pixels to export
});

// Export Time Since Last Fire to Google Drive
Export.image.toDrive({
  image: timeSinceLastFireBand,                 // The image to export
  description: 'Time_Since_Last_Fire_2018',     // Task description
  folder: 'EarthEngineExports',                 // Folder in your Google Drive (optional)
  fileNamePrefix: 'Time_Since_Last_Fire_2018',  // File name
  region: ee.Geometry.Rectangle([-74, -34, -30, 5]), // Define the export region (Brazil's bounding box as an example)
  scale: 30,                                    // Resolution in meters
  crs: 'EPSG:4326',                             // Coordinate Reference System (WGS 84)
  maxPixels: 1e13                               // Maximum number of pixels to export
});

// Print available bands for debugging
print('Fire Frequency Bands:', fireFrequency.bandNames());
print('Time Since Last Fire Bands:', timeSinceLastFire.bandNames());





// Load the MapBiomas Collection 9 Integration Layer
var mapbiomas = ee.Image('projects/mapbiomas-public/assets/brazil/lulc/collection9/mapbiomas_collection90_integration_v1');

// Select the classification band for the latest year (e.g., 2018)
var forestFormation = mapbiomas.select('classification_2018')
                                .eq(3); // Filter only forest formation (code 3)

// Define visualization parameters for Forest Formation
var forestVisParams = {
  min: 0,
  max: 1,
  palette: ['ffffff', '006400'] // White for non-forest, green for forest
};

// Add the filtered Forest Formation layer to the map
Map.addLayer(forestFormation, forestVisParams, 'Forest Formation (2018)');

// Center the map on Brazil
Map.setCenter(-55.491477, -13.430146, 4); // Longitude, Latitude, Zoom Level

// Export the Forest Formation layer to Google Drive
Export.image.toDrive({
  image: forestFormation.selfMask(), // Mask to export only forest areas
  description: 'Forest_Formation_2018', // Task description
  folder: 'EarthEngineExports',        // Folder in your Google Drive (optional)
  fileNamePrefix: 'Forest_Formation_2018', // File name
  region: ee.Geometry.Rectangle([-74, -34, -30, 5]), // Define the export region (Brazil's bounding box as an example)
  scale: 30,                           // Resolution in meters
  crs: 'EPSG:4326',                    // Coordinate Reference System (WGS 84)
  maxPixels: 1e13                      // Maximum number of pixels to export
});

// Print available bands for debugging
print('MapBiomas Bands:', mapbiomas.bandNames());

# Download NAME THE NUMBERS OF THE FILE NAMES AND SAY WHAT FOLDERS



