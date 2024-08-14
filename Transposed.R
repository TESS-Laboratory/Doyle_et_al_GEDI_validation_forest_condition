# Preparing GEDI data for classification - potentially another chapter
# Transpose to see difference in slope in diff degradation types along waveform etc

# NOT WORKING -Error in if (is.na(x)) NA_crs_ else make_crs(paste0("EPSG:", x)) : the condition has length > 1

# rh, shot number, classification, geometry needed FOR CHRIS

forest_age <- raster::extract(secondaryforest2024, allGEDI2AB, method='simple')
allGEDI2AB <- rbind(allGEDI2AB, forest_age)

burn_freq <- terra::extract(burnedforest, allGEDI2AB, method='simple')
allGEDI2AB <- rbind(allGEDI2AB, burn_freq)

validation <- terra::extract(forestclass, allGEDI2AB, method='simple')
allGEDI2AB <- rbind(allGEDI2AB, validation)

allGEDI2AB_aged <- allGEDI2AB %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age))

allGEDI2AB <- allGEDI2AB_aged %>%
  process_GEDI_degradation()

# Transpose the data and select only relevant rows for waveforms

allGEDI2AB <- allGEDI2AB %>%
  as.data.frame() %>%
  select(shot_number, geometry, starts_with("rh"))

