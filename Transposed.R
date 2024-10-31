# Preparing GEDI data for classification - potentially another chapter
# Transpose to see difference in slope in diff degradation types along waveform etc

# NOT WORKING -Error in if (is.na(x)) NA_crs_ else make_crs(paste0("EPSG:", x)) : the condition has length > 1

# Load necessary libraries
library(tidyr)
library(ggplot2)
library(dplyr)



# rh, shot number, classification, geometry needed FOR CHRIS

forest_age <- raster::extract(secondaryforest2024, allGEDI2AB, method='simple')
allGEDI2AB$forest_age <- forest_age

burn_freq <- terra::extract(burnedforest, allGEDI2AB, method='simple')
allGEDI2AB$burn_freq <- burn_freq

allGEDI2AB_aged <- allGEDI2AB %>%
  mutate(forest_age = ifelse(is.na(forest_age), 99, forest_age))

allGEDI2AB <- allGEDI2AB_aged %>%
  process_GEDI_degradation()

# Transpose the data and select only relevant rows for waveforms

allGEDI2AB_trans <- allGEDI2AB_ALS %>%
  as.data.frame() %>%
  select(shot_number, geometry, Degradation, starts_with("rh"))



# Reshape the data to long format
allGEDI2AB_trans_plot <- allGEDI2AB_trans %>%
  pivot_longer(cols = starts_with("rh"), 
               names_to = "rh_value", 
               values_to = "waveform") %>%
  mutate(rh_value = as.numeric(gsub("rh", "", rh_value)))

# Plot the data
ggplot(allGEDI2AB_trans_plot, aes(x = rh_value, y = waveform, group = shot_number, color = Degradation)) +
  geom_line(size = 0.2) +
  facet_wrap(~ Degradation) +
  theme_minimal() +
  labs(title = "Waveform by Degradation Type", x = "Relative Height (rh)", y = "Waveform Value") +
  theme(legend.position = "none")



# Calculate the average waveform for each degradation type
avg_waveform <- allGEDI2AB_trans_plot %>%
  group_by(Degradation, rh_value) %>%
  summarize(mean_waveform = mean(waveform, na.rm = TRUE))

# Plot the average waveform by degradation type
ggplot(avg_waveform, aes(x = rh_value, y = mean_waveform, color = Degradation)) +
  geom_line(size = 1.5) +
  theme_minimal() +
  labs(title = "Average Elevation by Degradation Type", 
       x = "Relative Height (rh)", 
       y = "Average Elevation Value (m)") +
  theme(legend.position = "bottom")


#Need rx_energy values and num_detectedmodes from GEDI2A waveform








