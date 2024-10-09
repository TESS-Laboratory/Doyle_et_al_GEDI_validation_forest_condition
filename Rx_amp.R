
# Need to make whole dataframe cumulative/ amp values for regressions them merge with smaller dataset

# Reconfigured 2A download including the rx_cumulative energy values (needs to be intergrated)
allGEDI2A <- st_transform(allGEDI2A, st_crs(GEDI2AB))

# Get values for rx_energy for each rh by calculating non-cumulative differnce between values        
allGEDI2A_non_cum <- allGEDI2A

# Loop through the rx_cum columns, de-cumulate, and make any negative values positive (negative cumulative pattern rh100-0)
for (i in 100:1) {
  # De-cumulate the rx_cum values
  allGEDI2A_non_cum[[paste0("rx_", i)]] <- allGEDI2A[[paste0("rx_cum", i)]] - allGEDI2A[[paste0("rx_cum", i - 1)]]
  
  # Multiply negative values by -1 to make them positive
  allGEDI2A_non_cum[[paste0("rx_", i)]][allGEDI2A_non_cum[[paste0("rx_", i)]] < 0] <- 
    allGEDI2A_non_cum[[paste0("rx_", i)]][allGEDI2A_non_cum[[paste0("rx_", i)]] < 0] * -1
}

# First value rx_0 is the same as rx_cum0
allGEDI2A_non_cum$rx_0 <- allGEDI2A$rx_cum0

# Create a proxy for amplitude using the square root of the energy values (formula:𝐸∝𝐴^2 -
# the energy of a wave is directly proportional to the square of the amplitude)
for (i in 0:100) {
  allGEDI2A_non_cum[[paste0("amp_", i)]] <- sqrt(allGEDI2A_non_cum[[paste0("rx_", i)]])
}


allGEDI2A_amp <- allGEDI2A_non_cum


#PART 2

# Select only relative height data and unique identifier, transforming dataset
GEDI2AB_trans_amp <- allGEDI2A_amp %>%
  as.data.frame() %>%
  select(shot_number, starts_with("amp_"))

# Regression function to cumulative waveforms
result_df_amp <- apply(GEDI2AB_trans_amp, 1, rh_linear_regression)

# Convert the result to a dataframe and set column names
result_df_amp <- t(data.frame(result_df_amp))
result_df_amp  <- as.data.frame(result_df_amp, stringsAsFactors = FALSE)
colnames(result_df_amp) <- c("shot_number", "W_intercept", "W_slope", "W_variance")

result_df_amp <- result_df_amp %>%
  mutate(W_intercept = as.numeric(W_intercept),
         W_slope = as.numeric(W_slope),
         W_variance = as.numeric(W_variance))




# Using waveform lidar package preferred format for additional summary parameters
# Convert data.frame to data.table for easier manipulation

GEDI2AB_trans_amp <- as.data.table(GEDI2AB_trans_amp)

# Separate the shot_number from the waveform data, removing shot column
shot_numbers <- GEDI2AB_trans_amp$shot_number
waveform_data <- GEDI2AB_trans_amp[, -1, with = FALSE] 

# Add an index column at the beginning ensuring the index is the first column
waveform_data[, index := .I]
setcolorder(waveform_data, c("index", setdiff(names(waveform_data), "index")))

# Apply lpeak
lpeak_results <- apply(waveform_data, 1, safe_lpeak)
lpeak_results <- lapply(lpeak_results, function(x) {
  if (!is.null(x$peaks)) {
    # Summarize by counting the number of TRUE values
    return(data.frame(n_peaks = sum(x$peaks, na.rm = TRUE)))
  } else {
    return(data.frame(n_peaks = NA))
  }
})
lpeak_dt <- rbindlist(lpeak_results, fill = TRUE)

# Apply maxamp
maxamp_results <- apply(waveform_data, 1, safe_maxamp)
maxamp_dt <- rbindlist(maxamp_results, fill = TRUE)

# Reattach shot numbers
waveform_results <- cbind(shot_number = shot_numbers, lpeak_dt, maxamp_dt)
waveform_results  <- as.data.frame(waveform_results)

# Remerge results with original GEDI2A dataframe and name GEDI regressions

allGEDI2A_amp <- left_join(allGEDI2A_amp, result_df_amp, by = "shot_number")
allGEDI2A_amp <- left_join(allGEDI2A_amp, waveform_results, by = "shot_number")


allGEDI2A_amp <- allGEDI2A_amp %>%
  select(shot_number, W_intercept, W_slope, W_variance,
         max_amp, n_peaks, starts_with("amp_"), matches("^rh([0-9]|[1-9][0-9]|100)$")) %>%
  select(-amp_0)


sf::st_write(allGEDI2A_amp, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2A_amp_regression.fgb", delete_dsn = TRUE, overwrite = TRUE)
# allGEDI2AB_reg <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allGEDI2AB_regressions.fgb")




# PANEL 3 - for smaller dataset violins


# Merging data with orginal dataframes to get Degradation status
# Remove geometries to join data by shot_number to other dataframes
allGEDI2A_amp_no_geom <- st_set_geometry(allGEDI2A_amp, NULL)

# Merge datasets based on shot_number to create two datasets: 2A/2B and additional 4A
GEDI2A_amp <- GEDI2AB %>%
  left_join(allGEDI2A_amp_no_geom, by = "shot_number") 

GEDI2A_amp <- GEDI2A_amp %>%
  select(-matches("\\.x$")) %>%
  rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))



# Step 1: Calculate the average amplitude for each Degradation type across amp_1 to amp_100
testGEDI2A_amp_long <- testGEDI2A_amp %>%
  st_set_geometry(NULL) %>%  # Remove geometry
  pivot_longer(cols = starts_with("amp_"), names_to = "amp_index", values_to = "amplitude") %>%
  mutate(amp_index = as.numeric(gsub("amp_", "", amp_index))) %>%
  group_by(Degradation, amp_index) %>%
  summarise(avg_amplitude = mean(amplitude, na.rm = TRUE)) %>%
  ungroup()


testGEDI2A_rh_long <- testGEDI2A_amp %>%
  st_set_geometry(NULL) %>%  # Remove geometry
  pivot_longer(
    cols = starts_with("rh"),  # Include all columns that start with 'rh'
    names_to = "rh_index", values_to = "relative_height"
  ) %>%
  mutate(
    is_numeric_rh = grepl("^rh(\\d{1,2}|100)$", rh_index),  # Identify if the column is a numeric rh column
    rh_index = if_else(is_numeric_rh, as.numeric(gsub("rh", "", rh_index)), NA_real_)  # Convert only valid numeric rh indices
  ) %>%
  group_by(Degradation, rh_index) %>%
  summarise(avg_rh = mean(relative_height, na.rm = TRUE)) %>%
  ungroup()

# Combine the average amplitude and average relative height
combined_data <- testGEDI2A_amp_long %>%
  inner_join(testGEDI2A_rh_long, by = c("Degradation", "amp_index" = "rh_index"))

ggplot(combined_data, aes(x = avg_amplitude, y = avg_rh, color = Degradation, group = Degradation)) +
  geom_line(size = 1) +
  labs(title = "Average Amplitude vs. Average Relative Height by Degradation Type",
       x = "Average Relative Height (rh_1 to rh_100)",
       y = "Average Amplitude (amp_1 to amp_100)") +
  theme_minimal() +
  scale_color_viridis_d()






row <- GEDI2A_amp[728, grep("^amp_", colnames(GEDI2A_amp))]

# Plot the energy values for the selected row (excluding rx_0)
plot(as.numeric(row), type = "l", col = "blue", lwd = 2,
     xlab = "Index (rx_1 to rx_100)", ylab = "Energy (rx_cum)", 
     main = "Energy Values from rx_1 to rx_100")



# Select the specific row (row 728 in this case)
row <- GEDI2A_amp[1, ]

# Extract the amp and rh values (amp_0 to amp_100 and rh_0 to rh_100)
amp_values <- as.numeric(row[grep("^amp_", colnames(row))])
rh_values <- as.numeric(row[grep("^rh", colnames(row))])
rh_values <- rh_values[-1] 

# Plot the amplitude vs. relative height for the selected row
plot(amp_values, rh_values, type = "l", col = "black", lwd = 2,
     xlab = "Amplitude (amp_0 to amp_100)", ylab = "Relative Height (rh_0 to rh_100)",
     main = "Amplitude vs. Relative Height for Row 728",
     ylim = c(-10, 50))  # Set y-axis range from -10 to 60






# For the PCA

allGEDI2A_amp_PCA <- allGEDI2A_amp %>%
  select(shot_number, W_intercept, W_slope, W_variance,
         max_amp, n_peaks)

allGEDI2A_amp_PCA_nogeom <- st_set_geometry(allGEDI2A_amp_PCA, NULL)

# Merge amp dataset with overall allGEDI2AB_ALS
allGEDI2AB_ALS_amp <- allGEDI2AB_ALS %>%
  left_join(allGEDI2A_amp_PCA_nogeom, by = "shot_number") 






















# Apply fslope
fslope_results <- apply(waveform_data, 1, safe_fslope)
valid_fslope_results <- lapply(fslope_results, function(x) if (is.data.frame(x) || is.list(x)) x else NULL)
fslope_dt <- rbindlist(valid_fslope_results, fill = TRUE)

# Apply integral
integral_results <- apply(waveform_data, 1, safe_integral)
valid_integral_results <- lapply(integral_results, function(x) if (is.data.frame(x) || is.list(x)) x else NULL)
integral_dt <- rbindlist(valid_integral_results, fill = TRUE)











