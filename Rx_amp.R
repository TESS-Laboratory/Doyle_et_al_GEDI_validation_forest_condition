

# PANEL 3 - for smaller dataset violins

# Merging data with orignal dataframes to get Degradation status
# Remove geometries to join data by shot_number to other dataframes
allGEDI2A_amp_no_geom <- st_set_geometry(allGEDI2A_amp, NULL)

# Merge datasets based on shot_number to create two datasets: 2A/2B and additional 4A
GEDI2A_amp <- GEDI2AB %>%
  left_join(allGEDI2A_amp_no_geom, by = "shot_number") 

GEDI2A_amp <- GEDI2A_amp %>%
  select(-matches("\\.x$")) %>%
  rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))









# Step 1: Calculate the average amplitude for each Degradation type across amp_1 to amp_100
allGEDI2A_amp_long <- allGEDI2A_amp %>%
  st_set_geometry(NULL) %>%  # Remove geometry
  pivot_longer(cols = starts_with("amp_"), names_to = "amp_index", values_to = "amplitude") %>%
  mutate(amp_index = as.numeric(gsub("amp_", "", amp_index))) %>%
  group_by(Degradation, amp_index) %>%
  summarise(avg_amplitude = mean(amplitude, na.rm = TRUE)) %>%
  ungroup()


allGEDI2A_rh_long <- allGEDI2A_amp %>%
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
combined_data <- allGEDI2A_amp_long %>%
  inner_join(testGEDI2A_rh_long, by = c("Degradation", "amp_index" = "rh_index"))

ggplot(combined_data, aes(x = avg_amplitude, y = avg_rh, color = Degradation, group = Degradation)) +
  geom_line(size = 1) +
  labs(title = "Average Amplitude vs. Average Relative Height by Degradation Type",
       x = "Average Relative Height (rh_1 to rh_100)",
       y = "Average Amplitude (amp_1 to amp_100)") +
  theme_minimal() +
  scale_color_viridis_d()


# Correct the axis: Amplitude on x-axis, Relative Height on y-axis
ggplot(combined_data, aes(x = avg_rh, y = avg_amplitude, color = Degradation, group = Degradation)) +
  geom_line(size = 1) +
  labs(title = "Waveform Amplitude vs. Elevation by Degradation Type",
       x = "Elevation (m)",
       y = "Waveform Amplitude") +
  theme_minimal() +
  scale_color_manual(values = c("Burned 1-3" = "#e9a2b4", 
                                "Burned 4+" = "#ca0020", 
                                "Intact" = "#92c5de", 
                                "Logged" = "#0073e6")) +
  coord_flip() +  # Flip axes here
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold")
  )
















# CODE TO PLOT INDIVIDUAL WAVEFORMS AMPS

#codes: Intact (648, 665, 728), Logged (4,9 646), Burned 1-3 (1, 44, 73), Burned 4+ (2, 285, 416)



row <- GEDI2A_amp[646, grep("^amp_", colnames(GEDI2A_amp))]

# Plot the energy values for the selected row (excluding rx_0)
plot(as.numeric(row), type = "l", col = "#0073e6", lwd = 2,
     xlab = "Index (rx_1 to rx_100)", ylab = "Energy (rx_cum)", 
     main = "Energy Values from rx_1 to rx_100")

# Select the specific row (row 728 in this case)
row <- allGEDI2A_amp[44, ]

# Extract the amp and rh values (amp_0 to amp_100 and rh_0 to rh_100)
amp_values <- as.numeric(row[grep("^amp_", colnames(row))])
rh_values <- as.numeric(row[grep("^rh", colnames(row))])
rh_values <- rh_values[-1] 

# Plot the amplitude vs. relative height for the selected row
plot(amp_values, rh_values, type = "l", col = "#ca0020", lwd = 2,
     xlab = "Amplitude (amp_0 to amp_100)", ylab = "Relative Height (rh_0 to rh_100)",
     main = "Amplitude vs. Relative Height for Row 728")
     
     
     #ylim = c(-10, 50))  # Set y-axis range from -10 to 60






























