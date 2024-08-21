# waveformlidar summaries that did not work on 2A data but likely would on 1B waveform data



GEDI2AB_trans <- as.data.table(GEDI2AB_trans)

# Separate the shot_number from the waveform data
shot_numbers <- GEDI2AB_trans$shot_number
waveform_data <- GEDI2AB_trans[, -1, with = FALSE]  # Removing the shot_number column

# Add an index column at the beginning
waveform_data[, index := .I]

# Ensure the index is the first column
setcolorder(waveform_data, c("index", setdiff(names(waveform_data), "index")))

# Functions to apply

# Apply the decom function to each waveform (each row)
# A function to safely apply `decom`
# Helper function for fslope
safe_fslope <- function(row) {
  result <- tryCatch({
    fslope(as.numeric(row[-1]), smooth = TRUE, thres = 0.22, width = 5, tr = 1)
  }, error = function(e) return(list(FS = NA, ROUGH = NA)))
  
  if (is.null(result)) {
    return(list(FS = NA, ROUGH = NA))
  } else {
    return(result)
  }
}

# Helper function for integral
safe_integral <- function(row) {
  result <- tryCatch({
    integral(as.numeric(row[-1]), smooth = TRUE, rescale = TRUE, thres = 0.2, width = 3, tr = 1, dis = 20)
  }, error = function(e) return(list(ground_integral = NA, veg_integral = NA, total_integral = NA, veg_to_total = NA)))
  
  if (is.null(result)) {
    return(list(ground_integral = NA, veg_integral = NA, total_integral = NA, veg_to_total = NA))
  } else {
    return(result)
  }
}

# Helper function for lpeak
safe_lpeak <- function(row) {
  result <- tryCatch({
    peaks <- lpeak(as.numeric(row[-1]), span = 3)
    return(list(peaks = peaks))
  }, error = function(e) return(list(peaks = NA)))
  
  if (is.null(result)) {
    return(list(peaks = NA))
  } else {
    return(result)
  }
}

# Helper function for maxamp
safe_maxamp <- function(row) {
  result <- tryCatch({
    max_amp <- maxamp(as.numeric(row[-1]), smooth = TRUE, thres = 0.2, width = 3)
    return(list(max_amp = max_amp))
  }, error = function(e) return(list(max_amp = NA)))
  
  if (is.null(result)) {
    return(list(max_amp = NA))
  } else {
    return(result)
  }
}

# Helper function for npeaks
safe_npeaks <- function(row) {
  result <- tryCatch({
    n_peaks <- npeaks(as.numeric(row[-1]), drop = c(0, 0), smooth = TRUE, threshold = 0.2)
    return(list(n_peaks = n_peaks))
  }, error = function(e) return(list(n_peaks = NA)))
  
  if (is.null(result)) {
    return(list(n_peaks = NA))
  } else {
    return(result)
  }
}


# Apply fslope
fslope_results <- lapply(1:nrow(waveform_data), function(i) safe_fslope(waveform_data[i, ]))
fslope_dt <- rbindlist(fslope_results, fill = TRUE)

# Apply integral
integral_results <- apply(waveform_data, 1, safe_integral)
integral_dt <- rbindlist(integral_results, fill = TRUE)

# Apply lpeak
lpeak_results <- apply(waveform_data, 1, safe_lpeak)
lpeak_dt <- rbindlist(lpeak_results, fill = TRUE)

# Apply maxamp
maxamp_results <- apply(waveform_data, 1, safe_maxamp)
maxamp_dt <- rbindlist(maxamp_results, fill = TRUE)

# Apply npeaks
npeaks_results <- apply(waveform_data, 1, safe_npeaks)
npeaks_dt <- rbindlist(npeaks_results, fill = TRUE)

