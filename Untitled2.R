# ------ Extracting metrics and canopy cover from ALS within GEDI footprints ------


# THIS NEEDS WORK, IT IS GOING TO HAVE TO BE DONE MANUALLY FOR EACH CRS

# Load the ALS catalog of the final processed .laz files

final_ALS <- readLAScatalog('/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/final_ALS_Chap1')


# Sort of works! Metric extraction (doesn't work with above catalog processing function)
# Currently using both allGEDI2A and allGEDI as 2A has height and regressions 
# and allGEDI has canopy and carbon

als_metrics <- plot_metrics(fin_catalog, .stdmetrics_z, allGEDI2A_gradient, radius = 12.5)


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




# TEMPORARILY MERGING CATALOGS (600ish points) these dataframe were made on server individually

allGEDI2A_19S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/allGEDI2A_19S_metrics.fgb")
allGEDI2A_20S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/allGEDI2A_20S_metrics.fgb")
allGEDI2A_21S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/allGEDI2A_21S_metrics.fgb")
allGEDI2A_CAUT20S_metrics <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/allGEDI2A_CAUT20S_metrics.fgb")

allGEDI2A_19S_metrics <- st_transform(allGEDI2A_19S_metrics, "EPSG:32643")
allGEDI2A_20S_metrics <- st_transform(allGEDI2A_20S_metrics, "EPSG:32643")
allGEDI2A_21S_metrics <- st_transform(allGEDI2A_21S_metrics, "EPSG:32643")
allGEDI2A_CAUT20S_metrics <- st_transform(allGEDI2A_CAUT20S_metrics, "EPSG:32643")

st_crs(allGEDI2A_19S_metrics)

# Remove duplicated rows
allGEDI2A_19S_metrics <- distinct(allGEDI2A_19S_metrics)
allGEDI2A_20S_metrics <- distinct(allGEDI2A_20S_metrics)
allGEDI2A_21S_metrics <- distinct(allGEDI2A_21S_metrics)
allGEDI2A_CAUT20S_metrics <- distinct(allGEDI2A_CAUT20S_metrics)

# Example: Standardizing column names
# Assuming columns are in the same order and just have different names
colnames(allGEDI2A_20S_metrics) <- colnames(allGEDI2A_19S_metrics)
colnames(allGEDI2A_21S_metrics) <- colnames(allGEDI2A_19S_metrics)
colnames(allGEDI2A_CAUT20S_metrics) <- colnames(allGEDI2A_19S_metrics)

# Combine the dataframes into one dataframe using bind_rows
merged_df <- bind_rows(allGEDI2A_19S_metrics, 
                       allGEDI2A_20S_metrics,
                       allGEDI2A_21S_metrics,
                       allGEDI2A_CAUT20S_metrics)

# Remove rows with NAs in the 'year' column
allheight <- merged_df %>%
  filter(!is.na(zq95))

sf::st_write(allheight, "/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allheight_draft.fgb", delete_dsn = TRUE, overwrite = TRUE)
allheight <- read_sf("/Users/emilydoyle/Documents/workspace_data/Doyle_et_al_GEDI_validation_forest_condition_data/Output_data/allheight_draft.fgb")





#' Standard LiDAR metrics
#'
#' This function computes a set of metrics from LiDAR point clouds. For metric
#' definitions and references, see Table 1 of Mahoney et al (2022).
#'
#' @param Z Return height for each point
#' @param ReturnNumber Return number of each point
#' @param min,max The minimum and maximum valid Z value. Returns outside of
#' this range will be discarded before metric computation.
#'
#' @references
#' Michael J Mahoney, Lucas K Johnson, Eddie Bevilacqua & Colin M Beier
#' (2022) Filtering ground noise from LiDAR returns produces inferior models of
#' forest aboveground biomass in heterogenous landscapes, GIScience & Remote
#' Sensing, 59:1, 1266-1280, DOI: 10.1080/15481603.2022.2103069
#'
#' @return A `data.frame` with 40 columns, containing various LiDAR metrics.
#'
#' @examples
#' lidar_preds(rnorm(1000, 3), sample(1:3, 1000, TRUE, c(0.7, 0.2, 0.1)))
#'
#' @export
lidar_preds <- function(Z, ReturnNumber, min = 0, max = Inf) {
  density_pred_template <- lapply(
    paste0("d", seq(0.1, 0.9, 0.1) * 100),
    \(x) stats::setNames(data.frame(NA_real_), x)
  ) |>
    do.call(what = cbind)
  
  height_pred_template <- lapply(
    paste0("h", c(seq(0.1, 0.9, 0.1), 0.95, 0.99) * 100),
    \(x) stats::setNames(data.frame(NA_real_), x)
  ) |>
    do.call(what = cbind)
  
  out <- cbind(
    n = NA_integer_,
    zmean = NA_real_,
    max = NA_real_,
    min = NA_real_,
    quad_mean = NA_real_,
    cv = NA_real_,
    z_kurt = NA_real_,
    z_skew = NA_real_,
    L2 = NA_real_,
    L3 = NA_real_,
    L4 = NA_real_,
    L_cv = NA_real_,
    L_skew = NA_real_,
    L_kurt = NA_real_,
    height_pred_template,
    density_pred_template,
    cancov = NA_real_,
    quad_mean_c = NA_real_,
    zmean_c = NA_real_,
    cv_c = NA_real_,
    hvol = NA_real_,
    rpc1 = NA_real_
  )
  
  include <- Z >= min & Z <= max
  
  if (sum(include) < 10) {
    return(out)
  }
  
  Z <- Z[include]
  ReturnNumber <- ReturnNumber[include]
  
  out[["n"]] <- length(Z)
  out[["zmean"]] <- mean(Z)
  out[["max"]] <- max(Z)
  out[["min"]] <- min(Z)
  out[["quad_mean"]] <- sqrt(mean(Z^2))
  out[["cv"]] <- stats::sd(Z) / mean(Z)
  out[["z_kurt"]] <- e1071::kurtosis(Z, type = 2)
  out[["z_skew"]] <- e1071::skewness(Z, type = 2)
  l_moments <- l_moment_preds(Z)
  out[names(l_moments)] <- l_moments
  out[names(height_pred_template)] <- height_preds(Z)
  out[names(density_pred_template)] <- density_preds(Z)
  out[["cancov"]] <- mean(Z > 2)
  
  if (sum(Z > 2.5) > 1) {
    out[["quad_mean_c"]] <- sqrt(mean((Z[Z > 2.5])^2))
    out[["zmean_c"]] <- mean(Z[Z > 2.5])
    out[["cv_c"]] <- stats::sd(Z[Z > 2.5]) / out$zmean_c
  } else {
    out[["quad_mean_c"]] <- 0
    out[["zmean_c"]] <- 0
    out[["cv_c"]] <- 0
  }
  
  out$hvol <- out$cancov * out$zmean
  out$rpc1 <- mean(ReturnNumber == 1)
  out
}

#' Relative height metrics function
#' For use with lidR::plot_metrics or lidR::pixel_metrics functions. Assumes
#' the point cloud has been normalised to ground level.
#' @param Z Return height for each point
#' @param min,max The minimum and maximum valid Z value. Returns outside of
#' this range will be discarded before metric computation.
#' @return A `data.frame` with 100 columns, containing relative height metrics
#' at 1 percentile intervals.
#' @export
rh_preds <- function(Z, min = 0, max = Inf) {
  out <- lapply(
    paste0("rh", seq(0.01, 1, 0.01) * 100),
    \(x) stats::setNames(data.frame(NA_real_), x)
  ) |>
    do.call(what = cbind)
  include <- Z >= min & Z <= max
  
  if (sum(include) < 10) {
    return(out)
  }
  
  Z <- Z[include]
  Z <- Z[include]
  out[names(out)] <- height_preds(Z, percentiles = seq(0.01, 1, 0.01))
  return(out)
}


