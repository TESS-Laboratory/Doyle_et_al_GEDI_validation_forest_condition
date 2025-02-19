# Script with all necassary packages for the project - Emily Doyle Jan 2024
#install.packages("purrr")
#pak::pkg_install("Permian-Global-Research/chewie")

library(waveformlidar)
library(data.table)


# Data reading
#library(xlread)
library(vroom)
library(stringr)
library(pak)

# Data wrangling
library(dplyr)
library(tidyr)
library(tidyverse)
library(broom)
library(purrr)

# Processing/ modelling
library(lidR)
library(chewie)
library(ca)
library(waveformlidar)
library(nnet)

# Spatial data
library(sf)
library(raster)
library(terra)

# Stats and tables
library(DescTools)
library(gtsummary)

# Visualisation
library(mapview)
library(ggplot2)
library(ggeffects)
library(MASS)
library(viridis)
library(plotly)
library(leafgl)
library(patchwork)
library(grid)
library(reshape2)
#check
library(performance)
library(kableExtra)
library(scales)





# Communications
library(cli)


# Pipeline work 
library(targets)


# NOT SURE WHERE THIS ONE GOES
library(lmomco)



# Install and load the ggrepel package if not already done
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel")
}
library(ggrepel)

