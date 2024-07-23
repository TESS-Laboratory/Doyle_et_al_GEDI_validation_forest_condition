# Script with all necassary packages for the project - Emily Doyle Jan 2024
#install.packages("purrr")
#pak::pkg_install("Permian-Global-Research/chewie")

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


# Processing
library(lidR)
library(chewie)
library(ca)

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
library(plotly)
install.packages('leafgl')

# Communications
library(cli)


# Pipeline work 
library(targets)


# NOT SURE WHERE THIS ONE GOES
library(lmomco)
