---

# Analysis code for "Evaluating GEDI for quantifying forest condition across a gradient of degradation and recovery in Amazonian rainforests"

This repository contains R Markdown and a workflow script for downloading, pre-processing, analysing and visualising the data presented in Doyle et al. (2024).

**Contributors:** Emily L. Doyle, Hugh A. Graham, Chris A. Boulton, Timothy M. Lenton, Ted R. Feldpausch, Andrew M. Cunliffe

Publication:

Repository: DOI: 10.5281/zenodo.14103328

**To evaluate the use of GEDI data to inform forest condition along a gradient of forest degradation, this study aimed to:**

(A) Compare the relative accuracy of estimated height profiles and canopy cover derived from spaceborne lidar against comparable metrics derived from airborne lidar (ALS), across various forest conditions.

(B) Evaluate GEDI forest structure variation along a gradient of forest condition.

(C) Investigate the potential of principal component analysis to derive a continuous descriptor of forest condition that characterises the forest degradation continuum, using GEDI data.

## File structure

This repo contains the following structure:

-   R folder containing the entire workflow script (Workflow_publish.R) and functions.R required to run the code

-   Packages.R file to run softwares to run the project code

-   Input folder containing the polygonised study regions

-   Output folder containing the .fgb dataframes created throughout the workflow

## Workflow

![](images/Doyle%20et%20al%202024%20workflow-02.jpg)

## Data access

The Sustainable Landscape Project lidar is available from the Oak Ridge National Laboratory (ORNL) Distributed Active Archive Center (DAAC); <https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1644>

The following datasets were used to define the forest condition gradient and validate forest cover were:

1.  Amazonian Annual Secondary forest age from 1985 to 2018 (Silva Junior et al., 2020); <https://doi.org/10.1038/s41597-020-00600-4>

2.  MapBiomas Fire Frequency (version 3.0) downloaded from Google Earth Engine toolkit, Rio Cautario (2023) and DAAC sites (2018/2021); <https://earthengine.googlesource.com/users/mapbiomas/user-toolkit/+/59f6cf84a1c91fb9fe116c939910bb8453302e60/mapbiomas-user-toolkit-download-mosaics.js>

3.  MapBiomas Project - Collection 8 of annual series of maps of Land Cover and Use in Brazil (Souza et al., 2020), for Rio Cautario site validation (2019 - 2022); [https://plataforma.brasil.mapbiomas.org/](https://plataforma.brasil.mapbiomas.org/cobertura?activeBaseMap=9&layersOpacity=100&activeModule=coverage&activeModuleContent=coverage%3Acoverage_main&activeYear=2023&mapPosition=-15.114553%2C-51.459961%2C4&timelineLimitsRange=1985%2C2023&baseParams%5BgroupType%5D=geometry&baseParams%5BactiveClassTreeOptionValue%5D=default&baseParams%5BactiveClassTreeNodeIds%5D=1%2C7%2C8%2C9%2C10%2C11%2C2%2C12%2C13%2C14%2C15%2C16%2C3%2C18%2C19%2C28%2C30%2C31%2C32%2C33%2C34%2C29%2C35%2C36%2C37%2C38%2C20%2C21%2C4%2C22%2C23%2C24%2C25%2C5%2C26%2C27%2C6&baseParams%5BactiveSubmodule%5D=coverage_main&baseParams%5ByearRange%5D=1985-2023)

## References

Silva Junior, Celso H. L., Viola H. A. Heinrich, Ana T. G. Freire, Igor S. Broggio, Thais M. Rosan, Juan Doblas, Liana O. Anderson, et al. 2020. ‘Benchmark Maps of 33 Years of Secondary Forest Age for Brazil’. *Scientific Data* 7 (1): 269. <https://doi.org/10.1038/s41597-020-00600-4.>

Souza, Carlos M., Julia Z. Shimbo, Marcos R. Rosa, Leandro L. Parente, Ane A. Alencar, Bernardo F. T. Rudorff, Heinrich Hasenack, et al. 2020. ‘Reconstructing Three Decades of Land Use and Land Cover Changes in Brazilian Biomes with Landsat Archive and Earth Engine’. *Remote Sensing* 12 (17): 2735. <https://doi.org/10.3390/rs12172735.>
