------------------------------------------------------------------------

# Analysis code for "Evaluating GEDI for quantifying forest structure across a gradient of degradation in Amazonian rainforests"

This repository contains R Markdown and a workflow script for downloading, pre-processing, analysing and visualising the data presented in Doyle et al. (In Review).

**Contributors:** Emily L. Doyle, Hugh A. Graham, Chris A. Boulton, Timothy M. Lenton, Ted R. Feldpausch, Andrew M. Cunliffe

Publication: (in review at Environmental Research Letters)

Repository: DOI: 10.5281/zenodo.14103328

**To advance the use of GEDI data to quantify forest structural state along a gradient of forest degradation, this study aimed to:**

(A) Compare the relative accuracy of estimated height profiles and canopy cover derived from spaceborne lidar against comparable metrics derived from airborne lidar across a gradient of forest degradation.

(B) Evaluate GEDI forest structure variation along a gradient of forest degradtion.

(C) Investigate

    the potential of principal component analysis applied to GEDI data to derive a continuous descriptor of forest structural state that characterises the forest degradation continuum

## File structure

This repo contains the following structure:

-   R folder containing the entire workflow script (Workflow_publish.R) and functions.R required to run the code

-   Packages.R file to run softwares to run the project code

-   GEE_script.R file with scripts to download MapBiomas data from Google Earth Engine

-   Input folder containing the polygonised study regions

-   Output folder containing the .fgb dataframes created throughout the workflow to reduce computation time

## Workflow

![Methodological flowchart for the analytical pipeline in this study](images/Doyle%20et%20al%202025%20workflow(Figure2).pdf)

## Data access

The Sustainable Landscape Project lidar is available from the Oak Ridge National Laboratory (ORNL) Distributed Active Archive Center (DAAC); <https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1644>

The following datasets were used to define the forest condition gradient and validate forest cover were:

1.  MapBiomas Secondary Forest (Collection 9.0) (MapBiomas Project, 2024) downloaded from Google Earth Engine toolkit, Rio Cautario (2023) and DAAC sites (2018/2021); [https://code.earthengine.google.com/](https://code.earthengine.google.com/))

2.  MapBiomas Fire Frequency (Fire Collection 3.0) downloaded from Google Earth Engine toolkit, Rio Cautario (2023) and DAAC sites (2018/2021); [https://code.earthengine.google.com/](https://code.earthengine.google.com/))

3.  MapBiomas Time since the last Fire (Fire Collection 3.0) downloaded from Google Earth Engine toolkit, Rio Cautario (2023) and DAAC sites (2018/2021); [https://code.earthengine.google.com/](https://code.earthengine.google.com/))

4.  MapBiomas Project - Collection 9 of annual series of maps of Land Cover and Use in Brazil (Souza et al., 2020), for forest validation; [https://code.earthengine.google.com/](https://code.earthengine.google.com/))

All datasets visible in MapBiomas platform prior to download using Google Earth Engine - code provided in GEE_Scripts. R file; [https://plataforma.brasil.mapbiomas.org/](https://plataforma.brasil.mapbiomas.org/cobertura?activeBaseMap=9&layersOpacity=100&activeModule=coverage&activeModuleContent=coverage%3Acoverage_main&activeYear=2023&mapPosition=-15.114553%2C-51.459961%2C4&timelineLimitsRange=1985%2C2023&baseParams%5BgroupType%5D=geometry&baseParams%5BactiveClassTreeOptionValue%5D=default&baseParams%5BactiveClassTreeNodeIds%5D=1%2C7%2C8%2C9%2C10%2C11%2C2%2C12%2C13%2C14%2C15%2C16%2C3%2C18%2C19%2C28%2C30%2C31%2C32%2C33%2C34%2C29%2C35%2C36%2C37%2C38%2C20%2C21%2C4%2C22%2C23%2C24%2C25%2C5%2C26%2C27%2C6&baseParams%5BactiveSubmodule%5D=coverage_main&baseParams%5ByearRange%5D=1985-2023)

## References

MapBiomas Project. 2024. ‘Collection 9 of the Annual Land Cover and Land Use Maps of Brazil (1985- 2023)’. MapBiomas Data. <https://doi.org/10.58053/MAPBIOMAS/XXUKA8.>

Souza, Carlos M., Julia Z. Shimbo, Marcos R. Rosa, Leandro L. Parente, Ane A. Alencar, Bernardo F. T. Rudorff, Heinrich Hasenack, et al. 2020. ‘Reconstructing Three Decades of Land Use and Land Cover Changes in Brazilian Biomes with Landsat Archive and Earth Engine’. *Remote Sensing* 12 (17): 2735. <https://doi.org/10.3390/rs12172735.>
