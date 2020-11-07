# Maps

Maps describing the study population.

* Responsible: Mercè

## Scripts

* `epi_maps.R` creates maps describing the study population
* `epi_map_MC_Estimation.R` creates maps for the Monte Carlo estimates

The scripts expect the KoCo data to be in the standard locations, and in addition
depend on geospatial data in the `maps_data` folder.

Output is written to `maps_output`.

## Dependencies

On Ubuntu, install the following via `apt-get`. On other systems, they may be
named differently. Installing the below R packages can give informative error
messages on what is required.

* libgdal-dev
* libudunits2-dev
* libprotobuf-dev
* libqj-dev

Further, the following R packages are required, installable via
`install.packages(...)`.

* ggmap
* ggplot2
* dplyr
* tidyr
* rgdal
* spdep
* stringr
* sf
* sp
* colorspace
* cowplot
* geojson
* maps
