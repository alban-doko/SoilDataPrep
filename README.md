# SoilDataPrep

Prepare soil input (e.g. for meso-scale hydrological modelling with WASA), using the global datasets SoilGrids and Pelletier et al. (2016). Pedotransfer functions from other packages ```(ptf.rawls\{soilwaterptf\}, euptf\{euptf\})``` are applied to calculate soil characteristics.

Pelletier, J.D. et al. 2016, A gridded global data set of soil, immobile regolith, and sedimentary deposit thicknesses for regional and global land surface modeling, Journal of Advances in Modeling Earth Systems, 8.

## Installation
```

#install these libraries from CRAN, if you don't have them
library(sp)
library(raster)
library(rgdal)
library(rpart)
library(gWidgets)
library(gWidgetstcltk)
library(curl)
library(data.table)
library(panelaggregation)


#install euptf
  curl::curl_download(url = "https://esdac.jrc.ec.europa.eu/public_path/shared_folder/themes/euptf.zip", destfile = "euptf.zip")
  unzip(zipfile = "euptf.zip")
  untar("euptf_1.4.tar.gz")
  system("R CMD INSTALL euptf") #install from source
  unlink(c("euptf.zip", "euptf_vignette_1.4.pdf","euptf_1.4.tar.gz","euptf"), recursive = TRUE, force = TRUE) #clean up
  library(euptf)

library(devtools)
#install soiltexture
  install_github(repo = "julienmoeys/soiltexture/pkg/soiltexture") #install soiltexture package

#install soilwaterfun  
  install_github(repo = "rforge/soilwater/pkg/soilwaterfun") 

#install soilwaterfun package
  #install_github(repo = "rforge/soilwater/pkg/soilwaterptf") 
  install_github(repo = "tillf/soilwater/pkg/soilwaterptf") #use different fork, as the original has not been updated yet

library(soiltexture)
library(soilwaterfun)
library(soilwaterptf)

#install soilwaterfun package
  install_github(repo = "sophiaup/SoilDataPrep/SoilDataPrep")
  
library(SoilDataPrep)

```

## Input
DEM and shapefile of the catchment

