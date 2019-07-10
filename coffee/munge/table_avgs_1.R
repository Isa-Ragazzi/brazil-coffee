library(tmap)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(brazilmaps)

## import climate scenarios
HG_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='CN', year=50)
BC_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=50)

##map brazil
bra <- get_brmap(geo ="Region",
          geo.filter = NULL,
          class = "sf")
tm_shape(bra)+
  tm_fill()+
  tm_borders()