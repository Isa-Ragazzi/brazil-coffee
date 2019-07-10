library(raster)
#load in some raster shtacks
t.mean.files <- list.files("../worldclim/wc2.0_30s_tavg/", ".tif", full.names=TRUE)
t.mean <- stack(t.mean.files)
 