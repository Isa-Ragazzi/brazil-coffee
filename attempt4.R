library(rgdal)
library(raster)
library(sp)
library(maps)
library(ggplot2)
install.packages("rnaturalearth")
library(rnaturalearth)

# Loading country border (level=0 [default])
# -----------------------------------------------------------------
map <- sp::plot(ne_countries(country = 'brazil'))

brazil <- ne_countries(country = 'brazil')

brazil = raster(brazil)
# Loading regions @ level = 1])
# -----------------------------------------------------------------
sp::plot(ne_states(country = 'brazil')) 

brazil_states <- ne_states(country = 'brazil')

#download worldclim biofactors data
r <- getData("worldclim", var="bio", res=10)

#specifically downloading temp 
r <- r[[c(1)]]

#try to rename bio1 and bio12
#names(r) <- c("Temp","Precip","") # - does not work for me

# extract all points and values for all variables
points <- spsample(as(r@extent, 'SpatialPolygons'), n=100, type="regular")
values <- raster:: extract(r, points)

