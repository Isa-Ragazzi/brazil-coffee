library(rgdal)
library(raster)
library(sp)
library(maps)
library(ggplot2)
library(GADMTools)

# Loading country border (level=0 [default])
# -----------------------------------------------------------------
map <- gadm_sf_loadCountries("BRA", level=0, basefile = "./")
#gadm_plot(map) + theme_light()

# Loading regions @ level = 1])
# -----------------------------------------------------------------
BRA <- gadm_sp_loadCountries(c("BRA"), level=2, basefile = "./")
gadm_plot(BRA)

# extracting one region at a time

bahia <- gadm_subset(BRA, level = 1, regions = "Bahia")
#gadm_plot(bahia) #%>% gadm_showNorth("tl") %>% gadm_showScale('bl')

## unsure of how to combine Brazil map data with world clim...

#download worldclim biofactors data
r <- getData("worldclim", var="bio", res=10)

#specifically downloading temp 
r <- r[[c(1)]]

#try to rename bio1 and bio12
#names(r) <- c("Temp","Precip","") # - does not work for me

# extract all points and values for all variables
points <- spsample(as(r@extent, 'SpatialPolygons'), n=100, type="regular")
values <- raster:: extract(r, points)

# bind into df
df <- cbind.data.frame(coordinates(bahia), values)
summary (df)
plot(BRA,values)



