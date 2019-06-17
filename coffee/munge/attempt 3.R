library(rgdal)
library(raster)
library(sp)
library(maps)
library(ggplot2)
library(GADMTools)

# Loading country border (level=0 [default])
# -----------------------------------------------------------------
map <- gadm_sf_loadCountries("BRA", basefile = "./")
gadm_plot(map) + theme_light()

# Loading regions @ level = 1])
# -----------------------------------------------------------------
BRA <- gadm_sp_loadCountries(c("BRA"), level=1, basefile = "./")
gadm_plot(BRA)

# extracting one region at a time

bahia <- gadm_subset(BRA, level = 1, regions = "Bahia")
gadm_plot(bahia) %>% gadm_showNorth("tl") %>% gadm_showScale('bl')

## unsure of how to combine Brazil map data with world clim...

#download worldclim biofactors data
r <- getData("worldclim", var="bio", res=10)

#specifically downloading temp and precipitation
r <- r[[c(1:12)]]

#try to rename bio1 and bio12
#names(r) <- c("Temp","Precip","") # - does not work for me

# extract all points and values for all variables
#points <- spsample(as(r@extent, 'SpatialPolygons'), n=100, type="random")
values <- raster:: extract(r, points)

# bind into df
df <- cbind.data.frame(coordinates(bahia), values)
summary (df)
ggplot2(df)




# first goal is to extract Brazil and group data by administrative region
# figure out how to apply this to all bioclimatic variables, such that we 
# have all 12 variables for each coordinate in the country
# then group coordinates into regions
# run MLR on the data by each region, using current production area and yield to
# predict future yield (need ipcc projections which are in an r package somewhere)
# run Machine Learning model on the same data to predict future yield
# compare error terms of each to determine which is the best model
# transform dataframe back to map for visualization purposes
# calculate approx cost to brazil's economy given the % of GDP allocated to 
# coffee production and suggestions for what this means for coffee prices globally

# f <- r[[1]]
# g <- r[[2]]
# 
# f_points <- spsample(as(f@extent, 'SpatialPolygons'), n=100, type="random")
# g_points <- spsample(as(g@extent, 'SpatialPolygons'), n=100, type="random")
# 
# f_values <- raster:: extract(f, f_points)
# g_values <- raster:: extract(g, g_points)
# 
# rf2<- raster(coordinates(f_points), f_values)
# rf3 <- raster(coordinates(g_points), g_values)
# 
# rf4 <- stack(coordinates(points), points)




