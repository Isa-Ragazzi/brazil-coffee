library(rgdal)
library(raster)
library(sp)
library(maps)
library(ggplot2)
library(GADMTools)

# Loading country border (level=0 [default])
# -----------------------------------------------------------------
map <- gadm_sp_loadCountries("BRA", level=0, basefile = "./")
#gadm_plot(map) + theme_light()

# Loading regions @ level = 1])
# -----------------------------------------------------------------
BRA <- gadm_sp_loadCountries(c("BRA"), level=1, basefile = "./")
gadm_plot(BRA)

# extracting one region at a time

bahia <- gadm_subset(BRA, level = 1, regions = "Bahia")
mg <- gadm_subset(BRA, level = 1, regions = "Minas Gerais")

#gadm_plot(bahia) #%>% gadm_showNorth("tl") %>% gadm_showScale('bl')

## unsure of how to combine Brazil map data with world clim...

#download worldclim biofactors data
r <- getData("worldclim",var="bio", res=10)
# Don't forget that WorldClim data has a scale factor of 10, so Temp = -37 is -3.7 ºC.

#specifically downloading temp 
# r <- r[[c(1)]] removed this to make df that has all variables - see line 58

## extract Brazil coordinates from r
brazil_r <- crop(r, bbox(BRA$spdf@bbox))
plot(brazil_r, "bio1")

## extract bahia region coordinates from r
bahia_r <- crop(r, bahia$spdf@bbox)
bio1_bahia <- plot(bahia_r, "bio1")

###### extract all points and values for all variables in Brazil ######
points <- spsample(as(brazil_r@extent, 'SpatialPolygons'), n=100, type="random")
values <- raster:: extract(brazil_r, points)

###### bind into df ######
df <- cbind.data.frame(coordinates(points), values)
plot(df)
summary (df)

###### extract points and values for bahia ####
region_points <- spsample(as(bahia_r@extent, 'SpatialPolygons'), n=100, type="random")
region_values <- raster:: extract(bahia_r, region_points) 

##### bind into df ####
region_df <- cbind.data.frame(coordinates(region_points), region_values)
region_df$region <- "bahia"
plot(region_df)
summary (region_df)

######## extract data by region ########
region_names <- listNames(BRA, level = 1)

# assign regions
output <- data_frame()

for(x in region_names) {
    x <- gadm_subset(BRA, level = 1, regions = x)
    # print(x$spdf$NAME_1)
    region_r <- crop(r, x$spdf@bbox)
    region_points <- spsample(as(region_r@extent, 'SpatialPolygons'), n=1000, type="random")
    region_values <- raster:: extract(region_r, region_points) 
    region_df <- cbind.data.frame(coordinates(region_points), region_values)
    region_df$region <- x$spdf$NAME_1
    output <- rbind(output, region_df)
}
