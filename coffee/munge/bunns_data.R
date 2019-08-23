library(foreign)
# I think these are global locations, definitely not only Brazil
arabica <- read.csv("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/Arabica.csv")
robusta <- read.csv("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/Robusta_global.csv")

# this has the agricultural codes, UF(region), county, and locality of Bunn's locations -- they don't match the excel files
dbf.AglomeradosSubnormais2010_Limites <- read.dbf("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.dbf")
# shapefiles with the same info as dbf
shp.AglomeradosSubnormais2010_Limites <- readOGR("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.shp")
shx.AglomeradosSubnormais2010_Limites <- readOGR("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.shx")

proj4string(shp.AglomeradosSubnormais2010_Limites) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
plot(shp.AglomeradosSubnormais2010_Limites)
plot(shx.AglomeradosSubnormais2010_Limites)

# get coordinates of coffee farms
dd <- shp.AglomeradosSubnormais2010_Limites
proj4string(dd) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
coordinates <- as.data.frame(coordinates(dd))
names(coordinates)[1] <- "x"
names(coordinates)[2] <- "y"
plot(dd)

# for loop to assign regions to farm locations
output_test <- data_frame()

for(y in region_names) {
  y <- gadm_subset(BRA, level = 1, regions = y)
  temp_region_r <- crop(locations, y$spdf@bbox)
  region_points <- temp_region_r@coords
  temp_region_df <- as.data.frame(region_points)
  temp_region_df$region <- y$spdf$NAME_1
  temp <- left_join(temp_region_df, coordinates, by = c("x","y"))
  output_test <- rbind(output_test, temp)
  remove(temp_region_r, region_points, temp_region_df, temp)
}

bunn_output_summary <- output_test %>% group_by(region) %>% count()

#

# FUTURE OUTPUT DATFRAME MADE INTO MAP
#install.packages("devtools)
library(devtools)

## Loading required package: rspatial

d <- future_output
# plot annual average temp
plot(sort(d$bc85bi501), ylab='Avg Temp', las=1, xlab='Stations')

# quick map - coords go long:lat for SpatialPoints function x = long y = lat
library(sp)
dsp <- SpatialPoints(d[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
dsp <- SpatialPointsDataFrame(dsp, d)
sp::plot(dsp)


# all climate data for coffee farm locations
historic_bunn <- left_join(output_test, historic_sum_fixed, by = "region")
future_bunn <- left_join(output_test, future_sum_fixed, by = "region")
all_climate_bunn <- left_join(output_test, all_region_climate, by = "region")

# attempt to map coffee farm locations and avg temp for one year

zz <- all_climate_bunn %>% filter(year == "2050") %>% select("x","y","avgtmp")
plot(sort(zz$avgtmp), ylab='Avg Temp', las=1, xlab='Coordinates')

# quick map - coords go long:lat for SpatialPoints function x = long y = lat
library(sp)
zzsp <- SpatialPoints(zz[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
zzsp <- SpatialPointsDataFrame(zzsp, zz)
zzzsp <- as.data.frame(zzsp)
sp::plot(zzsp)
sp::plot(BRA$spdf) + geom_point(zzsp)

brasil <- map_data("world", region = "Brazil")
library(ggplot2)
ggplot() + 
  geom_polygon(data = brasil, aes(x = long, y = lat)) +
  coord_equal() +
  geom_point(data = zzzsp, aes(x = x, y = y)) 

