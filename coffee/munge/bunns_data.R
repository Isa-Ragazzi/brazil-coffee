library(foreign)
# I think these are global locations, definitely not only Brazil
arabica <- read.csv("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/Arabica.csv")
robusta <- read.csv("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/Robusta_global.csv")

# this has the agricultural codes, UF(region), county, and locality of Bunn's locations -- they don't match the excel files
dbf.AglomeradosSubnormais2010_Limites <- read.dbf("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.dbf")
# shapefiles with the same info as dbf
shp.AglomeradosSubnormais2010_Limites <- readOGR("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.shp")
shx.AglomeradosSubnormais2010_Limites <- readOGR("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.shx")

plot(shp.AglomeradosSubnormais2010_Limites)
plot(shx.AglomeradosSubnormais2010_Limites)

test4 <-  spTransform(shx.AglomeradosSubnormais2010_Limites, proj4string(HG_model))
test4 <- raster(test4)

# form dataframe and assign regions 
bunn_output <- data_frame()
for(x in region_names) {
  x <- gadm_subset(BRA, level = 1, regions = x)
  region_r <- crop(test4, x$spdf@bbox)
  region_points <- spsample(as(test4@extent, 'SpatialPolygons'), n=1000, type="random")
  region_df <- cbind.data.frame(coordinates(region_points))
  region_df$region <- x$spdf$NAME_1
  bunn_output <- rbind(bunn_output, region_df)
}

bunn_output_summary <- bunn_output %>% group_by(region) %>% count()
# so there's an equal ## for each region which makes me think the locations are not all separate coffee farms? 
# idk we'll see, see if you can figure it out and otherwise I'll email him haha

historic_coffee <- left_join(bunn_output, summary, by = "region")
future_coffee <- left_join(bunn_output, future_summary, by = "region")
future_coffee = future_coffee[,c(-4,-5)]
names(future_coffee)[names(future_coffee)=='x.x']<-"x"
names(future_coffee)[names(future_coffee)=='y.x']<-"y"

###############################################3
# attempting to plot his data
library(maps)
library(mapdata)
library(ggplot2)
library(rgdal)
library(sp)
library(raster)
library(tidyverse)
library(dplyr)
library(data.table)
library(purrr)
library(geonames)

df <- read.csv("C:/Users/fishaikh/Documents/7_SSIRF/brazil-coffee/coffee/data/Area Harvested with Code_100.csv")
df <- data.frame(df)
head(df)

options(geonamesUsername = "fizzys")
places <- c(df$Municipality)

get_coords <- function(name, country, fcode){
    res <- GNsearch(name_equals = name, country=country, fcode=fcode)
    out <- data.frame(name = res$toponymName, lat=res$lat, lon= res$lng)
    return(out)
}

GNresult <- places %>%
    map(get_coords, country = "BR", fcode = "ADM2") %>%
    rbindlist()
print(GNresult)

df <- mutate(df, Latitude = GNresult$lat)
df <- mutate(df, Longitude = GNresult$lon)

bra2 <- getData('GADM', country='BRA', level=2)
plot(bra2, border='gray', col='light gray')

mapPoints <- bra2 + geom_point(aes(x = lon, y = lat, size = "Average '06-'10", data = df, alpha = 0.5))