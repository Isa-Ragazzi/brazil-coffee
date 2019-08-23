library(rgdal)
library(RColorBrewer)

# http://www.fao.org/geonetwork/srv/en/metadata.show?id=14057
dpath<- "/Users/isabellaragazzi/brazil-coffee/coffee/data/Map4_2/lgp/w001001.adf"

x <- new("GDALReadOnlyDataset", dpath)
getDriver(x)
getDriverLongName(getDriver(x))
xx<-asSGDF_GROD(x)
r <- raster(xx)

# GLC 2000
# https://forobs.jrc.ec.europa.eu/products/glc2000/products.php
dpath<- "/Users/isabellaragazzi/brazil-coffee/coffee/data/Grid/sam2000mon6/w001001.adf"

x <- new("GDALReadOnlyDataset", dpath)
getDriver(x)
getDriverLongName(getDriver(x))
xx<-asSGDF_GROD(x)
r <- raster(xx)
plot(r)

# limit to Brazil
library(GADMTools)

# Loading regions @ level = 1])
# -----------------------------------------------------------------
BRA <- gadm_sp_loadCountries(c("BRA"), level=0, basefile = "./")
gadm_plot(BRA)
coordinates <- spsample(as(extent(BRA$spdf), 'SpatialPolygons'), n=1000, type="random")
coordinates1 <- raster:: extract(BRA$spdf, points)
df1 <- cbind.data.frame(coordinates(coordinates), coordinates1)


# 
brazil_extract <- crop(r, bbox(BRA$spdf@bbox))
brazil_extract <- crop(r, extent(BRA$spdf))
# 
## plot(brazil_extract) + geom_point(data = df1, aes(x = x, y = y, fill = NULL, colour = "black"))
# 
slot <- 1:815
coordinates <- data_frame()
for(x in slot) {
  coords <- coordinates(BRA$spdf@polygons[[x]]@Polygons[[x]])
  print(x)
  coordinates <- rbind(coordinates, coords)
}
coords <- coordinates(BRA$spdf@polygons[[2]])
