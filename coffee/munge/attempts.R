install.packages("rgdal")

library(rgdal)
library(raster)

#download world clim data
download.file("http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_5m_bio.zip","~/Github/brazil-coffee/coffee/data/tmp")
data <- unzip("~/Github/brazil-coffee/coffee/data/tmp", files = "wc2.0_bio_5m_01.tif")
data2 <- unzip("~/Github/brazil-coffee/coffee/data/tmp", files = "wc2.0_bio_5m_02.tif")
bio_1 <- raster(data)
bio_2 <- raster(data2)
plot(bio_1)
plot(bio_2)


# checking the data
proj4string(bio_1)
str(bio_1@data)
# will be making this a function to grab every variable from the website
## get_climate_data <- function(x)
summary(bio_1)
summary(bio_2)
