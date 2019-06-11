install.packages("rgdal")

library(rgdal)
library(raster)

#download world clim data
download.file("http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_5m_bio.zip","/Users/isabellaragazzi/brazil-coffee/coffee/data/tmp")
data <- unzip("/Users/isabellaragazzi/brazil-coffee/coffee/data/tmp", files = "wc2.0_bio_5m_01.tif")

bio_1 <- raster(data)
plot(bio_1)

# will be making this a function to grab every variable from the website
## get_climate_data <- function(x)