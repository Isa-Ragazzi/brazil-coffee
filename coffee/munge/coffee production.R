data <- read.csv("/Users/isabellaragazzi/Desktop/New folder/Arabica.csv")
data <- data[,c(1,3,2)]
brazil_production <- crop(r, data[,c(3,2)])

# load Scotland shapefile, which came with the package
dsn <- system.file("bahia_r", package = "rgdal")[1]
shapefile <- readOGR(dsn=dsn, layer = 1)
shapefile <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) #change CRS

# create dummy data frame with coordinates in Scotland (I clicked randomly on Google Maps)
csvfile <- data

# convert data frame to SpatialPoints class
coordinates(csvfile) <- ~lat+lon

# make sure the two files share the same CRS
csvfile@proj4string <- shapefile@proj4string

# visual check
bio1_bahia <- plot(r, "bio1") 
test <- raster(csvfile)
## idk what Im doing

plot(combo)
axis(1) # showing the axes helps to check whether the coordinates are what you expected
axis(2)
