library(rgdal)
library(raster)
library(sp)

#r = raster("tmin01")
r <- getData("worldclim", var="bio", res=10)

r <- r[[c(1,12)]]

# names(r) <- c("Temp","Precip","") - does not work for me

points <- spsample(as(r@extent, 'SpatialPolygons'), n=100, type="random")

values <- raster:: extract(r, points)

df <- cbind.data.frame(coordinates(points), values)

<<<<<<< HEAD
head (df)

plot(r[[1]])
plot(points, add=T)
=======
summary (df)
>>>>>>> 89542ca07f7a780e0eef557bf0154e5618ba4ae4
