## import climate scenarios

HG_model <- getData('CMIP5', var='tmax', res=10, rcp=85, model='HG', year=50)
BC_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=50)

plot(AC_model)

points <- spsample(as(HG_model@extent, 'SpatialPolygons'), n=100, type="random")
values <- raster:: extract(HG_model, points)

brazil_hg <- crop(points, bbox(BRA$spdf))
plot(brazil_hg)
# bind into df
df <- cbind.data.frame(coordinates(points), values)
