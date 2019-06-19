## import climate scenarios

HG_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='CN', year=50)
BC_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=50)

# bio 1 variable comparison
plot(HG_model, 1)
plot(r, 1)
# plot HG model layer 3 for Brazil
brazil_hg <- crop(HG_model, bbox(BRA$spdf))
plot(brazil_hg, 3)

## creating a dataframe
points <- spsample(as(brazil_hg@extent, 'SpatialPolygons'), n=234, type="random")
values <- raster:: extract(HG_model, points)

# bind into df
df <- cbind.data.frame(coordinates(points), values)
