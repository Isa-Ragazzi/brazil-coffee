## import climate scenarios
## remember that temperature data is reported in C*10

HG_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=50)
BC_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=50)

# bio 1 variable comparison 
plot(HG_model, 1)
plot(r, 1)

# locations
brazil_hg <- crop(HG_model, bbox(BRA$spdf@bbox))

## creating a dataframe
points <- spsample(as(brazil_hg@extent, 'SpatialPolygons'), n=1000, type="random")
values <- raster:: extract(HG_model, points)

# bind into df
df <- cbind.data.frame(coordinates(points), values)

##
bahia_tmp <- crop(HG_model, bahia$spdf@bbox)
plot(bahia_tmp)
# assign regions to locations
future_output <- data_frame()

for(y in region_names) {
  y <- gadm_subset(BRA, level = 1, regions = y)
  # print(x$spdf$NAME_1)
  temp_region_r <- crop(HG_model, y$spdf@bbox)
  region_points <- spsample(as(temp_region_r@extent, 'SpatialPolygons'), n=100, type="random")
  region_values <- raster:: extract(temp_region_r, region_points) 
  temp_region_df <- cbind.data.frame(coordinates(region_points), region_values)
  temp_region_df$region <- y$spdf$NAME_1
  future_output <- rbind(future_output, temp_region_df)
  remove(temp_region_r, region_points, temp_region_df)
}

# summarise by region
future_summary <- future_output %>% group_by(region) %>% summarise_all(list(mean), na.rm = TRUE)

