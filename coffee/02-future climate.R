## import climate scenarios
## remember that temperature data is reported in C*10

HG_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=50)
HG_model70 <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=70)

BC_model <- getData('CMIP5', var='bio', res=5, rcp=85, model='BC', year=50)

# bio 1 variable comparison 
plot(HG_model, 1)
plot(r, 1)

# locations
brazil_hg <- crop(HG_model, bbox(BRA$spdf@bbox))
plot(brazil_hg, 1)
## creating a dataframe
points <- spsample(as(brazil_hg@extent, 'SpatialPolygons'), n=1000, type="random")
values <- raster:: extract(HG_model, points)

# bind into df
df <- cbind.data.frame(coordinates(points), values)

##
bahia_tmp <- crop(HG_model, bahia$spdf@bbox)
plot(bahia_tmp)
# assign regions to locations
library(GADMTools)
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

# year 70 
future_output70 <- data_frame()
for(y in region_names) {
  y <- gadm_subset(BRA, level = 1, regions = y)
  # print(x$spdf$NAME_1)
  temp_region_r <- crop(HG_model70, y$spdf@bbox)
  region_points <- spsample(as(temp_region_r@extent, 'SpatialPolygons'), n=100, type="random")
  region_values <- raster:: extract(temp_region_r, region_points) 
  temp_region_df <- cbind.data.frame(coordinates(region_points), region_values)
  temp_region_df$region <- y$spdf$NAME_1
  future_output70 <- rbind(future_output70, temp_region_df)
  remove(temp_region_r, region_points, temp_region_df)
}
# calculating percent of locations in each region under drought
future_output$total <- 1
total_locations <- future_output
total_locations$drought <- NA
for(row in 1:nrow(total_locations)) {
  if(is.na(total_locations$bc85bi5017[[row]])) {
    total_locations$drought[[row]] <- NA
  } else if (total_locations$bc85bi5017[[row]] > 40) { 
    total_locations$drought[[row]] <- 0
  } else
  {total_locations$drought[[row]] <- 1}
}

## year 2070
# calculating percent of locations in each region under drought
future_output70$total <- 1
total_locations70 <- future_output70
total_locations70$drought <- NA
for(row in 1:nrow(total_locations70)) {
  if(is.na(total_locations70$bc85bi7017[[row]])) {
    total_locations70$drought[[row]] <- NA
  } else if (total_locations70$bc85bi7017[[row]] > 40) { 
    total_locations70$drought[[row]] <- 0
  } else
  {total_locations70$drought[[row]] <- 1}
}

write.csv(future_output70, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/future_output70.csv")
write.csv(total_locations70, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/total_loactions70.csv")


## go to script 00 for continued cleaning of 70 data

total_locations = total_locations %>% group_by(region) %>% filter(!is.na(drought)) %>% mutate(total = sum(total))

total_drought <- total_locations %>% group_by(region) %>% mutate(drought = sum(drought))
total_drought = total_drought %>% mutate(drought_percent = drought/total)
total_drought = total_drought %>% dplyr::select("region", "total","drought", "drought_percent")
total_drought = unique(total_drought)

# summarise by region
future_summary <- future_output %>% group_by(region) %>% summarise_all(list(mean), na.rm = TRUE)
future_summary = future_summary %>% select(-c(x,y, total))

# rename columns
names(future_summary)

names(future_summary)[2]<-"BIO_1_Annual Mean Temp"
names(future_summary)[3]<-"BIO_2_Mean Diurnal Range" #(Mean of monthly (max temp - min temp))
names(future_summary)[4]<-"BIO_3_Isothermality" # (BIO2/BIO7) (* 100)
names(future_summary)[5]<-"BIO_4_Temp Seasonality" # (standard deviation *100)" 
names(future_summary)[6]<-"BIO_5_Max Temp of Warmest Month" 
names(future_summary)[7]<-"BIO_6_Min Temp of Coldest Month"
names(future_summary)[8]<-"BIO_7_Temp Annual Range" # (BIO5-BIO6)
names(future_summary)[9]<-"BIO_8_Mean Temp of Wettest Quarter" 
names(future_summary)[10]<-"BIO_9_Mean Temp of Driest Quarter" 
names(future_summary)[11]<-"BIO_10_Mean Temp of Warmest Quarter" 
names(future_summary)[12]<-"BIO_11_Mean Temp of Coldest Quarter" 
names(future_summary)[13]<-"BIO_12_Annual Precipitation" 
names(future_summary)[14]<-"BIO_13_Precipitation of Wettest Month" 
names(future_summary)[15]<-"BIO_14_Precipitation of Driest Month" 
names(future_summary)[16]<-"BIO_15_Precipitation Seasonality"  #(Coefficient of Variation)
names(future_summary)[17]<-"BIO_16_Precipitation of Wettest Quarter" 
names(future_summary)[18]<-"BIO_17_Precipitation of Driest Quarter" 
names(future_summary)[19]<-"BIO_18_Precipitation of Warmest Quarter" 
names(future_summary)[20]<-"BIO_19_Precipitation of Coldest Quarter" 

# select necessary variables and add in drought data
future_sum <- future_summary %>% select("region","BIO_1_Annual Mean Temp", "BIO_5_Max Temp of Warmest Month", "BIO_6_Min Temp of Coldest Month", 
                                        "BIO_13_Precipitation of Wettest Month", "BIO_14_Precipitation of Driest Month", "BIO_17_Precipitation of Driest Quarter")
future_sum = left_join(future_sum, total_drought, by = "region")


# write.csv
write.csv(future_sum, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/future_sum.csv", row.names = FALSE)

#manually change future sum regions to deal with accent marks
library(readr)
future_sum_fixed <- read_csv("brazil-coffee/coffee/data/clean/future_sum_fixed.csv")
View(future_sum_fixed)

