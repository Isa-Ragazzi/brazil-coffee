# need avgtmp, tmax, tmin, pmax, pmin, drought_percent, drought, total, region, UF, year
all_climate_bunn <- all_climate
fuccc <- all_climate_bunn %>% mutate(drought = NA)
for(row in 1:nrow(fuccc)) {
  if(is.na(fuccc$precip[[row]])) {
    fuccc$drought[[row]] <- NA
  } else if (fuccc$precip[[row]] > 40) { 
    fuccc$drought[[row]] <- 1
  } else
  {fuccc$drought[[row]] <- 0}
}

write.csv(fuccc, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/fuccc.csv")
read.csv(fuccc, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/fuccc.csv")

fuccc <- fuccc %>% tidyr::separate(year, c("year", "month"), sep= "_") 
fuccc = fuccc[!is.na(fuccc$avgtmp),]
fuccc <- fuccc %>% select("x","y", "year","month","avgtmp","tmax", "tmin","precip","drought")
bunn_climate <- fuccc
bunn_climate = bunn_climate %>% group_by(x, y, year) %>% mutate(precipitation = sum(precip), pmax = max(precip), pmin = min(precip))
bunn_climate$model <- "cru"
# future
future_output1 <- future_output
future_output1$year <- 2050
future_output70$year <- 2070

total_locations_1 <- future_output1
total_locations_1$drought <- NA
for(row in 1:nrow(total_locations_1)) {
  if(is.na(total_locations_1$bc85bi5017[[row]])) {
    total_locations_1$drought[[row]] <- NA
  } else if (total_locations_1$bc85bi5017[[row]] > 40) { 
    total_locations_1$drought[[row]] <- 0
  } else
  {total_locations_1$drought[[row]] <- 1}
}
total_locations_drought <- total_locations_1[,c("x","y","drought")]
total_locations70_drought <- total_locations70[,c("x","y","drought")]


names(future_output1)[3]<-"BIO_1_Annual Mean Temp"
names(future_output1)[4]<-"BIO_2_Mean Diurnal Range" #(Mean of monthly (max temp - min temp))
names(future_output1)[5]<-"BIO_3_Isothermality" # (BIO2/BIO7) (* 100)
names(future_output1)[6]<-"BIO_4_Temp Seasonality" # (standard deviation *100)" 
names(future_output1)[7]<-"BIO_5_Max Temp of Warmest Month" 
names(future_output1)[8]<-"BIO_6_Min Temp of Coldest Month"
names(future_output1)[9]<-"BIO_7_Temp Annual Range" # (BIO5-BIO6)
names(future_output1)[10]<-"BIO_8_Mean Temp of Wettest Quarter" 
names(future_output1)[11]<-"BIO_9_Mean Temp of Driest Quarter" 
names(future_output1)[12]<-"BIO_10_Mean Temp of Warmest Quarter" 
names(future_output1)[13]<-"BIO_11_Mean Temp of Coldest Quarter" 
names(future_output1)[14]<-"BIO_12_Annual Precipitation" 
names(future_output1)[15]<-"BIO_13_Precipitation of Wettest Month" 
names(future_output1)[16]<-"BIO_14_Precipitation of Driest Month" 
names(future_output1)[17]<-"BIO_15_Precipitation Seasonality"  #(Coefficient of Variation)
names(future_output1)[18]<-"BIO_16_Precipitation of Wettest Quarter" 
names(future_output1)[19]<-"BIO_17_Precipitation of Driest Quarter" 
names(future_output1)[20]<-"BIO_18_Precipitation of Warmest Quarter" 
names(future_output1)[21]<-"BIO_19_Precipitation of Coldest Quarter" 

names(future_output70)[3]<-"BIO_1_Annual Mean Temp"
names(future_output70)[4]<-"BIO_2_Mean Diurnal Range" #(Mean of monthly (max temp - min temp))
names(future_output70)[5]<-"BIO_3_Isothermality" # (BIO2/BIO7) (* 100)
names(future_output70)[6]<-"BIO_4_Temp Seasonality" # (standard deviation *100)" 
names(future_output70)[7]<-"BIO_5_Max Temp of Warmest Month" 
names(future_output70)[8]<-"BIO_6_Min Temp of Coldest Month"
names(future_output70)[9]<-"BIO_7_Temp Annual Range" # (BIO5-BIO6)
names(future_output70)[10]<-"BIO_8_Mean Temp of Wettest Quarter" 
names(future_output70)[11]<-"BIO_9_Mean Temp of Driest Quarter" 
names(future_output70)[12]<-"BIO_10_Mean Temp of Warmest Quarter" 
names(future_output70)[13]<-"BIO_11_Mean Temp of Coldest Quarter" 
names(future_output70)[14]<-"BIO_12_Annual Precipitation" 
names(future_output70)[15]<-"BIO_13_Precipitation of Wettest Month" 
names(future_output70)[16]<-"BIO_14_Precipitation of Driest Month" 
names(future_output70)[17]<-"BIO_15_Precipitation Seasonality"  #(Coefficient of Variation)
names(future_output70)[18]<-"BIO_16_Precipitation of Wettest Quarter" 
names(future_output70)[19]<-"BIO_17_Precipitation of Driest Quarter" 
names(future_output70)[20]<-"BIO_18_Precipitation of Warmest Quarter" 
names(future_output70)[21]<-"BIO_19_Precipitation of Coldest Quarter" 

future_output1 = left_join(future_output1, total_locations_drought, by = c("x","y"))
future_output70 = left_join(future_output70, total_locations70_drought, by = c("x","y"))

# rename Bio 1 to avgtmp
# rename Bio 5 to tmax
# rename Bio 6 to tmin
# rename Bio 13 ro pmax
# rename Bio 14 to pmin
# remove Bio 17 
names(future_output1)
names(future_output1)[3]<-"avgtmp"
names(future_output1)[7]<-"tmax" 
names(future_output1)[8]<-"tmin" 
names(future_output1)[14] <- "precipitation"
names(future_output1)[15]<-"pmax" 
names(future_output1)[16]<-"pmin" 

future_output2 = future_output1 %>% select("x", "y","avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")
future_output2$year = 2050
future_output2$model = "BC"

names(future_output2)

names(future_output70)
names(future_output70)[3]<-"avgtmp"
names(future_output70)[7]<-"tmax" 
names(future_output70)[8]<-"tmin" 
names(future_output70)[14] <- "precipitation"
names(future_output70)[15]<-"pmax" 
names(future_output70)[16]<-"pmin" 
future_output70 = future_output70 %>% select("x", "y","avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")
future_output70$year = 2070
future_output70$model = "BC"

names(bunn_climate)
future_output2 = future_output2 %>% select("x", "y","year","avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought", "model")
future_output70 = future_output70 %>% select("x", "y","year","avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought", "model")
future_output2 = rbind(future_output2, future_output70)
bunn_climate2 = bunn_climate %>% select("x", "y","year","avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought", "model")

# save files
write.csv(future_output2, "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/future_output2")
write.csv(bunn_climate2, "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/bunn_climate2")
