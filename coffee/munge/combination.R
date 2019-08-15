# run historic_data_summary and future climate scripts prior to this one


## ------------- CLIMATE ----------------##
# matching column names to create aggregate table
names(historic_sum_fixed)
names(future_sum_fixed) # model BC

# rename Bio 1 to avgtmp
# rename Bio 5 to tmax
# rename Bio 6 to tmin
# rename Bio 13 ro pmax
# rename Bio 14 to pmin
# remove Bio 17 
future_sum_2 <- future_sum_fixed
names(future_sum_2)[2]<-"avgtmp"
names(future_sum_2)[3]<-"tmax" 
names(future_sum_2)[4]<-"tmin" 
names(future_sum_2)[5]<-"pmax" 
names(future_sum_2)[6]<-"pmin" 
future_sum_2 = future_sum_2[,-7]

# match orders of both tables
future_sum_2 = future_sum_2 %>% select("region", "UF", "year", "avgtmp", "tmax", "tmin","pmax","pmin", "drought_percent", "drought", "total")
historic_sum_2 <- historic_sum_fixed
# not using totalprecip bc I think it was calculated wrong, not using average precip bc not available for future data
historic_sum_2 = historic_sum_2 %>% select("region", "UF", "year", "avgtmp", "tmax", "tmin","pmax","pmin", "drought_percent", "drought", "total")
#check that column names match
names(historic_sum_2)
names(future_sum_2)

#combine into one table
all_region_climate <- rbind(historic_sum_2,future_sum_2)

##=========== COFFEE AND AGRICULTURAL DATA============ ##



