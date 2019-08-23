library(foreign)
library(dplyr)
library(GADMTools)
library(ggplot2)
# global land coverage data from SPAM

global_physical <- read.dbf("/Users/isabellaragazzi/Downloads/spam2010v1r0_global_phys_area.dbf/spam2010v1r0_global_physical-area_ta.dbf")
brazil_physical <- global_physical %>% filter(ISO3 == "BRA") %>% filter(ACOF_A != 0)

#filtering for brazil to find total physical crop area
brazil_all <- global_physical %>% filter(ISO3 == "BRA") %>% tidyr::gather(key = "crop", value = "area", 10:51, na.rm = TRUE) %>% 
  filter(area != 0)
brazil_all = brazil_all %>% group_by(X,Y) %>% mutate(total_crop = sum(area))
brazil_all = brazil_all[,c(-16,-17)]
brazil_all = unique(brazil_all)

# I think CELL5M is the total area of the pixel, so here trying to see how much of that total size is crop area
brazil_all = brazil_all %>% mutate(size = total_crop/as.numeric(CELL5M))

#save results
write.csv(brazil_physical, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/brazil_physical")
write.csv(brazil_all, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/brazil_all")

# looking at SPAM's coverage of coffee producing areas, much larger than Bunn's, would ignore for purposes of our research
brazil_current_coffee <- brazil_physical %>% group_by(X,Y) %>% mutate(coffee_area = ACOF_A/CELL5M, region = NAME_ADM1) %>% 
  select("X","Y","coffee_area", "region")
write.csv(brazil_current_coffee, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/brazil_current_coffee")

# mapping SPAM's data
# total area where there is some coffee grown
ggplot(brazil_physical, aes(x = x, y = y))
gadm_plot(BRA) + geom_point(data = brazil_physical, aes(x = X, y = Y)) + ggtitle("Arabic Producing Physical areas")
# , size=as.numeric(ACOF_A)

# mapping total agricultural area
ggplot() + geom_point(data = brazil_all, aes(x = X, y = Y)) + ggtitle("Total Agricultural Area")

# can add the next line to see Bunn's data overlayed 
# + geom_point(data = mapping_clusters, aes(x = x, y = y, color = factor(fit2.cluster))) 

# view coffee producing areas recorded by SPAM as compared to Bunn
ggplot() + geom_point(data = brazil_current_coffee, aes(x = X, y = Y)) + ggtitle("Total Coffee Producing Area") +  geom_point(data = mapping_clusters, aes(x = x, y = y, color = factor(fit2.cluster))) 

# gadm_plot(BRA) + geom_point(data = brazil_all, aes(x = X, y = Y, size=as.numeric(area), color = crop)) 

# finding future climate for all crop producing areas
#use future output2

library(fuzzyjoin)
brazil_all = read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/brazil_all")
# get coordinates for crop area
crop_coords <- brazil_all %>% select("X","Y")  
names(crop_coords) <- c("x","y")
crop_coords = unique(crop_coords)

# split coordinates to make code run faster
crop_coords_1 <- crop_coords[1:11300,]
crop_coords_2 <- crop_coords[11301:22600,]
crop_coords_3 <- crop_coords[22601:33000,]
crop_coords_4 <- crop_coords[33001:43000,]
crop_coords_5 <- crop_coords[43001:nrow(crop_coords),]

# join SPAM crop coordinates to future climate data for 2050 and 2070
future_output2 = as.data.frame(future_output2)
names(future_output2)
future_climate_data <- future_output2[,-1]

future_climate_data50 = future_climate_data %>% filter(year == 2050) %>% select("x","y","avgtmp","tmax","tmin","precipitation","pmax","pmin","drought")
future_climate_data70 = future_climate_data %>% filter(year == 2070) %>% select("x","y","avgtmp","tmax","tmin","precipitation","pmax","pmin","drought")

## FUNCTIONS ###
join_coords_future50 <- function(x) {
  geo_left_join(x, future_climate_data50, by = c("x","y"), method = "geo", max_dist = 50, unit = "km")}
  
join_coords_future70 <-   function(x) {
  geo_left_join(x, future_climate_data70, by = c("x","y"), method = "geo", max_dist = 50, unit = "km")}

### 2050 ####

future_cropland2050 <- join_coords_future50(crop_coords_1)
future_cropland2050_2 <- join_coords_future50(crop_coords_2)
future_cropland2050_3 <- join_coords_future50(crop_coords_3)
future_cropland2050_4 <- join_coords_future50(crop_coords_4)
future_cropland2050_5 <- join_coords_future50(crop_coords_5)

# check names of columns
names(future_cropland2050)
names(future_cropland2050_2)
names(future_cropland2050_3)
names(future_cropland2050_4)
names(future_cropland2050_5)

# bind
cropland_2050 <- rbind(future_cropland2050, future_cropland2050_2, future_cropland2050_3, 
                       future_cropland2050_4, future_cropland2050_5)
# clean a bit
cropland_2050 = cropland_2050 %>% select(-c("x.y","y.y")) %>% 
  mutate(avgtmp = avgtmp/10, tmax = tmax/10, tmin = tmin/10) %>% na.omit
# save
write.csv(cropland_2050, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/cropland2050")


### 2070

future_cropland2070 <- join_coords_future70(crop_coords_1)
future_cropland2070_2 <- join_coords_future70(crop_coords_2)
future_cropland2070_3 <- join_coords_future70(crop_coords_3)
future_cropland2070_4 <- join_coords_future70(crop_coords_4)
future_cropland2070_5 <- join_coords_future70(crop_coords_5)

# check names of columns
names(future_cropland2070)
names(future_cropland2070_2)
names(future_cropland2070_3)
names(future_cropland2070_4)
names(future_cropland2070_5)

# bind
cropland_2070 <- rbind(future_cropland2070, future_cropland2070_2, future_cropland2070_3, 
                       future_cropland2070_4, future_cropland2070_5)
# clean a bit
cropland_2070 = cropland_2070 %>% select(-c("x.y","y.y")) %>% 
  mutate(avgtmp = avgtmp/10, tmax = tmax/10, tmin = tmin/10) %>% na.omit
# save
write.csv(cropland_2070, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/cropland2070")



