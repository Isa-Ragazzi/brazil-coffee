library(foreign)
library(dplyr)
library(GADMTools)
library(ggplot2)
# global land coverage data

global_physical <- read.dbf("/Users/isabellaragazzi/Downloads/spam2010v1r0_global_phys_area.dbf/spam2010v1r0_global_physical-area_ta.dbf")
brazil_physical <- global_physical %>% filter(ISO3 == "BRA") %>% filter(ACOF_A != 0)
brazil_all <- global_physical %>% filter(ISO3 == "BRA") %>% gather(key = "crop", value = "area", 10:51, na.rm = TRUE) %>% 
              filter(area != 0)
brazil_all = brazil_all %>% group_by(X,Y) %>% mutate(total_crop = sum(area))
brazil_all = brazil_all[,c(-16,-17)]
brazil_all = unique(brazil_all)
brazil_all = brazil_all %>% mutate(size = total_crop/as.numeric(CELL5M))

brazil_current_coffee <- brazil_physical %>% group_by(X,Y) %>% mutate(coffee_area = ACOF_A/CELL5M, region = NAME_ADM1) %>% 
                          select("X","Y","coffee_area", "region")

coordinatesX <- as.data.frame(brazil_physical$X)
coordinatesY <- as.data.frame(brazil_physical$Y)
coordinates <- cbind(coordinatesX, coordinatesY)
names(coordinates)[1] <- "x"
names(coordinates)[2] <- "y"

ggplot(brazil_physical, aes(x = x, y = y))

gadm_plot(BRA) + geom_point(data = brazil_physical, aes(x = X, y = Y)) + ggtitle("Arabic Producing Physical areas")
# , size=as.numeric(ACOF_A)
ggplot() + geom_point(data = brazil_all, aes(x = X, y = Y)) + ggtitle("Total Agricultural Area")
  
geom_point(data = mapping_clusters, aes(x = x, y = y, color = factor(fit2.cluster))) 

ggplot() + geom_point(data = brazil_current_coffee, aes(x = X, y = Y)) + ggtitle("Total Agricultural Area") + 
  geom_point(data = mapping_clusters, aes(x = x, y = y, color = factor(fit2.cluster))) 

# gadm_plot(BRA) + geom_point(data = brazil_all, aes(x = X, y = Y, size=as.numeric(area), color = crop)) 

## match coordinates to future temperatures
write.csv(brazil_all, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/brazil_all")
# use future output2
library(fuzzyjoin)
crop_coords <- brazil_all %>% select("X","Y")  
names(crop_coords) <- c("x","y")
future_output2 = as.data.frame(future_output2)
names(future_output2)
future_climate_data = future_output2[,-1]
future_climate_data = future_climate_data %>% filter(year == 2050)
future_climate_data = future_climate_data %>% filter(year == 2070)

future_cropland2050 <- geo_left_join(crop_coords, future_climate_data, by = c("x","y"), method = "geo", max_dist = 50, unit = "km")
future_cropland2070 <- geo_left_join(crop_coords, future_climate_data, by = c("x","y"), method = "geo", max_dist = 50, unit = "km")

# complete cases out of 6235 = 6118
future_cropland %>% select("x.x","y.x") %>% unique.data.frame() %>% nrow()
future_cropland %>% na.omit(avgtmp) %>% select("x.x","y.x") %>% unique.data.frame() %>% nrow()
names(future_cropland)[1] <-"x"
names(future_cropland)[2] <-"y"
future_cropland = future_cropland %>% filter(complete.cases(future_cropland)==TRUE) %>% select(-c("x.y","y.y"))
future_cropland %>% select("x","y") %>% unique() %>% nrow()

                                                                                                                                