library(ggplot2)
library(ranger)
library(caret)
library(fuzzyjoin)
## functions
join_coords_present <- function(x) {
  geo_left_join(x, na.omit(combo_tmp), by = c("x","y"), method = "geo", max_dist = 100, unit = "km")}

# import necessary files

cropland_2050 <- read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/cropland2050")
cropland_2070 <- read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/cropland2070")
brazil_current_coffee <- read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/brazil_current_coffee")

cropland_2050 = cropland_2050 %>% select(-c("X"))
ggplot() + geom_point(data = cropland_2050, aes(x = x.x, y = y.x)) + ggtitle("Total Agricultural Area 2050") 
#+ geom_point(data = mapping_clusters, aes(x = x, y = y, color = factor(fit2.cluster))) 

# exclude points outside of the range for the variables in each current zone's data 
cropland_2050[,c(3:9)] <- apply(cropland_2050[,c(3:9)], 2, function(x) scale(x,center = TRUE, scale = TRUE))
range_avgtmp <- range(combo_tmp$avgtmp)
range_tmax <- range(combo_tmp$tmax)
range_tmin <- range(combo_tmp$tmin)
range_precip <- range(combo_tmp$precipitation)
range_pmax <- range(combo_tmp$pmax)
range_pmin <- range(combo_tmp$pmin)
range_drought <- range(combo_tmp$drought)

cropland_2050 = cropland_2050 %>% filter(between(avgtmp, min(range_avgtmp), max(range_avgtmp)))
cropland_2050 = cropland_2050 %>% filter(between(tmax, min(range_tmax), max(range_tmax)))
cropland_2050 = cropland_2050 %>% filter(between(tmin, min(range_tmin), max(range_tmin)))
cropland_2050 = cropland_2050 %>% filter(between(precipitation, min(range_precip), max(range_precip)))
cropland_2050 = cropland_2050 %>% filter(between(pmax, min(range_pmax), max(range_pmax)))
cropland_2050 = cropland_2050 %>% filter(between(pmin, min(range_pmin), max(range_pmin)))
cropland_2050 = cropland_2050 %>% filter(between(drought, min(range_drought), max(range_drought)))

# check map 
ggplot() + geom_point(data = cropland_2050, aes(x = x.x, y = y.x)) + ggtitle("Total Suitable Agricultural Area 2050") 
#+ geom_point(data = mapping_clusters, aes(x = x, y = y, color = factor(fit2.cluster))) 

# apply formula from script 6 to determine climate clusters for each suitable point

predicted_clusters_50 <- predict(model, cropland_2050[,c(3:9)])
predicted_clusters_50 = as.data.frame(predicted_clusters_50)
names(predicted_clusters_50) <- "AEZ"
predicted_clusters_50 = cbind(as.data.frame(cropland_2050), as.data.frame(predicted_clusters_50))

## 2070
cropland_2070 = cropland_2070 %>% select(-c("X"))
ggplot() + geom_point(data = cropland_2070, aes(x = x.x, y = y.x)) + ggtitle("Total Agricultural Area 2070") 
#+ geom_point(data = mapping_clusters, aes(x = x, y = y, color = factor(fit2.cluster))) 

# exclude points outside of the range for the variables in each current zone's data 
cropland_2070[,c(3:9)] <- apply(cropland_2070[,c(3:9)], 2, function(x) scale(x,center = TRUE, scale = TRUE))

cropland_2070 = cropland_2070 %>% filter(between(avgtmp, min(range_avgtmp), max(range_avgtmp)))
cropland_2070 = cropland_2070 %>% filter(between(tmax, min(range_tmax), max(range_tmax)))
cropland_2070 = cropland_2070 %>% filter(between(tmin, min(range_tmin), max(range_tmin)))
cropland_2070 = cropland_2070 %>% filter(between(precipitation, min(range_precip), max(range_precip)))
cropland_2070 = cropland_2070 %>% filter(between(pmax, min(range_pmax), max(range_pmax)))
cropland_2070 = cropland_2070 %>% filter(between(pmin, min(range_pmin), max(range_pmin)))
cropland_2070 = cropland_2070 %>% filter(between(drought, min(range_drought), max(range_drought)))

# check map 
ggplot() + geom_point(data = cropland_2070, aes(x = x.x, y = y.x)) + ggtitle("Total Suitable Agricultural Area 2070") 
#+ geom_point(data = mapping_clusters, aes(x = x, y = y, color = factor(fit2.cluster))) 

# apply formula from script 6 to determine climate clusters for each suitable point

predicted_clusters_70 <- predict(model, cropland_2070[,c(3:9)])
predicted_clusters_70 = as.data.frame(predicted_clusters_70)
names(predicted_clusters_70) <- "AEZ"
predicted_clusters_70 = cbind(as.data.frame(cropland_2070), as.data.frame(predicted_clusters_70))

# CURRENT COFFEE - applying current climate conditions to SPAM coffee area data to find coffee points 
# that are within the range of Bunn's AEZs to map current suitable production area
# combo_tmp has averaged current climate conditions
brazil_current_coffee = brazil_current_coffee %>% select(-c("X.1"))
names(brazil_current_coffee)[1] <- "x"
names(brazil_current_coffee)[2] <- "y"
current_coords <- brazil_current_coffee[,1:2]
current_coords <- as.data.frame(current_coords) %>% na.omit
## this does not work
brazil_currently <- geo_left_join(current_coords, combo_tmp, by = c("x","y"), method = "geo", max_dist = 100, unit = "km")

######### MAPPING ##########
# map clusters 2050
ggplot() + geom_point(data = predicted_clusters_50, aes(x = x.x, y = y.x, color = AEZ, alpha = 1/5)) + ggtitle("AEZs 2050") 

# map clusters 2070
ggplot() + geom_point(data = predicted_clusters_70, aes(x = x.x, y = y.x, color = AEZ, alpha = 1/5)) + ggtitle("AEZs 2070") 
