library(tidyverse)
library(dplyr)
library(raster)
library(GADMTools)
# install.packages("caret")
library(caret)
library(ggplot2)


# CURRENT CLIMATE BRAZIL
global_physical <- read.dbf("/Users/isabellaragazzi/Downloads/spam2010v1r0_global_phys_area.dbf/spam2010v1r0_global_physical-area_ta.dbf")
# this data can be downloaded at: http://mapspam.info/data/
# Citation: International Food Policy Research Institute, 2019, 
# “Global Spatially-Disaggregated Crop Production Statistics Data for 2010 Version 1.0”, 
# https://doi.org/10.7910/DVN/PRFF8V, Harvard Dataverse, V1

brazil_physical <- global_physical %>% filter(ISO3 == "BRA") %>% filter(ACOF_A != 0)

#filtering for brazil to find total physical crop area
brazil_all <- global_physical %>% filter(ISO3 == "BRA") %>% tidyr::gather(key = "crop", value = "area", 10:51, na.rm = TRUE) %>% 
  filter(area != 0)

# limit to current coffee producing areas
brazil_all <- read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/brazil_all")
current_coords <- brazil_all %>% select("X","Y") %>% unique

### CURRENT CLIMATE 1970-2000 average
w = getData('worldclim', var='bio', res=5)

#check
plot(w, "bio1")

# crop to current agricultural area
brazil_current <- w
plot(brazil_current, "bio1")
coordinates(brazil_current)
points <- current_coords
values <- raster:: extract(brazil_current, current_coords)

# bind into df
df <- cbind.data.frame(points, values)
# check 
ggplot() + geom_point(data = df, aes(x = X, y = Y))


## ============================== FUTURE CLIMATE ==================================== ##

BC_model <- getData('CMIP5', var='bio', res=5, rcp=85, model='BC', year=50)
BC_model70 <- getData('CMIP5', var='bio', res=5, rcp=85, model='BC', year=70)

# 2050 ===================crop to current agricultural area ==========
brazil_50 <-BC_model
plot(brazil_50, 1)
coordinates(brazil_50)
points <- current_coords
values <- raster:: extract(brazil_50, current_coords)

# bind into df
df_50 <- cbind.data.frame(points, values)
# check 
ggplot() + geom_point(data = df_50, aes(x = X, y = Y))


# 2070 =================== crop to current agricultural area ==========
brazil_70 <- BC_model70
plot(brazil_70, 1)
coordinates(brazil_70)
points <- current_coords
values <- raster:: extract(brazil_70, current_coords)

# bind into df
df_70 <- cbind.data.frame(points, values)
# check 
ggplot() + geom_point(data = df_70, aes(x = X, y = Y))

# save outputs
write.csv(df, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/df_current.csv")
write.csv(df_50, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/df_50.csv")
write.csv(df_70, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/df_70.csv")

# make a year column for current data, then combine all three
df$year <- 2000
df_50$year <- 2050
df_70$year <- 2070
names(df) <- c("x","y","1","2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","year")
names(df_50) <- c("x","y","1","2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","year")
names(df_70) <- c("x","y","1","2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","year")


all_wc <- rbind(df,df_50, df_70)
# rename Bio 1 to avgtmp, rename Bio 2 to diurnal_range *, rename Bio 5 to tmax *, rename Bio 6 to tmin *, bio 12 * precipitation, 
# bio 15* precipitation seasonlity, bio 18 *  precipitation of warmest quarter, rename Bio 13 to pmax, rename Bio 14 to pmin, 
# rename Bio 17 to pquarter
all_wc = all_wc %>% rename("avgtmp" = "1","diurnal_range"="2","isothermality" = "3",
                           "tmax" = "5", "tmin" = "6", "precipitation" = "12","pmax" = "13", 
                           "pmin" = "14", "pseasonality" = "15", "pquarter" = "17", "pwarmest" = "18")
corr <- cor(all_wc[,c(3:21)])
# World Clim reports temperature in C* 10 so correct for that 
all_climate_data <- all_wc %>% select("x","y","year","avgtmp", "diurnal_range","isothermality","tmax", 
                                      "tmin","precipitation","pmax","pmin","pseasonality", "pquarter", "pwarmest",
                                      "4","7","8","9","10","11","16","19") 

### ================================================== BUNNS DATA ===================================###
# shapefiles with the same info as dbf - source: Christian Bunn, https://www.researchgate.net/profile/Christian_Bunn
shp.AglomeradosSubnormais2010_Limites <- readOGR("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.shp")
# get coordinates of coffee farms
dd <- shp.AglomeradosSubnormais2010_Limites
proj4string(dd) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
coordinates <- coordinates(dd)
names(coordinates)[1] <- "x"
names(coordinates)[2] <- "y"
plot(dd)

# current climate
# crop to current agricultural area
bunn_farms <- crop(w, coordinates)
plot(bunn_farms, "bio1")
coordinates(bunn_farms)
points_bunn <- coordinates
values_bunn <- raster:: extract(bunn_farms, coordinates)

# bind into df
df_bunn <- cbind.data.frame(points_bunn, values_bunn)
# check 
ggplot() + geom_point(data = df_bunn, aes(x = x, y = y))


# =========================================================== 2050 BUNN DATA ================================= ##
# BC_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=50)
# BC_model70 <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=70)

# 2050 ===================crop to current agricultural area ==========
bunn_50 <- crop(BC_model, coordinates)
plot(bunn_50, 1)
coordinates(bunn_50)
points_bunn <- coordinates
values_bunn <- raster:: extract(bunn_50, coordinates)

# bind into df
df_50 <- cbind.data.frame(points_bunn, values_bunn)
# check 
ggplot() + geom_point(data = df_50, aes(x = x, y = y))


# ================================================================== 2070 BUNN DATA ========================== ##
bunn_70 <- crop(BC_model70, coordinates)
plot(bunn_70, 1)
coordinates(bunn_70)
points_bunn <- coordinates
values_bunn <- raster:: extract(bunn_70, coordinates)

# bind into df
df_70 <- cbind.data.frame(points_bunn, values_bunn)
# check 
ggplot() + geom_point(data = df_70, aes(x = x, y = y))

# save outputs
write.csv(df_bunn, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/bunn_current.csv")
write.csv(df_50, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/bunn_50.csv")
write.csv(df_70, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/bunn_70.csv")

# make a year column for current data, then combine all three
df_bunn$year <- 2000
df_50$year <- 2050
df_70$year <- 2070

names(df_bunn) 
names(df_50) 
names(df_70) 

names(df_bunn) <- c("x","y","1","2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","year")
names(df_50) <- c("x","y","1","2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","year")
names(df_70) <- c("x","y","1","2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","year")

all_bunn <- rbind(df_bunn,df_50, df_70)

# rename
all_bunn = all_bunn %>% rename("avgtmp" = "1","diurnal_range"="2","isothermality" = "3","tmax" = "5", "tmin" = "6", 
                               "precipitation" = "12","pmax" = "13", "pmin" = "14", "pseasonality" = "15", "pquarter" = "17", 
                               "pwarmest" = "18")

# reorder columns
all_bunn <- all_bunn %>% select("x","y","year","avgtmp", "diurnal_range", "isothermality", "tmax", "tmin","precipitation","pmax",
                                "pmin", "pquarter", "pseasonality", "pwarmest","4","7","8","9","10","11","16","19")

# save outputs
write.csv(all_bunn, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/all_bunn.csv")


# ====================================== PREPARING FOR THE MODEL ==============================================##
# generate z-scores for variables in columns 4:10 using the scale() function
bunn_z_current <- all_bunn %>% filter(year==2000) 
bunn_z_2050 <- all_bunn %>% filter(year==2050)
bunn_z_2070 <- all_bunn %>% filter(year==2070)
all_climate_z <- all_climate_data %>% na.omit

bunn_z_current[,c("avgtmp", "diurnal_range", "isothermality", "tmax", "tmin","precipitation","pmax","pmin", "pquarter", "pseasonality", 
                  "pwarmest")] <- apply(bunn_z_current[,c("avgtmp", "tmax", "diurnal_range", "isothermality","tmin","precipitation","pmax",
                                                          "pmin", "pquarter","pseasonality", "pwarmest")], 2, function(x) scale(x,center = TRUE, scale = TRUE))

bunn_z_2050[,c("avgtmp", "diurnal_range", "isothermality", "tmax", "tmin","precipitation","pmax","pmin", "pquarter", "pseasonality", 
               "pwarmest")] <- apply(bunn_z_2050[,c("avgtmp", "diurnal_range", "isothermality", "tmax", "tmin","precipitation","pmax","pmin", 
                                                    "pquarter", "pseasonality", "pwarmest")], 2, function(x) scale(x,center = TRUE, scale = TRUE))

bunn_z_2070[,c("avgtmp", "diurnal_range", "isothermality", "tmax", "tmin","precipitation","pmax","pmin", "pquarter", "pseasonality", 
               "pwarmest")] <- apply(bunn_z_2070[,c("avgtmp", "diurnal_range", "isothermality","tmax", "tmin","precipitation","pmax","pmin", "pquarter", 
                                                    "pseasonality", "pwarmest")], 2, function(x) scale(x,center = TRUE, scale = TRUE))
all_climate_z[,c("avgtmp", "diurnal_range", "isothermality", "tmax", "tmin","precipitation","pmax","pmin", "pquarter", "pseasonality", 
                 "pwarmest")] <- apply(all_climate_z[,c("avgtmp", "diurnal_range", "isothermality","tmax", "tmin","precipitation","pmax","pmin", "pquarter", 
                                                        "pseasonality", "pwarmest")], 2, function(x) scale(x,center = TRUE, scale = TRUE))

bunn_z_current = bunn_z_current %>% na.omit
bunn_z_2050 = bunn_z_2050 %>% na.omit
bunn_z_2070 = bunn_z_2070 %>% na.omit

# check correlations between variables, ignore pquarter bc that was just an intermediary variable for pquarter
res <- cor(all_climate_z[,-c(1:3)]) # all Brazilian climate

# recheck to assure no high correlation between key variables
res <- cor(all_climate_z[,c("diurnal_range", "tmax", "tmin","precipitation", "pseasonality", "pwarmest")]) # all Brazilian climate

# # additional exploratory correlations
# res <- cor(bunn_z_current[,c("avgtmp","diurnal_range", "isothermality", "tmax", "tmin","precipitation","pmax","pmin", "pquarter","pseasonality", "pwarmest")]) # current
# res <- cor(bunn_z_2050[,c("avgtmp", "diurnal_range", "isothermality","tmax", "tmin","precipitation","pmax","pmin", "pquarter","pseasonality", "pwarmest")])  # 2050
# res <- cor(combo2050[,c("avgtmp","diurnal_range", "isothermality", "tmax", "tmin","precipitation","pmax","pmin", "pquarter", "pseasonality", "pwarmest")]) # 2070

# Determine number of clusters - FUNCTION 
number_of_clusters <- function(df) {
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(df, 
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

# isolate key variables
bunn_z_cluster_analysis = bunn_z_current[,c("diurnal_range", "tmax", "tmin","precipitation", "pseasonality", "pwarmest")] # current

# determine number of clusters
number_of_clusters(bunn_z_cluster_analysis)

# finding = 4 clusters

# K-Means Cluster Analysis
fit <- kmeans(bunn_z_cluster_analysis, 4) # 4 cluster solution

# # get cluster means (summary of the four climates) 
# aggregate(bunn_z_cluster_analysis,by=list(fit$cluster),FUN=mean)
# store AEZ mean summary
aezs <- aggregate(bunn_z_cluster_analysis,by=list(fit$cluster),FUN=mean)

# append cluster assignment to df
bunn_z_clusters_current <- data.frame(bunn_z_current, fit$cluster)

# save cluster means 
write.csv(aezs, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/aezs")

# one way ANOVA - We want to know if there is any significant difference between the average variables for each AEZ
# Compute the analysis of variance - FUNCTION
anova <- function(variable, column, data) {
  res.aov <- aov(variable ~ column, data = data)
  summary(res.aov) # Summary of the analysis
}

# run ANOVA tests for key variables

anova(bunn_z_clusters_current$diurnal_range, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$tmax, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$tmin, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$precipitation, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$pwarmest, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$pseasonality, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)

# calculate # of coffee locations in each cluster
bunn_z_clusters_current %>% filter(fit.cluster == 1) %>% nrow()
bunn_z_clusters_current %>% filter(fit.cluster == 2) %>% nrow()
bunn_z_clusters_current %>% filter(fit.cluster == 3) %>% nrow()
bunn_z_clusters_current %>% filter(fit.cluster == 4) %>% nrow()
# all good bc all statistically significant, p<.01, high F-stat

## =========================== CREATING THE MODEL== RANDOM FOREST - train data on current, test on future===

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(bunn_z_clusters_current))

# Randomly order data
shuffled_fit <- bunn_z_clusters_current[rows,]

# create training and testing datasets
split <- round(nrow(shuffled_fit)*.6)
train <- shuffled_fit[1:split,]
test <- shuffled_fit[(split+1):(nrow(shuffled_fit)),] 

(fmla <- as.factor(fit.cluster) ~ pwarmest + diurnal_range + pseasonality + tmax + tmin + precipitation)
model <- train(fmla, train, method = "ranger", trControl = trainControl(
  method = "cv", number = 5))
summary(model)

#prediction on test dataset
p <- predict(model, test)
test$fit.cluster = as.factor(test$fit.cluster)
fit_cluster <- test$fit.cluster
table(p)

# checking accuracy with confusion matrix
confusionMatrix(data = p, reference = fit_cluster)
table(p, test$fit.cluster)

#calculate RMSE for test data set 
p = as.numeric(p)
error <- p - test[,"fit.cluster"]
sqrt(mean(error^2))

## ========= MAPPING BUNN's DATA =================#

## mapping Bunn's coffee 
bunn_z_clusters_current$AEZ <- bunn_z_clusters_current$fit.cluster

## isolate farms by cluster

AEZ1_farms <- bunn_z_clusters_current %>% filter(AEZ == 1)
AEZ2_farms <- bunn_z_clusters_current %>% filter(AEZ == 2)
AEZ3_farms <- bunn_z_clusters_current %>% filter(AEZ == 3)
AEZ4_farms <- bunn_z_clusters_current %>% filter(AEZ == 4)

# map all clusters
gadm_plot(BRA) + geom_point(data = AEZ1_farms, aes(x = x, y = y, color = "Cluster 1", alpha = .05)) + 
  geom_point(data = AEZ2_farms, aes(x = x, y = y, color = "Cluster 2", alpha = .05)) + 
  geom_point(data = AEZ4_farms, aes(x = x, y = y, color = "Cluster 4", alpha = .05)) + 
  geom_point(data = AEZ3_farms, aes(x = x, y = y, color = "Cluster 3", alpha = .05)) +
  ggtitle("Current Coffee Farms AEZ Classifications") # current

#### ============================= DETERMINING COFFEE PRODUCTION SUITABILITY

# limiting agricultural area to areas that fit within the range of bunn's coffee locations
range_tmax <- range(bunn_z_clusters_current$tmax)
range_tmin <- range(bunn_z_clusters_current$tmin)
range_precip <- range(bunn_z_clusters_current$precipitation)
range_pwarmest <- range(bunn_z_clusters_current$pwarmest)
range_pseasonality <- range(bunn_z_clusters_current$pseasonality)
range_diurnal <- range(bunn_z_clusters_current$diurnal_range)

# extract coordinates by year
suitability_current <- all_climate_z %>% filter(year == 2000)
suitability_50 <- all_climate_z %>% filter(year == 2050)
suitability_70 <- all_climate_z %>% filter(year == 2070)

# CURRENT suitable terrain

suitability_current = suitability_current %>% filter(between(tmax, min(range_tmax), max(range_tmax)))
suitability_current = suitability_current %>% filter(between(tmin, min(range_tmin), max(range_tmin)))
suitability_current = suitability_current %>% filter(between(precipitation, min(range_precip), max(range_precip)))
suitability_current = suitability_current %>% filter(between(diurnal_range, min(range_diurnal), max(range_diurnal)))
suitability_current = suitability_current %>% filter(between(pwarmest, min(range_pwarmest), max(range_pwarmest)))
suitability_current = suitability_current %>% filter(between(pseasonality, min(range_pseasonality), max(range_pseasonality)))

# rebind to climate variables needed for finalisation of suitability
suitability_current_values <- all_climate_data[,c(1:4,9,13,15:22)] %>% filter(year==2000) %>% right_join(suitability_current, by = c("x","y"))
suitability_current_values = suitability_current_values%>% filter(pquarter.x >= 30)
suitability_current_values = suitability_current_values%>% filter(avgtmp.x <= 260)
suitability_current_values = suitability_current_values%>% filter(avgtmp.x >= 170)
suitability_current_values = suitability_current_values%>% filter(precipitation.x >= 1000)
suitability_current_values = suitability_current_values%>% filter(precipitation.x <= 2000)

# map check
# ggplot() + geom_point(data = suitability_current_values, aes(x = x, y = y, alpha = .5)) + ggtitle("Current Suitable Coffee Production Area") 

# reform suitability_current to include only points that fit suitability conditions
suitability_current_values = suitability_current_values %>% select("x","y") 
suitability_current = suitability_current %>% right_join(suitability_current_values, by = c("x","y"))

# 2050 suitable terrain
suitability_50 = suitability_50 %>% filter(between(tmax, min(range_tmax), max(range_tmax)))
suitability_50 = suitability_50 %>% filter(between(tmin, min(range_tmin), max(range_tmin)))
suitability_50 = suitability_50 %>% filter(between(precipitation, min(range_precip), max(range_precip)))
suitability_50 = suitability_50 %>% filter(between(diurnal_range, min(range_diurnal), max(range_diurnal)))
suitability_50 = suitability_50 %>% filter(between(pwarmest, min(range_pwarmest), max(range_pwarmest)))
suitability_50 = suitability_50 %>% filter(between(pseasonality, min(range_pseasonality), max(range_pseasonality)))

# rebind to climate variables needed for finalisation of suitability
suitability_50_values <- all_climate_data[,c(1:4,9,13,15:22)] %>% filter(year==2050) %>% right_join(suitability_50, by = c("x","y"))
suitability_50_values = suitability_50_values%>% filter(pquarter.x >= 30)
suitability_50_values = suitability_50_values%>% filter(avgtmp.x <= 260)
suitability_50_values = suitability_50_values%>% filter(avgtmp.x >= 170)
suitability_50_values = suitability_50_values%>% filter(precipitation.x >= 1000)
suitability_50_values = suitability_50_values%>% filter(precipitation.x <= 2000)

# map check
# ggplot() + geom_point(data = suitability_50_values, aes(x = x, y = y, alpha = .5)) + ggtitle("Current Suitable Coffee Production Area") 

# reform suitability_current to include only points that fit suitability conditions
suitability_50_values = suitability_50_values %>% select("x","y") 
suitability_50 = suitability_50 %>% right_join(suitability_50_values, by = c("x","y"))

# 2070 suitable terrain

suitability_70 = suitability_70 %>% filter(between(tmax, min(range_tmax), max(range_tmax)))
suitability_70 = suitability_70 %>% filter(between(tmin, min(range_tmin), max(range_tmin)))
suitability_70 = suitability_70 %>% filter(between(precipitation, min(range_precip), max(range_precip)))
suitability_70 = suitability_70 %>% filter(between(diurnal_range, min(range_diurnal), max(range_diurnal)))
suitability_70 = suitability_70 %>% filter(between(pwarmest, min(range_pwarmest), max(range_pwarmest)))
suitability_70 = suitability_70 %>% filter(between(pseasonality, min(range_pseasonality), max(range_pseasonality)))

# rebind to climate variables needed for finalisation of suitability
suitability_70_values <- all_climate_data[,c(1:4,9,13,15:22)] %>% filter(year==2070) %>% right_join(suitability_70, by = c("x","y"))
suitability_70_values = suitability_70_values%>% filter(pquarter.x >= 30)
suitability_70_values = suitability_70_values%>% filter(avgtmp.x <= 260)
suitability_70_values = suitability_70_values%>% filter(avgtmp.x >= 170)
suitability_70_values = suitability_70_values%>% filter(precipitation.x >= 1000)
suitability_70_values = suitability_70_values%>% filter(precipitation.x <= 2000)

# map check
# ggplot() + geom_point(data = suitability_70_values, aes(x = x, y = y, alpha = .5)) + ggtitle("Current Suitable Coffee Production Area") 

# reform suitability_current to include only points that fit suitability conditions
suitability_70_values = suitability_70_values %>% select("x","y") 
suitability_70 = suitability_70 %>% right_join(suitability_70_values, by = c("x","y"))



# ASSIGN CLUSTERS WITH MODEL 


predicted_clusters_current <- predict(model, suitability_current) %>% as.data.frame #current
names(predicted_clusters_current) <- "AEZ"
predicted_clusters_current = cbind(as.data.frame(suitability_current), as.data.frame(predicted_clusters_current))

predicted_clusters_50 <- predict(model, suitability_50) %>% as.data.frame #2050
names(predicted_clusters_50) <- "AEZ"
predicted_clusters_50 = cbind(as.data.frame(suitability_50), as.data.frame(predicted_clusters_50))

predicted_clusters_70 <- predict(model, suitability_70) %>% as.data.frame #2070
names(predicted_clusters_70) <- "AEZ"
predicted_clusters_70 = cbind(as.data.frame(suitability_70), as.data.frame(predicted_clusters_70))

# ### extract major arabica regions
brazil_area <- brazil_all %>% select("X","Y", "area", "NAME_ADM1")
brazil_area = brazil_area %>% group_by(X,Y) %>% mutate(area = sum(area))
brazil_area$max <- 1000
brazil_area = brazil_area %>% unique %>% mutate(area = area/max) 
names(brazil_area)[1] <- "x"
names(brazil_area)[2] <- "y"
brazil_area = brazil_area[,-5]

regions_only_current = predicted_clusters_current %>% left_join(brazil_area,by = c("x","y"))
regions_only_50 = predicted_clusters_50 %>% left_join(brazil_area,by = c("x","y"))
regions_only_70 = predicted_clusters_70 %>% left_join(brazil_area,by = c("x","y"))

regions <- c("Minas Gerais","Espirito Santo","Sao Paulo","Bahia","Rondonia")
regions_only_current = regions_only_current %>% filter(NAME_ADM1 %in% regions)
regions_only_50 = regions_only_50 %>% filter(NAME_ADM1 %in% regions)
regions_only_70 = regions_only_70 %>% filter(NAME_ADM1 %in% regions)

######### ================= FINAL MAPPING OF TOTAL SUITABLE CROPLAND ================= ##########
# current area
AEZ1_current <- predicted_clusters_current %>% filter(AEZ == 1)
AEZ2_current <- predicted_clusters_current %>% filter(AEZ == 2)
AEZ3_current <- predicted_clusters_current %>% filter(AEZ == 3)
AEZ4_current <- predicted_clusters_current %>% filter(AEZ == 4)

#2050 area
AEZ1_50 <- predicted_clusters_50 %>% filter(AEZ == 1)
AEZ2_50 <- predicted_clusters_50 %>% filter(AEZ == 2)
AEZ3_50 <- predicted_clusters_50 %>% filter(AEZ == 3)
AEZ4_50 <- predicted_clusters_50 %>% filter(AEZ == 4)

#2070 area
AEZ1_70 <- predicted_clusters_70 %>% filter(AEZ == 1)
AEZ2_70 <- predicted_clusters_70 %>% filter(AEZ == 2)
AEZ3_70 <- predicted_clusters_70 %>% filter(AEZ == 3)
AEZ4_70 <- predicted_clusters_70 %>% filter(AEZ == 4)


# map clusters current area 
gadm_plot(BRA) + geom_point(data = AEZ1_current, aes(x = x, y = y, color = "Cluster 1", alpha = .05)) + 
  geom_point(data = AEZ2_current, aes(x = x, y = y, color = "Cluster 2", alpha = .05)) + 
  geom_point(data = AEZ4_current, aes(x = x, y = y, color = "Cluster 4", alpha = .05)) + 
  geom_point(data = AEZ3_current, aes(x = x, y = y, color = "Cluster 3", alpha = .05)) + 
  ggtitle("Current Suitable Cropland for Arabica Production") 

# map clusters 2050
gadm_plot(BRA)  + geom_point(data = AEZ1_50, aes(x = x, y = y, color = "Cluster 1", alpha = .05)) + 
  geom_point(data = AEZ2_50, aes(x = x, y = y, color = "Cluster 2", alpha = .05)) + 
  geom_point(data = AEZ4_50, aes(x = x, y = y, color = "Cluster 4", alpha = .05)) + 
  geom_point(data = AEZ3_50, aes(x = x, y = y, color = "Cluster 3", alpha = .05)) + 
  ggtitle("Suitable Cropland for Arabica Production - 2050") 

# map clusters 2070
gadm_plot(BRA) + geom_point(data = AEZ1_70, aes(x = x, y = y, color = "Cluster 1", alpha = .05)) + 
  geom_point(data = AEZ2_70, aes(x = x, y = y, color = "Cluster 2", alpha = .05)) + 
  geom_point(data = AEZ4_70, aes(x = x, y = y, color = "Cluster 4", alpha = .05)) + 
  geom_point(data = AEZ3_70, aes(x = x, y = y, color = "Cluster 3", alpha = .05)) + 
  ggtitle("Suitable Cropland for Arabica Production - 2070") 

######### ================= FINAL MAPPING OF MAJOR REGIONS SUITABLE CROPLAND ================= ##########

region_AEZ1_current <- regions_only_current %>% filter(AEZ == 1)
region_AEZ2_current <- regions_only_current %>% filter(AEZ == 2)
region_AEZ3_current <- regions_only_current %>% filter(AEZ == 3)
region_AEZ4_current <- regions_only_current %>% filter(AEZ == 4)

region_AEZ1_50 <- regions_only_50 %>% filter(AEZ == 1)
region_AEZ2_50 <- regions_only_50 %>% filter(AEZ == 2)
region_AEZ3_50 <- regions_only_50 %>% filter(AEZ == 3)
region_AEZ4_50 <- regions_only_50 %>% filter(AEZ == 4)

region_AEZ1_70 <- regions_only_70 %>% filter(AEZ == 1)
region_AEZ2_70 <- regions_only_70 %>% filter(AEZ == 2)
region_AEZ3_70 <- regions_only_70 %>% filter(AEZ == 3)
region_AEZ4_70 <- regions_only_70 %>% filter(AEZ == 4)

# map current clusters 
gadm_plot(BRA) + geom_point(data = region_AEZ1_current, aes(x = x, y = y, color = "Cluster 1", alpha = .05)) + 
  geom_point(data = region_AEZ2_current, aes(x = x, y = y, color = "Cluster 2", alpha = .2)) + 
  geom_point(data = region_AEZ4_current, aes(x = x, y = y, color = "Cluster 4", alpha = .2)) + 
  ggtitle("Current Suitable Coffee Production Area in Major Regions") 

# map 2050 clusters
gadm_plot(BRA) + geom_point(data = region_AEZ1_50, aes(x = x, y = y, color = "Cluster 1", alpha = .05)) + 
  geom_point(data = region_AEZ2_50, aes(x = x, y = y, color = "Cluster 2", alpha = .2)) + 
  geom_point(data = region_AEZ4_50, aes(x = x, y = y, color = "Cluster 4", alpha = .2)) + 
  geom_point(data = region_AEZ3_50, aes(x = x, y = y, color = "Cluster 3", alpha = .2)) +  
  ggtitle("Suitable Coffee Production Area 2050 in Major Regions") 

# map 2070 clusters
gadm_plot(BRA) + geom_point(data = region_AEZ1_70, aes(x = x, y = y, color = "Cluster 1", alpha = .05)) + 
  geom_point(data = region_AEZ2_70, aes(x = x, y = y, color = "Cluster 2", alpha = .2)) + 
  geom_point(data = region_AEZ4_70, aes(x = x, y = y, color = "Cluster 4", alpha = .2)) + 
  geom_point(data = region_AEZ3_70, aes(x = x, y = y, color = "Cluster 3", alpha = .2)) + 
  ggtitle("Suitable Coffee Production Area 2070 in Major Regions") 

########## Calculating change in area

# total cropland
AEZ1_current %>% nrow # cluster 1
AEZ1_50 %>% nrow
AEZ1_70 %>% nrow

AEZ2_current %>% nrow # cluster 2
AEZ2_50 %>% nrow
AEZ2_70 %>% nrow

AEZ3_current %>% nrow # cluster 3
AEZ3_50 %>% nrow
AEZ3_70 %>% nrow

AEZ4_current %>% nrow # cluster 4
AEZ4_50 %>% nrow
AEZ4_70 %>% nrow

# major regions
region_AEZ1_current %>% nrow # cluster 1
region_AEZ1_50 %>% nrow
region_AEZ1_70 %>% nrow

region_AEZ2_current %>% nrow # cluster 2
region_AEZ2_50 %>% nrow
region_AEZ2_70 %>% nrow

region_AEZ3_current %>% nrow # cluster 3
region_AEZ3_50 %>% nrow
region_AEZ3_70 %>% nrow

region_AEZ4_current %>% nrow # cluster 4
region_AEZ4_50 %>% nrow
region_AEZ4_70 %>% nrow