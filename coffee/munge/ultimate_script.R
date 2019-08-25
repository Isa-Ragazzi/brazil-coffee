library(raster)
library(raster)

# CURRENT CLIMATE BRAZIL
global_physical <- read.dbf("/Users/isabellaragazzi/Downloads/spam2010v1r0_global_phys_area.dbf/spam2010v1r0_global_physical-area_ta.dbf")
brazil_physical <- global_physical %>% filter(ISO3 == "BRA") %>% filter(ACOF_A != 0)

#filtering for brazil to find total physical crop area
brazil_all <- global_physical %>% filter(ISO3 == "BRA") %>% tidyr::gather(key = "crop", value = "area", 10:51, na.rm = TRUE) %>% 
  filter(area != 0)

brazil_all <- read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/brazil_all")

current_coords <- brazil_all %>% select("X","Y") %>% unique

### CURRENT CLIMATE 1970-2000 average
w = getData('worldclim', var='bio', res=10)

plot(w, "bio1")

# crop to current agricultural area
brazil_current <- crop(w, current_coords)
plot(brazil_current, "bio1")
coordinates(brazil_current)
points <- current_coords
values <- raster:: extract(brazil_current, current_coords)

# bind into df
df <- cbind.data.frame(points, values)
# check 
ggplot() + geom_point(data = df, aes(x = x, y = y))


## ============================== FUTURE CLIMATE ==================================== ##

BC_model <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=50)
BC_model70 <- getData('CMIP5', var='bio', res=10, rcp=85, model='BC', year=70)

# 2050 ===================crop to current agricultural area ==========
brazil_50 <- crop(BC_model, current_coords)
plot(brazil_50, 1)
coordinates(brazil_50)
points <- current_coords
values <- raster:: extract(brazil_50, current_coords)

# bind into df
df_50 <- cbind.data.frame(points, values)
# check 
ggplot() + geom_point(data = df_50, aes(x = x, y = y))


# 2070 =================== crop to current agricultural area ==========
brazil_70 <- crop(BC_model70, current_coords)
plot(brazil_70, 1)
coordinates(brazil_70)
points <- current_coords
values <- raster:: extract(brazil_70, current_coords)

# bind into df
df_70 <- cbind.data.frame(points, values)
# check 
ggplot() + geom_point(data = df_70, aes(x = x, y = y))

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
# rename Bio 1 to avgtmp
# rename Bio 5 to tmax
# rename Bio 6 to tmin
# rename Bio 13 ro pmax
# rename Bio 14 to pmin
# rename Bio 17 to pquarter
all_wc = all_wc %>% rename("avgtmp" = "1", "tmax" = "5", "tmin" = "6", "precipitation" = "12","pmax" = "13", "pmin" = "14", "pquarter" = "17")

# World Clim reports temperature in C* 10 so correct for that 
all_climate_data <- all_wc %>% select("x","y","year","avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "pquarter") %>% 
  mutate(avgtmp = avgtmp/10, tmax = tmax/10, tmin = tmin/10)

# create binary drought variable, 0 = drought, 1 = no drought
all_climate_data <- all_climate_data %>% mutate(drought = NA)
for(row in 1:nrow(all_climate_data)) {
  if(is.na(all_climate_data$pquarter[[row]])) {
    all_climate_data$drought[[row]] <- NA
  } else if (all_climate_data$pquarter[[row]] > 40) { 
    all_climate_data$drought[[row]] <- 1
  } else
  {all_climate_data$drought[[row]] <- 0}
}

### ================================================== BUNNS DATA ===================================###
# shapefiles with the same info as dbf
shp.AglomeradosSubnormais2010_Limites <- readOGR("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.shp")
# get coordinates of coffee farms
dd <- shp.AglomeradosSubnormais2010_Limites
proj4string(dd) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
coordinates <- as.data.frame(coordinates(dd))
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
bunn_70 <- crop(BC_model, coordinates)
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
all_bunn = all_bunn %>% rename("avgtmp" = "1", "tmax" = "5", "tmin" = "6", "precipitation" = "12","pmax" = "13", "pmin" = "14", "pquarter" = "17")

# World Clim reports temperature in C* 10 so correct for that 
all_bunn <- all_bunn %>% select("x","y","year","avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "pquarter") %>% 
  mutate(avgtmp = avgtmp/10, tmax = tmax/10, tmin = tmin/10)

# create binary drought variable, 0 = drought, 1 = no drought
all_bunn <- all_bunn %>% mutate(drought = NA)
for(row in 1:nrow(all_bunn)) {
  if(is.na(all_bunn$pquarter[[row]])) {
    all_bunn$drought[[row]] <- NA
  } else if (all_bunn$pquarter[[row]] > 40) { 
    all_bunn$drought[[row]] <- 1
  } else
  {all_bunn$drought[[row]] <- 0}
}


# save outputs
write.csv(all_bunn, file ="/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/all_bunn.csv")


# ====================================== PREPARING FOR THE MODEL ==============================================##
# generate z-scores for variables in columns 4:10 using the scale() function
bunn_z_current <- all_bunn %>% filter(year==2000) 
bunn_z_2050 <- all_bunn %>% filter(year==2050)
bunn_z_2070 <- all_bunn %>% filter(year==2070)
all_climate_z <- all_climate_data %>% na.omit

bunn_z_current[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")] <- apply(bunn_z_current[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")], 2, function(x) scale(x,center = TRUE, scale = TRUE))

bunn_z_2050[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")] <- apply(bunn_z_2050[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")], 2, function(x) scale(x,center = TRUE, scale = TRUE))

bunn_z_2070[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")] <- apply(bunn_z_2070[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")], 2, function(x) scale(x,center = TRUE, scale = TRUE))
all_climate_z[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")] <- apply(all_climate_z[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")], 2, function(x) scale(x,center = TRUE, scale = TRUE))

bunn_z_current = bunn_z_current %>% na.omit
bunn_z_2050 = bunn_z_2050 %>% na.omit
bunn_z_2070 = bunn_z_2070 %>% na.omit

# check correlations between variables, ignore pquarter bc that was just an intermediary variable for drought

res <- cor(all_climate_z[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin","drought")]) # all Brazilian climate
res <- cor(bunn_z_current[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")]) # current
res <- cor(bunn_z_2050[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")])  # 2050
res <- cor(combo2050[,c("avgtmp", "tmax", "tmin","precipitation","pmax","pmin", "drought")]) # 2070

# based on this, could remove "avgtmp" and "pmin" because of extremely high correlations with other variables

# Determine number of clusters - FUNCTION 
number_of_clusters <- function(df) {
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(df, 
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

bunn_z_cluster_analysis = bunn_z_current[,c("tmax", "avgtmp", "tmin","precipitation","pmax","pmin", "drought")] # current
number_of_clusters(bunn_z_current)

# finding = 4 clusters

# K-Means Cluster Analysis
fit <- kmeans(bunn_z_cluster_analysis, 4) # 4 cluster solution

# get cluster means (summary of the four climates) 
aggregate(bunn_z_cluster_analysis,by=list(fit$cluster),FUN=mean)

# append cluster assignment to df
bunn_z_clusters_current <- data.frame(bunn_z_current, fit$cluster)

# store AEZ mean summary
aezs <- aggregate(bunn_z_cluster_analysis,by=list(fit$cluster),FUN=mean)

write.csv(aezs, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/aezs")

# one way ANOVA - We want to know if there is any significant difference between the average variables for each AEZ

library(dplyr)
# Compute the analysis of variance - FUNCTION
anova <- function(variable, column, data) {
  res.aov <- aov(variable ~ column, data = data)
  summary(res.aov) # Summary of the analysis
}
anova(bunn_z_clusters_current$avgtmp, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$tmax, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$tmin, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$precipitation, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$pmax, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$pmin, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)
anova(bunn_z_clusters_current$drought, bunn_z_clusters_current$fit.cluster, bunn_z_clusters_current)

# all good bc all statistically significant, p<.01, high F-stat

## =========================== CREATING THE MODEL== RANDOM FOREST - train data on current, test on future===

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(bunn_z_clusters_current))

# Randomly order data
shuffled_fit <- bunn_z_clusters_current[rows,]

# create training and testing datasets
split <- round(nrow(shuffled_fit)*.8)
train <- shuffled_fit[1:split,]
test <- shuffled_fit[(split+1):(nrow(shuffled_fit)),] 

#initial model - Fizzah please look at this and see what you think, I was just messing around with the function 
# so it may not be the best model, if you know how to check the validity of the model please do so
# install.packages("caret")
library(caret)

(fmla <- as.factor(fit.cluster) ~ avgtmp + tmax + tmin + pmax + pmin + drought + precipitation)
model <- train(fmla, train, method = "ranger", trControl = trainControl(
  method = "cv", number = 5))
summary(model)


#prediction on test dataset
p <- predict(model, test)

#calculate RMSE for test data set --- this is not working....
real <- test[12]
p = as.numeric(p)
error <- p - real
sqrt(mean(error^2))

# predicting on new data
projected_clusters_50 <- predict(model, bunn_z_2050)
projected_clusters_50  = as.data.frame(projected_clusters_50)
names(projected_clusters_50) <- "fit.cluster"

clusters_2050 <- cbind(as.data.frame(bunn_z_2050), as.data.frame(projected_clusters_50))

## ========= MAPPING BUNN's DATA =================#
library(ggplot2)
library(GADMTools)

## mapping Bunn's coffee 
gadm_plot(BRA) + geom_point(data = bunn_z_clusters_current, aes(x = x, y = y, colour = factor(fit.cluster))) + ggtitle("Coffee Farm Locations and Climate") # current

gadm_plot(BRA) + geom_point(data = clusters_2050, aes(x = x, y = y, colour = factor(fit.cluster))) + ggtitle("Coffee Farm Future 2050") #future

#### ============================= DETERMINING COFFEE PRODUCTION SUITABILITY

# limiting agricultural area to areas that fit within the range of bunn's coffee locations
range_avgtmp <- range(bunn_z_clusters_current$avgtmp)
range_tmax <- range(bunn_z_clusters_current$tmax)
range_tmin <- range(bunn_z_clusters_current$tmin)
range_precip <- range(bunn_z_clusters_current$precipitation)
range_pmax <- range(bunn_z_clusters_current$pmax)
range_pmin <- range(bunn_z_clusters_current$pmin)
range_drought <- range(bunn_z_clusters_current$drought)


suitability_current <- all_climate_z %>% filter(year == 2000)
suitability_50 <- all_climate_z %>% filter(year == 2050)
suitability_70 <- all_climate_z %>% filter(year == 2070)

# current suitable terrain
suitability_current = suitability_current %>% filter(between(avgtmp, min(range_avgtmp), max(range_avgtmp)))
suitability_current = suitability_current %>% filter(between(tmax, min(range_tmax), max(range_tmax)))
suitability_current = suitability_current %>% filter(between(tmin, min(range_tmin), max(range_tmin)))
suitability_current = suitability_current %>% filter(between(precipitation, min(range_precip), max(range_precip)))
suitability_current = suitability_current %>% filter(between(pmax, min(range_pmax), max(range_pmax)))
suitability_current = suitability_current %>% filter(between(pmin, min(range_pmin), max(range_pmin)))
suitability_current = suitability_current %>% filter(between(drought, min(range_drought), max(range_drought)))

# 2050 suitable terrain
suitability_50 = suitability_50 %>% filter(between(avgtmp, min(range_avgtmp), max(range_avgtmp)))
suitability_50 = suitability_50 %>% filter(between(tmax, min(range_tmax), max(range_tmax)))
suitability_50 = suitability_50 %>% filter(between(tmin, min(range_tmin), max(range_tmin)))
suitability_50 = suitability_50 %>% filter(between(precipitation, min(range_precip), max(range_precip)))
suitability_50 = suitability_50 %>% filter(between(pmax, min(range_pmax), max(range_pmax)))
suitability_50 = suitability_50 %>% filter(between(pmin, min(range_pmin), max(range_pmin)))
suitability_50 = suitability_50 %>% filter(between(drought, min(range_drought), max(range_drought)))

# 2070 suitable terrain
suitability_70 = suitability_70 %>% filter(between(avgtmp, min(range_avgtmp), max(range_avgtmp)))
suitability_70 = suitability_70 %>% filter(between(tmax, min(range_tmax), max(range_tmax)))
suitability_70 = suitability_70 %>% filter(between(tmin, min(range_tmin), max(range_tmin)))
suitability_70 = suitability_70 %>% filter(between(precipitation, min(range_precip), max(range_precip)))
suitability_70 = suitability_70 %>% filter(between(pmax, min(range_pmax), max(range_pmax)))
suitability_70 = suitability_70 %>% filter(between(pmin, min(range_pmin), max(range_pmin)))
suitability_70 = suitability_70 %>% filter(between(drought, min(range_drought), max(range_drought)))

# assign clusters to suitable area 

predicted_clusters_current <- predict(model, suitability_current) %>% as.data.frame
names(predicted_clusters_current) <- "AEZ"
predicted_clusters_current = cbind(as.data.frame(suitability_current), as.data.frame(predicted_clusters_current))

predicted_clusters_50 <- predict(model, suitability_50) %>% as.data.frame
names(predicted_clusters_50) <- "AEZ"
predicted_clusters_50 = cbind(as.data.frame(suitability_50), as.data.frame(predicted_clusters_50))

predicted_clusters_70 <- predict(model, suitability_70) %>% as.data.frame
names(predicted_clusters_70) <- "AEZ"
predicted_clusters_70 = cbind(as.data.frame(suitability_70), as.data.frame(predicted_clusters_70))

######### ================= FINAL MAPPING ================= ##########
# current area
gadm_plot(BRA) + geom_point(data = predicted_clusters_current, aes(x = x, y = y, color = AEZ, alpha = 1/6)) + ggtitle("Current Suitable Coffee Production Area") 

# map clusters 2050
gadm_plot(BRA) + geom_point(data = predicted_clusters_50, aes(x = x, y = y, color = AEZ, alpha = 1/5)) + ggtitle("Suitable Coffee Production Area 2050") 

# map clusters 2070
gadm_plot(BRA) + geom_point(data = predicted_clusters_70, aes(x = x, y = y, color = AEZ, alpha = 1/5)) + ggtitle("Suitable Coffee Production Area 2070") 

