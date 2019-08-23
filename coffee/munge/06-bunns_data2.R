library(foreign)
# I think these are global locations, definitely not only Brazil
arabica <- read.csv("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/Arabica.csv")
robusta <- read.csv("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/Robusta_global.csv")

# this has the agricultural codes, UF(region), county, and locality of Bunn's locations -- they don't match the excel files
dbf.AglomeradosSubnormais2010_Limites <- read.dbf("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.dbf")
# shapefiles with the same info as dbf
shp.AglomeradosSubnormais2010_Limites <- readOGR("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.shp")
shx.AglomeradosSubnormais2010_Limites <- readOGR("/Users/isabellaragazzi/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D5171835-04CF-43CC-98EA-86D92861DAFE/New folder/aglomerados_subnormais2010_limites/AglomeradosSubnormais2010_Limites.shx")

proj4string(shp.AglomeradosSubnormais2010_Limites) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
plot(shp.AglomeradosSubnormais2010_Limites)
plot(shx.AglomeradosSubnormais2010_Limites)

# get coordinates of coffee farms
dd <- shp.AglomeradosSubnormais2010_Limites
proj4string(dd) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
coordinates <- as.data.frame(coordinates(dd))
names(coordinates)[1] <- "x"
names(coordinates)[2] <- "y"
plot(dd)

## test ##

bunn_climate2 <- read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/bunn_climate2")
future_output2 <- read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/future_output2")

# bunn_climate2
library(fuzzyjoin)
library(dplyr)
#check how many farm coordinates conserved
coordinates %>% select("x","y") %>% unique %>% nrow()
bunn_climate2 = as.data.frame(bunn_climate2)
# closest match - 50 km 
delete <- geo_left_join(coordinates, bunn_climate2, by = c("x","y"), method = "geo", max_dist = 50, unit = "km")
# complete cases out of 6329 = 1457
delete %>% na.omit("x.y") %>% select("x.x","y.x") %>% unique %>% nrow()

# remaining coordinates - closest match 100 km
na.delete <- delete[is.na(delete$year),]
na.delete = na.delete[,1:2]
names(na.delete) <- c("x","y")
na.delete = unique(na.delete)
nrow(na.delete)
na.delete = geo_left_join(na.delete, bunn_climate2, by = c("x","y"), method = "geo", max_dist = 100, unit = "km")

# complete cases out of 6329 = 2921
na.delete %>% na.omit("x.y") %>% select("x.x","y.x") %>% unique %>% nrow()

# remaining coordinates - closest match 150 km
na.delete2 <- na.delete[is.na(na.delete$year),]
na.delete2 = na.delete2[,1:2]
names(na.delete2) <- c("x","y")
na.delete2 = unique(na.delete2)
nrow(na.delete2)
na.delete2 = geo_left_join(na.delete2, bunn_climate2, by = c("x","y"), method = "geo", max_dist = 150, unit = "km")
# complete cases out of 6329 = 1543
na.delete2 %>% na.omit("x.y") %>% select("x.x","y.x") %>% unique %>% nrow()

# remaining coordinates - closest match 200 km
na.delete3 <- na.delete2[is.na(na.delete2$year),]
na.delete3 = na.delete3[,1:2]
names(na.delete3) <- c("x","y")
na.delete3 = unique(na.delete3)
nrow(na.delete3)
na.delete3 = geo_left_join(na.delete3, bunn_climate2, by = c("x","y"), method = "geo", max_dist = 200, unit = "km")
# complete cases out of 6329 = 314
na.delete3 %>% na.omit("x.y") %>% select("x.x","y.x") %>% unique %>% nrow()

# combine all coordinates

delete = delete[complete.cases(delete$year),]
na.delete = na.delete[complete.cases(na.delete$year),]
na.delete2 = na.delete2[complete.cases(na.delete2$year),]
na.delete3 = na.delete3[complete.cases(na.delete3$year),]
delete = delete[,-3]
na.delete = na.delete[,-3]
na.delete2 = na.delete2[,-3]
na.delete3 = na.delete3[,-3]
names(delete)
names(na.delete)
names(na.delete2)
names(na.delete3)

combo_bunn <- rbind(delete,na.delete,na.delete2,na.delete3)
names(combo_bunn)
combo_bunn = combo_bunn[,c(-3,-4)]
names(combo_bunn)[1] <-"x"
names(combo_bunn)[2] <-"y"

# future climate 
bunn_coords <- combo_bunn[,c(1:2)] %>% unique
nrow(bunn_coords)
# closest match - 50 km 
future_output2 = as.data.frame(future_output2)
names(future_output2)
future <- geo_left_join(bunn_coords, future_output2, by = c("x","y"), method = "geo", max_dist = 50, unit = "km")

# complete cases out of 6235 = 6118
future %>% select("x.x","y.x") %>% unique.data.frame() %>% nrow()
future %>% na.omit(avgtmp) %>% select("x.x","y.x") %>% unique.data.frame() %>% nrow()
names(future)[1] <-"x"
names(future)[2] <-"y"
future = future %>% filter(complete.cases(future)==TRUE) %>% select(-c("x.y","y.y"))
future %>% select("x","y") %>% unique() %>% nrow()

#combine historic and future data
future_combo = future %>% select("x","y","year","avgtmp","tmax","tmin","precipitation",
                                 "pmax","pmin","drought", "model")
names(combo_bunn)
names(future_combo)
combo <- rbind(combo_bunn, future_combo)

# save files
write.csv(combo, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/combo")
# attempt to map coffee farm locations and avg temp for one year

zz <- combo %>% filter(year == "2070") %>% select("x","y","avgtmp")
plot(sort(zz$avgtmp), ylab='Avg Temp', las=1, xlab='Coordinates')

# quick map - coords go long:lat for SpatialPoints function x = long y = lat
library(sp)
zzsp <- SpatialPoints(zz[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
zzsp <- SpatialPointsDataFrame(zzsp, zz)
zzzsp <- as.data.frame(zzsp) %>% select("x","y","avgtmp")
sp::plot(zzsp)

# brasil <- map_data("world", region = "Brazil")
library(GADMTools)
choropleth(BRA, zz, value="avgtmp", breaks = "quantile", steps = 5, adm.join="x",
           legend = NULL, labels = NULL, palette=NULL,
           title="Average Temperature", subtitle = NULL, caption = NULL)

library(ggplot2)
futureee <- future_output2[,c(2:5)]
gadm_plot(BRA) + geom_point(data = zzzsp,  aes(x = x, y = y, colour = "red")) + geom_area(data = futureee, aes(x = x, y = y, colour = "red"))

# generate z-scores for variables in columns 4:10 using the scale() function

combo1 <- combo %>% filter(year %in% c(2050, 2070)) %>% mutate(avgtmp = avgtmp/10, tmax = tmax/10, tmin = tmin/10)
combo_tmp <- combo %>% filter(!year %in% c(2050, 2070))
combo_tmp <- combo_tmp %>% group_by(x,y) %>% mutate(avgtmp = mean(avgtmp), tmax = mean(tmax), tmin = min(tmin),
                                                        precipitation = mean(precipitation), pmax = mean(pmax), 
                                                        pmin = mean(pmin), drought = mean(drought)) #current
combo_tmp <- combo_tmp %>% select(-c("model","year"))
combo_tmp <-  unique(combo_tmp) %>% na.omit
combo_tmp[,c(3:9)] <- apply(combo_tmp[,c(3:9)], 2, function(x) scale(x,center = TRUE, scale = TRUE))


combo1 <- combo1 %>% group_by(x,y) %>% mutate(avgtmp = mean(avgtmp), tmax = mean(tmax), tmin = min(tmin),
                                                    precipitation = mean(precipitation), pmax = mean(pmax), 
                                                    pmin = mean(pmin), drought = mean(drought)) #future
combo2050 <- combo1 %>% filter(year ==2050) %>% select(-c("model","year"))
combo2050 <-  unique(combo2050) %>% na.omit
combo2050[,c(3:9)] <- apply(combo2050[,c(3:9)], 2, function(x) scale(x,center = TRUE, scale = TRUE))


# check correlations
res <- cor(combo2050[,c(3:9)]) # future
res <- cor(combo_tmp[,c(3:9)])  # current
#cluster analysis
combo_2050_cluster<- combo2050[,c(3:9)] %>% na.omit #future
combo_tmp1 = combo_tmp[,c(3:9)] # current
# Determine number of clusters - FUNCTION 
number_of_clusters <- function(df) {
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
}

number_of_clusters(combo_tmp1)
number_of_clusters(combo_2050_cluster)

# K-Means Cluster Analysis
fit2 <- kmeans(combo_tmp1, 4) # 4 cluster solution
fit <- kmeans(combo_2050_cluster, 4) # 4 cluster solution

# get cluster means 
aggregate(combo_tmp1,by=list(fit2$cluster),FUN=mean)

# append cluster assignment
combo3 <- data.frame(combo_2050_cluster, fit$cluster)

combo33 <- data.frame(combo_tmp1, fit2$cluster)
# summarize the AEZs
combo4 <- aggregate(combo_2050_cluster,by=list(fit$cluster),FUN=mean)
combo44 <- aggregate(combo_tmp1,by=list(fit2$cluster),FUN=mean)

aez_summary <- combo44
write.csv(aez_summary, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/aez_summary")
# 4 zones, 1 Hot - Dry, 2 Cool- Wet, 3 Cool - Dry, 4 Hot - Wet

# one way ANOVA We want to know if there is any significant difference between the average variables for each AEZ
library(dplyr)
# Compute the analysis of variance - FUNCTION
anova <- function(variable, column, data) {
  res.aov <- aov(variable ~ column, data = data)
  summary(res.aov) # Summary of the analysis
}
anova(combo33$avgtmp, combo33$fit2.cluster, combo33)
anova(combo33$tmax, combo33$fit2.cluster, combo33)
anova(combo33$tmin, combo33$fit2.cluster, combo33)
anova(combo33$precipitation, combo33$fit2.cluster, combo33)
anova(combo33$pmax, combo33$fit2.cluster, combo33)
anova(combo33$pmin, combo33$fit2.cluster, combo33)
anova(combo33$drought, combo33$fit2.cluster, combo33)

res.aov <- aov(avgtmp ~ fit.cluster, data = combo3)
summary(res.aov) # Summary of the analysis
res.aov <- aov(tmax ~ fit.cluster, data = combo3)
summary(res.aov) # Summary of the analysis
res.aov <- aov(tmin ~ fit.cluster, data = combo3)
summary(res.aov) # Summary of the analysis
res.aov <- aov(pmax ~ fit.cluster, data = combo3)
summary(res.aov) # Summary of the analysis
res.aov <- aov(pmin ~ fit.cluster, data = combo3)
summary(res.aov) # Summary of the analysis
res.aov <- aov(precipitation ~ fit.cluster, data = combo3)
summary(res.aov) # Summary of the analysis
res.aov <- aov(drought ~ fit.cluster, data = combo3)
summary(res.aov) # Summary of the analysis

## combo33 has AEZs CURRENT CLIMATE
# save output
write.csv(combo33, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/combo33")
read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/combo33")
## random forest - train data on current, test on future

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(combo33))

# Randomly order data
shuffled_fit <- combo33[rows,]

# create training and testing datasets
split <- round(nrow(shuffled_fit)*.8)
train <- shuffled_fit[1:split,]
test <- shuffled_fit[(split+1):(nrow(shuffled_fit)),] 

#initial model 
install.packages("caret")
library(caret)

(fmla <- as.factor(fit2.cluster) ~ avgtmp + tmax + tmin + pmax + pmin + drought)
model <- train(fmla, train, method = "ranger", trControl = trainControl(
                method = "cv", number = 5))
summary(model)

#prediction on test dataset
p <- as.data.frame(predict(model, test))
p2050 <- predict(model, combo2050[,c(3:9)])
p2050 <- as.data.frame(p2050)
names(p2050) <- "fit2.cluster"

clusters_2050 <- cbind(as.data.frame(combo2050), as.data.frame(p2050))

#calculate RMSE
error <- p - test[, "fit2.cluster"]
sqrt(mean(error^2))

## combine combo33 with combo_tmp to get coordinates
mapping_clusters <- left_join(combo_tmp, combo33, by = c("avgtmp","tmax","tmin","precipitation",
                                                         "pmax","pmin","drought"))
mapping_clusters = unique(mapping_clusters)
write.csv(mapping_clusters, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/mapping_clusters")

library(ggplot2)
library(GADMTools)
gadm_plot(BRA) + geom_point(data = mapping_clusters, aes(x = x, y = y, colour = factor(fit2.cluster))) + ggtitle("Current") # current

gadm_plot(BRA) + geom_point(data = clusters_2050, aes(x = x, y = y, colour = factor(fit2.cluster))) + ggtitle("Future 2050") #future


