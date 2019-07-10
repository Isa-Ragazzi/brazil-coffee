library(raster)

#manually load in jan temp data, need to improve this
temp1 <- raster("wc2.0_30s_tavg_01.tif")

#manually add regions, need to improve
region <- c("Bahia","Minas Gerais","Piaui")
lat <- c(12.58, 18.51, 7.72)
lon <- c(41.70, 44.55, 42.73)

#create dataframe
df <- data.frame(region,lon,lat,row.names=region)

#create plot colors
tempcol <- colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))

#plot brazil
plot(temp1, xlim=c(-80,-15), ylim=c(-50,10), col=tempcol(100))