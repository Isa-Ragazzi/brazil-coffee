# Extracting CRU Climate data: CRU TS v4.01
# Complete guide available at: http://www.benjaminbell.co.uk

# pre processing
years <- 2011:2017
years.2001 <- 2001:2010
years.1991 <- 1991:2000
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  
# Load packages
library(ncdf4)
library(raster)
library(cruts)

# ========== PRECIPITATION FROM 1901 to 2016 ============== #
# Load the CRU TS precipitation dataset into R
pre <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.2011.2017.pre.dat.nc", varname="pre") # monthly precipitation 2011-2017
pre.2001 <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.2001.2010.pre.dat.nc", varname="pre") # monthly precipitation 2011-2017
pre.1991 <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.1991.2000.pre.dat.nc", varname="pre") # monthly precipitation 2011-2017

# just Brazil
brazil_pre <- crop(pre$X1901.01.16, bbox(BRA$spdf@bbox))
plot(brazil_pre)

# just Minas Gerais
mg_pre <- crop(pre$X1901.01.16, bbox(mg$spdf@bbox))
plot(mg_pre)

# data frame
brazil_r <- crop(r, bbox(BRA$spdf@bbox))
locations <- spsample(as(brazil_r@extent, 'SpatialPolygons'), n=1000, type="random")

precipitation_values <- raster:: extract(pre, locations)
pre.sites <- cbind.data.frame(coordinates(locations), precipitation_values)
pre.sites.years <- pre.sites[,c(-1,-2)]
names(pre.sites.years) <- paste(rep(years, each=12), rep(month, times=7), sep="_")
pre.sites.loc <- pre.sites[,c(1,2)]
pre.sites <- cbind(pre.sites.loc,pre.sites.years)
pre.sites <- tidyr::gather(pre.sites, key = "year", value = "precip", -c(1:2))

precipitation.2001_values <- raster:: extract(pre.2001, locations)
pre.2001.sites <- cbind.data.frame(coordinates(locations), precipitation.2001_values)
pre.2001.sites.years <- pre.2001.sites[,c(-1,-2)]
names(pre.2001.sites.years) <- paste(rep(years.2001, each=12), rep(month, times=7), sep="_")
pre.2001.sites.loc <- pre.2001.sites[,c(1,2)]
pre.2001.sites <- cbind(pre.2001.sites.loc,pre.2001.sites.years)
pre.2001.sites <- tidyr::gather(pre.2001.sites, key = "year", value = "precip", -c(1:2))

precipitation.1991_values <- raster:: extract(pre.1991, locations)
pre.1991.sites <- cbind.data.frame(coordinates(locations), precipitation.1991_values)
pre.1991.sites.years <- pre.1991.sites[,c(-1,-2)]
names(pre.1991.sites.years) <- paste(rep(years.1991, each=12), rep(month, times=7), sep="_")
pre.1991.sites.loc <- pre.1991.sites[,c(1,2)]
pre.1991.sites <- cbind(pre.1991.sites.loc,pre.1991.sites.years)
pre.1991.sites <- tidyr::gather(pre.1991.sites, key = "year", value = "precip", -c(1:2))

precip <- rbind(pre.sites, pre.2001.sites)
precip = rbind(precip, pre.1991.sites)
# ================ TEMPERATURE ================= #
# Load the CRU TS datasets into R

tmn <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.2011.2017.tmn.dat.nc", varname="tmn") # min monthly temperature
tmn.2001 <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.2001.2010.tmn.dat.nc", varname="tmn") # min monthly temperature 2001-2010
tmn.1991 <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.1991.2000.tmn.dat.nc", varname="tmn") # min monthly temperature 1991-2000

tmx <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.2011.2017.tmx.dat.nc", varname="tmx") # max monthly temperature 2011-2017
tmx.2001 <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.2001.2010.tmx.dat.nc", varname="tmx") # max monthly temperature 2001-2010
tmx.1991 <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.1991.2000.tmx.dat.nc", varname="tmx") # max monthly temperature 1991-2000


tmp <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/cru_ts4.02.2011.2017.tmp.dat 2.nc", varname="tmp") # Mean monthly temperature
tmp.2001 <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.2001.2010.tmp.dat.nc", varname="tmp") # 2001-2010 Mean monthly temperature
tmp.1991 <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.1991.2000.tmp.dat.nc", varname="tmp") # 1991-2000 Mean monthly temperature

# Extract climate data from the RasterBrick as a data.frame

tmn_values <- raster:: extract(tmn, locations)
tmn.sites <- cbind.data.frame(coordinates(locations), tmn_values) # min temperature
tmn.sites.years <- tmn.sites[,c(-1,-2)]
names(tmn.sites.years) <- paste(rep(years, each=12), rep(month, times=7), sep="_")
tmn.sites.loc <- tmn.sites[,c(1,2)]
tmn.sites <- cbind(tmn.sites.loc,tmn.sites.years)
tmn.sites <- tidyr::gather(tmn.sites, key = "year", value = "tmin", -c(1:2))

tmn.2001_values <- raster:: extract(tmn.2001, locations)
tmn.2001.sites <- cbind.data.frame(coordinates(locations), tmn.2001_values) # min temperature
tmn.2001.sites.years <- tmn.2001.sites[,c(-1,-2)]
names(tmn.2001.sites.years) <- paste(rep(years.2001, each=12), rep(month, times=7), sep="_")
tmn.2001.sites.loc <- tmn.2001.sites[,c(1,2)]
tmn.2001.sites <- cbind(tmn.2001.sites.loc,tmn.2001.sites.years)
tmn.2001.sites <- tidyr::gather(tmn.2001.sites, key = "year", value = "tmin", -c(1:2))

tmn.1991_values <- raster:: extract(tmn.1991, locations)
tmn.1991.sites <- cbind.data.frame(coordinates(locations), tmn.1991_values) # min temperature
tmn.1991.sites.years <- tmn.1991.sites[,c(-1,-2)]
names(tmn.1991.sites.years) <- paste(rep(years.1991, each=12), rep(month, times=7), sep="_")
tmn.1991.sites.loc <- tmn.1991.sites[,c(1,2)]
tmn.1991.sites <- cbind(tmn.1991.sites.loc,tmn.1991.sites.years)
tmn.1991.sites <- tidyr::gather(tmn.1991.sites, key = "year", value = "tmin", -c(1:2))

tmin <- rbind(tmn.1991.sites, tmn.2001.sites)
tmin <- rbind(tmn.sites,tmin)

tmx_values <- raster:: extract(tmx, locations)
tmx.sites <- cbind.data.frame(coordinates(locations), tmx_values) # max temperature
tmx.sites.years <- tmx.sites[,c(-1,-2)]
names(tmx.sites.years) <- paste(rep(years, each=12), rep(month, times=7), sep="_")
tmx.sites.loc <- tmx.sites[,c(1,2)]
tmx.sites <- cbind(tmx.sites.loc,tmx.sites.years)
tmx.sites <- tidyr::gather(tmx.sites, key = "year", value = "tmax", -c(1:2))

tmx.2001_values <- raster:: extract(tmx.2001, locations)
tmx.2001.sites <- cbind.data.frame(coordinates(locations), tmx.2001_values) # max temperature
tmx.2001.sites.years <- tmx.2001.sites[,c(-1,-2)]
names(tmx.2001.sites.years) <- paste(rep(years.2001, each=12), rep(month, times=7), sep="_")
tmx.2001.sites.loc <- tmx.2001.sites[,c(1,2)]
tmx.2001.sites <- cbind(tmx.2001.sites.loc,tmx.2001.sites.years)
tmx.2001.sites <- tidyr::gather(tmx.2001.sites, key = "year", value = "tmax", -c(1:2))

tmx.1991_values <- raster:: extract(tmx.1991, locations)
tmx.1991.sites <- cbind.data.frame(coordinates(locations), tmx.1991_values) # max temperature
tmx.1991.sites.years <- tmx.1991.sites[,c(-1,-2)]
names(tmx.1991.sites.years) <- paste(rep(years.1991, each=12), rep(month, times=7), sep="_")
tmx.1991.sites.loc <- tmx.1991.sites[,c(1,2)]
tmx.1991.sites <- cbind(tmx.1991.sites.loc,tmx.1991.sites.years)
tmx.1991.sites <- tidyr::gather(tmx.1991.sites, key = "year", value = "tmax", -c(1:2))

tmax <- rbind(tmx.sites, tmx.2001.sites)
tmax = rbind(tmax, tmx.1991.sites)

tmp_values <- raster:: extract(tmp, locations)
tmp.sites <- cbind.data.frame(coordinates(locations), tmp_values) # min temperature
tmp.sites.years <- tmp.sites[,c(-1,-2)]
names(tmp.sites.years) <- paste(rep(years, each=12), rep(month, times=7), sep="_")
tmp.sites.loc <- tmp.sites[,c(1,2)]
tmp.sites <- cbind(tmp.sites.loc,tmp.sites.years)
tmp.sites <- tidyr::gather(tmp.sites, key = "year", value = "avgtmp", -c(1:2))
#tmp.sites %>% !is.na(summary(tmp.sites$X2011.01.16 = mean(tmp.sites$X2011.01.16)))

tmp.2001_values <- raster:: extract(tmp.2001, locations)
tmp.2001.sites <- cbind.data.frame(coordinates(locations), tmp.2001_values) # avg temperature
tmp.2001.sites.years <- tmp.2001.sites[,c(-1,-2)]
names(tmp.2001.sites.years) <- paste(rep(years.2001, each=12), rep(month, times=10), sep="_")
tmp.2001.sites.loc <- tmp.2001.sites[,c(1,2)]
tmp.2001.sites <- cbind(tmp.2001.sites.loc,tmp.2001.sites.years)
tmp.2001.sites <- tidyr::gather(tmp.2001.sites, key = "year", value = "avgtmp", -c(1:2))

tmp.1991_values <- raster:: extract(tmp.1991, locations)
tmp.1991.sites <- cbind.data.frame(coordinates(locations), tmp.1991_values) # avg temperature
tmp.1991.sites.years <- tmp.1991.sites[,c(-1,-2)]
names(tmp.1991.sites.years) <- paste(rep(years.1991, each=12), rep(month, times=10), sep="_")
tmp.1991.sites.loc <- tmp.1991.sites[,c(1,2)]
tmp.1991.sites <- cbind(tmp.1991.sites.loc,tmp.1991.sites.years)
tmp.1991.sites <- tidyr::gather(tmp.1991.sites, key = "year", value = "avgtmp", -c(1:2))

temp_total <- rbind(tmp.sites,tmp.2001.sites)
temp_total <- rbind(temp_total, tmp.1991.sites)

all_climate <- temp_total %>% full_join(tmax, by=c("x","y","year"))
all_climate <- all_climate %>% full_join(tmin, by=c("x","y","year"))

all_climate <- all_climate %>% full_join(precip, by=c("x","y","year"))

## Fizz can you look into summarising each column in the "all climate" data by region? I can't 
## figure out how to group the lat/long points based on their region

