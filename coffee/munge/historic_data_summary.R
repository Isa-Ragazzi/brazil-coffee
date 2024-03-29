# Extracting CRU Climate data: CRU TS v4.01
# Complete guide available at: http://www.benjaminbell.co.uk

# pre processing
years <- 2011:2017
years.2001 <- 2001:2010
years.1991 <- 1991:2000
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_order <- read.csv("/Users/isabellaragazzi/brazil-coffee/coffee/data/month_order.csv")

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


tmp <- brick("/Users/isabellaragazzi/brazil-coffee/coffee/data/cru_ts4.02.2011.2017.tmp.dat 2.nc", varname="tmp") # Mean monthly temperature
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

# remove unnecessary intermediary datasets
remove(tmn_values, tmn.2001, tmn, tmn.1991,tmx, tmx.2001, tmx.1991, pre, pre.2001, pre.1991,mg_pre, pre.sites,
       pre.sites.loc,pre.sites.years, pre.1991.sites, pre.1991.sites, pre.1991.sites.loc, pre.1991.sites.years,
       pre.2001, pre.2001.sites,pre.2001.sites.loc,pre.2001.sites.years, precipitation_values, precipitation.1991_values,
       precipitation.2001_values, tmx, tmx_values, tmx.1991_values, tmp, tmp.1991, tmp.2001, tmp_values, tmp.1991_values, tmp.2001_values, tmp.sites,
       tmp.1991.sites, tmp.1991.sites.loc, tmp.1991.sites.years, tmp.2001.sites, tmp.2001.sites.loc, tmp.2001.sites.years,
       temp_total, tmx.1991.sites, tmx.1991.sites.loc, tmx.1991.sites.years, tmx.2001.sites, tmx.2001.sites.loc,
       tmx.2001.sites.years, tmx.2001_values, tmn.sites, tmn.sites.loc, tmn.sites.year, tmn.sites.years, tmx.sites.years, 
       tmp.sites.years, tmx.sites, tmx.sites.loc,tmx.sites.year,tmp.sites.loc, tmp.sites.year, tmn_values, tmn.1991_values,
       tmn.1991.sites, tmn.1991.sites.loc, tmn.1991.sites.years, tmn.2001.sites, tmn.2001.sites.loc, tmn.2001.sites.years, tmn.2001_values, tmax, tmin)


all_climate_pt_1 <- all_climate[1:162000,]
all_climate_pt_2 <- all_climate[162001:324000,]
write.csv(all_climate_pt_1, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/all_climate_pt_1.csv", row.names = FALSE)
write.csv(all_climate_pt_2, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/all_climate_pt_2.csv", row.names = FALSE)

# Fizz, I can't upload the OG files because they are too large, but I've saved the output (all_climate_pt_1 and pt_2) 
# in the "data" folder
## run this to recombine: 
# all_climate <- rbind(all_climate_pt_1,all_climate_pt_2)

# for loop to assign regions
temp_output <- data_frame()

for(y in region_names) {
  y <- gadm_subset(BRA, level = 1, regions = y)
  temp_region_r <- crop(locations, y$spdf@bbox)
  region_points <- temp_region_r@coords
  temp_region_df <- as.data.frame(region_points)
  temp_region_df$region <- y$spdf$NAME_1
  temp <- left_join(temp_region_df, all_climate, by = c("x","y"))
  temp <- temp %>% separate(year, c("year", "month"), sep= "_")
  temp_output <- rbind(temp_output, temp)
  remove(temp_region_r, region_points, temp_region_df, temp)
}

# finding months with more than 40 mm rain vs through with under 40mm (drought conditions) - takes a long time to run
# temp_output2 <- data_frame()
temp_output1 <- temp_output %>% mutate(drought = NA)
for(row in 1:nrow(temp_output1)) {
  if(is.na(temp_output1$precip[[row]])) {
    temp_output1$drought[[row]] <- NA
  } else if (temp_output1$precip[[row]] > 40) { 
    temp_output1$drought[[row]] <- 1
  } else
  {temp_output1$drought[[row]] <- 0}
}

# calculating percent of locations in each region under drought annually
temp_output2 <- temp_output1
temp_output2$total <- 1
all_locations <- temp_output2
all_locations = all_locations %>% group_by(region, year) %>% filter(!is.na(drought)) %>% mutate(total = sum(total))

all_drought <- all_locations %>% group_by(region, year) %>% mutate(drought = sum(drought))
all_drought <- all_drought %>% mutate(drought = total-drought)
all_drought = all_drought %>% mutate(drought_percent = drought/total)
all_drought = all_drought %>% dplyr::select("region", "total","drought", "drought_percent")
all_drought = unique(all_drought)


# ordering the data chronologically
temp_output2 = as.data.frame(temp_output2)
temp_output2 = temp_output2 %>% mutate(drought_percent = (total - nodroughtcount)/total)
temp_output2 = temp_output2 %>% select(-c(nodroughtcount,total))

temp_output2 = temp_output2 %>% merge(month_order, by = "month")

# make ID columns to assure chronological order by region, year, month
temp_output2 <- temp_output2 %>% 
                mutate(regionID = group_indices(., region), yearID = group_indices(., year), monthID = month_order)

# yearly summary by region
year_sum <- temp_output1 %>%  group_by(region, year, month) %>% mutate(totalprecip = mean(precip))
annual_summary <- year_sum %>%  group_by(region, year) %>% summarise(avgtmp = mean(avgtmp, na.rm = TRUE), 
                                                                              tmax = mean(tmax, na.rm = TRUE),
                                                                              tmin = mean(tmin, na.rm = TRUE),
                                                                              avgprecip = mean(precip, na.rm = TRUE),
                                                                              totalprecip = sum(totalprecip, na.rm = TRUE),
                                                                              pmax = max(precip, na.rm = TRUE),
                                                                              pmin = min(precip, na.rm = TRUE))
annual_summary = as.data.frame(annual_summary)
# joining data on drought by region for annual summary by region
historic_sum <- left_join(annual_summary, all_drought, by = c("region","year"))

# write.csv to data file
write.csv(historic_sum, file = "/Users/isabellaragazzi/brazil-coffee/coffee/data/clean/historic_sum.csv", row.names = FALSE)

# manually change historic sum regions to deal with accent marks, added year 
historic_sum_fixed <- read_csv("brazil-coffee/coffee/data/clean/historic_sum_fixed.csv")
historic_sum_fixed = historic_sum_fixed[,c(1:13)]


# Additional monthly summary for each region - not needed in ultimate output
monthly_sum <- temp_output2 %>%  group_by(region, year, month) %>% summarise(avgtmp = mean(avgtmp, na.rm = TRUE), 
                                                                             tmax = mean(tmax, na.rm = TRUE),
                                                                             tmin = mean(tmin, na.rm = TRUE),
                                                                             avgprecip = mean(precip, na.rm = TRUE),
                                                                             totalprecip = sum(precip, na.rm = TRUE),
                                                                             pmax = max(precip, na.rm = TRUE),
                                                                             pmin = min(precip, na.rm = TRUE),
                                                                             nodroughtcount = sum(drought),
                                                                             total = sum(total))

