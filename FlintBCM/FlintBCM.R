# Extract climate data from FlintBCM
# Created by Megan Bontrager, 14 Nov 2018
# Modified from code by Alec Chiono

# This script assumes you have downloaded the Flint .nc files from https://cida.usgs.gov/thredds/CA-BCM-Catalog.html
# These are large files (2-12 gb)



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # for plot aesthetics



# Prepare locality data ---------------------------------------------------

# Read in data and select necessary columns
locs = read.csv("sample_locations.csv") %>% 
  select(id, latitude, longitude) %>% 
  distinct() %>% # Remove duplicates
  mutate(latitude_aea = latitude, longitude_aea = longitude) # Make duplicate lat/long columns that will be transformed to math the Flint equal area projection

# Define projection of flint data
flint_prj = '+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' 

# Designate latitude and longitude columns as coordinates
coordinates(locs) = ~longitude_aea + latitude_aea

# Define original projection (which is unprojected)
proj4string(locs) = CRS("+proj=longlat +ellps=WGS84")

# Transform locations from original projection to be consistent with the flint data, view this
locs_t = spTransform(locs, CRS(flint_prj)); plot(locs_t)

# Make data frame from spatial points object, omit "optional" column
# This data frame has the original coordinates but also transformed coordinates
locs_p = data.frame(locs_t) %>% dplyr::select(-optional)

# We need to read in one climate file to set indices for locations
cwd = nc_open("../../flint-data/HST_Monthly_cwd.nc")

# Now we need to pull the indices of the nearest cell to each locality
# This pulls the x and y coordinates in the .nc file that are nearest to each location in our dataset
for (i in 1:nrow(locs_p)) {
  locs_p$long_start[i] = which.min(abs(locs_p$longitude_aea[i] - cwd$dim$x$vals))
  locs_p$lat_start[i] = which.min(abs(locs_p$latitude_aea[i] - cwd$dim$y$vals))
}

# Check that all rows got values
summary(locs_p$long_start); summary(locs_p$lat_start)

# Do they look similar to the map?
plot(locs_p$long_start, locs_p$lat_start) 
plot(locs_t)

# Make a time vector from the climate data file; dates are formatted as number of days since 1895-10-01
t = ncvar_get(cwd, "time") + as.Date('1895-10-01')



# Extract climate data ----------------------------------------------------

file_list = list.files(path = "../../flint-data/")

for (i in 1:length(file_list)){
  
}






### MULTIPLE FILES AT A TIME
setwd('E:/BCM Monthly')
dat_list <- list(NA) #create blank list to store data in
list_num <- 1 #dummy variable to keep track of position in list
all_nc_files <- c(grep('tmx',list.files(),value=T),grep('tmn',list.files(),value=T),grep('ppt',list.files(),value=T),grep('cwd',list.files(),value=T)) #list temp, precip, and cwd .nc files in directory
bcm_time_start <- as.Date('1895-10-01') # set start date for data
mv.bcm$collection_date_bcm <- as.Date(paste(format(mv.bcm$collection_date,'%Y'),format(mv.bcm$collection_date,'%m'),'01',sep='-'))

mv.bcm$gr_yr_start <- as.Date(paste(format(mv.bcm$collection_date,'%Y'),'09-01',sep='-'))
mv.bcm$gr_yr_start[as.numeric(format(mv.bcm$collection_date,'%m'))<=9] <-as.Date(paste(as.numeric(format(mv.bcm$collection_date[as.numeric(format(mv.bcm$collection_date,'%m'))<=9],'%Y'))-1,'09-01',sep='-')) 

for(j in all_nc_files){ #for each .nc file
  
  ncin <- nc_open(j) # open .nc file
  t_nc <- ncvar_get(ncin,'time') # retrieve vector of time
  date_nc <- t_nc+bcm_time_start # adjust time to appropriate Date format
  tcount <- length(date_nc) # retrieve count dates to supply in ncvar_get()
  full_var_name <- names(ncin$var) # retrieve name of climate data (eg "HST_Monthly_cwd"); must use to retrieve data from .nc file
  clim_var <- substr(full_var_name,nchar(full_var_name)-2,nchar(full_var_name)) #shorten variable name to 3 letter code (eg 'cwd'); use to store in dataframe
  
  
  for(i in 1:nrow(mv.bcm)){ # for each population
    
    # find grid that population is located in by finding nearest lon and lat
    lonstart <- which.min(abs(mv.bcm$longitude[i]-ncin$dim$x$vals))
    latstart <- which.min(abs(mv.bcm$latitude[i]-ncin$dim$y$vals))
    
    # find time span from sept 1 of that plant's growing season until collection date
    
    tstart <- which(date_nc==mv.bcm$gr_yr_start[i])
    tend <- which(date_nc==mv.bcm$collection_date_bcm[i])
    tcount <- tend+1-tstart
    date_start_end <- date_nc[tstart:tend]
    
    # retrieve value for climate data across time for grid in which population exists
    value <- ncvar_get(ncin,full_var_name,start=c(lonstart, latstart,1),count=c(1,1,tcount)) 
    
    
    tmp_data <- data.frame(mv.bcm[i,],clim_var,date=date_start_end,value) # compile population info, which variable, date, and value into one data.frame
    dat_list[[list_num]] <- tmp_data #put dataframe into dat list
    list_num <- list_num+1 #adjust dummy variable for new location in list
  }
  nc_close(ncin) #close nc file
  
} # may get 50+ warnings that says 'row names were found from a short variable and have been discarded'; do not worry about it

bcm_data <- do.call(rbind.data.frame,dat_list) #compile list into one dataframe

bcm_data_final <- bcm_data[,c('specimen_number','taxon_name','collection_date','clim_var','date','value')]

colnames(bcm_data_final)[colnames(bcm_data_final)=='date'] <- 'clim_date'



# extract climate data time series for each locality
# start indicates the cell to begin in. 
# count indicates how far to extract in dimensions x, y, and time

# begin with an empty matrix
num_locs = nrow(all_p)
# num_locs =
cwd_mat = matrix(NA, nrow = num_locs, ncol = length(t))

colnames(cwd_mat) = paste0("X", as.character(t))

# this function pulls data for each year at each location and keeps trying the next one if it encounters an error

for (i in 1:num_locs) {
  cwd_mat[i,] = tryCatch(
    {
      ncvar_get(cwd, 'HST_Monthly_cwd', start = c(all_p$long_start[i], all_p$lat_start[i], 1), count = c(1, 1, length(t)))
    },
    error = function(cond) {
      message(paste("Error with location ", i))
      message(cond)
      return("error")
    },
    warning=function(cond) {
      message(paste("Warning with location ", i))
      message(cond)
      return("warning")
    },
    finally={
      message(paste("Processed location ", i))
    }
  )
}

# generate columns to merge back on
info = all_p %>% dplyr::select(specimen_number, latitude, longitude, latitude_dd, longitude_dd)

cwd_df = cbind(info, as.data.frame(cwd_mat))
cwd_df[1:20, 1:40]

# reshape to tall format (this allows for easy filtering to year of collection etc.)
# if you get a warning message about NAs, make sure you're omitting all covariates merged on in the cbind() step above
cwd_tall = gather(cwd_df, key = "date", value = "cwd", -specimen_number, -latitude, -longitude, -latitude_dd, -longitude_dd) %>% 
  mutate(climate_year = as.numeric(substr(date, 2, 5)),
         climate_month = as.numeric(substr(date, 7, 8))) %>% 
  dplyr::select(-date)
summary(cwd_tall)

cwd_norm = cwd_tall %>% 
  filter(climate_year >=1960, climate_year < 1990) %>% 
  group_by(specimen_number, climate_year) %>% 
  summarize(cwd_annual_sum = sum(cwd)) %>% 
  group_by(specimen_number) %>% 
  summarize(cwd_annual_mean = mean(cwd_annual_sum))


# write out in wide format
write.csv(cwd_df, "flint_extract/cwd_wide.csv", row.names = FALSE)

# write out in tall format
write.csv(cwd_tall, "flint_extract/cwd_tall.csv", row.names = FALSE)



# snowpack ----------------------------------------------------------------

# these sections modified from Alec's code in Gremer lab box
# read in climate file
pck = nc_open("flint_data/HST_Monthly_pck.nc")

# need to pull the indices of the nearest cell to each locality
for (i in 1:nrow(all_p)) {
  all_p$long_start[i] = which.min(abs(all_p$longitude[i] - pck$dim$x$vals))
  all_p$lat_start[i] = which.min(abs(all_p$latitude[i] - pck$dim$y$vals))
}

summary(all_p$long_start); summary(all_p$lat_start)
plot(all_p$long_start, all_p$lat_start)

# pull out the time variable; dates are formatted as number of days since 1895-10-01
t.nc = ncvar_get(pck, "time") 
bcm.time.start = as.Date('1895-10-01')
# make time vector in a format that is consistent with specimen collection dates
t = t.nc + bcm.time.start

# extract climate data time series for each locality
# start indicates the cell to begin in. 
# count indicates how far to extract in dimensions x, y, and time

# begin with an empty matrix
num_locs = nrow(all_p)
pck_mat = matrix(NA, nrow = num_locs, ncol = length(t))

# turn dates into column names and make friendly by replacing hyphens with dots
colnames(pck_mat) = paste0("X", gsub("-", ".", as.character(t)))

# this function pulls data for each year at each location and keeps trying the next one if it encounters an error
# it shouldn't return errors if the .nc files are in good shape, so probably not cnecessary to wrap it in tryCatch
# but when I first downloaded the files I think they were corrupted, so some locations were returning errors
# takes 10 minutes on my old macbook
for (i in 1:num_locs) {
  pck_mat[i,] = tryCatch(
    {
      ncvar_get(pck, 'HST_Monthly_pck', start = c(all_p$long_start[i], all_p$lat_start[i], 1), count = c(1, 1, length(t)))
    },
    error = function(cond) {
      message(paste("Error with location ", i))
      message(cond)
      return("error")
    },
    warning=function(cond) {
      message(paste("Warning with location ", i))
      message(cond)
      return("warning")
    },
    finally={
      message(paste("Processed location ", i))
    }
  )
}

# make vectors of useful variables to merge on
specimen_number = all_p$specimen_number; length(specimen_number)
collection_year = all_p$year; length(collection_year)
collection_month = all_p$month; length(collection_month)
lat = all_p$latitude; length(lat)
long = all_p$longitude; length(long)
lat_dd = all_p$latitude_dd; length(lat_dd)
long_dd = all_p$longitude_dd; length(long_dd)
elevation_m = all_p$elevation_m; length(elevation_m)

pck_df = cbind(specimen_number, collection_year, collection_month, lat, long, lat_dd, long_dd, elevation_m, as.data.frame(pck_mat))
pck_df[1:20, 1:40]

# are these locations outside the geographic coverage of the dataset?
# column is arbitrary
(which(is.na(pck_df$X1991.01.01), TRUE))

ggplot(pck_df, aes(x = long, y = lat, color = is.na(X1896.01.01))) +
  geom_point()
# nope, they're in areas that should be covered
# need to revisit this, are other variables also na for these locations?

# reshape to tall format (this allows for easy filtering to year of collection etc.)
# if you get a warning message about NAs, make sure you're omitting all covariates merged on in the cbind() step above
pck_tall = gather(pck_df, key = "date", value = "pck", -specimen_number, -collection_year, -collection_month, -lat, -long, -lat_dd, -long_dd, -elevation_m) %>% 
  mutate(climate_year = as.numeric(substr(date, 2, 5)),
         climate_month = as.numeric(substr(date, 7, 8))) %>% 
  dplyr::select(-date)
summary(pck_tall)

# why are there negative snowpack values?
# which specimens have negative means?
pck_tall %>% filter(pck < 0) %>% group_by(specimen_number, lat_dd, long_dd) %>% summarize(n = n(), mean_pck = mean(pck)) %>% unique() %>% print(n = Inf)

# make a dataframe of normals
pck_means = pck_tall %>% 
  filter(climate_year > 1950 & climate_year < 1981) %>% 
  group_by(specimen_number, lat_dd, long_dd, elevation_m, climate_year) %>% 
  summarize(summed_pck = sum(pck)) %>% 
  group_by(specimen_number, lat_dd, long_dd, elevation_m) %>% 
  summarize(mean_summed_pck = mean(summed_pck))

ggplot() +
  geom_point(data = filter(pck_means, mean_summed_pck >= 0), aes(x = long_dd, y = lat_dd), color = "red") +
  geom_point(data = filter(pck_means, mean_summed_pck < 0), aes(x = long_dd, y = lat_dd), color = "blue")

# these are all near lassen
# I think they might just have the wrong sign?
ggplot() +
  geom_point(data = pck_means, aes(x = mean_summed_pck, y = elevation_m))
# must be

pck_df[,9:1388] = abs(pck_df[,9:1388])

# write out in wide format
write.csv(pck_df, "specimen_climate/pck_wide.csv", row.names = FALSE)

pck_tall$pck = abs(pck_tall$pck)

pck_tall %>% filter(is.na(pck)) %>% group_by(specimen_number) %>% summarize(n = n()) %>% unique() %>% print(n = 76)
# not sure why these are missing data. revisit later.
# attempted to diagnose using raster() instead of ncdf. no luck, same locationa are missing, but code is at the end.

# write out in tall format
write.csv(pck_tall, "specimen_climate/pck_tall.csv", row.names = FALSE)



# maximum temperature ----------------------------------------

# read in climate file
tmx = nc_open("flint_data/HST_Monthly_tmx.nc")

# need to pull the indices of the nearest cell to each locality
# this should be just the same as above but running it again in case there's any difference in climate layers
for (i in 1:nrow(all_p)) {
  all_p$long_start[i] = which.min(abs(all_p$longitude[i] - tmx$dim$x$vals))
  all_p$lat_start[i] = which.min(abs(all_p$latitude[i] - tmx$dim$y$vals))
}

summary(all_p$long_start); summary(all_p$lat_start)
plot(all_p$long_start, all_p$lat_start)

# pull out the time variable; dates are formatted as number of days since 1895-10-01
t.nc = ncvar_get(tmx, "time") 
bcm.time.start = as.Date('1895-10-01')
# make time vector in a format that is consistent with specimen collection dates
t = t.nc + bcm.time.start

# extract climate data time series for each locality
# start indicates the cell to begin in. 
# count indicates how far to extract in dimensions x, y, and time

# begin with an empty matrix
num_locs = nrow(all_p)
# num_locs =
tmx_mat = matrix(NA, nrow = num_locs, ncol = length(t))

colnames(tmx_mat) = paste0("X", as.character(t))

# this function pulls data for each year at each location and keeps trying the next one if it encounters an error

for (i in 1:num_locs) {
  tmx_mat[i,] = tryCatch(
    {
      ncvar_get(tmx, 'HST_Monthly_tmx', start = c(all_p$long_start[i], all_p$lat_start[i], 1), count = c(1, 1, length(t)))
    },
    error = function(cond) {
      message(paste("Error with location ", i))
      message(cond)
      return("error")
    },
    warning=function(cond) {
      message(paste("Warning with location ", i))
      message(cond)
      return("warning")
    },
    finally={
      message(paste("Processed location ", i))
    }
  )
}

tmx_df = cbind(specimen_number, collection_year, collection_month, lat, long, lat_dd, long_dd, elevation_m, as.data.frame(tmx_mat))
tmx_df[1:20, 1:40]

# are these locations outside the geographic coverage of the dataset?
# column is arbitrary
(which(is.na(tmx_df$X1991.01.01), TRUE))

ggplot(tmx_df, aes(x = long, y = lat, color = is.na(X1896.01.01))) +
  geom_point()
# nope, they're in areas that should be covered
# need to revisit this, are other variables also na for these locations?

# reshape to tall format (this allows for easy filtering to year of collection etc.)
# if you get a warning message about NAs, make sure you're omitting all covariates merged on in the cbind() step above
tmx_tall = gather(tmx_df, key = "date", value = "tmx", -specimen_number, -collection_year, -collection_month, -lat, -long, -lat_dd, -long_dd, -elevation_m) %>% 
  mutate(climate_year = as.numeric(substr(date, 2, 5)),
         climate_month = as.numeric(substr(date, 7, 8))) %>% 
  dplyr::select(-date)
summary(tmx_tall)

# write out in wide format
write.csv(tmx_df, "specimen_climate/tmx_wide.csv", row.names = FALSE)

# write out in tall format
write.csv(tmx_df, "specimen_climate/tmx_tall.csv", row.names = FALSE)



# minimum temperature ----------------------------------------

# read in climate file
tmn = nc_open("flint_data/HST_Monthly_tmn.nc")

# need to pull the indices of the nearest cell to each locality
# this should be just the same as above but running it again in case there's any difference in climate layers
for (i in 1:nrow(all_p)) {
  all_p$long_start[i] = which.min(abs(all_p$longitude[i] - tmn$dim$x$vals))
  all_p$lat_start[i] = which.min(abs(all_p$latitude[i] - tmn$dim$y$vals))
}

summary(all_p$long_start); summary(all_p$lat_start)
plot(all_p$long_start, all_p$lat_start)

# pull out the time variable; dates are formatted as number of days since 1895-10-01
t.nc = ncvar_get(tmn, "time") 
bcm.time.start = as.Date('1895-10-01')
# make time vector in a format that is consistent with specimen collection dates
t = t.nc + bcm.time.start

# extract climate data time series for each locality
# start indicates the cell to begin in. 
# count indicates how far to extract in dimensions x, y, and time

# begin with an empty matrix
num_locs = nrow(all_p)
# num_locs =
tmn_mat = matrix(NA, nrow = num_locs, ncol = length(t))

colnames(tmn_mat) = paste0("X", as.character(t))

# this function pulls data for each year at each location and keeps trying the next one if it encounters an error

for (i in 1:num_locs) {
  tmn_mat[i,] = tryCatch(
    {
      ncvar_get(tmn, 'HST_Monthly_tmn', start = c(all_p$long_start[i], all_p$lat_start[i], 1), count = c(1, 1, length(t)))
    },
    error = function(cond) {
      message(paste("Error with location ", i))
      message(cond)
      return("error")
    },
    warning=function(cond) {
      message(paste("Warning with location ", i))
      message(cond)
      return("warning")
    },
    finally={
      message(paste("Processed location ", i))
    }
  )
}

tmn_df = cbind(specimen_number, collection_year, collection_month, lat, long, lat_dd, long_dd, elevation_m, as.data.frame(tmn_mat))
tmn_df[1:20, 1:40]

# are these locations outside the geographic coverage of the dataset?
# column is arbitrary
(which(is.na(tmn_df$X1991.01.01), TRUE))

ggplot(tmn_df, aes(x = long, y = lat, color = is.na(X1896.01.01))) +
  geom_point()
# nope, they're in areas that should be covered
# need to revisit this, are other variables also na for these locations?

# reshape to tall format (this allows for easy filtering to year of collection etc.)
# if you get a warning message about NAs, make sure you're omitting all covariates merged on in the cbind() step above
tmn_tall = gather(tmn_df, key = "date", value = "tmn", -specimen_number, -collection_year, -collection_month, -lat, -long, -lat_dd, -long_dd, -elevation_m) %>% 
  mutate(climate_year = as.numeric(substr(date, 2, 5)),
         climate_month = as.numeric(substr(date, 7, 8))) %>% 
  dplyr::select(-date)
summary(tmn_tall)

# write out in wide format
write.csv(tmn_df, "specimen_climate/tmn_wide.csv", row.names = FALSE)

# write out in tall format
write.csv(tmn_df, "specimen_climate/tmn_tall.csv", row.names = FALSE)



# precipitation ----------------------------------------

# read in climate file
ppt = nc_open("flint_data/HST_Monthly_ppt.nc")

# need to pull the indices of the nearest cell to each locality
# this should be just the same as above but running it again in case there's any difference in climate layers
for (i in 1:nrow(all_p)) {
  all_p$long_start[i] = which.min(abs(all_p$longitude[i] - ppt$dim$x$vals))
  all_p$lat_start[i] = which.min(abs(all_p$latitude[i] - ppt$dim$y$vals))
}

summary(all_p$long_start); summary(all_p$lat_start)
plot(all_p$long_start, all_p$lat_start)

# pull out the time variable; dates are formatted as number of days since 1895-10-01
t.nc = ncvar_get(ppt, "time") 
bcm.time.start = as.Date('1895-10-01')
# make time vector in a format that is consistent with specimen collection dates
t = t.nc + bcm.time.start

# extract climate data time series for each locality
# start indicates the cell to begin in. 
# count indicates how far to extract in dimensions x, y, and time

# begin with an empty matrix
num_locs = nrow(all_p)
# num_locs =
ppt_mat = matrix(NA, nrow = num_locs, ncol = length(t))

colnames(ppt_mat) = paste0("X", as.character(t))

# this function pulls data for each year at each location and keeps trying the next one if it encounters an error

for (i in 1:num_locs) {
  ppt_mat[i,] = tryCatch(
    {
      ncvar_get(ppt, 'HST_Monthly_ppt', start = c(all_p$long_start[i], all_p$lat_start[i], 1), count = c(1, 1, length(t)))
    },
    error = function(cond) {
      message(paste("Error with location ", i))
      message(cond)
      return("error")
    },
    warning=function(cond) {
      message(paste("Warning with location ", i))
      message(cond)
      return("warning")
    },
    finally={
      message(paste("Processed location ", i))
    }
  )
}

ppt_df = cbind(specimen_number, collection_year, collection_month, lat, long, lat_dd, long_dd, elevation_m, as.data.frame(ppt_mat))
ppt_df[1:20, 1:40]

# are these locations outside the geographic coverage of the dataset?
# column is arbitrary
(which(is.na(ppt_df$X1991.01.01), TRUE))

ggplot(ppt_df, aes(x = long, y = lat, color = is.na(X1896.01.01))) +
  geom_point()
# nope, they're in areas that should be covered
# need to revisit this, are other variables also na for these locations?

# reshape to tall format (this allows for easy filtering to year of collection etc.)
# if you get a warning message about NAs, make sure you're omitting all covariates merged on in the cbind() step above
ppt_tall = gather(ppt_df, key = "date", value = "ppt", -specimen_number, -collection_year, -collection_month, -lat, -long, -lat_dd, -long_dd, -elevation_m) %>% 
  mutate(climate_year = as.numeric(substr(date, 2, 5)),
         climate_month = as.numeric(substr(date, 7, 8))) %>% 
  dplyr::select(-date)
summary(ppt_tall)

# write out in wide format
write.csv(ppt_df, "specimen_climate/ppt_wide.csv", row.names = FALSE)

# write out in tall format
write.csv(ppt_df, "specimen_climate/ppt_tall.csv", row.names = FALSE)



# potential evapotranspiration ----------------------------------------

# read in climate file
pet = nc_open("flint_data/HST_Monthly_pet.nc")

# need to pull the indices of the nearest cell to each locality
# this should be just the same as above but running it again in case there's any difference in climate layers
for (i in 1:nrow(all_p)) {
  all_p$long_start[i] = which.min(abs(all_p$longitude[i] - pet$dim$x$vals))
  all_p$lat_start[i] = which.min(abs(all_p$latitude[i] - pet$dim$y$vals))
}

summary(all_p$long_start); summary(all_p$lat_start)
plot(all_p$long_start, all_p$lat_start)

# pull out the time variable; dates are formatted as number of days since 1895-10-01
t.nc = ncvar_get(pet, "time") 
bcm.time.start = as.Date('1895-10-01')
# make time vector in a format that is consistent with specimen collection dates
t = t.nc + bcm.time.start

# extract climate data time series for each locality
# start indicates the cell to begin in. 
# count indicates how far to extract in dimensions x, y, and time

# begin with an empty matrix
num_locs = nrow(all_p)
# num_locs =
pet_mat = matrix(NA, nrow = num_locs, ncol = length(t))

colnames(pet_mat) = paste0("X", as.character(t))

# this function pulls data for each year at each location and keeps trying the next one if it encounters an error

for (i in 1:num_locs) {
  pet_mat[i,] = tryCatch(
    {
      ncvar_get(pet, 'HST_Monthly_pet', start = c(all_p$long_start[i], all_p$lat_start[i], 1), count = c(1, 1, length(t)))
    },
    error = function(cond) {
      message(paste("Error with location ", i))
      message(cond)
      return("error")
    },
    warning=function(cond) {
      message(paste("Warning with location ", i))
      message(cond)
      return("warning")
    },
    finally={
      message(paste("Processed location ", i))
    }
  )
}

pet_df = cbind(specimen_number, collection_year, collection_month, lat, long, lat_dd, long_dd, elevation_m, as.data.frame(pet_mat))
pet_df[1:20, 1:40]

# are these locations outside the geographic coverage of the dataset?
# column is arbitrary
(which(is.na(pet_df$X1991.01.01), TRUE))

ggplot(pet_df, aes(x = long, y = lat, color = is.na(X1896.01.01))) +
  geom_point()
# nope, they're in areas that should be covered
# need to revisit this, are other variables also na for these locations?

# reshape to tall format (this allows for easy filtering to year of collection etc.)
# if you get a warning message about NAs, make sure you're omitting all covariates merged on in the cbind() step above
pet_tall = gather(pet_df, key = "date", value = "pet", -specimen_number, -collection_year, -collection_month, -lat, -long, -lat_dd, -long_dd, -elevation_m) %>% 
  mutate(climate_year = as.numeric(substr(date, 2, 5)),
         climate_month = as.numeric(substr(date, 7, 8))) %>% 
  dplyr::select(-date)
summary(pet_tall)

# write out in wide format
write.csv(pet_df, "specimen_climate/pet_wide.csv", row.names = FALSE)

# write out in tall format
write.csv(pet_df, "specimen_climate/pet_tall.csv", row.names = FALSE)



# actual evapotranspiration ----------------------------------------

# read in climate file
aet = nc_open("flint_data/HST_Monthly_aet.nc")

# need to pull the indices of the nearest cell to each locality
# this should be just the same as above but running it again in case there's any difference in climate layers
for (i in 1:nrow(all_p)) {
  all_p$long_start[i] = which.min(abs(all_p$longitude[i] - aet$dim$x$vals))
  all_p$lat_start[i] = which.min(abs(all_p$latitude[i] - aet$dim$y$vals))
}

summary(all_p$long_start); summary(all_p$lat_start)
plot(all_p$long_start, all_p$lat_start)

# pull out the time variable; dates are formatted as number of days since 1895-10-01
t.nc = ncvar_get(aet, "time") 
bcm.time.start = as.Date('1895-10-01')
# make time vector in a format that is consistent with specimen collection dates
t = t.nc + bcm.time.start

# extract climate data time series for each locality
# start indicates the cell to begin in. 
# count indicates how far to extract in dimensions x, y, and time

# begin with an empty matrix
num_locs = nrow(all_p)
# num_locs =
aet_mat = matrix(NA, nrow = num_locs, ncol = length(t))

colnames(aet_mat) = paste0("X", as.character(t))

# this function pulls data for each year at each location and keeps trying the next one if it encounters an error

for (i in 1:num_locs) {
  aet_mat[i,] = tryCatch(
    {
      ncvar_get(aet, 'HST_Monthly_aet', start = c(all_p$long_start[i], all_p$lat_start[i], 1), count = c(1, 1, length(t)))
    },
    error = function(cond) {
      message(paste("Error with location ", i))
      message(cond)
      return("error")
    },
    warning=function(cond) {
      message(paste("Warning with location ", i))
      message(cond)
      return("warning")
    },
    finally={
      message(paste("Processed location ", i))
    }
  )
}

aet_df = cbind(specimen_number, collection_year, collection_month, lat, long, lat_dd, long_dd, elevation_m, as.data.frame(aet_mat))
aet_df[1:20, 1:40]

# are these locations outside the geographic coverage of the dataset?
# column is arbitrary
(which(is.na(aet_df$X1991.01.01), TRUE))

ggplot(aet_df, aes(x = long, y = lat, color = is.na(X1896.01.01))) +
  geom_point()
# nope, they're in areas that should be covered
# need to revisit this, are other variables also na for these locations?

# reshape to tall format (this allows for easy filtering to year of collection etc.)
# if you get a warning message about NAs, make sure you're omitting all covariates merged on in the cbind() step above
aet_tall = gather(aet_df, key = "date", value = "aet", -specimen_number, -collection_year, -collection_month, -lat, -long, -lat_dd, -long_dd, -elevation_m) %>% 
  mutate(climate_year = as.numeric(substr(date, 2, 5)),
         climate_month = as.numeric(substr(date, 7, 8))) %>% 
  dplyr::select(-date)
summary(aet_tall)

# write out in wide format
write.csv(aet_df, "specimen_climate/aet_wide.csv", row.names = FALSE)

# write out in tall format
write.csv(aet_df, "specimen_climate/aet_tall.csv", row.names = FALSE)



# snowfall ----------------------------------------

# read in climate file
snw = nc_open("flint_data/HST_Monthly_snw.nc")

# need to pull the indices of the nearest cell to each locality
# this should be just the same as above but running it again in case there's any difference in climate layers
for (i in 1:nrow(all_p)) {
  all_p$long_start[i] = which.min(abs(all_p$longitude[i] - snw$dim$x$vals))
  all_p$lat_start[i] = which.min(abs(all_p$latitude[i] - snw$dim$y$vals))
}

summary(all_p$long_start); summary(all_p$lat_start)
plot(all_p$long_start, all_p$lat_start)

# pull out the time variable; dates are formatted as number of days since 1895-10-01
t.nc = ncvar_get(snw, "time") 
bcm.time.start = as.Date('1895-10-01')
# make time vector in a format that is consistent with specimen collection dates
t = t.nc + bcm.time.start

# extract climate data time series for each locality
# start indicates the cell to begin in. 
# count indicates how far to extract in dimensions x, y, and time

# begin with an empty matrix
num_locs = nrow(all_p)
# num_locs =
snw_mat = matrix(NA, nrow = num_locs, ncol = length(t))

colnames(snw_mat) = paste0("X", as.character(t))

# this function pulls data for each year at each location and keeps trying the next one if it encounters an error

for (i in 1:num_locs) {
  snw_mat[i,] = tryCatch(
    {
      ncvar_get(snw, 'HST_Monthly_snw', start = c(all_p$long_start[i], all_p$lat_start[i], 1), count = c(1, 1, length(t)))
    },
    error = function(cond) {
      message(paste("Error with location ", i))
      message(cond)
      return("error")
    },
    warning=function(cond) {
      message(paste("Warning with location ", i))
      message(cond)
      return("warning")
    },
    finally={
      message(paste("Processed location ", i))
    }
  )
}

snw_df = cbind(specimen_number, collection_year, collection_month, lat, long, lat_dd, long_dd, elevation_m, as.data.frame(snw_mat))
snw_df[1:20, 1:40]

# are these locations outside the geographic coverage of the dataset?
# column is arbitrary
(which(is.na(snw_df$X1991.01.01), TRUE))

ggplot(snw_df, aes(x = long, y = lat, color = is.na(X1896.01.01))) +
  geom_point()
# nope, they're in areas that should be covered
# need to revisit this, are other variables also na for these locations?

# reshape to tall format (this allows for easy filtering to year of collection etc.)
# if you get a warning message about NAs, make sure you're omitting all covariates merged on in the cbind() step above
snw_tall = gather(snw_df, key = "date", value = "snw", -specimen_number, -collection_year, -collection_month, -lat, -long, -lat_dd, -long_dd, -elevation_m) %>% 
  mutate(climate_year = as.numeric(substr(date, 2, 5)),
         climate_month = as.numeric(substr(date, 7, 8))) %>% 
  dplyr::select(-date)
summary(snw_tall)

# write out in wide format
write.csv(snw_df, "specimen_climate/snw_wide.csv", row.names = FALSE)

# write out in tall format
write.csv(snw_df, "specimen_climate/snw_tall.csv", row.names = FALSE)





