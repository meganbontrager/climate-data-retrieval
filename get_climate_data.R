# libraries ---------------------------------------------------------------

library(tidyverse)
library(sp)
library(ncdf4) # for reading climate files (.nc)
library(mosaic) # for derived factor
library(cowplot) # for making multi-panel plots



# 1. compile PRISM data from multiple files and reformat ------------------

# put all your prism files in a folder called "data_prism"
# as an example I downloaded two 15 year chunks for your first 500 locations

each_data = list.files(pattern = "PRISM", path = "data_prism/")
# make a list of all .csv files in prism directory

# for each file in the list, read it in as a csv, then bind all of these together and put the result into a new object, all_data
# read in all the files
all_data = lapply(str_c("data_prism/", each_data), read.csv, skip = 10) %>% 
  # bind all the dataframes together
  bind_rows() %>% 
  # separate the date column into month and year
  separate(Date, into = c("year", "month"), sep = "-") %>% 
  # make these into "padded" columns with 0s in from of sinle digit months for merging
  mutate(year = as.numeric(year), month = as.numeric(month), month_pad = str_pad(month, 2, pad = "0"), clim_date = as.numeric(paste0(year, month_pad))) %>% 
  # drop unnecessary columns
  select(-month_pad) %>%
  # rename columns
  rename(specimen_number = Name, longitude_dd = Longitude, latitude_dd = Latitude, elev_m = Elevation..m., ppt_mm = ppt..mm., tmin = tmin..degrees.C., tave = tmean..degrees.C., tmax = tmax..degrees.C.)
# elevation data here was inferred by prism based on the lat/longs

# make sure new names look good
head(all_data)
summary(all_data)

# do we have a complete time series? (once you have all data this should make a nice even histogram)
ggplot(all_data, aes(year)) + geom_histogram(bins = 122)

write.csv(all_data, "data_prism/prism_climate_tall.csv", row.names = FALSE)

# specimen_number longitude latitude elev_m year month ppt_mm tmin tave tmax clim_date
#        SD241782 -120.6667  36.5839    331 1951     1  31.26  1.9  7.7 13.5    195101
#        SD241782 -120.6667  36.5839    331 1951     2  37.41  2.2  9.3 16.4    195102

# 2. reformat climateNA data ----------------------------------------------

# I didn't plug your data into climateNA because I don't have the program on my laptop, so this may need some adjusting if you use it
# this assumes you've run your data through climateNA with the specimen number in the ID1 column

# climate subsets
climna = read_csv("data_climatena/file.csv") %>% rename(specimen_number = ID1, year = Year)

# ake subset dataframes for each climate variable of interest
climna_tave = climna %>%
  dplyr::select(specimen_number, year, Tave01:Tave12)
climna_ppt = climna %>%
  dplyr::select(specimen_number, year, PPT01:PPT12)
climna_tmin = climna %>%
  dplyr::select(specimen_number, year, Tmin01:Tmin12)
climna_tmax = climna %>%
  dplyr::select(specimen_number, year, Tmax01:Tmax12)
climna_cmd = climna %>%
  dplyr::select(specimen_number, year, CMD01:CMD12)

# turn these into tall dataframes

tave_tall = climna_tave %>%
  # make rows for each month
  gather(month_pre, tave, Tave01:Tave12) %>% 
  # separate the row names into "Tave" (omit) and the numeric month at the 4th character
  separate(month_pre, 4, into = c("omit", "month")) %>% 
  mutate(month = as.numeric(month)) %>% 
  dplyr::select(-omit)

# same for other dataframes
ppt_tall = climna_ppt %>% 
  gather(month_pre, ppt, PPT01:PPT12) %>% 
  separate(month_pre, 3, into = c("omit", "month")) %>% 
  mutate(month = as.numeric(month)) %>% 
  dplyr::select(-omit)

tmin_tall = climna_tmin %>% 
  gather(month_pre, tmin, Tmin01:Tmin12) %>% 
  separate(month_pre, 4, into = c("omit", "month")) %>% 
  mutate(month = as.numeric(month)) %>% 
  dplyr::select(-omit)

tmax_tall = climna_tmax %>% 
  gather(month_pre, tmax, Tmax01:Tmax12) %>% 
  separate(month_pre, 4, into = c("omit", "month")) %>% 
  mutate(month = as.numeric(month)) %>% 
  dplyr::select(-omit)

cmd_tall = climna_cmd %>% 
  gather(month_pre, cmd, CMD01:CMD12) %>% 
  separate(month_pre, 3, into = c("omit", "month")) %>% 
  mutate(month = as.numeric(month)) %>% 
  dplyr::select(-omit)

# join these all together
join1 = left_join(tave_tall, ppt_tall)
join2 = left_join(join1, tmin_tall)
join3 = left_join(join2, tmax_tall)

# make a nice climate date column
climna_tall = left_join(join3, cmd_tall) %>% 
  mutate(month_pad = str_pad(month, 2, pad = "0")) %>% 
  mutate(clim_date = paste0(year, month_pad)) %>% 
  dplyr::select(-month_pad)

write.csv(climna_tall, "data_climatena/climatena_tall.csv", row.names = FALSE)



# 3. extract flint data ---------------------------------------------------

# read in data
allA = read_csv(file = "Latitude and Longitude data Lahari_sheet2.csv") %>% 
  # keep just the necessary columns
  dplyr::select(specimen_number = CollectionNumber, elev_m = `Elevation in m`, latitude = LatitudeDecimal, longitude = LongitudeDecimal)

allB = read_csv(file = "Latitude and Longitude data Lahari_sheet3.csv") %>% 
  # separate elevation number from units
  separate(elevation, into = c("elev_m", "units"), sep = " ") %>% 
  # keep just the necessary columns
  dplyr::select(specimen_number, elev_m, latitude, longitude)
allB$elev_m = as.numeric(allB$elev_m)

all = bind_rows(allA, allB) %>% 
  # keep only unique combinations
  unique()

# define projection of flint data
flint_prj = '+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' 

# are any rows missing location data? if so, remove
all1 = filter(all, !is.na(latitude), !is.na(longitude))
# make duplicate lat/long columns that will remain in decimal degrees
all1$latitude_dd = all1$latitude
all1$longitude_dd = all1$longitude
# designate latitude and longitude columns as coordinates
coordinates(all1) = ~longitude + latitude
# define originaal projection (unprojected)
proj4string(all1) = CRS("+proj=longlat +ellps=WGS84")
# transform locations from original projection to be consistent with the flint data
all_t = spTransform(all1, CRS(flint_prj)); plot(all_t)
# make data frame from spatial points object, omit "optional" column
all_p = data.frame(all_t) %>% dplyr::select(-optional)


# climatic water deficit 

# read in climate file
# these files are really big, can be downloaded from here: https://cida.usgs.gov/thredds/CA-BCM-Catalog.html

cwd = nc_open("../../Projects/dimensions-fitness-climate/flint_data/HST_Monthly_cwd.nc")

# need to pull the indices of the nearest cell to each locality
# this should be just the same as above but running it again in case there's any difference in climate layers
for (i in 1:nrow(all_p)) {
  all_p$long_start[i] = which.min(abs(all_p$longitude[i] - cwd$dim$x$vals))
  all_p$lat_start[i] = which.min(abs(all_p$latitude[i] - cwd$dim$y$vals))
}

# did all rows get values?
summary(all_p$long_start); summary(all_p$lat_start)
# do they look similar to the map?
plot(all_p$long_start, all_p$lat_start)

# pull out the time variable; dates are formatted as number of days since 1895-10-01
t.nc = ncvar_get(cwd, "time") 
bcm.time.start = as.Date('1895-10-01')
# make time vector in a format that is consistent with specimen collection dates
t = t.nc + bcm.time.start

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





