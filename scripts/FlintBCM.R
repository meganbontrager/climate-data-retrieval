# Extract climate data from FlintBCM
# Created by Megan Bontrager, 14 Nov 2018
# Modified from code by Alec Chiono

# This script assumes you have downloaded the Flint .nc files from https://cida.usgs.gov/thredds/CA-BCM-Catalog.html
# These are large files (2-12 gb)
# The below script assumes they are in a directory called flint-bcm that is in the same directory as this retrieval directory



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # for plot aesthetics
library(rgdal)
library(ncdf4)
library(magrittr)



# 1. Prepare locality data ---------------------------------------------------

# Read in data and select necessary columns
# Can toggle to file.choose to pull in your own climate data
locs = read.csv("sample_locations.csv") %>%
# locs = read.csv(file.choose()) %>%
  select(id, latitude, longitude) %>% 
  # Remove duplicates
  distinct() %>% 
  # Make duplicate lat/long columns that will be transformed to math the Flint equal area projection
  mutate(latitude_aea = latitude, longitude_aea = longitude) 

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
cwd = nc_open("../flint-bcm/HST_Monthly_cwd.nc")

# Now we need to pull the indices of the nearest cell to each locality
# This pulls the x and y coordinates in the .nc file that are nearest to each location in our dataset
for (i in 1:nrow(locs_p)) {
  locs_p$long_start[i] = which.min(abs(locs_p$longitude_aea[i] - cwd$dim$x$vals))
  locs_p$lat_start[i] = which.min(abs(locs_p$latitude_aea[i] - cwd$dim$y$vals))
}

# Check that all rows got values
summary(locs_p$long_start); summary(locs_p$lat_start)

# Do they look similar to the map? OK if they appear stretched.
plot(locs_p$long_start, locs_p$lat_start) 
plot(locs_t)

# Make a start date for the climate data file; dates are formatted as number of days since 1895-10-01
bcm_time_start <- as.Date('1895-10-01') # set start date for data



# 2. Extract climate data ----------------------------------------------------

# Make a list of climate files to extract from
file_list = list.files(path = "../flint-bcm", full.names = TRUE)

# Make an empty list to put the climate data in
climate_list = list()
  
# Now extract across all years and sites.
# This takes a while. You can make it faster by slimming down to the variables and timepoints you're interested in. But I would usually rather just wait for it to pull out all possibilities than have to re-run later when I realize I've forgotten something. 
# I imagine there is a faster way to do this. Let me know if you have ideas!

for (j in file_list){

  # Open first file
  ncin = nc_open(j) 
  
  # Get times from this file, format them as dates, and add them to the start date
  date_nc = ncvar_get(ncin,'time') + bcm_time_start 
  
  # Also format them as strings for 
  date_for_colnames = paste0("date_", str_sub(date_nc, 0, 4), "_", str_sub(date_nc, 6, 7))
  
  # Get 3-letter variable name from this layer
  full_var_name = names(ncin$var)
  var_name = str_sub(full_var_name, -3)
  
  # Make an empty data frame to put new info in
  temporary_data = data.frame(matrix(NA, nrow = length(locs_p$id), ncol = length(date_nc)))
  row.names(temporary_data) = locs_p$id
  colnames(temporary_data) = date_for_colnames
  
  # Now pull data for each population across the entire time series
  for(i in 1:nrow(locs_p)){ 

    # Retrieve values for climate data across time for grid cell nearest each population
    temporary_data[i,] = ncvar_get(ncin, full_var_name, start = c(locs_p$long_start[i], locs_p$lat_start[i], 1), count = c(1, 1, length(date_nc)))
  
    # Print counter
    if(i %% 10==0) {
      print(paste0(var_name, " site ", i))
    }
  }
  
  # Do some wrangling on this to make it tall
  temporary_data %<>% 
    rownames_to_column(var = "id") %>% 
    gather(date, value, -id) %>% 
    separate(date, into = c("drop", "clim_year", "clim_month")) %>% 
    dplyr::select(-drop)
  
  # Rename value column with variable name
  names(temporary_data)[4] = paste0(var_name)
  
  # Put this dataframe into a list
  climate_list[[j]] = temporary_data 
  
  # Close file
  nc_close(ncin) 
  
} 

# join together all the tall dataframes for all the variables
all_data = climate_list %>% reduce(left_join, by = c("id", "clim_year", "clim_month")) %>% 
  mutate(clim_year = as.numeric(clim_year), 
         clim_month = as.numeric(clim_month), 
         month_pad = str_pad(clim_month, 2, pad = "0"), 
         clim_date = as.numeric(paste0(clim_year, month_pad))) %>% 
  select(-month_pad) %>% 
  rename(tmin = tmn, tmax = tmx, ppt_mm = ppt) %>% 
  select(id, clim_year, clim_month, clim_date, everything())

write.csv(all_data, "data_tall/flintbcm_climate_tall.csv", row.names = FALSE)
