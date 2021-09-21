# Extract climate data from FlintBCM, recent years


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # for plot aesthetics
library(rgdal)
library(ncdf4)
library(magrittr)
library(raster)



# 1. Prepare locality data ---------------------------------------------------

# Read in data and select necessary columns
# Can toggle to file.choose to pull in your own climate data
# locs = read.csv("sample_locations.csv") %>%
locs = read.csv(file.choose()) %>%
  dplyr::select(id = site, latitude = decimalLatitude, longitude = decimalLongitude) %>% 
  # Remove duplicates
  distinct() %>% 
  # Make duplicate lat/long columns that will be transformed to match the Flint equal area projection
  mutate(latitude_aea = latitude, longitude_aea = longitude) %>% 
  filter(!is.na(latitude_aea))

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
cwd = nc_open("../../../../../Volumes/Dimensions1/flint_bcm_recent/cwd_wy2015.nc")

# Now we need to pull the indices of the nearest cell to each locality
# This pulls the x and y coordinates in the .nc file that are nearest to each location in our dataset
for (i in 1:nrow(locs_p)) {
  locs_p$long_start[i] = which.min(abs(locs_p$longitude_aea[i] - cwd$dim$x$vals))
  locs_p$lat_start[i] = which.min(abs(locs_p$latitude_aea[i] - cwd$dim$y$vals))
  locs_p$cell_long[i] = cwd$dim$x$vals[locs_p$long_start[i]]
  locs_p$cell_lat[i] = cwd$dim$y$vals[locs_p$lat_start[i]]
  locs_p$dist[i] = pointDistance(c(locs_p$longitude_aea[i], locs_p$latitude_aea[i]), 
                                 c(locs_p$cell_long[i], locs_p$cell_lat[i]), lonlat = FALSE) 
}

# Check that all rows got values
summary(locs_p$long_start); summary(locs_p$lat_start)

# If points are outside the range of BCM data, will still be assigned nearest point! Check how far out points are falling and filter out those outside the coverage range. 

summary(locs_p$cell_long - locs_p$longitude_aea)
summary(locs_p$cell_lat - locs_p$latitude_aea)
summary(locs_p$dist); hist(locs_p$dist, breaks = 50)

locs_p = locs_p %>% filter(dist < 200)
# Units are meters.

# Do they look similar to the map? OK if they appear stretched.
# why do lat starts appear flipped?
plot(locs_p$long_start, locs_p$lat_start) 
plot(locs_p$longitude_aea, locs_p$latitude_aea) 
plot(locs_t) # This includes points not filtered out

# Points inside the square grid of the data will still be included, but will get NAs extracted



# 2. Extract climate data ----------------------------------------------------

# Make a list of climate files to extract from
file_list = list.files(path = "../../../../../Volumes/Dimensions1/flint_bcm_recent", full.names = TRUE)

# Make an empty list to put the climate data in
climate_list = list()

for (j in 1:length(file_list)){
  
  # Open first file
  ncin = nc_open(file_list[j]) 
  
  file_year = paste(file_list[j]) %>% str_extract("[0-9]{4}") %>% as.numeric()
  file_month = 1:12
  # Month 1 = October of previous year!
  # Gets adjusted after data are out
  
  # Get 3-letter variable name from this layer
  var_name = names(ncin$var)
  
  # Make an empty data frame to put new info in
  temporary_data = data.frame(matrix(NA, nrow = length(locs_p$id), ncol = 12))
  row.names(temporary_data) = locs_p$id
  colnames(temporary_data) = file_month
  
  # Now pull data for each population across the entire time series
  for(i in 1:nrow(locs_p)){ 
    
    # Retrieve values for climate data across time for grid cell nearest each population
    temporary_data[i,] = ncvar_get(ncin, var_name, start = c(locs_p$long_start[i], locs_p$lat_start[i], 1), count = c(1, 1, length(file_month)))
    
    # Print counter
    if(i %% 10==0) {
      print(paste0(var_name, " site ", i))
    }
  }
  
  # Do some wrangling on this to make it tall
  temporary_data %<>% 
    rownames_to_column(var = "id") %>% 
    gather(month, value, -id) %>% 
    mutate(year = file_year,
           variable = tolower(var_name))
  
  # Put this dataframe into a list
  climate_list[[j]] = temporary_data 
  
  # Close file
  nc_close(ncin) 
  
} 


# join together all the tall dataframes for all the variables
all_data = climate_list %>% 
  reduce(bind_rows) %>% 
  pivot_wider(id_cols = c(id, month, year), names_from = variable, values_from = value) %>% 
  mutate(month = as.numeric(month),
         clim_month = if_else(month <= 3, month + 9, month - 3),
         clim_year = ifelse(month <= 3, year - 1, year),
         month_pad = str_pad(clim_month, 2, pad = "0"), 
         clim_date = as.numeric(paste0(clim_year, month_pad))) %>% 
  dplyr::select(-month_pad) %>% 
  dplyr::rename(tmin = tmn, tmax = tmx, ppt_mm = ppt) %>% 
  dplyr::select(id, clim_year, clim_month, clim_date, everything())

summary(all_data)
write.csv(all_data, "data_tall/flintbcm_climate_recent_years.csv", row.names = FALSE)

