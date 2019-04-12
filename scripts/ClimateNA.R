# Gather climate data from ClimateNA
# Created by Megan Bontrager, 14 Nov 2018



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # for plot aesthetics
library(magrittr)


# 1. Prep input data ------------------------------------------------------

locs = read_csv("sample_locations.csv")

# ClimateNA requires an input csv containing the columns ID1, ID2, lat, long, and (optionally) el.
# ID1 and ID2 can be anything you want 

locs_climna = locs %>% 
  mutate(ID2 = NA) %>% 
  select(ID1 = id, ID2, lat = latitude, long = longitude, el = elev_m)

write.csv(locs_climna, "inputs/climatena_input.csv", row.names = FALSE)



# 2. Plug data into ClimateNA ---------------------------------------------

# Open the ClimateNA program. Select "time series" in the dropdown menu under multiple locations. Select "monthly variables". Specify the input file we creaetd above, and specify an output file. 

# Generate monthly data for the time window of interest. Save these files to the ClimateNA directory. 




# 3. Reformat ClimateNA data ----------------------------------------------

# Data is formatted with one row per site/year, one column per variable/month

all_data = read_csv("raw_outputs/climatena_input_1901-2013AMT.csv") %>%
  select(-ID2) %>% 
  # Make data tall (one row per variable/site/month/year)
  gather(variable, value, -Year, -ID1, -Latitude, -Longitude, -Elevation) %>% 
  # Split month from variable name
  separate(variable, -2, into = c("variable", "month"), convert = TRUE) %>%
  # Spread again so that there is one row per site/year/month, one column per variable type
  spread(variable, value) %>% 
  # Make a climate date column
  mutate(Year = as.numeric(Year), 
         month = as.numeric(month), 
         month_pad = str_pad(month, 2, pad = "0"), 
         clim_date = as.numeric(paste0(Year, month_pad))) %>% 
  # Drop unnecessary columns
  select(-month_pad) %>%
  # Rename to standard variable names
  rename(id = ID1, longitude = Longitude, latitude = Latitude, elev_m = Elevation, clim_year = Year, clim_month = month, ppt_mm = PPT, tmin = Tmin, tave = Tave, tmax = Tmax)

# Rearrange columns
all_data %<>% 
  select(-latitude,-longitude, -elev_m) %>% 
  select(id, clim_year, clim_month, clim_date, everything()) 

# Now save this data
write.csv(all_data, "data_tall/climatena_climate_tall.csv", row.names = FALSE)


