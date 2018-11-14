# Gather climate data from PRISM
# Created by Megan Bontrager, 13 Nov 2018



# libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # for making multi-panel plots



# 1. Prep input data ------------------------------------------------------

locs = read.csv("sample_locations.csv")

# PRISM's data explorer requires an input csv (with no column headers) containing the columns latitude, longitude, and id (< 12 characters).

locs_prism = locs %>% 
  select(latitude, longitude, id)

colnames(locs_prism) = NULL

write.csv(locs_prism, "PRISM/prism_input.csv", row.names = FALSE)



# 2. Plug data into PRISM's data explorer ---------------------------------

# Go to: http://prism.oregonstate.edu/explorer/bulk.php

# Download monthly data for the time window(s) of interest. Save these files to the PRISM directory. 



# 3. Compile PRISM data from multiple files and reformat ------------------

# Make a list of all .csv files in prism directory
each_data = list.files(pattern = "PRISM_ppt", path = "PRISM/")
# If you didn't toggle ppt data on in the explorer, change the pattern above to match your files.

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

