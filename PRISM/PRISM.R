# Gather climate data from PRISM
# Created by Megan Bontrager, 13 Nov 2018



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # for plot aesthetics



# 1. Prep input data ------------------------------------------------------

locs = read.csv("sample_locations.csv")

# PRISM's data explorer requires an input csv (with no column headers) containing the columns latitude, longitude, and id (< 12 characters).

locs_prism = locs %>% 
  select(latitude, longitude, id)

colnames(locs_prism) = NULL

write.csv(locs_prism, "PRISM/prism_input.csv", row.names = FALSE)

# Or, if >500 locations
# write.csv(locs_prism[1:500,], "PRISM/prism_input_1.csv", row.names = FALSE)
# write.csv(locs_prism[501:1000,], "PRISM/prism_input_2.csv", row.names = FALSE)
# Continue until all locations are in 500 row csvs.


# 2. Plug data into PRISM's data explorer ---------------------------------

# Go to: http://prism.oregonstate.edu/explorer/bulk.php

# Download monthly data for the time window(s) of interest. Save these files to the PRISM directory. 
# Note that if you are working with > 500 locations, 


# 3. Compile PRISM data from multiple files and reformat ------------------

# Make a list of all .csv files in prism directory
each_data = list.files(pattern = "PRISM_ppt", path = "PRISM/")
# If you didn't toggle ppt data on in the explorer, change the pattern above to match your files.

# For each file in the list, read it in as a csv, then bind all of these together and put the result into a new object, all_data
# First, read in all the files
all_data = lapply(str_c("PRISM/", each_data), read.csv, skip = 10) %>% 
  # Bind all the dataframes together
  bind_rows() %>% 
  # Separate the date column into month and year
  separate(Date, into = c("clim_year", "clim_month"), sep = "-") %>% 
  # Make these into "padded" columns with 0s in from of single digit months for merging
  mutate(clim_year = as.numeric(clim_year), 
         clim_month = as.numeric(clim_month), 
         month_pad = str_pad(clim_month, 2, pad = "0"), 
         clim_date = as.numeric(paste0(clim_year, month_pad))) %>% 
  # Drop unnecessary columns
  select(-month_pad) %>%
  # Rename columns
  rename(specimen_number = Name, longitude = Longitude, latitude = Latitude, elev_prism_m = Elevation..m., ppt_mm = ppt..mm., tmin = tmin..degrees.C., tave = tmean..degrees.C., tmax = tmax..degrees.C.)
# Elevation data here was inferred by prism based on the lat/longs

# Make sure new names look good
head(all_data)
summary(all_data)

# Do we have a complete time series? Adjust bins to number of years in your dataset. Each bin should have 12 months * the number of sites in your dataset.  Once you have all data this should make a nice even histogram.
ggplot(data = all_data, aes(x = clim_year)) + 
  geom_histogram(bins = 60)
# Another check:
table(all_data$clim_year)

# Now save this data
write.csv(all_data, "PRISM/prism_climate_tall.csv", row.names = FALSE)


