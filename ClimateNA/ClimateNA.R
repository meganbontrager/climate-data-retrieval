# Gather climate data from ClimateNA
# Created by Megan Bontrager, 14 Nov 2018



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # for plot aesthetics



# 1. Prep input data ------------------------------------------------------

locs = read.csv("sample_locations.csv")

# ClimateNA requires an input csv containing the columns ID1, ID2, lat, long, and (optionally) el.
# ID1 and ID2 can be anything you want 

locs_climna = locs %>% 
  mutate(ID2 = NA) %>% 
  select(ID1 = id, ID2, lat = latitude, long = longitude, el = elev_m)

write.csv(locs_climna, "ClimateNA/climatena_input.csv", row.names = FALSE)



# 2. Plug data into ClimateNA ---------------------------------------------

# Open the ClimateNA program. Select "time series" in the dropdown menu under multiple locations. Select "monthly variables". Specify the input file we creaetd above, and specif

# Generate monthly data for the time window of interest. Save these files to the ClimateNA directory. 




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
  separate(Date, into = c("year", "month"), sep = "-") %>% 
  # Make these into "padded" columns with 0s in from of single digit months for merging
  mutate(year = as.numeric(year), month = as.numeric(month), month_pad = str_pad(month, 2, pad = "0"), clim_date = as.numeric(paste0(year, month_pad))) %>% 
  # Drop unnecessary columns
  select(-month_pad) %>%
  # Rename columns
  rename(specimen_number = Name, longitude = Longitude, latitude = Latitude, elev_m = Elevation..m., ppt_mm = ppt..mm., tmin = tmin..degrees.C., tave = tmean..degrees.C., tmax = tmax..degrees.C.)
# Elevation data here was inferred by prism based on the lat/longs

# Make sure new names look good
head(all_data)
summary(all_data)

# Do we have a complete time series? Adjust bins to number of years in your dataset. Each bin should have 12 months * the number of sites in your dataset.  Once you have all data this should make a nice even histogram.
ggplot(data = all_data, aes(x = year)) + 
  geom_histogram(bins = 60)
# Another check:
table(all_data$year)

# Now save this data
write.csv(all_data, "PRISM/prism_climate_tall.csv", row.names = FALSE)


