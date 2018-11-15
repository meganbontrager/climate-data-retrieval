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




# 3. Reformat ClimateNA data ----------------------------------------------

# Target columns:

# specimen_number longitude latitude elev_prism_m clim_year clim_month ppt_mm tmin tave tmax clim_date

# Now save this data
write.csv(all_data, "ClimateNA/climatena_climate_tall.csv", row.names = FALSE)


