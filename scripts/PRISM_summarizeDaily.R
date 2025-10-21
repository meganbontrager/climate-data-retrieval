# Code to determine DOY of first rain event for each specimen
# daily ppt data starts in 1981 so only specimens collected in 1982 to 2016 included
# 1082 specimens


library(tidyverse)
library(lubridate)

# Compile PRISM data --------------------------

# Read in specimen info to get infor about collection dates
specimen_info = read_csv("rainfall-fitness/data/compiled_specimen_data.csv") %>% 
  select(species, specimen, collection_day = day, collection_month = month, collection_year = year) %>% 
  mutate(collection_date = make_date(year = collection_year, day = collection_day, month = collection_month)) %>% 
  distinct()

# Make a list of all .csv files in prism directory
files <- dir("rainfall-fitness/data/PRISM_data/",
             pattern = "PRISM_ppt.*csv",
             full.names = TRUE)
files

# Create a tibble to hold the imported data and use map to read each file in 
# Skip 10 lines with prism metadata

dat <- tibble(path = files, filename = basename(path)) %>% 
  mutate(sheets = purrr::map(path, read_csv, skip = 10, col_types = c("c", "d", "d", "d", "D", "d", "d", "d", "d"))) %>%
  dplyr::select(-path) %>% 
  unnest(sheets) %>%
  distinct() %>% 
  # rename columns to be consistent with other data
  rename(specimen = Name, 
         long = Longitude, 
         lat = Latitude, 
         elev_m = `Elevation (m)`, 
         prism_date = Date, 
         ppt_mm = `ppt (mm)`, 
         tmin = `tmin (degrees C)`, 
         tmean = `tmean (degrees C)`, 
         tmax = `tmax (degrees C)`) %>% 
  dplyr::select(-filename) %>% 
  # split Date into Year, Month, Day
  separate(prism_date, into = c("prism_year", "prism_month", "prism_day"), sep = "-", remove = FALSE, convert = TRUE) %>%  
  left_join(., specimen_info) %>% 
  mutate(sept_1_date = make_date(year = collection_year-1, month = 9, day = 1),
         prism_DOWY = as.numeric(prism_date - sept_1_date),
         collection_DOWY = as.numeric(collection_date - sept_1_date)) %>% 
  # drop perennial species
  filter(!(species %in% c("barbatus", "cordatus", "morrisonii", "tortuosus")))  %>%
  distinct() 

dat %>% select(specimen) %>% distinct()
# 891 specimens

# First rain event ---------------------------

all_rain_events = dat %>% 
  # Omit August because we are limiting germination to sept 1 onward
  filter(prism_month != 8) %>% 
  filter(ppt_mm > 0) %>% 
  group_by(specimen) %>% 
  mutate(
         # assign numbers grouping days that are continuous
         # DOWY[-length(DOWY)] drop the last day
         # DOWY[-1] drop the first day
         # abs(DOWY[-length(DOWY)] - DOWY[-1]) take absolute value of the difference between day n and day n+1
         # abs(all_rain_events$DOWY[-length(all_rain_events$DOWY)] - all_rain_events$DOWY[-1]) > 1 if this difference is greater than 1, true, ottherwise false
         # and when true it adds 1 to the cumulative sum (i.e., starts a new group)
         rain_event_number = cumsum(c(1, abs(prism_DOWY[-length(prism_DOWY)] - prism_DOWY[-1]) > 1))) 

# https://stats.stackexchange.com/questions/107515/grouping-sequential-values-in-r


summed_rain_events = all_rain_events %>% 
  group_by(species, specimen, long, lat, elev_m, 
           collection_year, collection_month, collection_day, collection_date, collection_DOWY, sept_1_date, rain_event_number) %>% 
  summarize(rain_event_size_mm = sum(ppt_mm), 
            rain_event_duration_days = n(), 
            first_day_of_event = min(prism_DOWY), 
            first_date_of_event = min(prism_date))

ggplot(summed_rain_events) + 
  geom_histogram(aes(x = rain_event_size_mm), binwidth = 10) +
  facet_wrap(.~species) +
  # scale_y_log10() +
  geom_vline(xintercept = 10)

first_rain_event = summed_rain_events %>% 
  filter(rain_event_size_mm > 25) %>% 
  group_by(species, specimen, long, lat, elev_m, 
           collection_year, collection_month, collection_day, collection_date, collection_DOWY, sept_1_date) %>% 
  summarize(first_rain_date = min(first_date_of_event), 
            first_rain_DOWY = min(first_day_of_event)) %>% 
  mutate(season_length = collection_date - first_rain_date)

ggplot(first_rain_event, aes(y = first_rain_DOWY, x = lat)) +
  geom_point() +
  geom_smooth()

ggplot(first_rain_event) +
  geom_histogram(aes(x = first_rain_DOWY), binwidth = 30) +
  facet_wrap(.~species) +
  geom_vline(xintercept = 121)

first_rain_event %>%
  group_by(species) %>%
  summarize(prop_post_jan1 = sum(first_rain_DOWY>121)/n()) %>%
  arrange(prop_post_jan1)

ggplot(first_rain_event)+
  geom_histogram(aes(x = first_rain_DOWY), binwidth = 30)



# Last rain event ---------------------------

all_dry_events = dat %>%
  distinct() %>%
  # subset data since we expect season to end after March
  filter(prism_month %in% c(4,5,6,7)) %>%
  left_join(., specimen_info) %>% 
  filter(ppt_mm == 0) %>% 
  group_by(specimen) %>% 
  mutate(
         # assign numbers grouping days that are continuous
         # DOWY[-length(DOWY)] drop the last day
         # DOWY[-1] drop the first day
         # abs(DOWY[-length(DOWY)] - DOWY[-1]) take absolute value of the difference between day n and day n+1
         # abs(all_rain_events$DOWY[-length(all_rain_events$DOWY)] - all_rain_events$DOWY[-1]) > 1 if this difference is greater than 1, true, ottherwise false
         # and when true it adds 1 to the cumulative sum (i.e., starts a new group)
         dry_event_number = cumsum(c(1, abs(prism_DOWY[-length(prism_DOWY)] - prism_DOWY[-1]) > 1))) 

summed_dry_events = all_dry_events %>% 
  group_by(species, specimen, long, lat, elev_m, 
           collection_year, collection_month, collection_day, collection_date, collection_DOWY, sept_1_date, dry_event_number) %>% 
  summarize(dry_event_duration_days = n(), first_day_of_dry_event = min(prism_DOWY), first_date_of_dry_event = min(prism_date))

first_14day_dry_event = summed_dry_events %>% 
  filter(dry_event_duration_days >= 14) %>% 
  group_by(species, specimen, long, lat, elev_m, 
           collection_year, collection_month, collection_day, collection_date, collection_DOWY, sept_1_date) %>% 
  summarize(first_14dry_date = min(first_date_of_dry_event), 
            first_14dry_DOWY = min(first_day_of_dry_event)) %>% 
  mutate(coll_rel_dry = collection_DOWY - first_14dry_DOWY)


# Small versus big rain events and rain from germ date to collection date ------------------------------------

small_rain_events = summed_rain_events %>% 
  left_join(., first_rain_event) %>% 
  filter(first_day_of_event >= first_rain_DOWY) %>% 
  filter(first_day_of_event <= collection_DOWY) %>% 
  group_by(species, specimen, long, lat, elev_m, collection_year, collection_month, collection_day, collection_date, collection_DOWY, first_rain_DOWY) %>% 
  summarize(number_of_small_events = sum(rain_event_size_mm<=10), total_ppt_mm_prism = sum(rain_event_size_mm), total_rain_events = n()) 


# Average temp germ date to collection date -------------------------

temp_germ_to_coll = dat %>%
  left_join(first_rain_event) %>% 
  # Omit 6 specimens collected later than our data extends
  filter(collection_DOWY <= 330) %>%
  filter(prism_DOWY >= first_rain_DOWY, prism_DOWY <= collection_DOWY) %>% 
  group_by(species, specimen, long, lat, elev_m, collection_year, collection_month, collection_day, collection_date, collection_DOWY, first_rain_DOWY) %>% 
  summarize(tmean_first_rain_to_coll = mean(tmean)) %>% 
  mutate(day_first_rain_to_coll = collection_DOWY - first_rain_DOWY)

# Calculate growing degree days -------------------------------------

# subset PRISM data for each specimen to germ_date and collection date

# formula: (Tmax - Tmin/2) - Tb
# base temperature  = 3, Burghardt et al. 2015 Am Nat

GDD = dat %>%
  left_join(first_rain_event) %>% 
  # Omit 6 specimens collected later than our data extends
  filter(collection_DOWY <= 330) %>%
  filter(prism_DOWY >= first_rain_DOWY, prism_DOWY <= collection_DOWY) %>% 
  mutate(GDD = (tmax - tmin)/2 - 3) %>% 
  # per GDD definition, if (tmax-tmin)/2 < 3, GDD = 0, so dropping negative degree days
  filter(GDD > 0) %>%
  group_by(species, specimen, long, lat, elev_m, collection_year, collection_month, collection_day, collection_date, collection_DOWY, first_rain_DOWY) %>% 
  summarize(GDD = sum(GDD))



# Specimen-specific early/late rain -------------------

early_late_rain = dat %>%
  left_join(first_rain_event) %>% 
  # Omit 6 specimens collected later than our data extends
  filter(collection_DOWY <= 330) %>%
  filter(prism_DOWY >= first_rain_DOWY, prism_DOWY <= collection_DOWY) %>% 
  mutate(mid_DOWY = first_rain_DOWY + (collection_DOWY - first_rain_DOWY)/2, early_late = if_else(prism_DOWY < mid_DOWY, "early_ppt", "late_ppt")) %>% 
  group_by(species, specimen, long, lat, elev_m, collection_year, collection_month, collection_day, collection_date, collection_DOWY, first_rain_DOWY, early_late, mid_DOWY) %>% 
  summarize(ppt_mm = sum(ppt_mm)) %>% 
  pivot_wider(id_cols = c(species, specimen, long, lat, elev_m, collection_year, collection_month, collection_day, collection_date, collection_DOWY, first_rain_DOWY, mid_DOWY), names_from = early_late, values_from = ppt_mm)
  

all_data = full_join(first_rain_event, first_14day_dry_event) %>% 
  full_join(., early_late_rain) %>% 
  full_join(., GDD) %>% 
  full_join(., small_rain_events) %>% 
  full_join(., temp_germ_to_coll)

summary(all_data)

write_csv(all_data, "rainfall-fitness/data/climate_from_daily_data.csv")


