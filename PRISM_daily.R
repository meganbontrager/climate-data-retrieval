library(tidyverse)
library(terra)
# Pulling monthly/daily climate data from downloaded prism tif files

# May need to change path on different computers
data_path = "/Volumes/My Passport for Mac/climate_PRISM/time_series/us/an/800m/ppt/daily"
# Also change if you want to pull temp instead of precip
data_path = "/Volumes/My Passport for Mac/climate_PRISM/time_series/us/an/800m/tmean/daily"
# Also monthly data
data_path = "/Volumes/My Passport for Mac/climate_PRISM/time_series/us/an/800m/ppt/monthly"
data_path = "/Volumes/My Passport for Mac/climate_PRISM/time_series/us/an/800m/tmean/monthly"

# Get a list of all tif files
clim_tifs = list.files(path = data_path, pattern = "*.tif$", recursive = TRUE)

# Check if all expected files are there
# Dumb code here but fine
check = tibble(clim_tifs) %>% 
  separate(clim_tifs, into = c(NA, NA, "clim_tifs"), sep = "/") %>%
  mutate(clim_tifs = str_remove(clim_tifs, ".tif")) %>%
  separate(clim_tifs, into = c(NA, NA, NA, NA, "date"), sep = "_") %>%
  separate(date, sep = c(4,6), into = c("year", "month", "day"))

table(check$year)
# 2000-357 2001-361 
table(check$year, check$month)
check2 = check %>% filter(year == 2015)
table(check2$month, check2$day)

# Pull one to get CRS
file = rast(str_c(data_path, "/",clim_tifs[1]))
clim_crs = crs(file)

# Test data frame, can replace with real lat-longs
locations = tibble(
  id = as.character(c("site1", "site2")), 
  lat = as.numeric(c(40, 34)), 
  long = as.numeric(c(-122, -118))
)

# Make locations a spatial vector and add WGS1984 projection (best guess for iNat and herbarium data)
locations_sv = vect(locations, geom = c("long", "lat"), crs = "epsg:4326")
plot(locations_sv)

# Reproject locaitons to climate data projection
locations_rp = project(locations_sv, y = clim_crs)
plot(locations_rp)

# See how things line up (points should look the same)
plot(file)
plot(locations_rp, add = TRUE)
plot(locations_sv, add = TRUE)

# Make an empty list for output
output = list()

# Extract data from one raster at a time.
a = Sys.time()
for (i in 7780:length(clim_tifs)){
  file = rast(str_c(data_path, "/",clim_tifs[i]))
  dat = extract(file, locations_rp)
  dat$ID = NULL
  output[[i]] = dat
  print(i)
}
Sys.time() - a
# 2 locations takes ~12 min on 2022 macbook air


all_clim = bind_cols(output) %>% 
  mutate(id = locations$id) %>% 
  pivot_longer(cols = -id, names_to = "variable_date", values_to = "value") %>% 
  separate(variable_date, into = c(NA, "variable", NA, NA, "date"), sep = "_") 

write_csv(all_clim, "daily_ppt_test.csv")
