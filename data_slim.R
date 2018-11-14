dat = read_tsv("caulanthus_anceps.txt") %>% 
  separate(collection_date, into = c("year", "month", "day"), sep = "-") %>% 
  mutate(elev_m = as.numeric(str_extract(elevation, "[0-9]+")), 
         elev_unit = str_extract(elevation, "[aA-zZ]+"),
         month = as.numeric(month),
         year = as.numeric(year)) %>% 
  select(id = specimen_number, latitude, longitude, elev_m, year, month) %>% 
  filter(!is.na(latitude), !is.na(elev_m), year <2010, year >1950) %>% 
  distinct(latitude, longitude, .keep_all = TRUE)

summary(dat)

write.csv(dat, "sample_locations.csv", row.names = FALSE)
