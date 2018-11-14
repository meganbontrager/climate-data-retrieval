# Make a quick map of sites or specimen localities
# Created by Megan Bontrager, 13 Nov 2018


# libraries and data ------------------------------------------------------

library(tidyverse)
library(maps) # for state boundaries
library(cowplot) # for map aesthetics


# read in data: should be in csv format with columns latitude and longitude.
locs = read_csv(file = "sample_locations.csv")


# quick map (unprojected) -------------------------------------------------

states = map_data("state")

ggplot() +
  # set edges of map (can adjust depending on scale of localities)
  coord_quickmap(xlim = c(-125, -114), ylim = c(32, 43)) +
  # plot the map of states, group argument prevents ggplot from trying to connect different polygons
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "black", alpha = 0) +
  # plot localities
  geom_point(data = locs, aes(x = longitude, y = latitude), alpha = 0.5, size = 2) +
  # get rid of axes
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()) +
  # put a rectangle around the map
  panel_border(colour = "black", size = 1, remove = FALSE)





