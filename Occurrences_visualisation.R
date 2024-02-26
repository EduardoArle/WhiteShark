#load packages
library(sf); library(rnaturalearth); library(raster)

#list WDs
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Occurrences'

#load world map (package rnaturalearth)
world <- ne_countries(scale = 10, returnclass = "sf")
world <- st_make_valid(world)

#read table
setwd(wd_occ)
occ <- read.csv('C_carcharias_clean.csv')

#make spatial object (uses function crs from raster)
occ_sf <- st_as_sf(occ, coords = c('decimalLongitude', 'decimalLatitude'),
                   crs = crs(world))

#plot data
plot(st_geometry(world), col = 'grey', bg = 'azure2', border = F,
     main = 'Carcharodon carcharias', font.main = 3)

plot(st_geometry(occ_sf), add = T,
     pch = 21, cex = 0.6, col = 'black', bg = 'red')
