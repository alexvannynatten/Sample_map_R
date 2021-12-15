#==============================================================================
## Import packages
#==============================================================================

library(osmdata)
library(sf)
library(tidyverse)

#==============================================================================
## Imports sample data and makes bounding box around them
#==============================================================================

samples <- read_csv('sample_locations.csv')
samples <- samples %>%
	filter(complete.cases(longitude, latitude))
samples_sf <- st_as_sf(samples, 
	coords = c("longitude", "latitude"), crs = 4326)

msc <- 0.0002 # map scale: larger numbers for maps of larger areas
bbox <- st_bbox(samples_sf)
for(i in 1:length(bbox)){
	bbox[i] <- round(bbox[i] - bbox[i] * msc * sign(bbox[i]) * sign(2.5-i),3)
}

#==============================================================================
## Imports water, forest, and residential data from Open Street Maps
#==============================================================================

sf_df <- list()
for(i in c(
	'water', 
	'waterway',
	'landuse'
	 # can add more keys here (ex. 'place' for cities)
	))
{
sf_df[[i]] <- opq(bbox = st_bbox(bbox, crs = 4326), timeout = 100) %>%
add_osm_feature(i) %>%
osmdata_sf() %>% 
unname_osmdata_sf()
}

#==============================================================================
## Subsets and combines datasets into dataframes for plotting in ggplot
#==============================================================================

# Rivers and streams
waterway_sf <- sf_df[['waterway']]$osm_lines[c('geometry')] %>%
	st_make_valid() %>%
	st_crop(bbox)

# Lakes
water_sf <- rbind( 
	sf_df[['water']]$osm_polygons[c('geometry', 'water')],
	sf_df[['water']]$osm_multipolygons[c('geometry', 'water')]) %>%
	filter(!is.na(water)) %>%
	st_make_valid() %>%
	st_crop(bbox)

# Urban
urban_sf <- rbind(
	sf_df[['landuse']]$osm_polygons[c('geometry', 'landuse')],
	sf_df[['landuse']]$osm_multipolygons[c('geometry', 'landuse')]) %>%
	filter(landuse %in% c('residential', 'retail', 'industrial', 'commercial'))

# Forest
forest_sf <- rbind(
	sf_df[['landuse']]$osm_polygons[c('geometry', 'landuse')],
	sf_df[['landuse']]$osm_multipolygons[c('geometry', 'landuse')]) %>%
	filter(landuse %in% c('forest', 'meadow'))

#==============================================================================
## Subsets and combines datasets into dataframes for plotting in ggplot
#==============================================================================

sample_map <- ggplot() +
	geom_sf(data = urban_sf, colour = NA, fill = 'grey40', size = 0.3) +
	geom_sf(data = forest_sf, colour = '#6F874A', fill = '#6F874A', size = 0.5) + 
	geom_sf(data = water_sf, colour = '#1A525E', fill = '#BBD6DC', size = 0.3) +
	geom_sf(data = waterway_sf, colour = '#1A525E', size = 0.2) +
	geom_sf(data = samples_sf, colour = 'black', fill = '#FD3C46', shape = 21, size = 2) +
	coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)]) + 
	theme(panel.background = element_rect(fill = '#ECE9DE'),
		panel.border = element_rect(colour = 'black', fill = NA)) + 
	scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave("Sample_map.pdf", sample_map)


#==============================================================================
## Collects data for larger region and plots map to show bounding box location
#==============================================================================

land_sf <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE), crs = 4326)

land_sf_sampled <- land_sf %>%
	st_make_valid() %>%
	st_crop(bbox) %>%
	pull(ID)
land_sf_sampled <- land_sf[land_sf$ID %in% land_sf_sampled, ]

lakes_sf <- st_as_sf(maps::map("lakes", plot = FALSE, fill = TRUE), crs = 4326)
lakes_sf <- st_difference(lakes_sf, lakes_sf)

msc2 <- 0.075 # Map scale for inset - must be larger value than first scale
bbox2 <- bbox
bbox3 <- bbox
for(i in 1:length(bbox2)){
	bbox2[i] <- round(bbox[i] - bbox[i] * msc2 * sign(bbox[i]) * sign(2.5-i),3)
	bbox3[i] <- round(bbox[i] - bbox[i] * (msc2*0.9) * sign(bbox[i]) * sign(2.5-i),3)
}

inset_map <- ggplot() +
	geom_sf(data = st_crop(st_make_valid(land_sf), bbox2), 
		colour = 'grey50', fill = 'white', size = 0.3) +
	geom_sf(data = st_crop(st_make_valid(land_sf_sampled), bbox2),
	 colour = 'black', fill = '#ECE9DE', size = 0.3) +
	geom_sf(data = st_crop(st_make_valid(lakes_sf), bbox2),
	 colour = '#1A525E', fill = '#BBD6DC', size = 0.2) +
	geom_rect(mapping = 
		aes(xmin=bbox[[1]], xmax=bbox[[3]], ymin=bbox[[2]], ymax=bbox[[4]]), 
		color='black', fill = NA) +
	coord_sf(xlim = bbox3[c(1,3)], ylim = bbox3[c(2,4)]) +
	theme(panel.background = element_rect(fill = '#BBD6DC'),
		panel.border = element_rect(colour = 'black', fill = NA)) + 
	scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))


ggsave("Inset_map.pdf", inset_map)

