#####################################################################
# Last updated 2023-05-01 - Alexander Van Nynatten
# Mostly automated method to plot sampling data
#####################################################################
## loads libraries and imports sample collection data

library(osmdata)
library(sf)
library(tidyverse)

sample_sf <- read_csv('Lake_Huron_fishing.csv') %>%
	st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#####################################################################
## Main Sampling Map

# Bounding box around sampling locations
bbox_sf <- sample_sf %>%
	st_buffer(5000) %>%
	st_bbox(crs = st_crs(4326)) %>%
	st_as_sfc()

# Collects data for water bodies and streams/rivers from Open Street Maps
sf_df <- list()
for(i in c('natural', 'waterway')){
	sf_df[[i]] <- opq(bbox = bbox_sf, timeout = 100) %>%
	add_osm_feature(i) %>%
	osmdata_sf() %>% 
	unname_osmdata_sf()
}

# Cleans and crops river and stream data
waterway_sf <- sf_df[['waterway']]$osm_lines[c('geometry', 'waterway')] %>%
	st_make_valid() %>%
	st_crop(bbox_sf)

# Cleans and crops lake data
water_sf <- rbind(
	sf_df[['natural']]$osm_polygons[c('geometry', 'natural')],
	sf_df[['natural']]$osm_multipolygons[c('geometry', 'natural')]) %>%
	filter(natural == 'water') %>%
	st_make_valid() %>%
	st_crop(bbox_sf)

# Plots the sampling sites onto the water features
ggplot() +
   	geom_sf(data = waterway_sf, 
   	colour = '#8CB5CD', size = 0.3) +
   	geom_sf(data = water_sf, 
   	fill = '#D1E1EB', colour = '#8CB5CD', size = 0.2) + 
  	geom_sf(data = sample_sf, 
    colour = "red", stroke = 0.5, shape = 21, size = 3) +
    coord_sf(expand = 0) +
	theme_test()

# Saves the plot
ggsave('sample_map.pdf')

#####################################################################
## Inset Map

# Collects data for the broader sampling location (need to supply country(s))
sm_world <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)) %>%
	filter(ID %in% 'Canada')

# Collects data for large lakes based on the country selected
sm_lakes <- sf::st_as_sf(maps::map('lakes', plot = FALSE, fill = TRUE)) %>%
	st_make_valid() %>%
	filter(!ID == '?') %>%
	slice(sf::st_as_sf(maps::map('lakes', plot = FALSE, fill = TRUE)) %>%
		st_make_valid() %>%
		filter(!ID == '?') %>%
		st_intersects(sm_world) %>%
		data.frame() %>%
		.$row.id
		)

# Plots the inset map showing the location sampled as a box on country map
ggplot() +
  geom_sf(data=sm_world, color="grey80", fill='white') +
   geom_sf(data=sm_lakes, color="grey80", fill='white') +
  geom_sf(data = bbox_sf, colour = 'black', fill = NA) +
  coord_sf(crs= "+proj=ortho +lat_0=66 +lon_0=-91") + 
  theme_void()

# Saves the plot
ggsave('inset_map.pdf')
