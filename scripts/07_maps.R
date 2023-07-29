library(tidyverse)
library(sf)
#2 maps
#one of all sites as an overview, this can use the aggregated data and just filter to a random year to plot

kelp_all_stations_by_site <- readRDS("./data/kelp_data/no_hyper/kelp_all_sites_with_additional.rds") |> filter(site != "BIG_SUR")

#bboxes
la_aoi = st_as_sfc(st_bbox(c(xmin = -118.5728,ymin = 33.4258, xmax = -117.5842, ymax = 34.0823) ,crs = 4326)) 
ventura_SB_aoi = st_as_sfc(st_bbox(c(xmin = -119.92926, ymin = 34.113362, xmax = -119.091303, ymax = 34.50394),crs = 4326))
malibu_oxnard_aoi <- st_as_sfc(st_bbox(c(xmin = -119.050698, ymin = 33.954456, xmax = -118.653106, ymax = 34.118464),crs = 4326))
san_nicolas_aoi = st_as_sfc(st_bbox(c(xmin = -119.7041, ymin = 33.1144, xmax = -119.3104, ymax = 33.3818),crs = 4326))
san_clemente_aoi = st_as_sfc(st_bbox(c(xmin = -118.7068, ymin = 32.7246, xmax = -118.2553, ymax = 33.081),crs = 4326))
catalina_aoi =st_as_sfc(st_bbox(c(xmin = -118.6644, ymin = 33.2148, xmax = -118.2128, ymax = 33.5692),crs = 4326))
sb_island_aoi = st_as_sfc(st_bbox(c(xmin = -119.089542, ymin = 33.432824, xmax = -118.984303, ymax = 33.521955),crs = 4326))
#expand with more bboxes
san_diego_aoi <- st_as_sfc(st_bbox(c(xmin = -117.441675, ymin =32.531964,xmax =-116.884438, ymax = 33.266572),crs = 4326))
pt_conception_aoi <- st_as_sfc(st_bbox(c(xmin = -120.872419,ymin = 34.378331, xmax = -120.211339,ymax = 35.151108),crs = 4326))
big_sur_aoi <- st_as_sfc(st_bbox(c(xmin = -121.912, ymin =35.4042,xmax =-120.9726, ymax = 36.3621),crs = 4326))
monteray_santa_cruz_aoi <- st_as_sfc(st_bbox(c(xmin = -122.2159, ymin =36.4823,xmax =-121.6104, ymax = 36.9971),crs = 4326))
san_francisco_aoi <- st_as_sfc(st_bbox(c(xmin = -122.833912, ymin =37.170562,xmax =-121.54644, ymax = 38.150612),crs = 4326))

#-119.050698,33.959767,-118.459684,34.118464
#subset sites
la_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "LA") |>
  st_as_sf()

ventura_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "VENTURA_SB") |>
  st_as_sf()

malibu_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "MAL_OX") |>
  st_as_sf()

san_clemente_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "SAN_CLEMENTE") |>
  st_as_sf()

san_nicolas_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "SAN_NICOLAS") |>
  st_as_sf()

sb_island_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "SB_ISLAND") |>
  st_as_sf()

catalina_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "CATALINA") |>
  st_as_sf()

san_diego_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "SAN_DIEGO") |>
  st_as_sf()

pt_conception_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "PT_CONCEPTION") |>
  st_as_sf()

big_sur_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "BIG_SUR_2") |>
  st_as_sf()

monteray_santa_cruz_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "MONTERAY_SC") |>
  st_as_sf()

san_francisco_plot_subset <- kelp_all_stations_by_site |>
  filter(year == 1990,
         site == "SAN_FRANCISCO") |>
  st_as_sf()
library(mapview)
#hack it with mapview for now
mapview(la_aoi, alpha.regions = 0, lwd = 3, color = "red") +
  mapview(ventura_SB_aoi, alpha.regions = 0, lwd = 3, color = "orange") +
  mapview(malibu_oxnard_aoi, alpha.regions = 0, lwd = 3, color = "yellow") +
  mapview(san_nicolas_aoi, alpha.regions = 0, lwd = 3, color = "blue") +
  mapview(san_clemente_aoi, alpha.regions = 0, lwd = 3, color = "purple") +
  mapview(catalina_aoi, alpha.regions = 0, lwd = 3, color = "white") +
  mapview(sb_island_aoi, alpha.regions = 0, lwd = 3, color = "aquamarine") +
  mapview(san_diego_aoi, alpha.regions = 0, lwd = 3, color = "darkolivegreen3") +
  mapview(pt_conception_aoi, alpha.regions = 0, lwd = 3, color = "cadetblue4") +
  mapview(big_sur_aoi, alpha.regions = 0, lwd = 3, color = "bisque") +
  mapview(monteray_santa_cruz_aoi, alpha.regions = 0, lwd = 3, color = "antiquewhite4") +
  mapview(san_francisco_aoi , alpha.regions = 0, lwd = 3, color = "cyan2") +
  mapview(la_plot_subset, col.regions = "green",color = "green", cex = 2) +
  mapview(ventura_plot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(malibu_plot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(san_clemente_plot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(san_nicolas_plot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(sb_island_plot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(catalina_plot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(san_diego_plot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(pt_conception_plc x c ot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(big_sur_plot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(monteray_santa_cruz_plot_subset, col.regions = "green", color = "green", cex = 2) +
  mapview(san_francisco_plot_subset, col.regions = "green", color = "green", cex = 2) 
  

#other map: example of one site through time
#zoom way in (just a few stations) and show their change through time with color
kelp_by_station <- readRDS("./data/kelp_data/old_no_0s/mesma_LA.rds")|>  
  filter(year %in% c(1995, 1996, 1997, 1998)) |>
  filter(quarter == 3)

kelp_by_station_control <- readRDS("./data/kelp_data/old_no_0s/mesma_catalina.rds") |>  
  filter(year %in% c(1995, 1996, 1997, 1998)) |>
  filter(quarter == 3)


library(mapview)
mapview(kelp_by_station_control)
cat_aoi <- c(xmin = -118.4540416743,
            ymin = 33.3169829015,
            xmax = -118.4425481657,
            ymax = 33.3245901954)

map_aoi <- c(xmin = -118.378073,ymin = 33.734232,xmax = -118.364762,ymax = 33.743082)

map_crop <- kelp_by_station |>
  st_crop(map_aoi)

map_crop_cat <- kelp_by_station_control |>
  st_crop(cat_aoi)

map_crop_3_years <- map_crop |>
  filter(year %in% c(1995, 1996, 1997, 1998))  |>
  filter(quarter == 3) |>
  group_by(year, station) |>
  filter(biomass == max(biomass))

map_crop_3_years_cat <- map_crop_cat |>
  filter(year %in% c(1995, 1996, 1997, 1998))  |>
  filter(quarter == 3) |>
  group_by(year, station) |>
  filter(biomass == max(biomass))
map_3_years_cat <- kelp_by_station_control |>
  filter(year %in% c(1995, 1996, 1997, 1998))  |>
  filter(quarter == 3) |>
  group_by(year, station) |>
  filter(biomass == max(biomass))

library(basemapR)
library(viridis)

my_bbox <- st_bbox(c(xmin = -118.378986,ymin =33.73479,xmax  = -118.369783,ymax =33.739762))

map_crop_3_years <- map_crop_3_years |>
  mutate(site = "Los Angeles") |> 
  mutate(site_year = paste(site, year, sep = " "))

map_crop_3_years$biomass_nas <- ifelse(map_crop_3_years$biomass==0,NA,map_crop_3_years$biomass)

map_crop_3_years$biomass_alphas <- ifelse(is.na(map_crop_3_years$biomass_nas),.3,1)

kelp_time <- ggplot() +
  base_map(my_bbox, increase_zoom = 0, basemap = "voyager", nolabels = T) +
  geom_sf(data = map_crop_3_years, mapping = aes(fill = biomass_nas, color = biomass_nas), shape = 22, size = 4) +
  scale_color_viridis(na.value = "red") +
  scale_fill_viridis(na.value = NA) +
  coord_sf(xlim = c(-118.376986, -118.369783), ylim = c(33.73479, 33.739762)) +
  facet_wrap(~site_year, ncol = 4) +
  theme(strip.text = element_text(size = 30, face = 'bold')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = 3) +
  scale_y_continuous(breaks = 3) +
  labs(color='Canopy Biomass \n (Wet Kg/pixel') 
kelp_time
ggsave(kelp_time, path= "./figs/benthics_2023", filename = "kelp_stations_la_named.jpg", dpi = 600, width = 20, height = 5, units = c("in"))

#add site names to facets
map_crop_3_years_cat <- map_crop_3_years_cat |>
  mutate(site = "Catalina Island") |> 
  mutate(site_year = paste(site, year, sep = " "))


map_crop_3_years_cat$biomass_nas <- ifelse(map_crop_3_years_cat$biomass==0,NA,map_crop_3_years_cat$biomass)

map_crop_3_years_cat$biomass_alphas <- ifelse(is.na(map_crop_3_years_cat$biomass_nas),.3,1)


cat_bbox <- st_bbox(cat_aoi)

cat_aoi <- c(xmin = -118.4540416743,
             ymin = 33.3169829015,
             xmax = -118.4425481657,
             ymax = 33.3245901954)
kelp_time_cat <- ggplot() +
  base_map(cat_bbox, increase_zoom = 0, basemap = "voyager", nolabels = T) +
  geom_sf(data = map_crop_3_years_cat, mapping = aes(fill = biomass_nas, color = biomass_nas), shape = 22, size = 3) +
  scale_color_viridis(na.value = "red") +
  scale_fill_viridis(na.value = NA) +
  coord_sf(xlim = c(cat_aoi[1], cat_aoi[3]), ylim = c(cat_aoi[2], cat_aoi[4])) +
  facet_wrap(~site_year, ncol = 4) +
  theme(strip.text = element_text(size = 30, face = 'bold')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = 3) +
  scale_y_continuous(breaks = 3) +
  labs(color='Canopy Biomass \n (Wet Kg/pixel') 
kelp_time_cat

ggsave(kelp_time_cat, path= "./figs/benthics_2023", filename = "kelp_stations_catalina_named.jpg", dpi = 600, width = 20, height = 5, units = c("in"))
######################



kelp_time

ggsave(kelp_time, path= "./figs/benthics_2023", filename = "kelp_stations.jpg", dpi = 600, width = 9.5, height = 9.5, units = c("in"))

ggplot(data = map_crop|> filter(quarter ==3)) +
  geom_sf(aes(color = biomass)) +
  facet_wrap(~year)



map_crop_1995 <- map_3_years_cat |>
  filter(year == 1995)

map_crop_1996 <- map_3_years_cat |>
  filter(year == 1996)
map_crop_1997 <- map_3_years_cat |>
  filter(year == 1997)
map_crop_1998 <- map_3_years_cat |>
  filter(year == 1998)
mapview(mapcrop_test, zcol = "biomass")
mapcrop_test <- map_crop_1995 |>
  filter(station == "330726")
mapview(map_crop_1995, cex = 5, zcol = "biomass") +
  mapview(map_crop_1996, cex = 5, zcol = "biomass") +
  mapview(map_crop_1997, cex = 5, zcol = "biomass") +
  mapview(map_crop_1998, cex = 5, zcol = "biomass")
library(tmap)

tm_shape(ca_map) +
  tm_lines() +
tm_shape(map_crop_3_years) +
  tm_dots(col = "biomass", size = 1) +
  tm_facets(by = "year") 

state_map <- USAboundaries::us_states()

CA_border <- read_sf("./data/boundaries/vx275xn8886.shp") |>
  st_transform(crs = 4326)

ca_map <- CA_border |>
  st_crop(map_crop_3_years)


ggplot(data =  CA_border |> st_crop(map_crop_3_years)) +
  geom_sf() +
  geom_sf(data = map_crop_3_years)

mapview(CA_border)
map_crop_3_years_reproj <- map_crop_3_years |>
  st_transform(crs = 3857)


install.packages("basemaps")
library(basemaps)
bmap <- basemap_ggplot(ext = st_bbox(map_crop_3_years_reproj), map_service = "carto",  map_type = "voyager")



library(terra)
library(tidyterra)
my_bbox <- ext(c(xmin = -118.378986,ymin =33.73479,xmax  = -118.369783,ymax =33.739762))
LA_1995 <- rast("./data/land_cover_data/historical_data/crops/LA/LA_1995_cropped.tif") |> project("epsg:4326") |>
  crop(map_crop_3_years)

LA_1996 <- rast("./data/land_cover_data/historical_data/crops/LA/LA_1995_cropped.tif") |> project("epsg:4326") |>
  crop(map_crop_3_years)

LA_1997 <- rast("./data/land_cover_data/historical_data/crops/LA/LA_1995_cropped.tif") |> project("epsg:4326") |>
  crop(map_crop_3_years)

LA_1998 <- rast("./data/land_cover_data/historical_data/crops/LA/LA_1995_cropped.tif") |> project("epsg:4326") |>
  crop(map_crop_3_years)

ggplot()+
  geom_spatraster(data = LA_1995) + 
  geom_sf(data = map_crop_3_years |> filter(year == 1995), mapping = aes(color = biomass)) +
  scale_color_viridis()
  
  
