library(tidync)
library(tidyverse)
library(mapview)
library(sf)

src <- tidync("./data/kelp_data/LandsatKelpBiomass_2022_Q4_withmetadata.nc")

print(src)


#I can only filter by stations
#so I need to make a list of stations that I can extract
#get longs
station_lats <- src %>% 
  activate("latitude") %>% 
  hyper_tibble()

station_longs <- src %>% 
  activate("longitude") %>% 
  hyper_tibble()

station_coords <- left_join(station_lats, station_longs) %>%
  select(station, latitude, longitude)

station_sf <- st_as_sf(station_coords, coords = c("longitude","latitude"))
st_crs(station_sf) <- 4326

plot(head(station_sf))

#now crop to just LA stations

#LA bounding box
la_aoi = c(xmin = -118.5728,ymin = 33.4258, xmax = -117.5842, ymax = 34.0823)
ventura_SB_aoi = c(xmin = -119.92926, ymin = 34.113362, xmax = -119.091303, ymax = 34.50394)
malibu_oxnard_aoi <- c(xmin = -119.29259, ymin = 33.954456, xmax = -118.653106, ymax = 34.250111)
san_nicolas_aoi = c(xmin = -119.7041, ymin = 33.1144, xmax = -119.3104, ymax = 33.3818)
san_clemente_aoi = c(xmin = -118.7068, ymin = 32.7246, xmax = -118.2553, ymax = 33.081)
catalina_aoi =c(xmin = -118.6644, ymin = 33.2148, xmax = -118.2128, ymax = 33.5692)
sb_island_aoi = c(xmin = -119.089542, ymin = 33.432824, xmax = -118.984303, ymax = 33.521955)
#######################
#expand with more bboxes
san_diego_aoi <- c(xmin = -117.441675, ymin =32.531964,xmax =-116.884438, ymax = 33.266572)
pt_conception_aoi <- c(xmin = -120.872419,ymin = 34.378331, xmax = -120.211339,ymax = 35.151108)
big_sur_aoi <- c(xmin = -121.922683,ymin = 35.470315,xmax = -120.750579,ymax = 36.39213) #if there was ever a bbox to redo with polygons, it's this one

#norcal bboxes
#did this one carefully to avoid gilroy
monteray_santa_cruz <- c(xmin = -122.2159, ymin =36.4823,xmax =-121.6104, ymax = 36.9971)
#pescadero to point reyes
san_francisco <- c(xmin = -122.833912, ymin =37.170562,xmax =-121.54644, ymax = 38.150612)
##############################################
bodega <- c(xmin = -123.18396, ymin =38.238128,xmax =-122.957505, ymax = 38.426667) # only 2 pixels??
#check with a bigger box
big_bodega <- c(xmin = -123.6058, ymin =37.8006,xmax =-122.5457, ymax = 38.8863) #plenty of sites above and below,
#must be bull kelp
# based on bell at al 2020, pretty sure there is no biomass north of point reyes.

jenner_gualala <- c(xmin = -123.677728, ymin =38.314523,xmax =-123.029069, ymax = 38.825783)
#centered on fort bragg, with buffer to south and seal rock to the north. north of seal rock is sandy beach
bragg <- c(xmin = -123.842577, ymin =39.389974,xmax =-123.763441, ymax = 39.486638) #no biomass stations but plenty of cover stations
#probably bull kelp
#including fortuna for now, as it seems like its right on an estruary and could be relavent 
humboldt <- c(xmin = -124.3975, ymin =40.5157,xmax =-124.051, ymax = 41.0816)
#south to point arena. only did the northern part of point arena as the southern part still isn't very developed and is unlikely to be affecting things to the north
mendocino <- c(xmin = -123.907796, ymin =38.929486,xmax =-123.560635, ymax = 39.345069)
crescent <- c(xmin = -124.344631, ymin =41.647269,xmax =-124.026714, ymax = 41.817328)

#also try this with a more sensible box
big_sur_2 <- c(xmin = -121.912, ymin =35.4042,xmax =-120.9726, ymax = 36.3621)


#Southern channel islands
#ci_aoi = c(xmin = -119.9878, ymin = 32.5989, xmax = -118.0302, ymax = 33.5945)
#

#crop to relevant stations
station_crop <- station_sf %>%
  st_crop(la_aoi)




#library(mapview)
mapview(station_crop)

#####################################
#now get rid of stations that are actually in our control site
#some stations from the control site were included because the initial bboxes are rectangles
#eliminate them
`%!in%` = Negate(`%in%`)

#drop the weird offshore sites or the sites that shouldn't be in these bboxes
bad_stations_LA <- c(334026, 334027, 334028, 334029, 334030, 314302, 334025, 334024, 314239, 314176)
bad_stations_ventura_SB <- c(258897, 261282, 264693, 264692, 268258, 268353,268259, 278820,278743,278744,280468,280614,280615,282602,282601,272047,271984,271983,271982,290954,290955,290934,292513,292938,293936,293938,293937)

bad_stations_malibu_oxnard <- 293936

station_crop <- station_crop |>
  filter(station %!in% bad_stations_LA)

#drop control sites from LA data by using the catalina bbox
#shouldn't have to do this for any other sites, there was just some overlap in the data retrieval boxes.
catalina_bbox <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -118.6644, ymin = 33.2148, xmax = -118.2128, ymax = 33.5692),  crs = 4326)))

station_crop <- station_crop |>
  filter(!st_within(geometry, catalina_bbox, sparse = F))
####################################################
#now get the biomass and join it to the station/lat/long

#biomass_cropped <- src %>% 
#  activate("biomass") %>% 
#  hyper_filter(station = station %in% station_crop$station) %>%
#  hyper_tibble()


#

#biomass_cropped2 <- src %>% 
 # activate("biomass") %>% 
  #hyper_filter(station = station %in% station_crop$station) %>%
  #hyper_tibble()


biomass_cropped <- src %>% 
  activate("biomass") %>% 
  hyper_tibble()

biomass_cropped <- biomass_cropped |>
  filter(station %in% station_crop$station)




#this makes sense - area measurements are on a 30x30 pixel basis, so the max coverage possible would be 900 m2
#area_cropped <- src %>% 
#  activate("area") %>% 
#  hyper_filter(station = station %in% station_crop$station) %>%
#  hyper_tibble()

#time_lut <- src %>% 
#  activate("D1") |>
#  hyper_tibble()


#biomass_with_time <- left_join(biomass_cropped, time_lut)

#attach lat/long
biomass_cropped <- biomass_cropped %>%
  left_join(station_crop)

biomass_cropped <- st_as_sf(biomass_cropped)
st_crs(biomass_cropped) <- 4326
####
#biomass_cropped <- biomass_cropped %>%
 #filter(biomass > 0)
#####
#attach lat/long
#area_cropped <- area_cropped %>%
#  left_join(station_crop)

#area_cropped <- st_as_sf(area_cropped)
#st_crs(area_cropped) <- 4326

#area_cropped <- area_cropped %>%
#  filter(area > 0)



#plot nonzeros
#grab and crop basemap 
#library(USAboundaries) 
#basemap <- us_states() %>%
 # st_crop(biomass_cropped)

##biomass_cropped$biomass <- as.numeric(biomass_cropped$biomass)

#ggplot(data = la_biomass_cropped) +
 # geom_sf(data = basemap) +
 # geom_sf(mapping = aes(color = biomass)) +
 # coord_sf() +
 # scale_color_viridis_c() +
 # theme_minimal()

#now bring in dates
date_year <- src %>% 
  activate("year") %>% 
  hyper_tibble()
date_quarter <- src %>% 
  activate("quarter") %>% 
  hyper_tibble()

#join dates together
all_dates <- left_join(date_year, date_quarter)

#and join to biomass
biomass_with_dates <- left_join(biomass_cropped, all_dates)
#area_with_dates <- left_join(area_cropped, all_dates)

#make a clean data column for plotting
biomass_with_dates <- biomass_with_dates %>%
  unite(year.quarter, year, quarter, remove = F, sep = ".") 
biomass_with_dates$year.quarter <- as.numeric(biomass_with_dates$year.quarter)

##area_with_dates <- area_with_dates %>%
 # unite(year.quarter, year, quarter, remove = F, sep = ".") 
#area_with_dates$year.quarter <- as.numeric(area_with_dates$year.quarter)
#save out all biomass/date/station/locations
saveRDS(biomass_with_dates, "./data/kelp_data/hyper_filter_testing_delete_after_meeting/mesma_la_no_hyper.rds")
#biomass_with_dates <- readRDS("./data/kelp_data/mesma_ventura_to_sb.rds")


#based on meeting with jarrett:
#my way is missing pixels that blank on kelp. stations with no kelp get disappeared, ratherthan counted as 0s
#need to make sure each year/site contains each station, with 0s as appropriate


#biomass_with_dates_all_stations$year.quarter <- as.character(biomass_with_dates_all_stations$year.quarter)

#biomass_with_dates_all_stations <- separate(data = biomass_with_dates_all_stations, col = year.quarter, into = c("year", "quarter"), remove = F)

#convert to sf so geometry column comes along
#biomass_with_dates_all_stations <- st_as_sf(biomass_with_dates_all_stations, crs = 4326)

#biomass_with_dates_all_stations <- st_as_sf(biomass_with_dates, crs = 4326)


#to plot a time series for one station:
#one_station <- biomass_with_dates |>
 # filter(station == "332656") |>
 # arrange(year.quarter)

#ggplot(one_station, mapping = aes(x = year.quarter, y = biomass)) +
 # geom_point() +
 # geom_line()

#biomass_with_dates <-biomass_with_dates |>
 # complete(station, year, quarter, fill = list(biomass = 0))

#test_complete <- st_as_sf(test_complete, crs = 4326)
##########################
#quarterly mean biomass/station 
biomass_quarterly <- biomass_with_dates |>
  group_by(year, quarter) |>
  summarise(mean_biomass_per_station = mean(biomass))



biomass_quarterly <- biomass_quarterly %>%
  unite(year.quarter, year, quarter, remove = F, sep = ".") 
biomass_quarterly$year.quarter <- as.numeric(biomass_quarterly$year.quarter)



#save out all quarterly biomass/date/station/locations
saveRDS(biomass_quarterly, "./data/kelp_data/no_hyper/mesma_san_nicolas_quarterly.rds")

#can stop here for dissertation analysis - the below is mostly just exploratory wrangling. 
#########################################################################################################
#########################################################################################################
ggplot(data = biomass_quarterly, mapping = aes(x = year.quarter, y = mean_biomass_per_station)) +
  geom_point() +
  geom_line() +
  stat_smooth(method = "lm")

ggplot(data = biomass_quarterly |> filter(year %in% c(1985:1987)), mapping = aes(color = mean_biomass_per_station)) +
  geom_sf() +
  facet_wrap(~year.quarter)

mapview(biomass_quarterly |> filter(year %in% c(1985)))
##################################
#if we want to do more accurate temps
#join temps based on station location

la_biomass_with_dates <- readRDS("./data/kelp_data/mesma_LA.rds")
ci_biomass_with_dates <- readRDS("./data/kelp_data/mesma_CI.rds")

la_OISST_data <- readRDS("./data/temp_data/oisst/OISST_quarterly_summaries_by_point_la.rds")
ci_OISST_data <- readRDS("./data/temp_data/oisst/OISST_quarterly_summaries_by_point_ci.rds")

la_OISST_data <- st_as_sf(la_OISST_data, coords = c("longitude", "latitude"), crs = 4326, remove = F)
ci_OISST_data <- st_as_sf(ci_OISST_data, coords = c("longitude", "latitude"), crs = 4326, remove = F)

#Make look up table of temp grid cells and their coords
la_OISST_data <- la_OISST_data |>
  mutate(lat.long = (paste(latitude , longitude , sep = "_"))) |>
  ungroup()

ci_OISST_data <- ci_OISST_data |>
  mutate(lat.long = (paste(latitude , longitude , sep = "_"))) |>
  ungroup()

la_grid_cells <- data.frame(lat.long = unique(la_OISST_data$lat.long),
                         grid_index = seq(1:length(unique(la_OISST_data$lat.long))))
ci_grid_cells <- data.frame(lat.long = unique(ci_OISST_data$lat.long),
                            grid_index = seq(1:length(unique(ci_OISST_data$lat.long))))

la_grid_cells <- la_grid_cells |>
  separate(col = lat.long, into = c("latitude", "longitude"), sep = "_", remove = F, convert = T)

ci_grid_cells <- ci_grid_cells |>
  separate(col = lat.long, into = c("latitude", "longitude"), sep = "_", remove = F, convert = T)

la_OISST_data <- left_join(la_OISST_data, la_grid_cells)
ci_OISST_data <- left_join(ci_OISST_data, ci_grid_cells)


#find NA grid cells as we do not want to include these in the spatial join
na_grid_cells_la <- la_OISST_data |>
  filter(is.na(mean_temp))

#none for the channel islands, lucky
na_grid_cells_ci <- ci_OISST_data |>
  filter(is.na(mean_temp))
  
na_grid_cells_la <- unique(na_grid_cells_la$grid_index)
na_grid_cells_ci <- unique(na_grid_cells_ci$grid_index)

#convert grid cells to an SF and remove the na cells. 
#note - only remove them from the data that will be joined to kelp for now - the temperature data plots better with them included, so I dont want to change that for now. 

`%!in%` = Negate(`%in%`)


la_grid_cells_sf <- st_as_sf(la_grid_cells, coords = c("longitude", "latitude"), crs = 4326, remove = F) |>
  filter(grid_index %!in% na_grid_cells_la)

ci_grid_cells_sf <- st_as_sf(ci_grid_cells, coords = c("longitude", "latitude"), crs = 4326, remove = F) |>
  filter(grid_index %!in% na_grid_cells_ci)

#now join to the nearest non NA grid cell by using this SF
la_kelp_joined <- st_join(la_biomass_with_dates, la_grid_cells_sf, st_nearest_feature) |>
  select(-longitude, -latitude, -lat.long)

ci_kelp_joined <- st_join(ci_biomass_with_dates, ci_grid_cells_sf, st_nearest_feature) |>
  select(-longitude, -latitude, -lat.long)

#now join temps on by year, quarter, and grid indx
la_kelp_temps <- left_join(la_kelp_joined, la_OISST_data)
ci_kelp_temps <- left_join(ci_kelp_joined, st_drop_geometry(ci_OISST_data))


#quarterly mean biomass/station (I think we want this)
la_biomass_quarterly <- la_kelp_temps |>
  group_by(year, quarter, grid_index, mean_temp, max_temp, min_temp) |>
  summarise(mean_biomass_per_station = mean(biomass))

ci_biomass_quarterly <- ci_kelp_temps |>
  group_by(year, quarter, grid_index, mean_temp, max_temp, min_temp) |>
  summarise(mean_biomass_per_station = mean(biomass))


la_biomass_quarterly <- la_biomass_quarterly %>%
  unite(year.quarter, year, quarter, remove = F, sep = ".") 
la_biomass_quarterly$year.quarter <- as.numeric(la_biomass_quarterly$year.quarter)

ci_biomass_quarterly <- ci_biomass_quarterly %>%
  unite(year.quarter, year, quarter, remove = F, sep = ".") 
ci_biomass_quarterly$year.quarter <- as.numeric(ci_biomass_quarterly$year.quarter)

#save out all quarterly biomass/date/station/locations
saveRDS(la_biomass_quarterly, "./data/kelp_data/mesma_LA_quarterly_gridded_temps.rds")
saveRDS(ci_biomass_quarterly, "./data/kelp_data/mesma_CI_quarterly_gridded_temps.rds")

