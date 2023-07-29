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


#crop to relevant stations
station_crop <- station_sf %>%
  st_crop(big_sur_2)




#library(mapview)
mapview(station_crop)




biomass_cropped <- src %>% 
  activate("biomass") %>% 
  hyper_tibble()


biomass_cropped <- biomass_cropped |>
  filter(station %in% station_crop$station)


#attach lat/long
biomass_cropped <- biomass_cropped %>%
  left_join(station_crop)

biomass_cropped <- st_as_sf(biomass_cropped)
st_crs(biomass_cropped) <- 4326
####

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
#and join to biomass


#make a clean data column for plotting
biomass_with_dates <- biomass_with_dates %>%
  unite(year.quarter, year, quarter, remove = F, sep = ".") 
biomass_with_dates$year.quarter <- as.numeric(biomass_with_dates$year.quarter)

##area_with_dates <- area_with_dates %>%
# unite(year.quarter, year, quarter, remove = F, sep = ".") 
#area_with_dates$year.quarter <- as.numeric(area_with_dates$year.quarter)
#save out all biomass/date/station/locations
saveRDS(biomass_with_dates, "./data/kelp_data/no_hyper/mesma_big_sur_2.rds")


#quarterly mean biomass/station 
biomass_quarterly <- biomass_with_dates |>
  group_by(year, quarter) |>
  summarise(mean_biomass_per_station = mean(biomass))



biomass_quarterly <- biomass_quarterly %>%
  unite(year.quarter, year, quarter, remove = F, sep = ".") 
biomass_quarterly$year.quarter <- as.numeric(biomass_quarterly$year.quarter)



#save out all quarterly biomass/date/station/locations
saveRDS(biomass_quarterly, "./data/kelp_data/no_hyper/mesma_big_sur_2_quarterly.rds")

