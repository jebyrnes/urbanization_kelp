library(tidyverse)
library(sf)
#load in all data files

#kelp
la_biomass_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_LA_quarterly.rds") |>
  mutate(site = "LA")
malibu_ox_biomass_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_malibu_to_oxnard_quarterly.rds")|>
  mutate(site = "MAL_OX")
san_clemente_biomass_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_san_clemente_quarterly.rds")|>
  mutate(site = "SAN_CLEMENTE")
san_nicolas_biomass_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_san_nicolas_quarterly.rds")|>
  mutate(site = "SAN_NICOLAS")
sb_island_biomass_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_sb_island_quarterly.rds")|>
  mutate(site = "SB_ISLAND")
ventura_to_sb_biomass_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_ventura_to_sb_quarterly.rds")|>
  mutate(site = "VENTURA_SB")
catalina_biomass_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_catalina_quarterly.rds")|>
  mutate(site = "CATALINA")
san_diego_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_san_diego_quarterly.rds")|>
  mutate(site = "SAN_DIEGO")
pt_conception_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_pt_conception_quarterly.rds")|>
  mutate(site = "PT_CONCEPTION")
big_sur_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_big_sur_quarterly.rds")|>
  mutate(site = "BIG_SUR")
big_sur_2_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_big_sur_2_quarterly.rds")|>
  mutate(site = "BIG_SUR_2")
monteray_santa_cruz_quarterly <- readRDS("./data/kelp_data/no_hyper/mesma_monteray_santa_cruz_quarterly.rds")|>
  mutate(site = "MONTERAY_SC")
san_francisco <- readRDS("./data/kelp_data/no_hyper/mesma_san_francisco_quarterly.rds")|>
  mutate(site = "SAN_FRANCISCO")

#rbind kelp sites together
kelp_all_sites <- rbind(la_biomass_quarterly,
                        malibu_ox_biomass_quarterly,
                        san_clemente_biomass_quarterly,
                        san_nicolas_biomass_quarterly,
                        sb_island_biomass_quarterly,
                        ventura_to_sb_biomass_quarterly,
                        catalina_biomass_quarterly,
                        san_diego_quarterly,
                        pt_conception_quarterly,
                        big_sur_quarterly,
                        big_sur_2_quarterly,
                        monteray_santa_cruz_quarterly,
                        san_francisco)

#save_sites_for_mapping
kelp_all_stations_by_site <- saveRDS(kelp_all_sites, "./data/kelp_data/no_hyper/kelp_all_sites_with_additional.rds")

#land cover
la_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_LA.rds")|>
  mutate(site = "LA")
malibu_ox_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_malibu_oxnard.rds")|>
  mutate(site = "MAL_OX")
san_clemente_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_san_clemente.rds")|>
  mutate(site = "SAN_CLEMENTE")
san_nicolas_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_san_nicolas.rds")|>
  mutate(site = "SAN_NICOLAS")
sb_island_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_sb_island.rds")|>
  mutate(site = "SB_ISLAND")
ventura_to_sb_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_ventura_sb.rds")|>
  mutate(site = "VENTURA_SB")
catalina_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_catalina.rds")|>
  mutate(site = "CATALINA")
san_diego_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_san_diego.rds")|>
  mutate(site = "SAN_DIEGO")
vandenberg_santa_maria_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_vandenberg_santa_maria.rds")|>
  mutate(site = "PT_CONCEPTION")
big_sur_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_big_sur.rds")|>
  mutate(site = "BIG_SUR")
big_sur_2_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_big_sur_2.rds")|>
  mutate(site = "BIG_SUR_2")
monteray_santa_cruz_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_monteray_santa_cruz.rds")|>
  mutate(site = "MONTERAY_SC")
san_francisco_urban_time_series <- readRDS("./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_san_francisco.rds")|>
  mutate(site = "SAN_FRANCISCO")

#new sites have mismatching class for "value" column, fix it
san_diego_urban_time_series$value <- as.numeric(san_diego_urban_time_series$value)
vandenberg_santa_maria_urban_time_series$value <- as.numeric(vandenberg_santa_maria_urban_time_series$value)
big_sur_urban_time_series$value <- as.numeric(big_sur_urban_time_series$value)
big_sur_2_urban_time_series$value <- as.numeric(big_sur_2_urban_time_series$value)
monteray_santa_cruz_urban_time_series$value <- as.numeric(monteray_santa_cruz_urban_time_series$value)
san_francisco_urban_time_series$value <- as.numeric(san_francisco_urban_time_series$value)

land_cover_all_sites <- rbind(la_urban_time_series,
                              malibu_ox_urban_time_series,
                              san_clemente_urban_time_series,
                              san_nicolas_urban_time_series,
                              sb_island_urban_time_series,
                              ventura_to_sb_urban_time_series,
                              catalina_urban_time_series,
                              san_diego_urban_time_series,
                              vandenberg_santa_maria_urban_time_series,
                              big_sur_urban_time_series,
                              big_sur_2_urban_time_series,
                              monteray_santa_cruz_urban_time_series,
                              san_francisco_urban_time_series)

#Load temperature data
la_oisst <- readRDS("./data/temp_data/oisst/la_oisst_quarterly.rds")|>
  mutate(site = "LA")
malibu_ox_oisst <- readRDS("./data/temp_data/oisst/malibu_oxnard_oisst_quarterly.rds")|>
  mutate(site = "MAL_OX")
san_clemente_oisst <- readRDS("./data/temp_data/oisst/san_clemente_oisst_quarterly.rds")|>
  mutate(site = "SAN_CLEMENTE")
san_nicolas_oisst <- readRDS("./data/temp_data/oisst/san_nicolas_oisst_quarterly.rds")|>
  mutate(site = "SAN_NICOLAS")
sb_island_oisst <- readRDS("./data/temp_data/oisst/sb_island_oisst_quarterly.rds")|>
  mutate(site = "SB_ISLAND")
ventura_sb_oisst <- readRDS("./data/temp_data/oisst/ventura_SB_oisst_quarterly.rds")|>
  mutate(site = "VENTURA_SB")
catalina_oisst <- readRDS("./data/temp_data/oisst/catalina_oisst_quarterly.rds")|>
  mutate(site = "CATALINA")
san_diego_oisst <- readRDS("./data/temp_data/oisst/san_diego_oisst_quarterly.rds")|>
  mutate(site = "SAN_DIEGO")
vandenberg_santa_maria_oisst <- readRDS("./data/temp_data/oisst/vandenberg_santa_maria_oisst_quarterly.rds")|>
  mutate(site = "PT_CONCEPTION")
big_sur_oisst <- readRDS("./data/temp_data/oisst/big_sur_oisst_quarterly.rds")|>
  mutate(site = "BIG_SUR")
big_sur_2_oisst <- readRDS("./data/temp_data/oisst/big_sur_2_oisst_quarterly.rds")|>
  mutate(site = "BIG_SUR_2")
monteray_santa_cruz_oisst <- readRDS("./data/temp_data/oisst/monteray_santa_cruz_oisst_quarterly.rds")|>
  mutate(site = "MONTERAY_SC")
san_francisco_oisst <- readRDS("./data/temp_data/oisst/san_francisco_oisst_quarterly.rds")|>
  mutate(site = "SAN_FRANCISCO")

temp_all_sites <- rbind(la_oisst,
                        malibu_ox_oisst,
                        san_clemente_oisst,
                        san_nicolas_oisst,
                        sb_island_oisst,
                        ventura_sb_oisst,
                        catalina_oisst,
                        san_diego_oisst,
                        vandenberg_santa_maria_oisst,
                        big_sur_oisst,
                        big_sur_2_oisst,
                        monteray_santa_cruz_oisst,
                        san_francisco_oisst)

#fix some col types

kelp_all_sites$year <- as.numeric(kelp_all_sites$year)
kelp_all_sites$quarter <- as.numeric(kelp_all_sites$quarter)


#join land cover to kelp
#one to many is ok - the data is structured with 1 row for urban cover, 1 for cropland
kelp_lc <- left_join(kelp_all_sites, land_cover_all_sites)

#remove NAs: no data for 1984 (too old) or 2022 (too recent)

kelp_lc <- kelp_lc %>%
  filter(!is.na(count))


#rename lc data to be more useful than count
#it's bad because this is downstream of more flexible code to summarize other types
colnames(kelp_lc)[colnames(kelp_lc) == 'count'] <- 'urban_pixel_count'

colnames(kelp_lc)[colnames(kelp_lc) == 'cover_fraction'] <- 'urban_pixel_fraction'


#join temps

kelp_lc_temp <- left_join(kelp_lc, temp_all_sites)



ggplot(data = kelp_lc_temp |> filter(attribute == "Developed"), mapping = aes(x = urban_pixel_fraction, y = mean_biomass_per_station)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~site, scales = "free")


#summer and spring only
summer_data <- kelp_lc_temp |>
  filter(quarter == 3)

spring_data <- kelp_lc_temp |>
  filter(quarter == 2) |>
  select(year, quarter, year.quarter, mean_temp, site, mean_biomass_per_station, attribute)

col_names <- c("year","quarter.y","year.quarter.y","spring_mean_temp","site", "spring_mean_biomass_per_station", "attribute")    

all.equal(col_names, names(spring_data))

names(spring_data) <- col_names

spring_data <- spring_data |>
  ungroup() |>
  select(-quarter.y, -year.quarter.y) |>
  filter(attribute == "Developed") |>
  select(-attribute)

colnames(summer_data)[colnames(summer_data) == 'max_temp'] <- 'summer_max_temp'
colnames(summer_data)[colnames(summer_data) == 'min_temp'] <- 'summer_min_temp'
colnames(summer_data)[colnames(summer_data) == 'mean_temp'] <- 'summer_mean_temp'
colnames(summer_data)[colnames(summer_data) == 'mean_biomass_per_station'] <- 'summer_mean_biomass_per_station'

#expand attribute col
summer_data_urban <- summer_data |>
  filter(attribute == "Developed")
summer_data_crops <- summer_data |>
  filter(attribute == "Cropland")
colnames(summer_data_crops)[colnames(summer_data_crops) == 'urban_pixel_count'] <- 'cropland_pixel_count'
colnames(summer_data_crops)[colnames(summer_data_crops) == 'urban_pixel_fraction'] <- 'cropland_pixel_fraction'

summer_data_crops <- summer_data_crops |>
  select(year, cropland_pixel_count,cropland_pixel_fraction, site)

summer_data <- left_join(summer_data_urban, summer_data_crops)

#join back on



kelp_data <- left_join(summer_data, spring_data, by = c("year", "site")) |>
  #drop NAs, there are a few grid points missing data for some quarters
  filter(!is.na(spring_mean_temp))

#reorganize 
kelp_data_clean <- kelp_data |>
  select(year, site, summer_mean_biomass_per_station, spring_mean_biomass_per_station,
         summer_mean_temp, summer_max_temp, spring_mean_temp, urban_pixel_fraction, cropland_pixel_fraction, total_pixels)


#need to calculate site level temp 
site_means <- kelp_data_clean |>
  group_by(site) |>
  mutate(site_mean_summer_temp = mean(summer_mean_temp),
         site_mean_spring_temp = mean(spring_mean_temp),
         site_mean_spring_biomass = mean(spring_mean_biomass_per_station),
         site_mean_urban = mean(urban_pixel_fraction)) |>
  ungroup()

#try everything at site level, instead of grid cell
#site_means <- kelp_data |>
# group_by(site, year) |>
# summarise(summer_mean_temp = mean(summer_mean_temp),
#          spring_mean_temp = mean(spring_mean_temp),
#          summer_mean_biomass = mean(summer_mean_biomass_per_station),
#          spring_mean_biomass = mean(spring_mean_biomass_per_station),
#         urban_pixel_fraction = mean(urban_pixel_fraction))

#site_means2 <- site_means |> 
# group_by(site) |>
# mutate(site_summer_mean_temp = mean(summer_mean_temp),
#        site_spring_mean_temp = mean(spring_mean_temp),
#       site_spring_mean_biomass = mean(spring_mean_biomass),
#       site_mean_urban = mean(urban_pixel_fraction))


#write_out
saveRDS(site_means, "./data/output/kelp_cover_temp_data_additional_sites_for_analysis.rds")
saveRDS(site_means,"./data/kelp_data/no_hyper/kelp_cover_temp_data_test.rds")

#