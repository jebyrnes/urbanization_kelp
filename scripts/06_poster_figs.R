library(tidyverse)

#data
kelp_data <- readRDS("./data/output/kelp_cover_temp_data_for_analysis.rds")

#make clean site name column for plotting
site_lut <- data.frame(site = c("LA","MAL_OX","SAN_CLEMENTE","SAN_NICOLAS","SB_ISLAND", "VENTURA_SB","CATALINA"),
                       site_name = c("Los Angeles", "Malibu to Oxnard", "San Clemente Island", "San Nicolas Island", "Santa Barbara Island", "Ventura to Santa Barbara", "Catalina Island")) 

kelp_data <- left_join(kelp_data, site_lut)


#metadata
#each row contains the average biomass per station for each temp grid point. 
#grid_index: temp data is at 1/4 degree resolution, all kelp stations were joined to the nearest grid point in this temp dataset. this column contains the grid point ID - see the kelp cropping script for grid index lat/lons
#mean biomass per station: each temp grid cell includes multiple (but not equal) numbers of kelp stations. Stations are joined to the nearest temp. point, and this column is the total biomass/number of stations 
#lc_pixel_count: number of "developed" pixels for each year/site

#basically, each row contains the mean/min/max temp, mean kelp biomass/station, and number of "developed' land cover pixels associated with that year/site for each point in the OISST data.





#Temperature time series
temp_plot <- ggplot(data = kelp_data, mapping = aes(x = year, y = summer_mean_temp)) +
  geom_point(size = 3) +
  geom_line() +
  stat_smooth(method = "lm", color = "red") +
 # scale_color_manual(values=c("#ef9b20", "#00bfa0")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        strip.text = element_text(size = 15)) +
  ylab("Mean Sea Surface Temperature (Celsius)") +
  xlab("Year") +
  #labs(color='Site Name') +
  facet_wrap(~site_name, nrow = 4)

ggsave(temp_plot, path= "./figs/benthics_2023", filename = "sst_timeseries3.jpg", dpi = 600, width = 6.15, height = 7.5, units = c("in"))

#with or without facets?^
#with or without stat smooth?

#LC time series

urban_cover <- ggplot(data = kelp_data , mapping = aes(y = urban_pixel_fraction, x = year)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        strip.text = element_text(size = 14)) +
  scale_y_continuous(breaks = breaks_pretty()) +
  ylab('Fraction of Pixels Classified as "Developed"') +
  xlab("Year") +
  facet_wrap(~site_name,scales = "free", ncol = 2)
urban_cover

ggsave(urban_cover, path= "./figs/benthics_2023", filename = "urban_cover3.jpg", dpi = 600, width = 6.15, height = 7.5, units = c("in"))

###
#kelp biomass time series
ggplot(kelp_data, mapping = aes(x = year, y = summer_mean_biomass_per_station, color = site_name)) +
  geom_point(size = 3) +
  stat_smooth(method = "lm") +
  #scale_color_manual(values=c("#ef9b20", "#00bfa0")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  ylab("Mean Kelp Biomass per Station") +
  xlab("Year") +
  theme(axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 18)) +
  facet_wrap(~site_name)




#station plots for each site
la_plot_subset <- kelp_data |>
  filter(year == 1990,
         site == "LA") |>
  st_as_sf()

ci_plot_subset <- site_means |>
  filter(year == 1990,
         site == "CI") |>
  st_as_sf()


ggplot(bb_ll) +
  geom_sf()
library(mapview)

la_site_bbox <- st_as_sfc(st_bbox(c(xmin = -118.5728,ymin = 33.4258, xmax = -117.5842, ymax = 34.0823), crs = 4326))

ci_site_bbox <- st_as_sfc(st_bbox(c(xmin = -119.9878, ymin = 32.5989, xmax = -118.0302, ymax = 33.5945), crs = 4326))

#hack it with mapview for now
mapview(la_site_bbox, alpha.regions = 0, lwd = 3, color = "red") +
  mapview(ci_site_bbox, alpha.regions = 0, lwd = 3, color = "red") +
  mapview(la_plot_subset, col.regions = "orange",color = "orange", cex = 3) +
  mapview(ci_plot_subset, col.regions = "green", color = "green", cex = 4)

kelp_all_sites

la_plot_subset <- kelp_data |>
  filter(site == "LA") |>
  st_as_sf()

station_subset_for_plot <- biomass_with_dates_all_stations |>
  filter(year == 1990)

mapview(la_plot_subset)
