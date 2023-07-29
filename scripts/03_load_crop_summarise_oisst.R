#code from https://cran.r-project.org/web/packages/heatwaveR/vignettes/OISST_preparation.html
# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing


# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

#function to help download data - deals with ERDDAP weirdness that doesn't like pulling more than 9 years at once
la_aoi = c(xmin = -118.5728,ymin = 33.4258, xmax = -117.5842, ymax = 34.0823)

ventura_SB_aoi = c(xmin = -119.92926, ymin = 34.113362, xmax = -119.091303, ymax = 34.50394)

malibu_oxnard_aoi <- c(xmin = -119.29259, ymin = 33.954456, xmax = -118.653106, ymax = 34.250111)

san_nicolas_aoi = c(xmin = -119.7041, ymin = 33.1144, xmax = -119.3104, ymax = 33.3818)

san_clemente_aoi = c(xmin = -118.7068, ymin = 32.7246, xmax = -118.2553, ymax = 33.081)

catalina_aoi =c(xmin = -118.6644, ymin = 33.2148, xmax = -118.2128, ymax = 33.5692)

sb_island_aoi = c(xmin = -119.089542, ymin = 33.432824, xmax = -118.984303, ymax = 33.521955)

san_diego_aoi <- c(xmin = -117.441675, ymin =32.531964,xmax =-116.884438, ymax = 33.266572)
vandenberg_santa_maria_aoi <- c(xmin = -120.872419,ymin = 34.378331, xmax = -120.211339,ymax = 35.151108)
big_sur_aoi <- c(xmin = -121.922683,ymin = 35.470315,xmax = -120.750579,ymax = 36.39213)
monteray_santa_cruz <- c(xmin = -122.2159, ymin =36.4823,xmax =-121.6104, ymax = 36.9971)
san_francisco <- c(xmin = -122.833912, ymin =37.170562,xmax =-121.54644, ymax = 38.150612)
#also try this with a more sensible box
big_sur_2 <- c(xmin = -121.912, ymin =35.4042,xmax =-120.9726, ymax = 36.3621)

aoi_lat <- big_sur_2[c(2,4)]
aoi_lon <- big_sur_2[c(1,3)]

# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(datasetx = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = aoi_lat,
                       longitude = aoi_lon,
                       fields = "sst")$data |> 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z")))
}

#make a df of temporal range to use with the above function
# Date download range by start and end dates per year
dl_years <- data.frame(date_index = 1:5,
                       start = as.Date(c("1984-01-01", "1990-01-01", 
                                         "1998-01-01", "2006-01-01", "2014-01-01")),
                       end = as.Date(c("1989-12-31", "1997-12-31", 
                                       "2005-12-31", "2013-12-31", "2021-12-31")))



# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
  OISST_data <- dl_years |> 
    group_by(date_index) |> 
    group_modify(~OISST_sub_dl(.x)) |> 
    ungroup()
) 
OISST_data <- OISST_data |>
  select(longitude, latitude, time, sst)

OISST_data |> 
  filter(time == "2019-12-01") |> 
  ggplot(aes(x = longitude, y = latitude)) +
  geom_tile(aes(fill = sst)) +
  # borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

saveRDS(OISST_data, "./data/temp_data/oisst/big_sur_2_oisst_raw.rds")
#OISST_data <- readRDS("./data/temp_data/oisst/la_oisst_raw.rds")

#quarterly means, max, min

OISST_data <- OISST_data |>
  mutate(year = as.numeric(year(time)),
         month = month(time),
         quarter = quarter(time),
         latitude = latitude,
         longitude = longitude)

OISST_quarterly_summaries <- OISST_data |>
  group_by(year, quarter) |>
  summarise(mean_temp = mean(sst, na.rm = T),
            max_temp = max(sst, na.rm = T),
            min_temp = min(sst, na.rm = T)) |>
  mutate(year.quarter = as.numeric(paste(year, quarter, sep = "."))) |>
  ungroup()


#don't worry about NAs - these are fine, they are the points over land. 


ggplot(data = OISST_quarterly_summaries, mapping = aes(x = year.quarter, y = mean_temp)) +
  geom_point() +
  stat_smooth(color = "red", method = "lm")

saveRDS(OISST_quarterly_summaries, "./data/temp_data/oisst/big_sur_2_oisst_quarterly.rds")

         