#for each year, you divide  # of pixels with kelp in an AOI by total # of pixels in an AOI that ever have kelp in them.

#load up some data
biomass_with_dates <- readRDS("./data/kelp_data/mesma_ventura_to_sb.rds")

#count unique stations/pixels where kelp is present across entire dataset
unique_pixels_with_kelp <- biomass_with_dates |> 
  summarise(n_unique_pixels = n_distinct(station))

#now summarise data yearly to find number of unique stations per year where kelp is present

unique_pixels_with_kelp_annual <- biomass_with_dates |> 
  group_by(year.quarter) |>
  summarise(n_unique_pixels = n_distinct(station)) |>
  mutate(total_aoi_cover = unique_pixels_with_kelp[[1]]) |>
  mutate(percent_cover = n_unique_pixels/total_aoi_cover)

