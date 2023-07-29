#functions to wrangle LULC data

library(tidyverse)
library(sf)
library(terra)
library(purrr)
library(tidyverse)
#load labels
LULC_labs <- read_csv("./data/land_cover_data/LULC_LUT_full_series.csv")[, 1:3]
LULC_labs$value <- as.character(LULC_labs$value)
#make a list of rasters
#need to adjust directory based on region - land cover came as 3 seperate products and
#I left them separate due to file sizes
read_dir <- "./data/land_cover_data/historical_data/full_series/central_ca/"
files <- list.files(read_dir, full.names = T)

#a_raster <- files[1]
#a_raster <- terra::rast(a_raster) |> terra::project("epsg:4326")
#
#ci_aoi = c(xmin = -119.9878, ymin = 32.5989, xmax = -118.0302, ymax = 33.5945)
#la_aoi = c(xmin = -118.5728,ymin = 33.4258, xmax = -117.5842, ymax = 34.0823)

#ventura_SB_aoi = c(xmin = -119.92926, ymin = 34.113362, xmax = -119.091303, ymax = 34.50394)

#malibu_oxnard_aoi <- c(xmin = -119.29259, ymin = 33.954456, xmax = -118.653106, ymax = 34.250111)

#san_nicolas_aoi = c(xmin = -119.7041, ymin = 33.1144, xmax = -119.3104, ymax = 33.3818)

#san_clemente_aoi = c(xmin = -118.7068, ymin = 32.7246, xmax = -118.2553, ymax = 33.081)

#catalina_aoi =c(xmin = -118.6644, ymin = 33.2148, xmax = -118.2128, ymax = 33.5692)

#sb_island_aoi = c(xmin = -119.089542, ymin = 33.432824, xmax = -118.984303, ymax = 33.521955)

#new bboxes
#san_diego_aoi <- c(xmin = -117.441675, ymin =32.531964,xmax =-116.884438, ymax = 33.266572)
#vandenberg_santa_maria_aoi <- c(xmin = -120.872419,ymin = 34.378331, xmax = -120.211339,ymax = 35.151108)
#big_sur_aoi <- c(xmin = -121.922683,ymin = 35.470315,xmax = -120.750579,ymax = 36.39213)
#monteray_santa_cruz <- c(xmin = -122.2159, ymin =36.4823,xmax =-121.6104, ymax = 36.9971)
#san_francisco <- c(xmin = -122.833912, ymin =37.170562,xmax =-121.54644, ymax = 38.150612)
#also try this with a more sensible box
#big_sur_2 <- c(xmin = -121.912, ymin =35.4042,xmax =-120.9726, ymax = 36.3621)

#change region and bbox each time

crop_land_cover_rasters <- function(
    #given a raster
    a_raster, 
    #an output dir
    write_dir = "./data/land_cover_data/historical_data/crops/",
    #a region
    region = "big_sur_2",
    #and a bbox of interest, in WGS84
    bbox = st_bbox(c(xmin = -121.912, ymin =35.4042,xmax =-120.9726, ymax = 36.3621))) {
  

  #tell us which raster we are cropping
  print(paste0("now cropping ",a_raster))
  
  #read it in
  r <- terra::as.factor(rast(a_raster))
  
  #convert bbox coords into a polygon with a CRS
  bbox_poly <- st_as_sfc(bbox)
  st_crs(bbox_poly) <- st_crs(4326)
  
  #then reproject to match our raster - this flexibility is needed as depending on the projection, some CA sites are in different UTM zones. 
  bbox_poly <- bbox_poly |>
    st_transform(crs = st_crs(r))
  
  #crop the raster
  r_crop <- crop(r, bbox_poly)
  
  #assemble a filename
  names(r_crop) <- a_raster
  file_name_out <- paste0(write_dir,region,"_", 
                          str_match_all(names(r_crop), "\\d{4}"), 
                          "_cropped.tif")
  
  #save it as an RDS
  writeRaster(r_crop, file_name_out)
  
  #also save the summary table
  #do the join here to help track down anything that doesn't match up perfectly - the years are a bit different in some cases and I didn't want to lose track
  summary <- freq(r_crop) |>
    left_join(LULC_labs) %>%
    mutate(year = str_match_all(names(r_crop), "\\d{4}"))
  
  file_name_out_table <- paste0(write_dir,"/summary_tables/", region,"_", 
                          str_match_all(names(r_crop), "\\d{4}"), 
                          "_summary.RDS")
  saveRDS(summary, file_name_out_table)
  #return the last one we did so we can spot check as needed. 
  return(r_crop)
}

#run with map 
test <- map(files, crop_land_cover_rasters)


#create a folder and move crops and summary tables

#now read in all summary tables and stitch together into long data - 1 row per category per year


tables <- list.files("./data/land_cover_data/historical_data/crops/summary_tables/big_sur_2", full.names = T, include.dirs = FALSE)



tables_df <- do.call("rbind", lapply(tables, readRDS)) 

#make col of pixels/site so we can calculate proportional coverage
tables_df <- tables_df |>
  group_by(year) |>
  mutate(year = as.numeric(year)) |>
  mutate(total_pixels = sum(count)) |>
  mutate(cover_fraction = count/total_pixels)

urban_time_series <- tables_df |> 
  dplyr::filter(attribute == "Developed"|
                  attribute == "Cropland") 


#need to standardize number of cells
#%urban, not n urban cells

saveRDS(urban_time_series, "./data/land_cover_data/historical_data/crops/summary_tables/urban_time_series_big_sur_2.rds")


ggplot(data = urban_time_series) + 
  geom_point(aes(x = as.numeric(year), y = cover_fraction, color = attribute))


