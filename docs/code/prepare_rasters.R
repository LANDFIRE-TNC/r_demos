
# Notes ----
# Code by Randy Swaty
# Oct 7, 2025
# Prepares LANDFIRE Rasters for use in GIS (e.g., R, ArcGIS Pro, QGIS)
# Please see https://landfire-tnc.github.io/r_demos/prep_rasters.html for more information

## Set up ----

# install packages if needed
# install.packages("foreign")
# install.packages("sf")
# install.packages("terra")
# install.packages("tidyverse")

# load packages

library(foreign)
library(sf)
library(terra)
library(tidyverse)

# read in Area of Interest (aoi) shapefile, plot to check

shp <- st_read("inputs/map_zone_35.shp", quiet = TRUE) %>% 
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

# read in .csv of CONUS-wide attributes for later joining to AoI BpS table

bps_conus_atts <- read_csv("inputs/LF20_BPS_220.csv")

# another way is directly from landfire.gov
# bps_url <- "https://landfire.gov/sites/default/files/CSV/LF2016/LF16_BPS.csv" # will likely get warning, but it's OK
# bps_conus_atts <- read.csv(bps_url)

evt_conus_atts <- read_csv("inputs/LF23_EVT_240.csv")

# evt_url <- "https://landfire.gov/sites/default/files/CSV/2024/LF2024_EVT.csv" # will likely get warning, but it's OK
# evt_conus_atts <- read.csv(evt_url)

# look at the first few rows of the CONUS-wide attribute tables
print(head(bps_conus_atts))
print(head(evt_conus_atts))


# Crop, mask and split out downloaded LANDFIRE layers from stacked raster, load into R environment

# load in landfire stacked raster
stacked_rasters <- rast("inputs/landfire_data.tif")

# crop and mask stacked raster 
aoi_stacked_rasters <- stacked_rasters %>%
  crop(shp) %>%
  mask(shp)

# "split" cropped and masked stacked raster into separate layers
for(lyr in names(aoi_stacked_rasters)) assign(lyr, aoi_stacked_rasters[[lyr]])


## Build raster attribute table for BpS data, write raster and attribute table to file

# assign categories
levels(US_200BPS) <- bps_conus_atts
activeCat(US_200BPS) <- "VALUE"

# get frequency table 
bps_freq <- freq(US_200BPS) %>%
  as.data.frame()

# join with attributes and calculate acres and percent
bps_aoi_atts <- bps_freq %>%
  rename(VALUE = value, COUNT = count) %>%
  mutate(VALUE = as.integer(VALUE)) %>%
  left_join(bps_conus_atts, by = "VALUE") %>%
  filter(COUNT != 0) %>%
  mutate(
    ACRES = round((COUNT * 900 / 4046.86), 0),
    REL_PERCENT = round((COUNT / sum(COUNT)) * 100, 3)) %>%
  arrange(desc(REL_PERCENT)) %>%
  select(-layer)  # Optional: remove 'layer' column

# write the raster to a file with specified options
writeRaster(US_200BPS, "outputs/bps_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

# write the attributes dataframe to a dbf file
write.dbf(bps_aoi_atts, "outputs/bps_aoi.tif.vat.dbf")

# write the attributes dataframe to a csv file
write.csv(bps_aoi_atts, "outputs/bps_aoi_attributes.csv")

# plot the cropped and masked raster-will just show values in the map.  Output not shown here.
plot(US_200BPS)

# look at the first few rows of the aoi attributes. Output not shown here.
print(head(bps_aoi_atts))


## Build raster attribute table for EVT data, write raster and attribute table to file

# assign categories
levels(US_240EVT) <- evt_conus_atts
activeCat(US_240EVT) <- "VALUE"

# get frequency table 
evt_freq <- freq(US_240EVT) %>%
  as.data.frame()

# join with attributes and calculate acres and percent
evt_aoi_atts <- evt_freq %>%
  rename(VALUE = value, COUNT = count) %>%
  mutate(VALUE = as.integer(VALUE)) %>%
  left_join(evt_conus_atts, by = "VALUE") %>%
  filter(COUNT != 0) %>%
  mutate(
    ACRES = round((COUNT * 900 / 4046.86), 0),
    REL_PERCENT = round((COUNT / sum(COUNT)) * 100, 3)
  ) %>%
  arrange(desc(REL_PERCENT)) %>%
  select(-layer)  # Optional: remove 'layer' column

# write the raster to a file with specified options
writeRaster(US_240EVT, "outputs/evt_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

# write the attributes dataframe to a dbf file
write.dbf(evt_aoi_atts, "outputs/evt_aoi.tif.vat.dbf")

# write the attributes dataframe to a csv file
write.csv(evt_aoi_atts, "outputs/evt_aoi_attributes.csv")

# plot the cropped and masked raster-will just show values in the map.  Output not shown here.
plot(US_240EVT)

# look at the first few rows of the aoi attributes. Output not shown here.
print(head(evt_aoi_atts))









