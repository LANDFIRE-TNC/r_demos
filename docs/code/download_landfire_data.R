

# Notes ----
# Code by Randy Swaty
# Oct 7, 2025
# Downloads LANDFIRE data with rlandfire package
# Please see https://landfire-tnc.github.io/r_demos/download.html and https://github.com/bcknr/rlandfire for more information


# Set up ----

# install packages if needed
# install.packages("foreign")
# install.packages("rlandfire")
# install.packages("sf")
# install.packages("terra")
# install.packages("tidyverse")

# load packages

library(foreign)
library(rlandfire)
library(sf)
library(terra)
library(tidyverse)

# read in Area of Interest (aoi) shapefile, plot to check

shp <- st_read("inputs/map_zone_35.shp", quiet = TRUE) %>% 
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

# plot the shape for fun (not much to look at on it's own!)
plot(shp)


# Download LANDFIRE data for Area of Interest (AoI), manage that data ----

aoi <- getAOI(shp)

products <-  c("200BPS", "240EVT")  # note, you can download multiple datasets at once
projection <- 5070  
resolution <- 30
email <- "your_email@wah.org" # REPLACE WITH YOUR E-MAIL ADDRESS PLEASE! 

# R specific arguments
save_file <- tempfile(fileext = ".zip")

# call API
ncal <- landfireAPIv2(
  products, 
  aoi, 
  projection, 
  resolution, 
  path = save_file,
  email = email)


# define the destination path
dest_file <- file.path("inputs", "landfire_data.zip")

# move and rename the file
file.rename(save_file, dest_file)

# create a temporary directory for unzipping
temp_dir <- tempfile()
dir.create(temp_dir)

# unzip the file into the temporary directory
unzip(dest_file, exdir = temp_dir)

# get the list of unzipped files
unzipped_files <- list.files(temp_dir, full.names = TRUE)

# rename each unzipped file to "landfire_data" with its full original extension
for (file in unzipped_files) {
  file_name <- basename(file)
  file_extension <- sub("^[^.]*", "", file_name)  # Extract the full extension
  new_file_path <- file.path("inputs", paste0("landfire_data", file_extension))
  file.rename(file, new_file_path)
}

# clean up the temporary directory
unlink(temp_dir, recursive = TRUE)





