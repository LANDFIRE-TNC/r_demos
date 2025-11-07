# Process selected LANDFIRE Model Ready and Raw Events
# Process Overview:
# 1 - Process non-fire treatments from Model Ready Events
# 2 - Process prescribed fire treatments from Raw Events
# 3 - Make a single treatment layer with non-fire + prescribed fire

library(tidyverse)
library(sf)
library(units)
library(arcgisbinding)
# initialize arc
arc.check_product() 


### NON FIRE TREATMENTS ----

## Get the data ----

# location of the ready Events example layer
ready_path <- 'inputs\\events_example_data.gdb\\events_ready'

# get the ready data from the gdb
ready_open <- arc.open(path = ready_path)

# make a where clause to select non-fire treatments of interest
# list of non-fire treatment types of interest
nonfire_types <- c("Other Mechanical", "Mastication", "Thinning", "Harvest")
# where clause including the year and type of treatments
where_types <- paste0("Year >= 2004 AND EVENT_TYPE IN ('", paste(nonfire_types, collapse = "', '"), "')")

# select features using the where clause
ready_select <- arc.select(ready_open, where_clause = where_types)

# convert the selected data to an sf object
ready_sf <- arc.data2sf(ready_select)

# check it out
ggplot(data = ready_sf) +
  geom_sf(aes(fill = Event_Type)) +
  scale_fill_viridis_d() +
  theme_minimal()

## Clean ----

ready_clean <- ready_sf %>%
  # select columns of interest
  select(Event_ID, Year, Event_Type, Event_Subtype, Agency, DB_Source, Start_Date, End_Date) %>%
  # add acres
  mutate(Acres = round(as.numeric(set_units(st_area(.), "acre")), 1)) %>%
  # convert Start_Date and End_Date to date only fields; no time
  mutate(DateStart = ymd(substring(Start_Date, first = 1, last = 10))) %>%
  mutate(DateEnd = ymd(substring(End_Date, first = 1, last = 10))) %>%
  # remove some columns
  select(-c(Start_Date, End_Date))


### FIRE TREATMENTS ----

## Get the data ----

# location of the ready Events example layer
raw_path <- 'inputs\\events_example_data.gdb\\events_raw'

# get the ready data from the gdb
raw_open <- arc.open(path = raw_path)

# make a where clause to select fire treatments
# list of fire treatment types of interest
fire_types <- c("Prescribed Fire")
# where clause including the year and type of treatments
where_types <- paste0("Year >= 2004 AND EVENT_TYPE IN ('", paste(fire_types, collapse = "', '"), "')")

# select features using the where clause
raw_select <- arc.select(raw_open, where_clause = where_types)

# convert the selected data to an sf object
raw_sf <- arc.data2sf(raw_select)

# check it out
ggplot(data = raw_sf) +
  geom_sf(aes(fill = Event_Type)) +
  scale_fill_viridis_d() +
  theme_minimal()

## Clean ----

# start here
# do basic cleaning as above
# classify burn type
# deal w/ overlap using loop





### EXPORT ----

st_write(events_west, "out/spatial/events/events_nofire.shp", append = FALSE) 


# ARCHIVE **************************

################################
### MAKE THE MINIMUM DATA ----
################################

# This section documents how I made the minimum working 
# dataset that this example uses. I include it here 
# as a demonstration of how a user could go directly
# to the Events gdb and extract the info that they want.

# # read in Area of Interest (aoi) shapefile, plot to check
# shp <- st_read("inputs/map_zone_35.shp", quiet = TRUE) %>% 
#   st_transform(crs = 5070) %>%
#   st_union() %>%
#   st_sf()

# # plot the shape for fun (not much to look at on it's own!)
# plot(shp)

# # location of the gdb layers
# ready_path <- 'C:\\PRJ\\LF\\EVENTS\\LF_Public_Events_1999_2022.gdb\\CONUS\\CONUS_230_PublicModelReadyEvents'
# 
# # get the ready data from the gdb
# ready_open <- arc.open(path = ready_path)
# 
# # select the actual data 
# ready_select <- arc.select(ready_open)
# 
# # make it an sf object
# ready_sf <- arc.data2sf(ready_select) 

# # repair geometry, project to match shp
# ready_sf_valid <- st_make_valid(ready_sf) %>%
#   st_transform(st_crs(shp))  # match shp projection
# 
# # clip to shp
# ready_zone35 <- ready_sf_valid %>%
#   st_intersection(shp)  
# 
# # export clipped example data 
# 
# # convert sf to arc.data
# #arc_ready <- arc.data(ready_zone35)
# 
# # Write to GDB
# arc.write(
#   path = "inputs/events_example_data.gdb/events_ready_example",  # full path including feature class name
#   data = ready_zone35
# )
