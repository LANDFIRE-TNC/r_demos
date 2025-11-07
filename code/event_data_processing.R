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

################################
### MAKE THE MINIMUM DATA ----
################################

# This section documents how I made the minimum working 
# dataset that this example uses. I include it here 
# as a demonstration of how a user could go directly
# to the Events gdb and extract the info that they want.

# read in Area of Interest (aoi) shapefile, plot to check
shp <- st_read("inputs/map_zone_35.shp", quiet = TRUE) %>% 
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

# plot the shape for fun (not much to look at on it's own!)
plot(shp)

# location of the gdb layers
ready_path <- 'C:\\PRJ\\LF\\EVENTS\\LF_Public_Events_1999_2022.gdb\\CONUS\\CONUS_230_PublicModelReadyEvents'

# get the ready data from the gdb
ready_open <- arc.open(path = ready_path)

# select the actual data 
ready_select <- arc.select(ready_open)

# make it an sf object
ready_sf <- arc.data2sf(ready_select) 

# repair geometry, project to match shp
ready_sf_valid <- st_make_valid(ready_sf) %>%
  st_transform(st_crs(shp))  # match shp projection

# clip to shp
ready_zone35 <- ready_sf_valid %>%
  st_intersection(shp)  

# export clipped example data 

# convert sf to arc.data
#arc_ready <- arc.data(ready_zone35)

# Write to GDB
arc.write(
  path = "inputs/events_example_data.gdb/events_ready_example",  # full path including feature class name
  data = ready_zone35
)


### BRING IN DATA 

# location of the ready Events example layer
ready_ex_path <- 'inputs\\events_example_data.gdb\\events_ready_example'

# get the ready data from the gdb
ready_ex_open <- arc.open(path = ready_ex_path)


# select the actual data ?????????????????
#ready_ex_select <- arc.select(ready_ex_open)

# apply the where clause to subset the data

# model ready events
# list of non-fire treatment types to keep
keep_types <- c("Other Mechanical", "Mastication", "Thinning", "Harvest")
# where clause for keep_types (listed above) and year >= 2013 
where_types <- paste0("Year >= 2013 AND EVENT_TYPE IN ('", paste(keep_types, collapse = "', '"), "')")

# Select features using the where clause
ready_ex_select <- arc.select(ready_ex_open, where_clause = where_types)

# Convert the selected data to sf
ready_sf <- arc.data2sf(ready_ex_select)

# ready_ex_select <- arc.select(ready, where_clause = where_types)
# 
# # make it an sf object
# ready <- arc.data2sf(ready_ex_open) 


### PROCESSING ----

events_west <- events %>%
  # select columns
  select(Event_ID, Year, Event_Type, Event_Subtype, Agency, DB_Source, Start_Date, End_Date) %>%
  # join the classes to the events
  left_join(types_class, by = c("Event_Type", "Event_Subtype")) %>%
  # # remove excluded types
  # filter(KEEP != "EXCLUDE") %>%
  # # clip to west
  # st_intersection(states_dissolved) %>% # this takes ~10 minutes
  # add acres
  mutate(Acres = round(as.numeric(set_units(st_area(.), "acre")), 1)) %>%
  # convert Start_Date and End_Date to date only fields; no time
  mutate(DateStart = ymd(substring(Start_Date, first = 1, last = 10))) %>%
  mutate(DateEnd = ymd(substring(End_Date, first = 1, last = 10))) %>%
  # remove some columns
  select(-c(Start_Date, End_Date, KEEP))

# check that all events have a "class" assignment
# test <- events_west %>%
#   filter(!Event_Type %in% c("Seeding", "Planting", "Reforestation")) %>%
#   filter(is.na(Class))


### EXPORT ----

st_write(events_west, "out/spatial/events/events_nofire.shp", append = FALSE) 
