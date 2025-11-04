# Get selected non-fire treatments from LF Ready Events and process for WDFFP Mel.
# 1/25/2025 updated to get 2004

library(tidyverse)
library(sf)
library(tigris)
library(units)
# sometimes this has to be reinstalled to work on Nimbus
#install.packages("arcgisbinding", repos="https://r.esri.com")
library(arcgisbinding)
# initialize arc
arc.check_product() 


### BRING IN DATA 

# western states spatial polygon
states <- states() %>%
  filter(STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "NV", "AZ", "CO", "NM", "UT", "WY")) %>%
  select(STUSPS) %>%
  st_transform(st_crs(5070))  # match lf
# dissolve states so that they don't split event polygons that occur in > 1 state
states_dissolved <- st_union(states)

# events
# list of non-fire treatment types to keep
keep_types <- c("Other Mechanical", "Mastication", "Thinning", "Harvest", 
                "Seeding", "Planting", "Reforestation")
# where clause for keep_types (listed above) and year >= 2004 
where_types <- paste0("Year >= 2004 AND EVENT_TYPE IN ('", paste(keep_types, collapse = "', '"), "')")
# get the ready data from the gdb
events_ready <- arc.open(path = 'in\\spatial_data\\events\\LF_Public_Events_1999_2023.gdb\\LF2023_Public_ModelReady_Events_CONUS')
# apply the where clause to subset the data
events_ready <- arc.select(events_ready, where_clause = where_types)
# make it an sf object
events <- arc.data2sf(events_ready) 
# check for invalid geometries (as needed)
# tx_valid <- st_is_valid(tx)
# tx_invalid <- tx[!tx_valid, ]
# repair geometry
events <- st_make_valid(events) 

# events classification for wdffp 
types_class <- read_csv("in/types_subtypes_classified_updated28Dec.csv")


### PROCESSING ----

events_west <- events %>%
  # select columns
  select(Event_ID, Year, Event_Type, Event_Subtype, Agency, DB_Source, Start_Date, End_Date) %>%
  # join the classes to the events
  left_join(types_class, by = c("Event_Type", "Event_Subtype")) %>%
  # remove excluded types
  filter(KEEP != "EXCLUDE") %>%
  # clip to west
  st_intersection(states_dissolved) %>% # this takes ~10 minutes
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
