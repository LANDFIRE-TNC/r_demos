# Get rx fire treatments from LF Raw Events, clip to the west and apply
# wdffp classification of burn type. The resulting layer has overlapping
# treatments; these will be resolved in another script.
# 1/25/2025 updated to get 2004

library(tidyverse)
library(sf)
library(tigris)
library(units)
library(arcgisbinding)
# initialize arc
arc.check_product() 


### BRING IN DATA ----

# raw rx fire events
rx_types <- c("Prescribed Fire") # list of treatment types to keep
# where clause for keep_types (listed above) and year >= 2004
where_rx <- paste0("Year >= 2004 AND EVENT_TYPE IN ('", paste(rx_types, collapse = "', '"), "')")
# get the raw data from the gdb
events_rx <- arc.open(path = 'in\\spatial_data\\events\\LF_Public_Events_1999_2023.gdb\\LF2023_Public_Raw_Events_CONUS')
# apply the where clause to subset the data
events_rx <- arc.select(events_rx, where_clause = where_rx)
# make it an sf object
rx <- arc.data2sf(events_rx) %>%
  # match lf projection
  st_transform(5070) %>%
  # make valid
  st_make_valid(rx) 

# wdffp fire type classification
rx_class <- read_csv("in/rx_types_classified_updated28Dec.csv")

# western states spatial polygon
states <- states() %>%
  filter(STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "NV", "AZ", "CO", "NM", "UT", "WY")) %>%
  select(STUSPS) %>%
  st_transform(st_crs(rx))  # match lf
# dissolve states so that they don't split event polygons that occur in > 1 state
states_dissolved <- st_union(states)


### PROCESSING EVENTS ----

rx_west <- rx %>%
  # select columns
  select(Event_ID, Year, Event_Type, Event_Subtype, Agency, DB_Source, Start_Date, End_Date) %>%
  # join the classes to the events
  left_join(rx_class, by = c("Event_Type", "Event_Subtype")) %>%
  # classify straglers that don't get a class assignment from rx_class
  # these have weird spaces or other issues and I wasn't able to fix in the lookup
  mutate(Class = case_when(Event_Subtype == "underburn " ~ "Broadcast",
                           Event_Subtype == "Fire -  " | Event_Subtype == "Rx  " | 
                             Event_Subtype == "RX " | Event_Subtype == " Site Prep" ~ "Unknown",
                           is.na(Event_Subtype) ~ "Unknown",
                           .default = Class)) %>%
  # clip to west
  st_intersection(states_dissolved) %>% # this takes ~10 minutes
  # add acres 
  mutate(Acres = round(as.numeric(set_units(st_area(.), "acre")), 1)) %>%
  # convert Start_Date and End_Date to date only fields; no time
  mutate(DateStart = ymd(substring(Start_Date, first = 1, last = 10))) %>%
  mutate(DateEnd = ymd(substring(End_Date, first = 1, last = 10))) %>%
  # remove some columns
  select(-c(Start_Date, End_Date))


### EXPORT
st_write(rx_west, "out/spatial/events/events_fire_raw.shp", append = FALSE)
