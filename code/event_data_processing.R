# Process selected LANDFIRE Model Ready and Raw Events
# Process Overview:
# 1 - Process non-fire treatments from Model Ready Events
# 2 - Process prescribed fire treatments from Raw Events
# 3 - Make a single treatment layer with non-fire + prescribed fire

library(tidyverse)
library(sf)
library(rmapshaper)
library(units)
library(arcgisbinding)
# initialize arc
arc.check_product() 

### DATA

# prescribed fire type classification
# *I made this classification. It is not a LANDFIRE
# product and should be reviewed by the user.*
rx_class <- read_csv("inputs/rx_types_classified_updated28Dec.csv")



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
raw_sf <- arc.data2sf(raw_select) %>%
  # make valid
  st_make_valid(raw_sf) 

# check it out
ggplot(data = raw_sf) +
  geom_sf(aes(fill = Event_Type)) +
  scale_fill_viridis_d() +
  theme_minimal()

## Clean ----

raw_clean <- raw_sf %>%
  # select columns of interest
  select(Event_ID, Year, Event_Type, Event_Subtype, Agency, DB_Source, Start_Date, End_Date) %>%
  # join the prescribed fire class using the look up table
  left_join(rx_class, by = c("Event_Type", "Event_Subtype")) %>%
  # add acres 
  mutate(Acres = round(as.numeric(set_units(st_area(.), "acre")), 1)) %>%
  # convert Start_Date and End_Date to date only fields; no time
  mutate(DateStart = ymd(substring(Start_Date, first = 1, last = 10))) %>%
  mutate(DateEnd = ymd(substring(End_Date, first = 1, last = 10))) %>%
  # remove some columns
  select(-c(Start_Date, End_Date))


## Remove overlaps ----

# List of years to process
year_list <- c(2004:2023) 

# list of types for final treatment layer
type_list <- c("Burn_broadcast", "Burn_pile", "Burn_unknown")  

# prep dataframe for the loops that will remove overlaps below
rawsimple <- raw_clean %>%
  # adjust the treatment type so that includes the class
  mutate(Event_Type = case_when(
    Event_Type == "Prescribed Fire" & Class == "Broadcast" ~ "Burn_broadcast",
    Event_Type == "Prescribed Fire" & Class == "Pile" ~ "Burn_pile",
    Event_Type == "Prescribed Fire" & Class == "Unknown" ~ "Burn_unknown",
    .default = Event_Type)) %>%
  # select columns of interest
  select(Year, Event_Type)


### RESOLVE OVERLAPPING POLYGONS OF THE SAME TYPE ----

union_polys <- list()

for (year in year_list) { 
  for (type in type_list) {
    
    # filter for year/type combo
    filtered <- rawsimple %>% filter(Year == year, Event_Type == type)
    
    # union
    union_filtered <- st_union(filtered)
    
    # check if union_filtered is empty or has a single geometry
    if (length(union_filtered) == 0) {
      next  # skip to the next iteration if no geometries
    }
    
    # convert the result back to an sf object, add year and type
    # st_union returns a geometry collection, not a full sf object 
    union_final <- st_as_sf(data.frame(geometry = union_filtered, 
                                       Year = year,
                                       Event_Type = type))
    
    # store the result in the list
    union_polys[[paste0("y", year, "_", type)]] <- union_final
  }
}

# combine all year/type unioned layers - using r bind
#rawsimple_union <- do.call(rbind, union_polys) 

# try bind_rows in stead
rawsimple_union <- bind_rows(union_polys)
# explictly make an sf object
rawsimple_union <- st_as_sf(rawsimple_union)
rawsimple_union <- st_make_valid(rawsimple_union)


### RESOLVE OVERLAPPING POLYGONS OF DIFFERENT TYPE ----

# Initialize an empty list to store final results
final_results <- list()

for (year in year_list) {
  
  # filter for each type
  broadcast <- rawsimple_union %>% filter(Year == year, Event_Type == "Burn_broadcast")
  pile <- rawsimple_union %>% filter(Year == year, Event_Type == "Burn_pile")
  unknown <- rawsimple_union %>% filter(Year == year, Event_Type == "Burn_unknown")
  
  # remove broadcast from piles
  if (nrow(pile) > 0 && nrow(broadcast) > 0) {
    pile_only <- ms_erase(pile, erase = broadcast)
  } else {
    pile_only <- pile
  }
  # combine broadcast and clean piles
  bcast_pile <- rbind(broadcast, pile_only)
  
  # remove broadcast/pile from unknown
  if (nrow(unknown) > 0 && nrow(bcast_pile) > 0) {
    unknown_only <- ms_erase(unknown, erase = bcast_pile)
  } else {
    unknown_only <- unknown
  }
  # combine broadcast/pile with clean unknown
  bcast_pile_unk <- rbind(broadcast, pile_only, unknown_only)
  
  # store the result in the list
  final_results[[paste0("final", year)]] <- bcast_pile_unk
}

# combine final results for all years
#tx_fire <- do.call(rbind, final_results) 
tx_fire <- bind_rows(final_results)

# check it out
ggplot(data = tx_fire) +
  geom_sf(aes(fill = Event_Type)) +
  scale_fill_viridis_d() +
  theme_minimal()


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
