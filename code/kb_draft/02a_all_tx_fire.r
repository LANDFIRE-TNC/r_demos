# This script takes all "nofire" treatment data sets (events + local),
# merges them into one layer and removes overlaps in two ways:

# 1: Within year/type overlap is addressed by unioning all polygons w/in a year/type 
# combination. This addresses, for example, overlapping broadcast burn polygons
# in the same year. These could be the same burn entered into different source datasets
# or adjacent burns with a slight overlap in geometry.

# 2. Type overlap w/in the same year is addressed by "erasing" areas of 
# overlap between polygons of different treatment type w/in the same year. Overlaps 
# are resolved in priority order: Burn_broadcast, Burn_pile, Burn_unknown. For example, 
# if broadcast and pile burn polygons overlap in the same year, only broadcast will be 
# retained in the area of overlap. 

# The result is one layer with all years with only one prescribed fire treatment type 
# per location per year.
# I didn't prioritize local datasets but used the treatment type hierarchy instead to determine
# which polygon would be retained when they overlapped.

# 1/25/2025 updated to get 2004

library(tidyverse)
library(sf)
library(rmapshaper)


### SET UP ----

# List of years to process
year_list <- c(2004:2023) 

# list of types for final treatment layer
type_list <- c("Burn_broadcast", "Burn_pile", "Burn_unknown")  
 

### BRING IN DATA ----

# local, fire
local <- st_read("out/spatial/addtx/local_fire.shp") %>%
  select(Year, Evnt_Ty, Class) 

# events, fire
event <- st_read("out/spatial/events/events_fire_raw.shp") %>%
  select(Year, Evnt_Ty, Class) 


### COMBINE ALL INPUTS 

alldata <- rbind(event, local) %>%
  # adjust the treatment type
  mutate(Evnt_Ty = case_when(
    Evnt_Ty == "Prescribed Fire" & Class == "Broadcast" ~ "Burn_broadcast",
    Evnt_Ty == "Prescribed Fire" & Class == "Pile" ~ "Burn_pile",
    Evnt_Ty == "Prescribed Fire" & Class == "Unknown" ~ "Burn_unknown",
    .default = Evnt_Ty)) %>%
  select(Year, Evnt_Ty)


### RESOLVE OVERLAPPING POLYGONS OF THE SAME TYPE ----

union_polys <- list()

for (year in year_list) { 
  for (type in type_list) {
    
    # filter for year/type combo
    filtered <- alldata %>% filter(Year == year, Evnt_Ty == type)
    
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
                                       Evnt_Ty = type))
    
    # store the result in the list
    union_polys[[paste0("y", year, "_", type)]] <- union_final
    }
}

# combine all year/type unioned layers
alldata_union <- do.call(rbind, union_polys) 
  

### RESOLVE OVERLAPPING POLYGONS OF DIFFERENT TYPE ----

# Initialize an empty list to store final results
final_results <- list()

for (year in year_list) {
  
  # filter for each type
  broadcast <- alldata_union %>% filter(Year == year, Evnt_Ty == "Burn_broadcast")
  pile <- alldata_union %>% filter(Year == year, Evnt_Ty == "Burn_pile")
  unknown <- alldata_union %>% filter(Year == year, Evnt_Ty == "Burn_unknown")

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
tx_fire <- do.call(rbind, final_results) 


### EXPORT ----
st_write(tx_fire, "out/spatial/treatments/tx_fire.shp", append = FALSE)
