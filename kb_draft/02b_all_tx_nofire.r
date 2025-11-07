# This script takes all "nofire" treatment data sets (events + local),
# merges them into one layer and removes overlaps in two ways:

# 1: Within year/type overlap is addressed by unioning all polygons w/in a year/type 
# combination. This removes, for example, two overlapping mechanical treatments
# in the same place in the same year.

# 2. Type overlap w/in the same year is addressed by "erasing" areas of 
# overlap between polygons of different treatment type w/in the same year. Overlaps 
# are resolved in priority order: Harvest remove, Harvest add, Thinning remove, Thinning add,
# Mechanical_remove, Mechanical_add, Planting/Seeding/Reforestation. For example, if thinning 
# and mechanical polygons overlap in the same year, only thinning will be retained in the 
# area of overlap. 

# The result is one layer with all years with only one treatment per location per year.
# I didn't prioritize local datasets but used the treatment type hierarchy instead to determine
# which polygon would be retained when they overlapped.
# Other Mechanical and Mastication are merged into one class called "Mechanical".
# Planting, Seeding, Reforestation are merged into one class called "PlantSeed". 

# 1/25/2025 updated to get 2004

library(tidyverse)
library(sf)
library(rmapshaper)


### SET UP ----

# List of years to process
year_list <- c(2004:2023) 

# list of types for final treatment layer
type_list <- c("Harvest_remove", "Harvest_add", "Thinning_remove", "Thinning_add", 
               "Mechanical_remove", "Mechanical_add", "PlantSeed")  
 

### BRING IN DATA ----

# local no fire
local <- st_read("out/spatial/addtx/local_nofire.shp") %>%
  select(Year, Evnt_Ty, Class) 

# events no fire
event <- st_read("out/spatial/events/events_nofire.shp") %>%
  select(Year, Evnt_Ty, Class) 


### COMBINE ALL INPUTS 

alldata <- rbind(event, local) %>%
  # adjust the treatment type
  mutate(Evnt_Ty = case_when(
    # harvest
    Evnt_Ty == "Harvest" & Class == "remove" ~ "Harvest_remove",
    Evnt_Ty == "Harvest" & Class == "add" ~ "Harvest_add",
    # thinning
    Evnt_Ty == "Thinning" & Class == "remove" ~ "Thinning_remove",
    Evnt_Ty == "Thinning" & Class == "add" ~ "Thinning_add",
    # mechanical
    Evnt_Ty == "Other Mechanical" & Class == "remove" ~ "Mechanical_remove",
    Evnt_Ty == "Mastication" & Class == "remove" ~ "Mechanical_remove",
    Evnt_Ty == "Other Mechanical" & Class == "add" ~ "Mechanical_add",
    Evnt_Ty == "Mastication" & Class == "add" ~ "Mechanical_add",
    # planting, seeding, restoration
    Evnt_Ty == "Planting" | Evnt_Ty == "Seeding" | Evnt_Ty == "Reforestation"
    ~ "PlantSeed",
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
  harvest_r <- alldata_union %>% filter(Year == year, Evnt_Ty == "Harvest_remove")
  harvest_a <- alldata_union %>% filter(Year == year, Evnt_Ty == "Harvest_add")
  thinning_r <- alldata_union %>% filter(Year == year, Evnt_Ty == "Thinning_remove")
  thinning_a <- alldata_union %>% filter(Year == year, Evnt_Ty == "Thinning_add")
  mechanical_r <- alldata_union %>% filter(Year == year, Evnt_Ty == "Mechanical_remove")
  mechanical_a <- alldata_union %>% filter(Year == year, Evnt_Ty == "Mechanical_add")
  plantseed <- alldata_union %>% filter(Year == year, Evnt_Ty == "PlantSeed")
  
  # 1. Resolve add/remove overlap 
  
  # harvest - erase removes from the adds
  if (nrow(harvest_a) > 0 && nrow(harvest_r) > 0) {
    hvst_add <- ms_erase(harvest_a, erase = harvest_r)
  } else {
    hvst_add <- harvest_a
  }
  # combine cleaned harvest add/remove
  all_hvst <- rbind(harvest_r, hvst_add)
  
  # thinning - erase removes from the adds
  if (nrow(thinning_a) > 0 && nrow(thinning_r) > 0) {
    thin_add <- ms_erase(thinning_a, erase = thinning_r)
  } else {
    thin_add <- thinning_a
  }
  # combine cleaned thinning add/remove
  all_thin <- rbind(thinning_r, thin_add)
  
  # mechanical - erase removes from the adds
  if (nrow(mechanical_a) > 0 && nrow(mechanical_r) > 0) {
    mech_add <- ms_erase(mechanical_a, erase = mechanical_r)
  } else {
    mech_add <- mechanical_a
  }
  # combine cleaned mechanical add/remove
  all_mech <- rbind(mechanical_r, mech_add)
  
  # 2. resolve type on type overlap
  
  # remove harvest from thinning
  thin_only <- ms_erase(all_thin, erase = all_hvst) 
  
  # combine harvest and cleaned thinning
  hvst_thin <- rbind(all_hvst, thin_only) 
  
  # remove harvest/thinning from mechanical
  mech_only <- ms_erase(all_mech, erase = hvst_thin) 
  
  # combine harvest/thinning and cleaned mechanical
  hvst_thin_mech <- rbind(all_hvst, thin_only, mech_only)
  
  # remove harvest/thinning/mechanical from plantseed
  plant_only <- ms_erase(plantseed, erase = hvst_thin_mech) 
  
  # combine harvest/thinning/mechanical and cleaned plantseed
  hvst_thin_mech_plnt <- rbind(all_hvst, thin_only, mech_only, plant_only)
  
  # store the result in the list
  final_results[[paste0("final", year)]] <- hvst_thin_mech_plnt
}

# combine final results for all years
tx_nofire <- do.call(rbind, final_results) 


### EXPORT ----
st_write(tx_nofire, "out/spatial/treatments/tx_nofire.shp", append = FALSE)
