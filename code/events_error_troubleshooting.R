# #filters out rows where the geometry column is NA. -- NONE
# rawsimple_union3 <- rawsimple_union %>%
#   # Add a column indicating whether geometry is na
#   mutate(is_na_geom = is.na(st_geometry(.))) %>%
#   # Create a data frame of only the empty geometries
#   filter(is_na_geom)

# #checks for geometries that are completely empty. -- NONE
# rawsimple_union2 <- rawsimple_union %>%
#   # Add a column indicating whether geometry is empty
#   mutate(is_empty_geom = st_is_empty(.)) %>%
#   # Create a data frame of only the empty geometries
#   filter(is_empty_geom)

# make valid - didn't help same warning
# st_make_valid(rawsimple_union)


# sometimes rbind causes issues with geometry so I explicitly
# convert the result to an sf object; w/o this there will be warnings
#rawsimple_union <- st_as_sf(rawsimple_union)

# check projection
st_crs(rawsimple_union)

# recheck na and empty
rawsimple_union <- do.call(rbind, union_polys) %>%
  #  filter(!is.na(st_geometry(.)))
  #  filter(!st_is_empty(.))

  
  # # sometimes rbind causes issues with geometry so I explicitly
  # # convert the result to an sf object; w/o this there will be warnings
  # rawsimple_union <- st_as_sf(rawsimple_union) 
  # #make valid - didn't help same warning
  # rawsimple_union <- st_make_valid(rawsimple_union)
