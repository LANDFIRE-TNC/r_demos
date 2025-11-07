# Combine fire and no fire treatment datasets to make one treatment database
# The output is west wide treatments for processing in Arc.

library(tidyverse)
library(sf)


### BRING IN DATA ----

# tx no fire
tx_nofire <- st_read("out/spatial/treatments/tx_nofire.shp")

# tx fire
tx_fire <- st_read("out/spatial/treatments/tx_fire.shp")


### COMBINE TREATEMENT DATASETS ---

tx_raw <- rbind(tx_nofire, tx_fire) 


### EXPORT ---

# export to work in ArcPro
st_write(tx_raw, "out/spatial/treatments/tx_raw.shp", append = FALSE)
