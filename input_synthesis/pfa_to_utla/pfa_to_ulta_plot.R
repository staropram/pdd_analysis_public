# plots a (geographical) map of PFA and UTLA areas

library(dplyr)
library(tmap)
library(tmaptools)
library(ggplot2)
library(RColorBrewer)
library(colorspace)

source("pfa_to_utla_load_data.R")
utla_to_pfa <- fread("outputs/utla_to_pfa.csv")

make_force_map <- function() {
   # use the low res boundaries
   pfa_to_plot <- pfa_lowres %>% filter(PFA22CD %in% unique(utla_to_pfa$PFACode))
   utla_to_plot <- utla_lowres %>% filter(UTLA22CD %in% unique(utla_to_pfa$UTLACode))
   
   # make an interactive map
   tmap_mode(mode="view")
   
   # Generate a pastel palette for 112 items
   pastel_palette <- qualitative_hcl(112, c = 35, l = 85)
   
   # create a bounding box for the viewport
   pf_bbox <- st_bbox(pfa)
   pf_bbox_sfc <- st_as_sfc(pf_bbox)
   
   tmap_map <- tm_basemap("OpenStreetMap") + 
      tm_view() +
      tm_shape(utla_to_plot,name="Upper Tier Local Authorities (2022)") + 
      tm_polygons(fill="UTLA22NM",legend.show=F,palette=pastel_palette) +
      tm_text(text = "UTLA22NM", position = "centroid", size = 0.8, col = "black") +
      tm_shape(pfa_to_plot,name="Police force areas (2022)") + 
      tm_polygons(col="black",fill=NA,lwd=3)
   
   
   print(tmap_map)
   tmap_save(tmap_map,"outputs/reoffending_baseline_map.html")
}

make_force_map()