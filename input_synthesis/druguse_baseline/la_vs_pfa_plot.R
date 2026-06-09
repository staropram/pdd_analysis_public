library(tmap)
library(tmaptools)
library(ggplot2)
library(RColorBrewer)
library(colorspace)
source("druguse_baseline_load_data.R")

pfa_to_utla_and_la <- fread("outputs/pfa_to_utla_and_la_map.tsv")

plot_la_and_pfa <- function() {
   # use the low res boundaries
   pfa_to_plot <- pfa_lowres %>% filter(PFA22CD %in% unique(utla_to_pfa$PFACode))
   utla_to_plot <- utla_lowres %>% filter(UTLA22CD %in% unique(utla_to_pfa$UTLACode))

   # make an interactive map
   tmap_mode(mode="view")

   # Generate a pastel palette for 112 items
   pastel_palette <- qualitative_hcl(112, c = 35, l = 85)

   # create a bounding box for the viewport
   pf_bbox <- st_bbox(pfa_lowres)
   pf_bbox_sfc <- st_as_sfc(pf_bbox)

   tmap_map <- tm_basemap("OpenStreetMap") + 
       tm_view() +
      tm_shape(pfa_to_plot,name="Police force areas (2022)") + 
      tm_polygons(col="black",fill=NA,lwd=3)+
      tm_shape(lad_to_plot,name="Local Authorities (2022)") + 
      tm_polygons(fill="LAD22NM",legend.show=F,palette=pastel_palette,alpha=0.5) +
      tm_text(text = "LAD22NM", position = "centroid", size = 0.8, col = "black")


   print(tmap_map)
   tmap_save(tmap_map,"outputs/la_and_pfa_map.html")
}

plot_la_and_pfa()
