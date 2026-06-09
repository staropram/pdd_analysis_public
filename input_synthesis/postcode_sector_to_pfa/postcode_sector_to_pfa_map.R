library(tmap)
library(colorspace)

source("postcode_sector_to_pfa_load_data.R")

utla_to_pfa <- fread("../pfa_to_utla/outputs/utla_to_pfa_full.csv")

# convert sector info to points
sector_to_utla_sf <- st_as_sf(sector_to_utla_extended, coords = c("long", "lat"), crs = 4326)

make_postcode_sector_to_pfa_map <- function() {
   # use the low res boundaries
   pfa_to_plot <-  pfa_lowres[pfa_lowres$PFA22CD %in% utla_to_pfa$PFACode, ]
   utla_to_plot <- utla_lowres[utla_lowres$UTLA22CD %in% utla_to_pfa$UTLACode,]
   
   # also plot the postcode sectors
   sectors_to_plot <- sector_to_utla_sf[sector_to_utla_sf$UTLACode %in% unique(sector_to_pfa$UTLACode),]
   
   # make an interactive map
   tmap_mode(mode="view")
   
   # Generate a pastel palette for 112 items
   pastel_palette <- qualitative_hcl(112, c = 35, l = 85)
   
   # create a bounding box for the viewport
   pf_bbox <- st_bbox(pfa_to_plot)
   pf_bbox_sfc <- st_as_sfc(pf_bbox)
   
   tmap_map <- tm_basemap("OpenStreetMap") + 
      tm_view() +
      tm_shape(utla_to_plot,name="Upper Tier Local Authorities (2022)") + 
      tm_polygons(fill="UTLA22NM",legend.show=F,palette=pastel_palette) +
      tm_text(text = "UTLA22NM", position = "centroid", size = 0.8, col = "black") +
      tm_shape(pfa_to_plot,name="Police force areas (2022)") + 
      tm_polygons(col="black",fill=NA,lwd=3) +
      tm_shape(sectors_to_plot,name="Post code sectors") +
      tm_dots()
   
   
   tmap_save(tmap_map,"outputs/postcode_sector_to_pfa.html")
}

make_postcode_sector_to_pfa_map()