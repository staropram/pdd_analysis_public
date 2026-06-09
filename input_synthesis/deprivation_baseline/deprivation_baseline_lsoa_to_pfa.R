# this script generates a list of LSOAs for each Police Force Area
# by using the Population Weighted Centroid (PWC) to signify
# inclusion

library(dplyr)
library(tmap)

# load the data in
source("deprivation_baseline_load_data.R")

# relevant to this task is 
# lsoa_pwc - the Population Weighted Centroids of each LSOA
# pfa - The Police Force Area boundaries
# pf_participating - Police Forces participating in our study


# test for a single pfa
test_pf <- pf_participating[1]$ForceName
test_pfa <- pfa %>% filter(PFA22NM==test_pf)
test_lsoas <- st_intersection(lsoa_pwc,test_pfa)

test_intersection_plot <- function() {
   tmap_mode(mode="view")

   tmap_map <- tm_basemap("OpenStreetMap") + 
      tm_shape(test_pfa,name=test_pf) + 
      tm_polygons(col="black",fill=NA,lwd=3)+
      tm_shape(test_lsoas,name="LSOA PWC") + 
      tm_dots(fill="lsoa11cd",legend.show=F)

   print(tmap_map)
}
#test_intersection_plot()

# compute child LSOA for each PFA we care about
pfa_to_lsoa <- rbindlist(lapply(pf_participating$ForceName,function(pfn) {
   lsoas <- st_intersection(
      lsoa_pwc,
      pfa %>% filter(PFA22NM==pfn)
   )
   data.table(
     `Force Name`=pfn,
     LSOAs=paste(lsoas$lsoa11cd,collapse=", ")
   )
}))

write.table(pfa_to_lsoa,file="outputs/pfa_to_lsoa.tsv",sep="\t",row.names=F)
