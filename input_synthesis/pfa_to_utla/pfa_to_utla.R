library(data.table)
library(dplyr)
library(readxl)
source("pfa_to_utla_load_data.R")


pfaToUTLA <- function(pfa,utla,fn_pfa_to_utla,fn_utla_to_pfa) {
   # it turns out the mapping between UTLA and PFA is 1:1
   # so we can just look at the centroids of the ULTA that fall
   # within the PFA
   
   # get centroids of ulta
   intersections <- st_intersection(st_centroid(utla_relevant),pfa)
   utla_select <- utla_relevant %>% filter(UTLA22NM %in% intersections$UTLA22NM)
   
   # create a table which lists the intersecting UTLA for each PFA
   pfa_to_utla <- rbindlist(lapply(pfa$PFA22NM,function(pfaname) {
      x <- intersections %>% filter(PFA22NM==pfaname)
      utlas <- paste0("'",x$UTLA22NM,"'")
      data.table(PoliceForce=pfaname,UTLAs=paste(utlas,collapse="\t"))
   }))
   
   # have the reverse which is UTLA to PFA
   utla_to_pfa <- intersections[,c("UTLA22NM","UTLA22CD","PFA22NM","PFA22CD")]
   utla_to_pfa <- st_drop_geometry(utla_to_pfa)
   names(utla_to_pfa) <- c("UTLAName","UTLACode","PFAName","PFACode")
   
   # output these tables
   fwrite(pfa_to_utla,fn_pfa_to_utla,sep="\t")
   fwrite(utla_to_pfa,fn_utla_to_pfa,sep="\t")
   
   list(
      pfa_to_utla=pfa_to_utla,
      utla_to_pfa=utla_to_pfa
   )
}

# make two maps, one just for our police force areas
# and one for all the police force areas listed by ONS

# filter the police force area boundaries to match only
# the participating police forces
pfa_relevant <- pfa %>% filter(PFA22NM %in% policeForceInfo$ForceNameLong)
   
# filter out London UTLAs since we don't have the Met
#utla_relevant <- utla %>% filter(! UTLA22CD %like% "E09")
utla_relevant <- utla

# map just the police forces we have
print("Mapping relevant PFA to UTLA")
pfa_to_utla_relevant_map <- pfaToUTLA(pfa_relevant,utla_relevant,PFA_to_UTLA_relevant_fn,UTLA_to_PFA_relevant_fn)
   
# map the entire dataset
print("Mapping all PFA to UTLA")
pfa_to_utla_full_map <- pfaToUTLA(pfa,utla,PFA_to_UTLA_full_fn,UTLA_to_PFA_full_fn)