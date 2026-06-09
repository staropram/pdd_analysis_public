# This script makes a version of the NDTMS extract that includes all the 
# records from "linked individuals": that is, individuals who were found
# during linkage to be in both PFD and NDTMS. In other words this creates
# a version of NDTMS with only those police force participants found to
# be in NDTMS.

# source file names and load required libraries
source("../../common/r/file_paths.R")
library(data.table)

# load in the full NDTMS with all linkage fields
print("Loading NDTMS with all linkage fields")
ndtms<-data.table(read_feather(NDTMS_journey_with_linkage_fields_fn_FEATHER_ndtms2))

# load in the link map
print("Loading in the link map")
link_map<-data.table(read_feather(NDTMS_to_PFD_unique_link_map_fn_ndtms2))

# now subset NDTMS by the PseudoIDs from the linkage
print("Subsetting NDTMS by linked PseudoIDs")
ndtms_pfd_subset<-ndtms[PseudoID_NDTMS %in% link_map$PseudoID_NDTMS]

# de-duplicate
print("De-duplicating")
ndtms_pfd_subset<-unique(ndtms_pfd_subset)
# include the PFD pseudoID too for cross-referencing
print("Merging in PFD PseudoIDs")
ndtms_pfd_subset<-merge(ndtms_pfd_subset,link_map,by="PseudoID_NDTMS",all.x=T)
setcolorder(ndtms_pfd_subset,c("PseudoID_PFD","PseudoID_NDTMS"))

#  save this subset
print("Writing NDTMS PFD subset")
arrow::write_feather(ndtms_pfd_subset,NDTMS_with_only_PFD_participants_fn_FEATHER_ndtms2)
