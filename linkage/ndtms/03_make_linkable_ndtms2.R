# This script makes a linkable version of NDTMS, stripped to the minimum
# identifiers required for linkage along with PseudoIDs

source("../../common/r/file_paths.R")
library(data.table)
library(arrow)
library(readr)

# load the data with pseudoID and force area
forceReload <- T
if(!exists("ndtms_linkable")|forceReload) {
  print("Loading NDTMS file with linkage fields")
  ndtms_linkable<-data.table(read_feather(NDTMS_journey_with_linkage_fields_fn_FEATHER_ndtms2))
}

# keep only the linkage columns and order them correctly
print("Extracting linkage columns")
linkageColumns<-c("PseudoID_NDTMS","FirstNameInitial","LastNameInitial","DOB","Sex","ForceName")
ndtms_linkable<-ndtms_linkable[,..linkageColumns]
# actually this should just be PseudoID
setcolorder(ndtms_linkable,linkageColumns)
# we have to make this unique so there are no duplicates
ndtms_linkable<-unique(ndtms_linkable)
setnames(ndtms_linkable,"PseudoID_NDTMS","PseudoID")

# write this out
print("Writing linkable NDTMS")
write_feather(ndtms_linkable,NDTMS_linkable_with_PFD_full_fn_FEATHER_ndtms2)
