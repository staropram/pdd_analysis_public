source("../../common/r/file_paths.R")
forceReload<-T

if(!exists("ndtms_linkable")|forceReload==T) {
   print("Loading linkable NDMTS")
   ndtms_linkable<-data.table(read_feather(NDTMS_linkable_with_PFD_full_fn_FEATHER_ndtms2))
   }

if(!exists("pfd_linkable")|forceReload==T) {
   print("Loading linkable PFD")
   pfd_linkable<-data.table(read_feather(PFD_linkable_with_NDTMS_fn_FEATHER))
}

print("Performing deterministic linkage")
# deterministically link the two datasets
match_cols<-c("FirstNameInitial","LastNameInitial","DOB","Sex","ForceName")
links<-merge(pfd_linkable,ndtms_linkable,by=match_cols,all=F)
# keep only those with a one-to-one link
links[, matchCount := .N, by = match_cols]
links<-links[matchCount==1,c("PseudoID.x","PseudoID.y")]
setnames(links,"PseudoID.x","PseudoID_PFD")
setnames(links,"PseudoID.y","PseudoID_NDTMS")

#links[,c("PseudoID_PFD","PseudoID_NDTMS")]
print("Writing linkage map")
arrow::write_feather(links,NDTMS_to_PFD_unique_link_map_fn_ndtms2)
