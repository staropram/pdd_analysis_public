# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# load in the clean data
if(!exists("pfd")) {
   pfd <- data.table(read_feather(PFD_complete_fn_FEATHER))
}

# this contains the function makeLinkablePFD I put it in a separate file
# as another script uses it
source("03_A_linkage_dataset_functions.R")

pfd_linkable_with_NDTMS <- makePFDLinkableWithNDTMS(pfd)

# write linkable data for PFD <--> NDTMS
print("Writing link data for PFD <--> NDTMS ")
#print("Writing FST")
#fst::write_fst(pfd_linkable_with_NDTMS,PFD_linkable_with_NDTMS_fn_FST)
print("Writing Feather")
arrow::write_feather(pfd_linkable_with_NDTMS,PFD_linkable_with_NDTMS_fn_FEATHER)

# write linkable data for PFD <--> PNC
pfd_linkable_with_PNC <- makePFDLinkableWithPNC(pfd)
print("Writing link data for PFD <--> PNC")
arrow::write_feather(pfd_linkable_with_PNC,PFD_linkable_with_PNC_fn_FEATHER)
write_xlsx(pfd_linkable_with_PNC,PFD_linkable_with_PNC_fn_XLSX)
