library(fst)
library(arrow)
library(data.table)
library(fastLink)

# flag to force reloading of source data
forceReload <- F

# load in the list of forces
source("../../common/r/force_list.R")

# load in the paths of all required input and output files
source("../../common/r/file_paths.R")

# load linkable ndtms data
if(!exists("ndtms_linkable")|forceReload) {
   ndtms_linkable_with_pfd <- read_fst(NDTMS_linkable_with_PFD_fn_FST,as.data.table = T)
}

# load in the police force data
if(!exists("pfd_linkable")|forceReload) {
   pfd_linkable_with_ndtms <- read_fst(PFD_linkable_with_NDTMS_fn_FST,as.data.table=T)
}


#fastlink_out <- fastLink(pfd_linkable,ndtms_linkable,  varnames = c("FirstNameInitial", "LastNameInitial"))
