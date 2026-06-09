## this serves as a placeholder for the linkage which is performed outside
# of R in splink, all we want to do here is check if the linkage files
# exist and if they do not, prompt the user to go and perform the linkage

# load the file paths in
source("../../common/r/file_paths.R")

linkUsingSplink <- F

if(linkUsingSplink) {
   # check if linkage outputs exist
   if(!file.exists(NDTMS_to_PFD_deterministic_link_predictions_full_fn_FEATHER_ndtms2)) {
      stop("In order to continue you need to perform the linkage in splink")
   }
} else {
   source("04a_deterministic_link_ndtms2.R")
}
