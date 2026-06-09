# this script takes the link predictions from Splink and then makes a final
# 1 <-> 1 map between PFD pseudoID and NDTMS pseudoID

# load in the paths of all required input and output files
source("../../common/r/file_paths.R")

if(!linkUsingSplink) {
   print("Not linking using splink, skipping uniqueness reduction")
} else {
   # required libs
   library(data.table)
   library(knitr)
   library(readxl)
   library(DT)
   
   # flag to control whether pfd data should be reloaded with every execution of the script
   forceReload<-T
   
   # load the link predictions
   if(!exists("ndtms_to_pfd_full_deterministic")|forceReload) {
      print("Reading in PFD <-> NDTMS deterministic link predictions (full)")
      ndtms_to_pfd_full_deterministic<-data.table(arrow::read_feather(NDTMS_to_PFD_deterministic_link_predictions_full_fn_FEATHER_ndtms2))
      setnames(ndtms_to_pfd_full_deterministic,"PseudoID_l","PseudoID_PFD")
      setnames(ndtms_to_pfd_full_deterministic,"PseudoID_r","PseudoID_NDTMS")
   }
   
   # function that takes a link prediction and returns only those links that are
   # 1 <-> 1 mappings
   getUniqueNDTMSLinkPredictions<-function(linkPrediction,tag="") {
      # remove anyone that isn't a 1 <-> 1 match
      nonUniqueLinks_PFD<-linkPrediction[,.N,by=PseudoID_PFD][N>1]$PseudoID_PFD # PFD
      print(paste0("non unique links (PFD):",length(nonUniqueLinks_PFD)))
      nonUniqueLinks_NDTMS<-linkPrediction[,.N,by=PseudoID_NDTMS][N>1]$PseudoID_NDTMS # NDTMS
      print(paste0("non unique links (NDTMS):",length(nonUniqueLinks_NDTMS)))
      linkPredictionUnique<-linkPrediction[!PseudoID_PFD %in% nonUniqueLinks_PFD]
      linkPredictionUnique<-linkPredictionUnique[!PseudoID_NDTMS %in% nonUniqueLinks_NDTMS]
      
      print(paste0("Number of unique linked data: (",tag,") ",nrow(linkPredictionUnique)))
      linkPredictionUnique
   }
   
   # get the link predictions
   link_predictions_full<-getUniqueNDTMSLinkPredictions(ndtms_to_pfd_full_deterministic,"full")
   # only save the link map
   link_predictions_full<-link_predictions_full[,c("PseudoID_PFD","PseudoID_NDTMS")]
   
   # save the link map
   print("Writing link map")
   arrow::write_feather(link_predictions_full,NDTMS_to_PFD_unique_link_map_fn_ndtms2)

}
