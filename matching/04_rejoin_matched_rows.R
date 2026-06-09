# this script takes the matches previously established and then
# recreates the canonical matched controls using the police force data
# this is useful when the police force data has changed or has new
# variables added so you don't have to run the matching all again
# because normally the matching variables have not changed
source("../common/r/file_paths.R")

# we need the pfda data
gForceReload <- T
if(!exists("pfda")|gForceReload==T) {
   print("Loading final PFD analysis dataset")
   pfda <- read_feather(PFD_analysis_full)
}


# load the information about which matches to use for canonical
optimal_k_table <- data.table(read.table(file="outputs/optimal_iv_force_k_selection.tsv",sep="\t",header=T))

# load the match data in and re-join it to the pfda data
x <- lapply(1:nrow(optimal_k_table),function(i){
   forceName <- optimal_k_table[i,ForceName]
   cohortGroup <- optimal_k_table[i,CohortGroup]
   k <- optimal_k_table[i,K]
   print(paste0("Loading matches for ",forceName,", G",cohortGroup," and K=",k))
   # get the data, need to reload this
   inFN <-  analysisDataPaths[[forceName]]$PlusMatchedControl[[cohortGroup]]
   inFN <- paste0(inFN,"_MR",k,".feather")
   d <- read_feather(inFN)
   
   # we just want the grouped incident ids
   groupedIncidentIDsToMatch <- data.table(GroupedIncidentID=d$GroupedIncidentID)
   
   # join again on the PFDA data
   newForceData <- pfda[groupedIncidentIDsToMatch,on="GroupedIncidentID",nomatch=0]
   
   # set outfile and write
   outFN <-  paste0(analysisDataPaths[[forceName]]$PlusMatchedControl[[cohortGroup]],".feather")
   print(paste0("Writing canonical data for ",forceName,", G",cohortGroup," and K=",k))
   write_feather(newForceData,outFN)
   T
})