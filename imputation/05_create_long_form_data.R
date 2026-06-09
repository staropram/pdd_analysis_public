# this script creates a long-form data to load in by the equity model
source("../common/r/file_paths.R")

# we can also create a "wide" version with 20 separate imputations per group
# for ethnicity and HO outcome: an extra 40 columns

# I suppose we could just have a separate table for the imputations that 
# probably makes more sense, which we then join in at runtime

group <- 1

# load in PFDA which we are going to join to
gForceReloadPFDA <- T
if(!exists("pfda")|gForceReloadPFDA==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
   pfda[,HOOutcomeCodeLong:=as.factor(HOOutcomeCodeLong)]
}

createLongFormData <- function(group) {
   print(paste0("Processing group ",group))
   # load in the 20 Ethnicity imputations
   print("Loading the ethnicity imputations for group")
   bayesG1EthFN <- paste0(analysis_path,"/BayesEthnicityImputation_G",group,".rds")
   dEth <- readRDS(bayesG1EthFN)
   
   # load in the home office outcomes predicted for each of the above ethnicity imputations
   print("Loading the outcome code imputations")
   combinedHOImputations <- paste0(analysis_path,"/BayesHOOutcomeImputation_G",group,"_Combined.rds")
   dHO <- readRDS(combinedHOImputations)
   
   # we need to create 20 datasets, and we'll also include the original data
   # we use the column variable .imp to denote the data
   # remove missing sex from PFDA since we can't use it
   pfda <- pfda[CohortGroup==group&!is.na(Sex),]
   # create the long data and add in the .imp column for each set
   # and add in the imputed data as we go along
   print("Constructing the long-form imputed data")
   imputedDataLong <- rbindlist(lapply(0:20,function(impIndex) {
      d <- copy(pfda)
      # set the index of this data
      d[,.imp:=impIndex]
      
      # if we are not the original data, we need to impute
      if(impIndex!=0) {
         # impute the ethnicity
         d <- merge(d,dEth[,c("GroupedIncidentID",..impIndex)],by="GroupedIncidentID",all.x=T)
         names(d)[names(d)==impIndex] <- "ImputedEthnicityCoarse"
         
         # and merge it in
         d[!is.na(ImputedEthnicityCoarse),EthnicityCoarse:=ImputedEthnicityCoarse]
         
         # impute the reduced home office outcome
         d <- merge(d,dHO[,c("GroupedIncidentID",..impIndex)],by="GroupedIncidentID",all.x=T)
         names(d)[names(d)==impIndex] <- "ImputedReducedHOOutcome"
         
         # now construct the imputed WasDivertedOrOC22
         # It is already accurate according to the known data, so now we "correct"
         # it by adding in the missing data
         d[ImputedReducedHOOutcome=="OC22",WasDivertedOrOC22:=T]
         
         # now construct the imputed WasCriminalised
         d[ImputedReducedHOOutcome %in% c("OC03","OC01"),WasCriminalised:=T]
         d[is.na(WasCriminalised),WasCriminalised:=F]
      }
      
      d
   }),fill=T)
   
   # save the long imputed ethnicity
   imputedLongOutputFN <- paste0(analysis_path,"PFD_Plus_Imputed_Long_Bayes_G",group,".feather")
   print(paste0("Writing long data to ",imputedLongOutputFN))
   arrow::write_feather(imputedDataLong,imputedLongOutputFN)
}

#imputedDataWide <- pfda
#imputedDataWide <- merge(d,dEth,by="GroupedIncidentID",all.x=T)

createLongFormData(1)
createLongFormData(2)
