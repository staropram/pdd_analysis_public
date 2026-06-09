# purpose here it to sanity check our imputed data
source("../common/r/file_paths.R")

# load in the original analysis dataset and the two longform imputed datasets
gForceReloadData <- F
if(!exists("pfda")|gForceReloadData==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
   pfda[,HOOutcomeCodeLong:=as.factor(HOOutcomeCodeLong)]
}


if(!exists("pfdaImputedG1")|gForceReloadData==T) {
   print("Loading imputed dataset for group 1")
   fn <- paste0(analysis_path,"PFD_Plus_Imputed_Long_Bayes_G1.feather")
   pfdaImputedG1 <- read_feather(fn)
}

if(!exists("pfdaImputedG2")|gForceReloadData==T) {
   print("Loading imputed dataset for group 2")
   fn <- paste0(analysis_path,"PFD_Plus_Imputed_Long_Bayes_G2.feather")
   pfdaImputedG2 <- read_feather(fn)
}

getStatsRow <- function(dName) {
   d <- get(dName)
   data.table(
      Name=dName,
      NumParticipant=uniqueN(d$PseudoID),
      NumIncident=uniqueN(d$GroupedIncidentID)
   )
}

pfdaG1 <- pfda[!is.na(Sex)&CohortGroup==1]
pfdaG2 <- pfda[!is.na(Sex)&CohortGroup==2]

dataToCompare <- list("pfdaG1","pfdaImputedG1","pfdaG2","pfdaImputedG2")
stats <- rbindlist(lapply(dataToCompare,function(dName) {
   getStatsRow(dName)
}))

print(stats)

# load in the separate imputations
print("Loading in separate imputations")
imputationsHOOutcome <- read_feather(bayes_ho_outcome_imputation_fn)
imputationsEthnicity <- read_feather(bayes_ethnicity_imputation_fn)

# check that number of imputations matches num missing
numMissingHOO <- pfda[!is.na(Sex)&is.na(HOOutcomeCodeLong),.N]
numUniqueHOOImputations <- uniqueN(imputationsHOOutcome$GroupedIncidentID)
numHOOImputationRows <- nrow(imputationsHOOutcome)
cat("Checking number of missing HO outcomes matches number imputed HO outcomes: ")
if(numMissingHOO==numUniqueHOOImputations & numMissingHOO==numHOOImputationRows)
   { cat("PASS\n") } else { stop("FAIL") }
numMissingEth <- pfda[!is.na(Sex)&is.na(EthnicityCoarse),.N]
numUniqueEthImputations <- uniqueN(imputationsEthnicity$GroupedIncidentID)
numEthImputationRows <- nrow(imputationsEthnicity)
cat("Checking number of missing ethnicities matches number imputed ethnicities: ")
if(numMissingEth==numUniqueEthImputations & numMissingEth==numEthImputationRows) 
   { cat("PASS\n") } else { stop("FAIL") }

# check that the IDs for the missing things match the missing ids
missingHOOIDs <- pfda[!is.na(Sex)&is.na(HOOutcomeCodeLong),GroupedIncidentID]
allMissingHOOImputed <- 
   all(missingHOOIDs %in% imputationsHOOutcome$GroupedIncidentID) &
   all(imputationsHOOutcome$GroupedIncidentID %in% missingHOOIDs)
cat("Checking that all missing HO outcome IDs have been imputed: ")
if(allMissingHOOImputed)
   { cat("PASS\n") } else { stop("FAIL") }
   
missingEthIDs <- pfda[!is.na(Sex)&is.na(EthnicityCoarse),GroupedIncidentID]
allMissingEthImputed <- 
   all(missingEthIDs %in% imputationsEthnicity$GroupedIncidentID) &
   all(imputationsEthnicity$GroupedIncidentID %in% missingEthIDs)
cat("Checking that all missing ethnicity IDs have been imputed: ")
if(allMissingEthImputed)
   { cat("PASS\n") } else { stop("FAIL") }