library(mice)
source('../common/r/file_paths.R')
# the reason we put imputation strategy in a different file is so we can run
# this rejoining bit independently, and it makes it explicit what strategy we
# engage with
source('00a_imputation_strategies.R')
source('00b_chosen_imputation_strategy.R')
# can also manually override here if necessary, but be careful
#imputationStrategyName <- "ForceOnlyNoDiversion"

imputationModel <- imputationStrategies[[imputationStrategyName]]


print("Joining the imputated data back to the main analysis data PFDA")

# load imputedPFDA if it isn't here
forceReloadImputedPFDA <- F
if(forceReloadImputedPFDA==T | !exists("imputedPFDA")) {
   dataIF <- paste0(analysis_path,'/MultiplyImputed/MultiImputedDataGeneral_',imputationStrategyName,'.RData')
   print(paste0("(Re)Loading imputedPFDA from",dataIF))
   load(dataIF)
}

forceReloadPFDA <- F
if(forceReloadPFDA==T | !exists("pfda")) {
   print("(Re)loading PFDA")
   pfda <-  data.table(read_feather(PFD_analysis_full))
}
# we also need PFDA, yes its in the overall data above at index 0, but lets keep it explicit

# extract the data
# we only care about the imputed variables, our ID, and the imputation
# since we will join back to PFDA
imputedVariables <- c(imputationModel$predictedOnlyVariables,imputationModel$predictorAndPredictorVariables)
imputedDataLong <- as.data.table(complete(imputedPFDA,action="long",include=T))
imputedDataLong <- imputedDataLong[,c("GroupedIncidentID",".imp",..imputedVariables)]

# merge in pfda (- the imputed variables)
print("Merging PFDA into the imputation")
imputedDataLong <- merge(imputedDataLong,pfda[,.SD,.SDcols=!imputedVariables],by="GroupedIncidentID",all.x=T)

# UPDATE DERIVED variables based on imputation
print("Updating derived variables for imputed data sets")

# we imputed HOOutcomeCodeLong so then we need to update the derived variables
# except for .imp==0 as that's our original data
print("Updating WasCriminalised")
imputedDataLong[.imp!=0&!is.na(HOOutcomeCodeLong),
                WasCriminalised:=HOOutcomeCodeLong %in% c("OC01","OC1A","OC03","OC3A")]

# assume the "diverted" flag cannot apply to non-diversion forces. (some forces provided it whilst
# claiming that they had no scheme)
print("Updating WasDivertedOrOC22")
imputedDataLong[,WasDivertedStrict:=WasDiverted]
diversionForces <- pfda[DiversionForce==T,unique(ForceName)]
imputedDataLong[!ForceName %in% diversionForces,WasDivertedStrict:=F]
imputedDataLong[,WasDivertedOrOC22:=WasDivertedStrict]
imputedDataLong[!is.na(HOOutcomeCodeLong)&HOOutcomeCodeLong=="OC22",WasDivertedOrOC22:=T]

# save this for analysis
imputedLongOutputFN <- paste0(analysis_path,"PFD_Plus_Imputed_Long_",imputationStrategyName,".feather")
print(paste0("Writing long data to ",imputedLongOutputFN))
arrow::write_feather(imputedDataLong,imputedLongOutputFN)

#  convert back to data format to be used in modelling
#imputedPFDA <- as.mids(imputedDataLong)