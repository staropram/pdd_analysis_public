imputationVariableList = list(
   
   idVariables = list("GroupedIncidentID"),
         
   predictedyVariables = list(
      "EthnicityCoarse",
      "Sex",
      "HOOutcomeCodeLong"
   ),
      
   predictorVariables = list(
      "CohortGroup",
      "ForceName",
      "WasDiverted",
      "AgeAtContactInDays_CenterStandard",
      "AgeAtContactInDaysSquared_CenterStandard",
      "TreatedForDrugsInLastFiveYears",
      "OffenceSimplified2",
      "MultiOffenceIncident"
  )
)
imputationVariables <- unlist(imputationVariableList,use.names=F,recursive=T)

# the imputation pipeline is going to be as follows
# 1. Construct bayes model for Ethnicity and predict
# 2. Use that to predict sex
# 3. Use that to predict outcome code

# these are the predictors we'll use in each step
basePredictors <- paste(
   "WasDiverted",
   "Sex",
   "AgeAtContactInDays_CenterStandard",
   "AgeAtContactInDaysSquared_CenterStandard",
   "TreatedForDrugsInLastFiveYears",
   "OffenceSimplified2",
   "MultiOffenceIncident",
   "(1 | ForceName)",
   sep = " + "
)

# these are the three prediction models in order to chain imputation
imputationFormulae <- list(
   EthnicityCoarse = as.formula(paste("EthnicityCoarse ~", basePredictors)),
   Sex = as.formula(paste("Sex ~ EthnicityCoarse +", basePredictors)),
   HOOutcomeCodeLong = as.formula(paste("HOOutcomeCodeLong ~ EthnicityCoarse +", basePredictors))
)
