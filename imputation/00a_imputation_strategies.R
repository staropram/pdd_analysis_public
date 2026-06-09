# this is a bunch of different imputation strategies for MICE

imputationStrategies = list(
   "DiversionOnly" = list(
      # VARIABLES USED
      
      # include ID so we can rejoin the data afterwards
      idVariables = c(
         "GroupedIncidentID"
      ),
      
      # variables we want to impute but not use for prediction
      predictedOnlyVariables = c(
         "Sex",
         "EthnicityCoarse",
         "HOOutcomeCodeLong"
      ),
      
      predictorAndPredictorVariables = c(
      ),
      
      predictorOnlyVariables = c(
         "WasDiverted",
         "AgeAtContactInDays_CenterStandard",
         "AgeAtContactInDaysSquared_CenterStandard",
         "TreatedForDrugsInLastFiveYears",
         "OffenceSimplified2",
         "MultiOffenceIncident"
      ),
      
      # IMPUTATION METHODS
      imputationMethods = c(
         # We need to define methods for all the imputed variables
         Sex="logreg",
         EthnicityCoarse="polyreg",
         HOOutcomeCodeLong="polyreg",
         
         # We don't need to set a method for everything else
         # but we have to list them anyway with an empty method
         # note that MICE does this implicitly if a variable is already
         # complete case
         WasDiverted="",
         GroupedIncidentID="",
         AgeAtContactInDays_CenterStandard="",
         AgeAtContactInDaysSquared_CenterStandard="",
         TreatedForDrugsInLastFiveYears="",
         OffenceSimplified2="",
         MultiOffenceIncident=""
      )
      
   ),
   
   "ForceOnly" = list(
      # VARIABLES USED
      
      # include ID so we can rejoin the data afterwards
      idVariables = c(
         "GroupedIncidentID"
      ),
      
      # variables we want to impute but not use for prediction
      predictedOnlyVariables = c(
         "Sex",
         "EthnicityCoarse",
         "HOOutcomeCodeLong"
      ),
      
      predictorAndPredictorVariables = c(
      ),
      
      predictorOnlyVariables = c(
         "ForceName",
         "AgeAtContactInDays_CenterStandard",
         "AgeAtContactInDaysSquared_CenterStandard",
         "TreatedForDrugsInLastFiveYears",
         "OffenceSimplified2",
         "MultiOffenceIncident"
      ),
      
      # IMPUTATION METHODS
      imputationMethods = c(
         # We need to define methods for all the imputed variables
         Sex="logreg",
         EthnicityCoarse="polyreg",
         HOOutcomeCodeLong="polyreg",
         
         # We don't need to set a method for everything else
         # but we have to list them anyway with an empty method
         # note that MICE does this implicitly if a variable is already
         # complete case
         ForceName="",
         GroupedIncidentID="",
         AgeAtContactInDays_CenterStandard="",
         AgeAtContactInDaysSquared_CenterStandard="",
         TreatedForDrugsInLastFiveYears="",
         OffenceSimplified2="",
         MultiOffenceIncident=""
      )
      
   ),
   
   "ForceAndDiversion" = list(
      # VARIABLES USED
      
      # include ID so we can rejoin the data afterwards
      idVariables = c(
         "GroupedIncidentID"
      ),
      
      # variables we want to impute but not use for prediction
      predictedOnlyVariables = c(
         "Sex",
         "EthnicityCoarse",
         "HOOutcomeCodeLong"
      ),
      
      predictorAndPredictorVariables = c(
      ),
      
      predictorOnlyVariables = c(
         "ForceName",
         "WasDiverted",
         "AgeAtContactInDays_CenterStandard",
         "AgeAtContactInDaysSquared_CenterStandard",
         "TreatedForDrugsInLastFiveYears",
         "OffenceSimplified2",
         "MultiOffenceIncident"
      ),
      
      # IMPUTATION METHODS
      imputationMethods = c(
         # We need to define methods for all the imputed variables
         Sex="logreg",
         EthnicityCoarse="polyreg",
         HOOutcomeCodeLong="polyreg",
         
         # We don't need to set a method for everything else
         # but we have to list them anyway with an empty method
         # note that MICE does this implicitly if a variable is already
         # complete case
         ForceName="",
         WasDiverted="",
         GroupedIncidentID="",
         AgeAtContactInDays_CenterStandard="",
         AgeAtContactInDaysSquared_CenterStandard="",
         TreatedForDrugsInLastFiveYears="",
         OffenceSimplified2="",
         MultiOffenceIncident=""
      )
      
   ),
   
   "NameEthnicityForceOnly" = list(
      # VARIABLES USED
      
      # include ID so we can rejoin the data afterwards
      idVariables = c(
         "GroupedIncidentID"
      ),
      
      # variables we want to impute but not use for prediction
      predictedOnlyVariables = c(
         "Sex",
         "EthnicityCoarse",
         "HOOutcomeCodeLong"
      ),
      
      predictorAndPredictorVariables = c(
      ),
      
      predictorOnlyVariables = c(
         "ForceName",
         "FirstNameEthnicity",
         "LastNameEthnicity",
         "AgeAtContactInDays_CenterStandard",
         "AgeAtContactInDaysSquared_CenterStandard",
         "TreatedForDrugsInLastFiveYears",
         "OffenceSimplified2",
         "MultiOffenceIncident"
      ),
      
      # IMPUTATION METHODS
      imputationMethods = c(
         # We need to define methods for all the imputed variables
         Sex="logreg",
         EthnicityCoarse="polyreg",
         HOOutcomeCodeLong="polyreg",
         
         # We don't need to set a method for everything else
         # but we have to list them anyway with an empty method
         # note that MICE does this implicitly if a variable is already
         # complete case
         ForceName="",
         FirstNameEthnicity="",
         LastNameEthnicity="",
         GroupedIncidentID="",
         AgeAtContactInDays_CenterStandard="",
         AgeAtContactInDaysSquared_CenterStandard="",
         TreatedForDrugsInLastFiveYears="",
         OffenceSimplified2="",
         MultiOffenceIncident=""
      )
      
   )
   
)

printImputationStrategy <- function(imputationStrategy) {
   
}