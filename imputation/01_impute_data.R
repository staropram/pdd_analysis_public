source("../common/r/file_paths.R")

# XXX note we change the imputation strategy here and this will cascade to all
# scripts that follow, thus it is best to change it in the sourced file
# but you can override for testing
source("00a_imputation_strategies.R")
source("00b_chosen_imputation_strategy.R")
#imputationStrategyName <- "ForceOnlyNoDiversion"

print(paste0("Using imputation strategy: ",imputationStrategyName))

source("01a_create_ethnicity_predictors.R")
library(mice)
library(Amelia)
library(mitools)
library(broom.mixed)
library(sjPlot)

# OPTIONS
# Should we force reloading of source data?
gForceReload <- T
# Should we force recompute of the imputed data
# if false, then script tries to reload saved data
forceRecomputeImputation <- T

# load in the analysis dataset for imputation
if(!exists("pfda")|gForceReload==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
   pfda[,HOOutcomeCodeLong:=as.factor(HOOutcomeCodeLong)]
}


# get the imputation strategy
imputationModel <- imputationStrategies[[imputationStrategyName]]

print(paste0("Imputation strategy ",imputationStrategyName,", details:"))
print(imputationModel)


imputationVariables <- c(
   imputationModel$idVariables,
   imputationModel$predictedOnlyVariables,
   imputationModel$predictorOnlyVariables
)

# this script creates ethnicity predictors and then merges them back into pfda
# we only need to run it if we are including the name-based ethnicity predictors
if("FirstNameEthnicity" %in% imputationVariables | "LastNameEthnicity" %in% imputationVariables) {
   print("Imputation model uses FirstNameEthnicity or LastNameEthnicity, creating those variables now")
   pfda <- createEthnicityPredictors(pfda,forceReloadPoliceData=T,mergeColumns=F)
}

# we only care about our imputation variables
pfdaAnalysis <- pfda[,..imputationVariables]

# the model doesn't converge unless we refactor our HOOutcodeCodeLong as there
# isn't enough information in the rarer outcomes, of course we only need
# to do this if the imputation model uses this variable
if("HOOutcomeCodeLong" %in% imputationVariables) {
   print("Imputation model uses HOOutcomeCodeLong, refactoring to allow convergence")
   keepLevels <- c("OC01","OC03","OC3A","OC08","OC22","Other")
   pfdaAnalysis[, HOOutcomeCodeLong := as.character(HOOutcomeCodeLong)]
   pfdaAnalysis[!(HOOutcomeCodeLong %in% keepLevels), HOOutcomeCodeLong := "Other"]
   pfdaAnalysis[, HOOutcomeCodeLong := factor(HOOutcomeCodeLong, levels = keepLevels)]
}


# this next bit is about creating the actual prediction matrix

# make sure that we do not impute id variables
# nor use them as predictors for other variables
# the next function creates a matrix of all 1s with 0s on diagonal
predictorMatrix <- make.predictorMatrix(pfdaAnalysis)
# setting the row to 0 means these variables will not be imputed
predictorMatrix[imputationModel$idVariables,] <- 0
# setting the col to 0 means these variables will not be used as predictors
predictorMatrix[,imputationModel$idVariables] <- 0

# Some other variables we want to use as predictors but not impute
# so we just set their rows to 0, this means that no variables are
# used to predict these variables hence they are not imputed
predictorMatrix[imputationModel$predictorOnlyVariables,] <- 0

# we don't want to use the predicted variables as predictors, but we
# want to predict them, so we set the cols of these to zero
predictorMatrix[,imputationModel$predictedOnlyVariables] <- 0

# note the variables that are both predictor and predictor are already 1

print("Performing the multi-imputation")
dataOF <- paste0(analysis_path,'/MultiplyImputed/MultiImputedDataGeneral_',imputationStrategyName,'.RData')


# try and load in existing models if not forcing recompute
loadedExistingData <- F
if(forceRecomputeImputation==F) {
   if(file.exists(dataOF)) {
      print("Loading existing multi-imputed data")
      load(dataOF)
      loadedExistingData <- T
      print("DONE")
   } else {
      print("Existing multi-imputed data not found, have to re-impute")
   }
} else {
   print("Have been asked to re-impute data")
}


if(!exists("imputedPFDA")|forceRecomputeImputation==T) {
   print("Re-imputing data now")
   # do the imputation in parallel
   imputedPFDA <- futuremice(
      pfdaAnalysis,
      m=20,
      n.core=4,
      parallelseed=123,
      predictorMatrix=predictorMatrix,
      method=imputationModel$imputationMethods
   )
   
   # save the imputed data
   print("Saving multi-imputed data")
   # we only need to save the variables we actually imputed
   save(imputedPFDA,file=dataOF)
}

#formula <- "EthnicityMissing ~ FirstNameEthnicity + AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard + Sex + OffenceSimplified2 + (1|ForceName)"
#m <- glmmTMB(EthnicityMissing ~ AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard + Sex + OffenceSimplified2 + (1|ForceName),data=d)
