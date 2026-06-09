source("../common/r/file_paths.R")

tmpDir <- "D:/StanTemp"
Sys.setenv(TEMP = tmpDir)
Sys.setenv(TMP = tmpDir)
Sys.setenv(TMPDIR = tmpDir)
dir.create(tmpDir, showWarnings = FALSE)

library(data.table)
library(brms)

# this speeds up brms
library(cmdstanr)
options(mc.cores = 4)
options(brms.backend = "cmdstanr")


# reload options
gForceReloadPFDA <- F
gForceRemodelBayesEthnicity <- F
gForceReImputeBayesEthnicity <- F

# load in the analysis dataset for imputation
if(!exists("pfda")|gForceReloadPFDA==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
   pfda[,HOOutcomeCodeLong:=as.factor(HOOutcomeCodeLong)]
}

# load in the imputation formulae
source("03a_bayesian_imputation_vars.R")

# do the imputation just on the predictors
pfdaImputationSubset <- pfda[,..imputationVariables]
pfdaImputationSubset[,EthnicityCoarse:=as.factor(EthnicityCoarse)]
# drop missing sex
pfdaImputationSubset <- pfdaImputationSubset[!is.na(Sex)]

# this is for repeatability, we need a bunch of random seeds
# which are random but depend on the master seed 
set.seed(123)
randomSeeds <- sample(124:523)
# legacy we used 123 as the first seed before
randomSeeds <- c(123,randomSeeds)

gRandomSeedIndex <- 0
nextRandomSeed <- function() {
   # note global incrementation
   gRandomSeedIndex <<- gRandomSeedIndex + 1
   randomSeeds[gRandomSeedIndex]
}

groups <- 1:2

# build a separate model for each group
bayesEthnicityModels <- lapply(groups,function(group) {
   # get a unique model name
   bayesModelOF <- paste0(analysis_path,"/BayesEthnicityModel_G",group,".rds")
   
   # get the data for this group, it has to be complete case for the model
   # and we get rid of sex and HO outcomecode long
   bayesDataFull <- pfdaImputationSubset[CohortGroup==group]
   bayesDataCompleteCase <- bayesDataFull[
      !is.na(EthnicityCoarse),
      -c("HOOutcomeCodeLong")
   ]
   
   # load in the existing model to save time unless forced to
   if(file.exists(bayesModelOF)&!gForceRemodelBayesEthnicity) {
      print(paste0("Loading bayesian ethnicity imputation model for group ",group))
      bayesEthnicityModel <- readRDS(bayesModelOF)
   } else {
      # otherwise construct the ethnicity model from scratch
      print(paste0("Building bayesian ethnicity imputation model for group ",group))
      bayesEthnicityModel <- brm(
         formula = imputationFormulae$EthnicityCoarse,
         data = bayesDataCompleteCase,
         family = categorical(),
         chains = 2,          # fewer chains (2 is enough for imputation)
         iter = 1000,         # 500 warmup + 500 sampling
         warmup = 500,
         cores = 2,
         seed = nextRandomSeed()
      )
      
      # save the data
      saveRDS(bayesEthnicityModel, file = bayesModelOF)
   }
   
   # return a list of the data we used and the model
   list(
      dataFull=bayesDataFull,
      dataCompleteCase=bayesDataCompleteCase,
      model=bayesEthnicityModel
   )
})
names(bayesEthnicityModels) <- groups

# now make 20 imputations for the missing ethnicities in each group
print("Imputating missing ethnicity")
numImputations <- 20
bayesEthnicityImputations <- lapply(groups,function(group) {
   print(paste0("Making bayes predictions for group "),group)
   
   # if it's already done we skip it
   bayesImputationOF <- paste0(analysis_path,"/BayesEthnicityImputation_G",group,".rds")
   if(file.exists(bayesImputationOF)&!gForceReImputeBayesEthnicity) {
      print("Found existing imputation, reloading")
      bayesLabelled <- readRDS(bayesImputationOF)
      return(bayesLabelled)
   }
   
   print("(Re)imputing from scratch")
   
   dataFull <- bayesEthnicityModels[[group]][["dataFull"]]
   dataMissing <- dataFull[is.na(EthnicityCoarse)]
   # need to refactor the OffenceSimplified2 for group
   dataMissing[,OffenceSimplified2:=fdroplevels(OffenceSimplified2)]
   model <- bayesEthnicityModels[[group]][["model"]]
   bayesImputations <- posterior_predict(model, newdata = dataMissing, ndraws = numImputations)
   bayesImputationsLevels <- levels(dataMissing$EthnicityCoarse)[bayesImputations]
   # extract the levels rather than indices as a matrix
   bayesLabelled <- data.table(t(matrix(attr(bayesImputations, "levels")[bayesImputations],
                         nrow = nrow(bayesImputations),
                         ncol = ncol(bayesImputations))))
   names(bayesLabelled) <- as.character(c(1:numImputations))
   # add in the prefix
   # reconstruct the data
   bayesLabelled[,GroupedIncidentID:=dataMissing$GroupedIncidentID]
   setcolorder(bayesLabelled,"GroupedIncidentID")
   
   # save imputation
   print("Saving imputation model")
   saveRDS(bayesLabelled,bayesImputationOF)
   
   bayesLabelled
})
names(bayesEthnicityImputations) <- as.character(groups)