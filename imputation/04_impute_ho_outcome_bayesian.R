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
gForceRemodelBayesHOOutcome <- F
gForceReImputeBayesHOOutcome <- F

# load in the analysis dataset for imputation
if(!exists("pfda")|gForceReloadPFDA==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
   pfda[,HOOutcomeCodeLong:=as.factor(HOOutcomeCodeLong)]
}

# load in the imputation formulae
source("03a_bayesian_imputation_vars.R")

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

# load in the imputed ethnicity datasets
groups <- 1:2
imputedEthnicityData <- lapply(groups,function(group){
   bayesImputationIF <- paste0(analysis_path,"/BayesEthnicityImputation_G",group,".rds")
   readRDS(bayesImputationIF)
})
names(imputedEthnicityData) <- as.character(groups)
numImputations <- ncol(imputedEthnicityData[[1]])-1

# now we need to construct a single model for each imputed ethnicity
# we decided we will just dump Sex when its missing
# subset pfda to only the imputation columns
# do the imputation just on the predictors
pfdaImputationSubset <- pfda[,..imputationVariables]
pfdaImputationSubset[,EthnicityCoarse:=as.factor(EthnicityCoarse)]
# drop missing sex
pfdaImputationSubset <- pfdaImputationSubset[!is.na(Sex)]
# the model takess too long if we keep HOOutcomeCodeLong as-is
# so we need to make it simpler
# isn't enough information in the rarer outcomes, of course we only need
# to do this if the imputation model uses this variable
print("Imputation model uses HOOutcomeCodeLong, refactoring to allow convergence")
keepLevels <- c("OC01","OC03","OC3A","OC08","OC22","Other")
pfdaImputationSubset[, HOOutcomeCodeLong := as.character(HOOutcomeCodeLong)]
pfdaImputationSubset[!is.na(HOOutcomeCodeLong)&!(HOOutcomeCodeLong %in% keepLevels), HOOutcomeCodeLong := "Other"]
pfdaImputationSubset[HOOutcomeCodeLong=="OC3A",HOOutcomeCodeLong:="OC03"]
pfdaImputationSubset[, HOOutcomeCodeLong := as.factor(HOOutcomeCodeLong)]


# build a separate model for each group and for each imputation
bayesHOOutcomeModels <- lapply(groups,function(group) {
   modelsByImputation <- lapply(1:numImputations,function(imputationIndex) {
      # get a unique model name
      bayesModelOF <- paste0(analysis_path,"/BayesHOOutcomeModel_G",group,"_Imputation",imputationIndex,".rds")
      
      # get the data we need
      imputedColumn <- paste(imputationIndex)
      imputedData <- imputedEthnicityData[[group]][,c("GroupedIncidentID",..imputedColumn)]
      names(imputedData)[2] <- "EthnicityCoarseImputed"
      # put the imputed ethnicity into the data
      modelData <- merge(pfdaImputationSubset[CohortGroup==group],imputedData,by="GroupedIncidentID",all.x=T)
      modelData[is.na(EthnicityCoarse),EthnicityCoarse:=EthnicityCoarseImputed]
      
      # now construct the home office outcome model
      # load in the existing model to save time unless forced to
      if(file.exists(bayesModelOF)&!gForceRemodelBayesHOOutcome) {
         print(paste0("Loading bayesian HO outcome imputation model for group=",group," ethnicity imputation=",imputationIndex))
         bayesHOOutcomeModel <- readRDS(bayesModelOF)
      } else {
         # otherwise construct the ethnicity model from scratch
         print(paste0("Building bayesian HO outcome imputation model for group=",group," ethnicity imputation=",imputationIndex))
         bayesHOOutcomeModel <- brm(
            formula = imputationFormulae$HOOutcomeCodeLong,
            data = modelData,
            family = categorical(),
            chains = 2,          # fewer chains (2 is enough for imputation)
            iter = 1000,         # 500 warmup + 500 sampling
            warmup = 500,
            cores = 2,
            seed = nextRandomSeed()
         )
         
         # save the data
         print("Saving model")
         saveRDS(bayesHOOutcomeModel, file = bayesModelOF)
      }
      
      list(
         data=modelData,
         model=bayesHOOutcomeModel
      )
   })
   names(modelsByImputation) <- as.character(1:numImputations)
   modelsByImputation
})
names(bayesHOOutcomeModels) <- as.character(groups)

numDraws <- 1

bayesHOOutcomeDatas <- lapply(groups,function(group) {
   lapply(1:numImputations,function(imputationIndex) {
      bayesImputationOF <- paste0(analysis_path,"/BayesHOOutcomeImputation_G",group,"_Imputation",imputationIndex,".rds")
      
      # get the current model
      currentModel <- bayesHOOutcomeModels[[group]][[imputationIndex]]
      
      dataFull <- currentModel$data
      dataMissing <- dataFull[is.na(HOOutcomeCodeLong)]
      # need to refactor the OffenceSimplified2 for group
      dataMissing[,OffenceSimplified2:=fdroplevels(OffenceSimplified2)]
      model <- currentModel$model
      bayesImputations <- posterior_predict(model, newdata = dataMissing, ndraws = numDraws)
      bayesImputationsLevels <- levels(dataMissing$HOOutcomeCodeLong)[bayesImputations]
      # extract the levels rather than indices as a matrix
      bayesLabelled <- data.table(t(matrix(attr(bayesImputations, "levels")[bayesImputations],
                            nrow = nrow(bayesImputations),
                            ncol = ncol(bayesImputations))))
      names(bayesLabelled) <- as.character(c(1:numDraws))
      # add in the prefix
      # reconstruct the data
      bayesLabelled[,GroupedIncidentID:=dataMissing$GroupedIncidentID]
      setcolorder(bayesLabelled,"GroupedIncidentID")
      # save imputation
      print(paste0("Saving imputated data for group ",group," and Ethnicity imputation ",imputationIndex))
      saveRDS(bayesLabelled,bayesImputationOF)
      bayesLabelled
   })
})

# collapse this into one dataset since we only have one draw per ethnicity base
g1Data <- bayesHOOutcomeDatas[[1]]
g1DataBound <- Reduce(function(a,b){
   names(b)[2] <- as.numeric(names(a)[length(names(a))]) + 1
   merge(a,b,by="GroupedIncidentID",all.x=T)},
   g1Data
)

# save this
combinedOutputFile <- paste0(analysis_path,"/BayesHOOutcomeImputation_G1_Combined.rds")
saveRDS(g1DataBound,combinedOutputFile)

# we will combine this again later into a single file
g2Data <- bayesHOOutcomeDatas[[2]]
g2DataBound <- Reduce(function(a,b){
   names(b)[2] <- as.numeric(names(a)[length(names(a))]) + 1
   merge(a,b,by="GroupedIncidentID",all.x=T)},
   g2Data
)
combinedOutputFile <- paste0(analysis_path,"/BayesHOOutcomeImputation_G2_Combined.rds")
saveRDS(g2DataBound,combinedOutputFile)