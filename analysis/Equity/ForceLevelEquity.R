# make the equity comparison for a given force
source("../../common/r/file_paths.R")
library(lme4)
library(glmmTMB)
library(ResourceSelection)
library(pROC)
library(sjPlot)
library(metafor)

set.seed(123)

gForceReload <- F
if(!exists("pfda")|gForceReload==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
   pfda1 <- pfda[CohortGroup==1]
   pfda2 <- pfda[CohortGroup==2]
}

pfda[,WasDivertedOrOC22OrOC08:=WasDiverted|(HOOutcomeCodeLong=="OC22")|(HOOutcomeCodeLong=="OC08")]

diversionForces <- pfda[DiversionForce==T,unique(ForceName)]
allForces <- pfda[,unique(ForceName)]
#diversionForces <- pfda[,unique(ForceName)]

equityFormula <- "WasDivertedOrOC22~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse 
   + OffenceSimplified"
equityFormula <-  as.formula(gsub("\n","",equityFormula))

# run the model for each diversion force

# load in multi-imputed data

miceDataFN <- paste0(analysis_path,"/MultiplyImputed/MultiImputedData.RData")
miceData <- load(miceDataFN)


extractMetaData <- function(models,ethnicityFilter) {
  rbindlist(lapply(names(models), function(modelName) {
      model <- models[[modelName]]
      coefSummary <- summary(model)$coefficients$cond  # Extract coefficients table
      data.table(
         Force = modelName,
         LogOdds = coefSummary[ethnicityFilter, "Estimate"],
         SE = coefSummary[ethnicityFilter, "Std. Error"]
      )
   }))
}

# run Asian model for each data
createMetaModel <- function(d,forcesOfInterest) {
   # run the models
   models <- lapply(forcesOfInterest,function(forceName) {
      print(paste0("Making model for ",forceName))
      glmmTMB(equityFormula,d[ForceName==forceName],family="binomial")
   })
   names(models) <- forcesOfInterest
   # extract Asian and black results
   resultsAsian <- extractMetaData(models,"EthnicityCoarseASIAN")
   resultsBlack <- extractMetaData(models,"EthnicityCoarseBLACK")
   metaAsian <- rma(yi = LogOdds, sei = SE, data = resultsAsian, method = "FE",level=95)
   metaBlack <- rma(yi = LogOdds, sei = SE, data = resultsBlack, method = "FE",level=95)
   list(
      asian=list(
         results=resultsAsian,
         meta=metaAsian
      ),
      black=list(
         results=resultsBlack,
         meta=metaBlack
      )
   )
}

testMeta <- createMetaModel(pfda1,diversionForces)

forest(testMeta$black$meta,slab = testMeta$black$results$Force,
       xlab = "Log-Odds Ratio (Black vs. White)",mlab = "Pooled Effect",
       col = "blue",order = "obs")
forest(testMeta$asian$meta,slab = testMeta$asian$results$Force,
       xlab = "Log-Odds Ratio (Asian vs. White)",mlab = "Pooled Effect",
       col = "blue",order = "obs")

# now do the meta for each imputed data
makeMetaModels <- T
if(makeMetaModels) {
   metaModels <- lapply(1:20,function(index) {
      print(paste0("Creating meta model",index))
      d <- data.table(complete(imputedPFDA,action=index))
      # we only want cohort 1
      d <- d[!OffenceSimplified %in% c("Shoplifting","Other theft","Assault","Criminal damage and arson","Assault on an emergency worker or constable")]
      d[,WasDivertedOrOC22:=(WasDiverted==T)|(HOOutcomeCodeLong=="OC22")]
      d[,WasDivertedOrOC22OrOC08:=(WasDivertedOrOC22==T)|(HOOutcomeCodeLong=="OC08")]
      # we only want to keep diversion forces
      d <- d[ForceName %in% diversionForces]
      createMetaModel(d,diversionForces)
   })
}