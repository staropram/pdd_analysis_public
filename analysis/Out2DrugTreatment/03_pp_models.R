source("01_import_data.R")
source("rubins_rules.R")
library(lme4)
library(glmmTMB)
library(mitools)
library(emmeans)

# This is the new analysis to be ran on the MOJ system
# There are restrictions here, NDTMS data must only be
# kept in memory, never on disk

# global variables
loadFinalModelsFromDisk <- T
numImputations <- 20

# seed
set.seed(134)

ppDrugTreatmentFormula <- "EnteredIntoDrugTreatment ~ WasDivertedOrOC22 + (1|ForceName)
+ offset(LogDrugTreatmentExposureTimeDays) + OffenceSimplified2
+ AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard + Sex + EthnicityCoarse
+ ForceReoffendingRate + ForceOCUPenetration + ForceFunding
+ HistoricalPossessionCount + HistoricalOtherDrugsCount
+ HistoricalTheftCount + HistoricalViolenceCount
+ TreatedForDrugsInLastFiveYears"
ppDrugTreatmentFormula <- gsub("\n","",ppDrugTreatmentFormula)
ppDrugTreatmentFormulaCox <- "Surv(DrugTreatmentExposureTimeDays,EnteredIntoDrugTreatment) ~ WasDivertedOrOC22 + (1|ForceName)
+ OffenceSimplified2
+ AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard + Sex + EthnicityCoarse
+ ForceReoffendingRate + ForceOCUPenetration + ForceFunding
+ HistoricalPossessionCount + HistoricalOtherDrugsCount
+ HistoricalTheftCount + HistoricalViolenceCount
+ TreatedForDrugsInLastFiveYears"
ppDrugTreatmentFormulaCox <- gsub("\n","",ppDrugTreatmentFormulaCox)

# function to run a model given a formula f, which allows
# different stats frameworks to be used
# f - formula
# d - data
# modelType - see code
runModelDrugTreatment <- function(f,d,modelType) {
   switch(modelType,
      glmer1 = glmer(
         f,
         data=d,
         family=binomial,
         control = glmerControl(optimizer = "Nelder_Mead")
      ),
      glmmTMB1 = glmmTMB(
         as.formula(f),
         data=d,
         family=binomial
      ),
      cox = coxme(
         as.formula(f),
         data=d
      )
   )
}

# create a model for each imputation
createPPModelsDrugTreatment <- function(cohortGroup,modelType,ppFormula,ddss=F,coxModel=F) {
   ppModels <- lapply(1:numImputations,function(imputationIndex) {
      print(paste0("Constructing model for imputation ",imputationIndex))
      
      # get the current imputation
      modelData <- pfda_imputed[[imputationIndex]]
      modelData <- modelData[CohortGroup==cohortGroup]
      
      print(paste0("Using formula: ",ppFormula))
      
      # merge in the historical crime variables
      modelData <- merge(modelData,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
      
      # merge in the drug treatment variables
      # remove the old variable first
      modelData[,EnteredIntoDrugTreatment:=NULL]
      modelData[,WasInTreatmentWithin28DaysPriorToContact:=NULL]
      modelData <- merge(modelData,drugTreatment,by="RandomGroupedIncidentID",all.x=T)
      
      if(coxModel==T) {
         modelData <- merge(
            modelData,
            pfda[,.(RandomGroupedIncidentID,DrugTreatmentExposureTimeDays)],
            by="RandomGroupedIncidentID",
            all.x=T
         )
      }
      
      # if this is the DDSS model, construct the disposal categories
      if(ddss) {
         print("Creating disposal categories for DDSS")
         modelData[,DisposalCategory:="Other"]
         # fill in the imputed outcome (we ignore other as its already the default)
         modelData[!is.na(ImputedHOOutcome)&!(ImputedHOOutcome=="Other"),HOOutcomeCodeLong:=ImputedHOOutcome]
         modelData[HOOutcomeCodeLong %in% c("OC08","OC22"),DisposalCategory:="Diverted"]
         modelData[HOOutcomeCodeLong %in% c("OC01","OC1A"),DisposalCategory:="Charged"]
         # 2 and 2A shouldn't strictly be in here but they are
         modelData[HOOutcomeCodeLong %in% c("OC02","OC2A"),DisposalCategory:="Cautioned"]
         modelData[HOOutcomeCodeLong %in% c("OC03","OC3A"),DisposalCategory:="Cautioned"]
         modelData[,DisposalCategory:=factor(DisposalCategory,levels = c("Charged", "Cautioned", "Diverted", "Other"))]
         
      }
      
      # exclude those in treatment within 28 days before
      modelData <- modelData[WasInTreatmentWithin28DaysPriorToContact==F]
      
      # drop unused levels
      modelData <- droplevels(modelData)
      
      # run the model
      currentModel <- runModelDrugTreatment(ppFormula,modelData,modelType)
      
      currentModel
   })
   ppModels
}


# this function gets the needed stats from a model
# m - the model
# term - the term to get stats for, eg. DiversionForceTRUE
extractModelEstimatesPP <- function(m, term) {
   modelSummary <- summary(m)
   modelCoefficients <- modelSummary$coefficients
   if(inherits(m,"glmmTMB")) {
      modelCoefficients <- modelCoefficients$cond
   }
   est <- modelCoefficients[term, "Estimate"]
   se  <- modelCoefficients[term, "Std. Error"]
   ci_low <- est - 1.96 * se
   ci_high <- est + 1.96 * se
   data.table(term = term, est, se, ci_low, ci_high)
}

extractAllEstimatesPP <- function(models,term) {
   imputationEstimates <- rbindlist(lapply(1:numImputations,function(imputationIndex) {
      m <-  models[[imputationIndex]]
      d <- extractModelEstimatesPP(m,term)
      d[,ImputationIndex:=imputationIndex]
      d
   }))
   imputationEstimates
}

print("Constructing PP models for group 1 EnteredIntoDrugTreatment ~ WasDivertedOrOC22")
modelsExist <- exists("ppG1DrugTreatmentModels")
if(!modelsExist) {
   modelFN <- paste0(moj_data_path,"/models/pp_drug_treatment_g1_final_models_ENCRYPTED.rds")
   if(loadFinalModelsFromDisk & fileExistsMultiPlatform(modelFN)) {
      print("Loading ppG1DrugTreatmentModels from disk")
      ppG1DrugTreatmentModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
   } else {
      print("Recreating ppG1DrugTreatmentModels")
      ppG1DrugTreatmentModels <- createPPModelsDrugTreatment(1,"glmmTMB1",ppDrugTreatmentFormula)
   }
}
print("Constructing PP models for group 2 EnteredIntoDrugTreatment ~ WasDivertedOrOC22")
modelsExist <- exists("ppG2DrugTreatmentModels")
if(!modelsExist) {
   modelFN <- paste0(moj_data_path,"/models/pp_drug_treatment_g2_final_models_ENCRYPTED.rds")
   if(loadFinalModelsFromDisk & fileExistsMultiPlatform(modelFN)) {
      print("Loading ppG2DrugTreatmentModels from disk")
      ppG2DrugTreatmentModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
   } else {
      print("Recreating ppG2DrugTreatmentModels")
      ppG2DrugTreatmentModels <- createPPModelsDrugTreatment(2,"glmmTMB1",ppDrugTreatmentFormula)
   }
}

combineResultsDrugTreatment <- function(models,coxme=F) {
   if(coxme==T) {
      betas <- MIextract(models, fun = function(x) coef(x))
      variances <- MIextract(models, fun = function(x) vcov(x))
   } else {
      # 'fixef' is the coef method for glmmTMB
      betas <- MIextract(models, fun = function(x) fixef(x)$cond)
      # 'vcov' is the vcov method for glmmTMB
      variances <- MIextract(models, fun = function(x) vcov(x)$cond)
   } 
   combined_results <- MIcombine(betas, variances)
   combined_results
}

ppG1DrugTreatmentPooled <- combineResultsDrugTreatment(ppG1DrugTreatmentModels)
ppG2DrugTreatmentPooled <- combineResultsDrugTreatment(ppG2DrugTreatmentModels)


# extract robust stats
# pr - pooled results
extractPooledStatsDrugTreatment <- function(pr,rowName="WasDivertedOrOC22TRUE",coxme=F) {
   
   prs <- summary(pr)
   
   # basic stuff from summary
   d <- data.table(
      logOR=prs[rowName,"results"],
      SE=prs[rowName,"se"],
      logLowerCI=prs[rowName,"(lower"],
      logUpperCI=prs[rowName,"upper)"],
      MissingInfo=prs[rowName,"missInfo"]
   )
   # get exponentiated ORs
   d[,OR:=exp(logOR)]
   d[,LowerCI:=exp(logLowerCI)]
   d[,UpperCI:=exp(logUpperCI)]
   # get p vals
   d[,DF:=pr$df[rowName]]
   d[,Z:=logOR/SE]
   d[,p:=2 * (1 - pnorm(abs(Z)))]
   d[,pT:=2 * pt(abs(Z), df = DF, lower.tail = FALSE)]
   
   if(coxme==T) {
      setnames(d,"OR","HR")
      setnames(d,"logOR","logHR")
   }
   
   d
}
ppG1DrugTreatmentStats <- extractPooledStatsDrugTreatment(ppG1DrugTreatmentPooled)
ppG2DrugTreatmentStats <- extractPooledStatsDrugTreatment(ppG2DrugTreatmentPooled)
ppG1DrugTreatmentSummary <- ppG1DrugTreatmentStats
ppG2DrugTreatmentSummary <- ppG2DrugTreatmentStats

print(ppG1DrugTreatmentStats)
print(ppG2DrugTreatmentStats)

# DDSS

ppDrugTreatmentFormulaDDSS <- "EnteredIntoDrugTreatment ~ DisposalCategory + (1|ForceName)
+ offset(LogDrugTreatmentExposureTimeDays) + OffenceSimplified2
+ AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard + Sex + EthnicityCoarse
+ ForceReoffendingRate + ForceOCUPenetration + ForceFunding
+ HistoricalPossessionCount + HistoricalOtherDrugsCount
+ HistoricalTheftCount + HistoricalViolenceCount
+ TreatedForDrugsInLastFiveYears"
ppDrugTreatmentFormulaDDSS <- gsub("\n","",ppDrugTreatmentFormulaDDSS)

# construct DDSS models
print("Constructing DDSS models for group 1 EnteredIntoDrugTreatment ~ WasDivertedOrOC22")
modelsExist <- exists("ddssG1DrugTreatmentModels")
if(!modelsExist) {
   modelFN <- paste0(moj_data_path,"/models/ddss_drug_treatment_g1_final_models_ENCRYPTED.rds")
   if(loadFinalModelsFromDisk & fileExistsMultiPlatform(modelFN)) {
      print("Loading ddssG1DrugTreatmentModels from disk")
      ddssG1DrugTreatmentModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
   } else {
      print("Recreating ddssG1DrugTreatmentModels")
      ddssG1DrugTreatmentModels <- createPPModels(1,"glmmTMB1",ppDrugTreatmentFormulaDDSS,ddss = T)
   }
}
print("Constructing DDSS models for group 2 EnteredIntoDrugTreatment ~ WasDivertedOrOC22")
modelsExist <- exists("ddssG2DrugTreatmentModels")
if(!modelsExist) {
   modelFN <- paste0(moj_data_path,"/models/ddss_drug_treatment_g2_final_models_ENCRYPTED.rds")
   if(loadFinalModelsFromDisk & fileExistsMultiPlatform(modelFN)) {
      print("Loading ddssG2DrugTreatmentModels from disk")
      ddssG2DrugTreatmentModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
   } else {
      print("Recreating ddssG2DrugTreatmentModels")
      ddssG2DrugTreatmentModels <- createPPModels(2,"glmmTMB1",ppDrugTreatmentFormulaDDSS,ddss = T)
   }
}

# pool them
ddssG1DrugTreatmentPooled <- combineResults(ddssG1DrugTreatmentModels)
ddssG2DrugTreatmentPooled <- combineResults(ddssG2DrugTreatmentModels)

# pool pairwise emmeans contrasts across a list of models
poolContrastsDrugTreatment <- function(models, specs, level = 0.95, ...) {
   
   # extract the contrast pairs per model
   pair_list <- lapply(models, function(m) {
      emm <- emmeans(m, specs = specs, ...)  # e.g. specs = ~ DisposalCategory
      data.table(summary(pairs(emm,type="link")))
   })
   
   # 2) Extract contrast betas + vcovs for each imputation
   betas <- MIextract(pair_list, fun = function(x) setNames(x$estimate,x$contrast))
   vars  <- MIextract(pair_list, fun = function(x) setNames(x$SE^2,x$contrast))
   
   # 3) Rubin pooling at contrast level
   pooled <- MIcombine(betas, vars)
   
   # 4) extract what we want from this
   logOR <- pooled$coefficients
   OR    <- exp(logOR)
   ci_log <- confint(pooled) # matrix: lower / upper (log scale)
   ci_OR  <- exp(ci_log)
   SE <- sqrt(diag(pooled$variance))
   z  <- logOR / SE
   df <- pooled$df
   
   out <- data.frame(
      contrast = names(logOR),
      logOR    = logOR,
      SE       = SE,
      OR       = OR,
      CI_low   = ci_OR[, 1],
      CI_high  = ci_OR[, 2],
      df       = pooled$df,
      z        = z,
      p        = 2 * pt(abs(z), df = df, lower.tail = FALSE),
      row.names = NULL
   )
   
   out
}

print("Constructing DDSS contrasts")
ddssG1DrugTreatmentContrasts <- poolContrastsDrugTreatment(ddssG1DrugTreatmentModels,specs = ~ DisposalCategory)
ddssG2DrugTreatmentContrasts <- poolContrastsDrugTreatment(ddssG2DrugTreatmentModels,specs = ~ DisposalCategory)

# save these locally (safe outputs)
g1FN <- "safe_outputs/ddss_g1_drug_treatment_summary.csv"
if(!file.exists(g1FN)) { write.csv(ddssG1DrugTreatmentContrasts,g1FN,row.names=F) }
g2FN <- "safe_outputs/ddss_g2_drug_treatment_summary.csv"
if(!file.exists(g2FN)) { write.csv(ddssG2DrugTreatmentContrasts,g2FN,row.names=F) }


pfda[,DisposalCategory:="Other"]
pfda[HOOutcomeCodeLong %in% c("OC08","OC22"),DisposalCategory:="Diverted"]
pfda[HOOutcomeCodeLong %in% c("OC01","OC1A"),DisposalCategory:="Charged"]
pfda[HOOutcomeCodeLong %in% c("OC02","OC2A"),DisposalCategory:="Cautioned"]
pfda[HOOutcomeCodeLong %in% c("OC03","OC3A"),DisposalCategory:="Cautioned"]
pfda[is.na(HOOutcomeCodeLong),DisposalCategory:=NA]

# save models, deliberately control this as we want to "freeze" our real models
if(F) {
   print("Writing G1 PP models")
   modelFN <- paste0(moj_data_path,"/models/pp_drug_treatment_g1_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ppG1DrugTreatmentModels,modelFN,gENCRYPTION_KEY) 
   print("Writing G2 PP models")
   modelFN <- paste0(moj_data_path,"/models/pp_drug_treatment_g2_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ppG2DrugTreatmentModels,modelFN,gENCRYPTION_KEY) 
   print("Writing G1 DDSS models")
   modelFN <- paste0(moj_data_path,"/models/ddss_drug_treatment_g1_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ddssG1DrugTreatmentModels,modelFN,gENCRYPTION_KEY) 
   print("Writing G1 DDSS models")
   modelFN <- paste0(moj_data_path,"/models/ddss_drug_treatment_g2_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ddssG2DrugTreatmentModels,modelFN,gENCRYPTION_KEY) 
}


# write full imputed models
groups <- 1:2
outcomes <- c("DrugTreatment")
lapply(groups,function(currentGroup){
   lapply(outcomes,function(currentOutcome) {
      # get variable name and output file name
      varName <- paste0("ppG",currentGroup,currentOutcome,"Models")
      print(paste0("Processing G",currentGroup,", ",currentOutcome))
      fn <- paste0("safe_outputs/g",currentGroup,"_pp_",currentOutcome,"_all_imputed_coefs.csv")
      
      # impute the coefficients
      models <- get(varName)
      betas <- MIextract(models,fun = function(x) fixef(x)$cond)
      variances <- MIextract(models,fun = function(x) vcov(x)$cond)
      m <- MIcombine(betas,variances)
      x <- as.data.table(summary(m),keep.rownames = "variable")
      print(paste0("Writing ",fn))
      fwrite(x,fn)
   })
})

# cox models
if(!exists("ppG1DTCoxModels")) {
   ppG1DTCoxModels <- createPPModelsDrugTreatment(1,"cox",ppDrugTreatmentFormulaCox,coxModel=T)
}
ppG1DTCoxModelsPooled <- combineResultsDrugTreatment(ppG1DTCoxModels,coxme = T)
ppG1DTCoxSummary <- extractPooledStatsDrugTreatment(ppG1DTCoxModelsPooled,coxme=T)
if(!exists("ppG2DTCoxModels")) {
   ppG2DTCoxModels <- createPPModelsDrugTreatment(2,"cox",ppDrugTreatmentFormulaCox,coxModel=T)
}
ppG2DTCoxModelsPooled <- combineResultsDrugTreatment(ppG2DTCoxModels,coxme = T)
ppG2DTCoxSummary <- extractPooledStatsDrugTreatment(ppG2DTCoxModelsPooled,coxme=T)

ppDTCoxSummary <- rbind(setcolorder(ppG1DTCoxSummary[,Group:="1"],"Group"),setcolorder(ppG2DTCoxSummary[,Group:="2"],"Group"))
flextable(ppDTCoxSummary[,.(Group,HR,LowerCI,UpperCI,p)]) %>% colformat_double(digits=4)

fwrite(ppDTCoxSummary,"safe_outputs/pp_drug_treatment_cox_summary.csv")

makeCompleteCaseDTCoxModel <- function(mData,ppFormula) {
   modelData <- copy(mData)
   
   print(paste0("Using formula: ",ppFormula))
   
   # merge in the historical crime variables
   #modelData <- merge(modelData,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
   
   # merge in the drug treatment variables
   # remove the old variable first
   #modelData[,EnteredIntoDrugTreatment:=NULL]
   #modelData[,WasInTreatmentWithin28DaysPriorToContact:=NULL]
   #modelData <- merge(modelData,drugTreatment,by="RandomGroupedIncidentID",all.x=T)
   
   # exclude those in treatment within 28 days before
   modelData <- modelData[WasInTreatmentWithin28DaysPriorToContact==F]
   
   # drop unused levels
   modelData <- droplevels(modelData)
   
   # run the model
   currentModel <- runModelDrugTreatment(ppFormula,modelData,"cox")
   
   currentModel
}

# make complete case cox models for the zph plots
ppG1DTCoxModelCC <- makeCompleteCaseDTCoxModel(pfda[CohortGroup==1],ppDrugTreatmentFormulaCox)
ppG1DTCoxModelCCZPH <- cox.zph(ppG1DTCoxModelCC)
library(devEMF)
fn <- paste0("../PublicStats/figures/g1_pp_cc_drug_treatment_proportionality.emf")
emf(fn,width=8,height=6,units="in")
plot(ppG1DTCoxModelCCZPH[1])
dev.off()

ppG2DTCoxModelCC <- makeCompleteCaseDTCoxModel(pfda[CohortGroup==2],ppDrugTreatmentFormulaCox)

#ppG1DTCoxModelZPH <- 
#zph <- cox.zph(model)