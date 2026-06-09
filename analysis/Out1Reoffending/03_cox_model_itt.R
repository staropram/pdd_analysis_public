source("01_import_data.R")
# script to help with being either on MOJ or DHSC system
source("../../import_scripts/01c_multi_platform_helper.R")
source("../../export_scripts/05a_crypto_functions.R")

library(survival)
library(mitools)
library(metafor)

loadFinalModelsFromDisk <- F
overwriteOutputs <- F

ittCoxFormulaReoffending <- "
   Surv(ExposureTimeDays, DidReoffend) ~ DiversionForce +
   OffenceSimplified2 +
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard +
   Sex + EthnicityCoarse +
   ForceReoffendingRate + ForceOCUPenetration + ForceFunding +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount
"


numImputations <- 20

# analysis wrapper
createModelITTReoffending <- function(d,f) { 
   fm <- as.formula(gsub("\n"," ",f))
   model <- coxph(fm,data=d) 
   model
}

# function that creates models for each force and its synthetic control
# for each imputed copy of the data
# d - row IDs of force data and their matched counterparts
# ittFormula - the formula to use in the GLM model
createITTReoffendingModels <- function(d,ittFormula) {
   # iterate over forces and create models for each imputation
   forceNames <- unique(d$MatchedForce)
   models <- lapply(forceNames,function(forceName){
      # iterate over imputations
      forceResults <- lapply(1:numImputations,function(imputationIndex) {
         # get the current match pairs,we can either use one match and keep it the 
         # same for all imputations or use 1 match per imputation 
         # start with using one match kept the same per imputation
         modelData <- copy(d[MatchedForce==forceName])
         # join the current imputation to it
         currentImputed <- pfda_imputed[[imputationIndex]]
         modelData <- merge(modelData,currentImputed,by="RandomGroupedIncidentID",all.x=T)
         
         # merge in the historical offence data and reoffending data from the PNC
         modelData <- merge(modelData,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
         modelData <- merge(modelData,reoffending,by="RandomGroupedIncidentID",all.x=T)
         modelData <- merge(modelData,drugReoffending,by="RandomGroupedIncidentID",all.x=T)
         
         modelData <- merge(modelData,unique(ndtms_ambig),by="RandomParticipantID",all.x=T)
         modelData[is.na(AmbiguousNDTMS),AmbiguousNDTMS:=F]
         #modelData[AmbiguousNDTMS==T,TreatedForDrugsInLast5Years:=T]
         #modelData[AmbiguousNDTMS==T&DiversionForce==F,TreatedForDrugsInLast5Years:=F]
         
         # drop unused levels of the offence
         modelData <- droplevels(modelData)
         
         # create the model
         print(paste0("Creating ITT model for ",forceName," imputation ",imputationIndex))
         m <- createModelITTReoffending(modelData,ittFormula)
         m
      })
      
      forceResults
   })
   names(models) <- forceNames
   models
}

combineCoxModels <- function(models) {
   betas <- MIextract(models, fun = function(x) coef(x))
   variances <- MIextract(models, fun = function(x) vcov(x))
   MIcombine(betas, variances)
}

# G1 analysis
ittG1BaseDataReoffending <- baseMatchPairs[MatchedCohort==1]

modelsExist <- exists("ittG1ReoffendingModels")
if(!modelsExist) {
   modelFN <- paste0(moj_data_path,"/models/itt_g1_reoffending_final_models_ENCRYPTED.rds")
   if(loadFinalModelsFromDisk & fileExistsMultiPlatform(modelFN)) {
      print("Loading ittG1ReoffendingModels from disk")
      ittG1ReoffendingModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
   } else {
      print("Recreating ittG1ReoffendingModels")
      ittG1ReoffendingModels <- createITTReoffendingModels(ittG1BaseDataReoffending,ittCoxFormulaReoffending)
   }
}
      ittG1ReoffendingModels <- createITTReoffendingModels(ittG1BaseDataReoffending,ittCoxFormulaReoffending)

poolITTReoffendingModels <- function(modelList) {
   rbindlist(lapply(names(modelList),function(forceName){
      # combine all the imputations for current force
      models <- modelList[[forceName]]
      betas <- MIextract(models, fun = function(x) coef(x))
      variances <- MIextract(models, fun = function(x) vcov(x))
      m <- MIcombine(betas, variances)
      
      # return the combined coefficients
      data.table(
         ForceName=forceName,
         yi=m$coefficients[["DiversionForceTRUE"]],
         sei=sqrt(m$variance["DiversionForceTRUE","DiversionForceTRUE"])
      )
   }))
}

# extract the estimates
ittG1ReoffendingPooledResults <- poolITTReoffendingModels(ittG1ReoffendingModels)
if(overwriteOutputs==T) {
   fwrite(ittG1ReoffendingPooledResults,"safe_outputs/g1_itt_reoffending_pooled_results.csv")
}

# Random-effects meta-analysis across forces
ittG1ReoffendingMetaModel <- rma(yi = yi, sei = sei, data = ittG1ReoffendingPooledResults, method = "REML")

makeRMASummaryTable <- function(mm) {
   forceData <- rbindlist(lapply(mm$data$ForceName,function(forceName){
      
      logEstimate <- mm$data[ForceName==forceName,yi]
      se <- mm$data[ForceName==forceName,sei]
      z <- logEstimate/se
      p <- 2 * pnorm(abs(z),lower.tail = F)
      lowerCI <- exp(logEstimate - se*qnorm(0.975))
      upperCI <- exp(logEstimate + se*qnorm(0.975))
      
      d <- data.table(
         Force=forceName,
         Estimate=exp(logEstimate),
         LowerCI=lowerCI,
         UpperCI=upperCI,
         p=p
      )
      
      d
   }))
   
   pooledData <- data.table(
      Force    = "Pooled effect",
      Estimate = exp(as.numeric(mm$beta)),
      LowerCI  = exp(mm$ci.lb),
      UpperCI  = exp(mm$ci.ub),
      p        = mm$pval
   )
   
   rbind(forceData,pooledData)
}
ittG1ReoffendingSummaryTable <- makeRMASummaryTable(ittG1ReoffendingMetaModel)
browser()

# forest plot on Cox HR scale
png(paste0("figures/g1_itt_reoffending.png"),width=800,height=600)
forest(ittG1ReoffendingMetaModel,
       slab   =ittG1ReoffendingPooledResults$ForceName,
       transf = exp, # show hazard ratios (exp(log-HR)) 
       refline = 1,  # HR = 1 (no effect)
       digits = 4,
       xlab   = paste0("Hazard Ratio (Group 1)")
)
dev.off()

# G2 analysis
ittG2BaseDataReoffending <- baseMatchPairs[MatchedCohort==2]
modelsExist <- exists("ittG2ReoffendingModels")
if(!modelsExist) {
   modelFN <- paste0(moj_data_path,"/models/itt_g2_reoffending_final_models_ENCRYPTED.rds")
   if(loadFinalModelsFromDisk & fileExistsMultiPlatform(modelFN)) {
      print("Loading ittG2ReoffendingModels from disk")
      ittG2ReoffendingModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
   } else {
      print("Recreating ittG2ReoffendingModels")
      ittG2ReoffendingModels <- createITTReoffendingModels(ittG2BaseDataReoffending,ittCoxFormulaReoffending)
   }
}

# extract the estimates
ittG2ReoffendingPooledResults <- poolITTReoffendingModels(ittG2ReoffendingModels)
fwrite(ittG2ReoffendingPooledResults,"safe_outputs/g2_itt_reoffending_pooled_results.csv")
# Random-effects meta-analysis across forces
ittG2ReoffendingMetaModel <- rma(yi = yi, sei = sei, data = ittG2ReoffendingPooledResults, method = "REML")

ittG2ReoffendingSummaryTable <- makeRMASummaryTable(ittG2ReoffendingMetaModel)

# Overall pooled effect (log-HR and HR)
#g2OverallEffects <- getOverallEffects(g2MetaModel)

# forest plot on Cox HR scale
png(paste0("figures/g2_itt_reoffending.png"),width=800,height=600)
forest(ittG2ReoffendingMetaModel,
       slab   =ittG2ReoffendingPooledResults$ForceName,
       transf = exp, # show hazard ratios (exp(log-HR)) 
       refline = 1,  # HR = 1 (no effect)
       digits = 4,
       xlab   = paste0("Hazard Ratio (Group 2)")
)
dev.off()

# save models, deliberately control this as we want to "freeze" our real models
if(F) {
   print("Writing G1 models")
   modelFN <- paste0(moj_data_path,"/models/itt_g1_reoffending_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ittG1ReoffendingModels,modelFN,gENCRYPTION_KEY) 
   modelFN <- paste0(moj_data_path,"/models/itt_g1_reoffending_metamodel_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ittG1ReoffendingMetaModel,modelFN,gENCRYPTION_KEY) 
   print("Writing G2 models")
   modelFN <- paste0(moj_data_path,"/models/itt_g2_reoffending_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ittG2ReoffendingModels,modelFN,gENCRYPTION_KEY) 
   modelFN <- paste0(moj_data_path,"/models/itt_g2_reoffending_metamodel_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ittG2ReoffendingMetaModel,modelFN,gENCRYPTION_KEY) 
}

# make some outputs for drug reoffending specifically
ittCoxFormulaDrugReoffending <- "
   Surv(ExposureTimeDaysDrugs, DidReoffendDrugs) ~ DiversionForce +
   OffenceSimplified2 +
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard +
   Sex + EthnicityCoarse +
   ForceReoffendingRate + ForceOCUPenetration + ForceFunding +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount
"

# group 1
ittG1DrugReoffendingModels <- createITTReoffendingModels(
   ittG1BaseDataReoffending,
   ittCoxFormulaDrugReoffending)
# pool the results
ittG1DrugReoffendingPooledResults <- poolITTReoffendingModels(ittG1DrugReoffendingModels)
fwrite(ittG1DrugReoffendingPooledResults,"safe_outputs/g1_itt_drug_reoffending_pooled_results.csv")
# make meta model
ittG1DrugReoffendingMetaModel <- rma(yi = yi, sei = sei, data = ittG1DrugReoffendingPooledResults, method = "REML")
# produce summary table
ittG1DrugReoffendingSummaryTable <- makeRMASummaryTable(ittG1DrugReoffendingMetaModel)
png(paste0("figures/g1_itt_drug_reoffending.png"),width=800,height=600)
forest(ittG1DrugReoffendingMetaModel,
       slab   =ittG1DrugReoffendingPooledResults$ForceName,
       transf = exp, # show hazard ratios (exp(log-HR)) 
       refline = 1,  # HR = 1 (no effect)
       digits = 4,
       xlab   = paste0("Hazard Ratio (Group 1)")
)
dev.off()

# group 2
ittG2DrugReoffendingModels <- createITTReoffendingModels(
   ittG2BaseDataReoffending,
   ittCoxFormulaDrugReoffending)
# pool the results
ittG2DrugReoffendingPooledResults <- poolITTReoffendingModels(ittG2DrugReoffendingModels)
fwrite(ittG2DrugReoffendingPooledResults,"safe_outputs/g2_itt_drug_reoffending_pooled_results.csv")
# make meta model
ittG2DrugReoffendingMetaModel <- rma(yi = yi, sei = sei, data = ittG2DrugReoffendingPooledResults, method = "REML")
# produce summary table
ittG2DrugReoffendingSummaryTable <- makeRMASummaryTable(ittG2DrugReoffendingMetaModel)
png(paste0("figures/g2_itt_drug_reoffending.png"),width=800,height=600)
forest(ittG2DrugReoffendingMetaModel,
       slab   =ittG2DrugReoffendingPooledResults$ForceName,
       transf = exp, # show hazard ratios (exp(log-HR)) 
       refline = 1,  # HR = 1 (no effect)
       digits = 4,
       xlab   = paste0("Hazard Ratio (Group 1)")
)
dev.off()


# save weights
## Reoffending
ittG1ReoffendingMetaWeights <- data.table(
   Force=ittG1ReoffendingPooledResults$ForceName,
   Weight=weights(ittG1ReoffendingMetaModel)
)
fwrite(ittG1ReoffendingMetaWeights,"safe_outputs/g1_itt_reoffending_weights.csv")
ittG2ReoffendingMetaWeights <- data.table(
   Force=ittG2ReoffendingPooledResults$ForceName,
   Weight=weights(ittG2ReoffendingMetaModel)
)
fwrite(ittG2ReoffendingMetaWeights,"safe_outputs/g2_itt_reoffending_weights.csv")
## Drug reoffending
ittG1DrugReoffendingMetaWeights <- data.table(
   Force=ittG1DrugReoffendingPooledResults$ForceName,
   Weight=weights(ittG1DrugReoffendingMetaModel)
)
fwrite(ittG1DrugReoffendingMetaWeights,"safe_outputs/g1_itt_drug_reoffending_weights.csv")
ittG2DrugReoffendingMetaWeights <- data.table(
   Force=ittG2DrugReoffendingPooledResults$ForceName,
   Weight=weights(ittG2DrugReoffendingMetaModel)
)
fwrite(ittG2DrugReoffendingMetaWeights,"safe_outputs/g2_itt_drug_reoffending_weights.csv")

# complete case models

ittCoxFormulaReoffendingCC <- "
   Surv(ExposureTimeDays, DidReoffend) ~ DiversionForce +
   OffenceSimplified2 +
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard +
   Sex + EthnicityCoarse +
   ForceReoffendingRate + ForceOCUPenetration + ForceFunding +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   (1|ForceName)
"

ittCoxFormulaDrugReoffendingCC <- "
   Surv(ExposureTimeDaysDrugs, DidReoffendDrugs) ~ DiversionForce +
   OffenceSimplified2 +
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard +
   Sex + EthnicityCoarse +
   ForceReoffendingRate + ForceOCUPenetration + ForceFunding +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   (1|ForceName)
"

makeCoxZPHModel <- function(baseData,f) {
   modelData <- merge(baseData,pfda_imputed[[1]],by="RandomGroupedIncidentID",all.x=T)
   modelData <- merge(modelData,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
   modelData <- merge(modelData,reoffending,by="RandomGroupedIncidentID",all.x=T)
   modelData <- merge(modelData,drugReoffending,by="RandomGroupedIncidentID",all.x=T)
   fm <- as.formula(gsub("\n"," ",f))
   model <- coxme(fm,droplevels(modelData))
}
ittG1ReoffendingZPHModel <- makeCoxZPHModel(ittG1BaseDataReoffending,ittCoxFormulaReoffendingCC)
ittG2ReoffendingZPHModel <- makeCoxZPHModel(ittG2BaseDataReoffending,ittCoxFormulaReoffendingCC)
ittG1DrugReoffendingZPHModel <- makeCoxZPHModel(ittG1BaseDataReoffending,ittCoxFormulaDrugReoffendingCC)
ittG2DrugReoffendingZPHModel <- makeCoxZPHModel(ittG2BaseDataReoffending,ittCoxFormulaDrugReoffendingCC)

ittZPHModels <- list(
   G1=list(
      Reoffending=ittG1ReoffendingZPHModel,
      DrugReoffending=ittG1DrugReoffendingZPHModel 
   ),
   G2=list(
      Reoffending=ittG2ReoffendingZPHModel,
      DrugReoffending=ittG2DrugReoffendingZPHModel 
   )
)

# make a table of the coeffs
library(devEMF)
lapply(names(ittZPHModels),function(group){
   models <- ittZPHModels[[group]]
   lapply(names(models),function(modelName){
      model <- get(paste0("itt",group,modelName,"ZPHModel"))
      zph <- cox.zph(model)
      fn <- paste0("figures/",tolower(group),"_itt_",modelName,"_proportionality.emf")
      emf(fn,width=8,height=6,units="in")
      plot(zph[1])
      dev.off()
   })
})

# make a table
lapply(names(ittZPHModels),function(group){
   models <- ittZPHModels[[group]]
   lapply(names(models),function(modelName){
      model <- get(paste0("itt",group,modelName,"ZPHModel"))
      zph <- cox.zph(model)
      fn <- paste0("safe_outputs/",tolower(group),"_itt_",modelName,"_zph.csv")
      zphDT <- as.data.table(zph$table, keep.rownames = "variable")
      fwrite(zphDT,fn)
   })
})
   
   

