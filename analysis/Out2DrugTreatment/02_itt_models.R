source("01_import_data.R")
#source("rubins_rules.R")
library(metafor)
library(stringr)
library(mitools)

# This is the new analysis to be ran on the MOJ system
# There are restrictions here, NDTMS data must only be
# kept in memory, never on disk

# this flag causes the final models to be loaded from disk rather than recompute
loadFinalModelsFromDisk <- F
overwriteOutputs <- F

# formulae
ittDrugTreatmentFormula <- "EnteredIntoDrugTreatment  ~ DiversionForce 
+ ForceReoffendingRate + ForceOCUPenetration + ForceFunding
+ offset(LogDrugTreatmentExposureTimeDays) + OffenceSimplified2
+ AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard + Sex + EthnicityCoarse
+ HistoricalPossessionCount + HistoricalOtherDrugsCount
+ HistoricalTheftCount + HistoricalViolenceCount
+ TreatedForDrugsInLastFiveYears"

ittDrugTreatmentFormulaCox <- "Surv(DrugTreatmentExposureTimeDays,EnteredIntoDrugTreatment) ~ DiversionForce 
+ ForceReoffendingRate + ForceOCUPenetration + ForceFunding
+ OffenceSimplified2
+ AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard + Sex + EthnicityCoarse
+ HistoricalPossessionCount + HistoricalOtherDrugsCount
+ HistoricalTheftCount + HistoricalViolenceCount
+ TreatedForDrugsInLastFiveYears"

# analysis wrapper
createModelITTDrugTreatment <- function(d,f) { 
   model <- glm(gsub("\n","",f),family=binomial,data=d) }
createModelITTDrugTreatmentCox <- function(d,f) { 
   fm <- as.formula(gsub("\n"," ",f))
   model <- coxph(fm,data=d) 
   model
}

set.seed(134)

# function that creates models for each force and its synthetic control
# for each imputed copy of the data
# d - row IDs of force data and their matched counterparts
# ittFormula - the formula to use in the GLM model
# outcomeVariable - string indicating the name of the exposure variable
createITTModelsDrugTreatment <- function(d,ittFormula,coxModel=F) {
   # iterate over forces and create models for each imputation
   forceNames <- unique(d$MatchedForce)
   models <- lapply(forceNames,function(forceName){
      # iterate over imputations
      forceResults <- lapply(1:20,function(imputationIndex) {
         # get the current match pairs,we can either use one match and keep it the 
         # same for all imputations or use 1 match per imputation 
         # start with using one match kept the same per imputation
         modelData <- copy(d[MatchedForce==forceName])
         # join the current imputation to it
         currentImputed <- pfda_imputed[[imputationIndex]]
         modelData <- merge(modelData,currentImputed,by="RandomGroupedIncidentID",all.x=T)
         
         # merge in the historical variables
         modelData <- merge(modelData,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
         if(F) {
            modelData <- merge(modelData,unique(ndtms_ambig),by="RandomParticipantID",all.x=T)
            modelData[is.na(AmbiguousNDTMS),AmbiguousNDTMS:=F]
         }
         
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
         
         # exclude those in treatment
         modelData <- modelData[WasInTreatmentWithin28DaysPriorToContact==F]
         
         # drop unused levels
         modelData <- droplevels(modelData)
         
         # create the model
         print(paste0("Creating ITT model for ",forceName," imputation ",imputationIndex))
         if(coxModel==T) {
            m <- createModelITTDrugTreatmentCox(modelData,ittFormula)
         } else {
            m <- createModelITTDrugTreatment(modelData,ittFormula)
         }
         m
      })
      
      forceResults
   })
   names(models) <- forceNames
   models
}

# G1 analysis
ittG1DrugTreatmentBaseData <- baseMatchPairs3[MatchedCohort==1]

modelsExist <- exists("ittG1DrugTreatmentModels")
if(!modelsExist) {
   modelFN <- paste0(moj_data_path,"/models/itt_g1_drug_treatment_final_models_ENCRYPTED.rds")
   if(loadFinalModelsFromDisk & fileExistsMultiPlatform(modelFN)) {
      print("Loading ittG1DrugTreatmentModels from disk")
      ittG1DrugTreatmentModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
   } else {
      print("Recreating ittG1DrugTreatmentModels")
      ittG1DrugTreatmentModels <- createITTModelsDrugTreatment(ittG1DrugTreatmentBaseData,ittDrugTreatmentFormula)
   }
}

ittPoolResultsDrugTreatment <- function(allModels) {
   forceNames <- names(allModels)
   rbindlist(lapply(forceNames,function(forceName){
      # combine all the imputations for current force
      models <- allModels[[forceName]]
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

# pool results across imputations within forces
ittG1DrugTreatmentPooledResults <- ittPoolResultsDrugTreatment(ittG1DrugTreatmentModels)
if(overwriteOutputs==T) {
   fwrite(ittG1DrugTreatmentPooledResults,"safe_outputs/g1_itt_drug_treatment_pooled_results.csv")
}

# Random-effects meta-analysis across forces on the log-odds scale
ittG1DrugTreatmentMetaModel <- rma(yi = yi, sei = sei, data = ittG1DrugTreatmentPooledResults, method = "REML")

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
ittG1DrugTreatmentSummaryTable <- makeRMASummaryTable(ittG1DrugTreatmentMetaModel)

if(overwriteOutputs==T) {
# forest plot on OR scale
png(paste0("figures/drug_treatment_itt_g1.png"),width=800,height=600)
forest(ittG1DrugTreatmentMetaModel,
       slab   =ittG1DrugTreatmentPooledResults$ForceName,
       transf = exp,             # show odds ratios
       refline = 1,              # OR=1 line
       digits = 4,
       xlab   = paste0("Odds Ratio (Group 1): Effect of diversion force on treatment entry")
)
dev.off()
}

# G2 analysis
ittG2DrugTreatmentBaseData <- baseMatchPairs3[MatchedCohort==2]

# create models
modelsExist <- exists("ittG2DrugTreatmentModels")
if(!modelsExist) {
   modelFN <- paste0(moj_data_path,"/models/itt_g2_drug_treatment_final_models_ENCRYPTED.rds")
   if(loadFinalModelsFromDisk & fileExistsMultiPlatform(modelFN)) {
      print("Loading ittG2DrugTreatmentModels from disk")
      ittG2DrugTreatmentModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
   } else {
      print("Recreating ittG2DrugTreatmentModels")
      ittG2DrugTreatmentModels <- createITTModelsDrugTreatment(ittG2DrugTreatmentBaseData,ittDrugTreatmentFormula)
   }
}
ittG2DrugTreatmentModels <- createITTModelsDrugTreatment(ittG2DrugTreatmentBaseData,ittDrugTreatmentFormula)

# pool results
ittG2DrugTreatmentPooledResults <- ittPoolResultsDrugTreatment(ittG2DrugTreatmentModels)
if(overwriteOutputs==T) {
   fwrite(ittG2DrugTreatmentPooledResults,"safe_outputs/g2_itt_drug_treatment_pooled_results.csv")
}

# Random-effects meta-analysis across forces on the log-odds scale
ittG2DrugTreatmentMetaModel <- rma(yi = yi, sei = sei, data = ittG2DrugTreatmentPooledResults, method = "REML")

ittG2DrugTreatmentSummaryTable <- makeRMASummaryTable(ittG2DrugTreatmentMetaModel)

# forest plot on OR scale
if(overwriteOutputs==T) {
   png(paste0("figures/drug_treatment_itt_g2.png"),width=800,height=600)
   forest(ittG2DrugTreatmentMetaModel,
          slab   =ittG2DrugTreatmentPooledResults$ForceName,
          transf = exp,             # show odds ratios
          refline = 1,              # OR=1 line
          digits = 4,
          xlab   = paste0("Odds Ratio (Group 2): Effect of diversion force on treatment entry")
   )
   dev.off()
}
browser()


# save models, deliberately control this as we want to "freeze" our real models
if(F) {
   print("Writing G1 models")
   modelFN <- paste0(moj_data_path,"/models/itt_g1_drug_treatment_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ittG1DrugTreatmentModels,modelFN,gENCRYPTION_KEY) 
   modelFN <- paste0(moj_data_path,"/models/itt_g1_drug_treatment_metamodel_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ittG1DrugTreatmentMetaModel,modelFN,gENCRYPTION_KEY) 
   print("Writing G2 models")
   modelFN <- paste0(moj_data_path,"/models/itt_g2_drug_treatment_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ittG2DrugTreatmentModels,modelFN,gENCRYPTION_KEY) 
   modelFN <- paste0(moj_data_path,"/models/itt_g2_drug_treatment_metamodel_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ittG2DrugTreatmentMetaModel,modelFN,gENCRYPTION_KEY) 
}


# save weights
## Drug reoffending
ittG1DrugTreatmentMetaWeights <- data.table(
   Force=ittG1DrugTreatmentPooledResults$ForceName,
   Weight=weights(ittG1DrugTreatmentMetaModel)
)
fwrite(ittG1DrugTreatmentMetaWeights,"safe_outputs/g1_itt_drug_treatment_weights.csv")
ittG2DrugTreatmentMetaWeights <- data.table(
   Force=ittG2DrugTreatmentPooledResults$ForceName,
   Weight=weights(ittG2DrugTreatmentMetaModel)
)
fwrite(ittG2DrugTreatmentMetaWeights,"safe_outputs/g2_itt_drug_treatment_weights.csv")



# cox models

ittG1DrugTreatmentCoxModels <- createITTModelsDrugTreatment(
   ittG1DrugTreatmentBaseData,
   ittDrugTreatmentFormulaCox,
   coxModel = T
)
ittG2DrugTreatmentCoxModels <- createITTModelsDrugTreatment(
   ittG2DrugTreatmentBaseData,
   ittDrugTreatmentFormulaCox,
   coxModel = T
)

# pool results
ittG1DrugTreatmentCoxPooled <- ittPoolResultsDrugTreatment(ittG1DrugTreatmentCoxModels)
ittG2DrugTreatmentCoxPooled <- ittPoolResultsDrugTreatment(ittG2DrugTreatmentCoxModels)
ittG1DrugTreatmentCoxPooled[,Force:=ForceName]
ittG2DrugTreatmentCoxPooled[,Force:=ForceName]

# make anonymous
set.seed(343)
g1PseudoMapping <- ittG1DrugTreatmentCoxSummary[,list(Force=unique(Force))]
g1PseudoMapping[Force!="Pooled effect",PseudoMap:=sample(LETTERS[1:.N],replace=F)]
g2PseudoMapping <- ittG2DrugTreatmentCoxSummary[,list(Force=unique(Force))]
g2PseudoMapping[Force!="Pooled effect",PseudoMap:=sample(LETTERS[1:.N],replace=F)]
pseudoMaps <- list(G1=g1PseudoMapping,G2=g2PseudoMapping)

# pool the results
ittG1DrugTreatmentCoxPooled <- merge(
   ittG1DrugTreatmentCoxPooled,
   g1PseudoMapping,
   by="Force",
   all.x=T
)
ittG1DrugTreatmentCoxPooled[,Force:=NULL]
ittG1DrugTreatmentCoxPooled[,ForceName:=NULL]
setnames(ittG1DrugTreatmentCoxPooled,"PseudoMap","ForceName")
setorder(ittG1DrugTreatmentCoxPooled,ForceName)
setcolorder(ittG1DrugTreatmentCoxPooled,c("ForceName"))

# pool the results
ittG2DrugTreatmentCoxPooled <- merge(
   ittG2DrugTreatmentCoxPooled,
   g2PseudoMapping,
   by="Force",
   all.x=T
)
ittG2DrugTreatmentCoxPooled[,Force:=NULL]
ittG2DrugTreatmentCoxPooled[,ForceName:=NULL]
setnames(ittG2DrugTreatmentCoxPooled,"PseudoMap","ForceName")
setorder(ittG2DrugTreatmentCoxPooled,ForceName)
setcolorder(ittG2DrugTreatmentCoxPooled,c("ForceName"))

ittG1DrugTreatmentCoxMetaModel <- rma(
   yi = yi,
   sei = sei,
   data = ittG1DrugTreatmentCoxPooled, method = "REML")

ittG1DrugTreatmentCoxSummaryTable <- makeRMASummaryTable(ittG1DrugTreatmentCoxMetaModel)

ittG2DrugTreatmentCoxMetaModel <- rma(
   yi = yi,
   sei = sei,
   data = ittG2DrugTreatmentCoxPooled, method = "REML")

ittG2DrugTreatmentCoxSummaryTable <- makeRMASummaryTable(ittG2DrugTreatmentCoxMetaModel)

      
fwrite(ittG1DrugTreatmentCoxPooled,"../PublicStats/tables/g1_itt_drug_treatment_cox_pooled.csv")
fwrite(ittG2DrugTreatmentCoxPooled,"../PublicStats/tables/g2_itt_drug_treatment_cox_pooled.csv")
fwrite(ittG1DrugTreatmentCoxSummaryTable,"../PublicStats/tables/g1_itt_drug_treatment_cox_summary.csv")
fwrite(ittG2DrugTreatmentCoxSummaryTable,"../PublicStats/tables/g2_itt_drug_treatment_cox_summary.csv")
      
      