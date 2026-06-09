source("01_import_data.R")

library(survival)
library(mitools)
library(coxme)
library(emmeans)

# set this flag to just load in the final models again
loadFinalModelsFromDisk <- T

ppCoxFormulaReoffending <- "
   Surv(ExposureTimeDays, DidReoffend) ~ WasDivertedOrOC22 +
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
   #frailty(ForceName,distribution='gaussian')
   #strata(ForceName)
   #cluster(ForceName)
   #ForceName

numImputations <- 20

# analysis wrapper
createReoffendingModel <- function(d,f) { 
   fm <- as.formula(gsub("\n"," ",f))
   #model <- coxph(fm,data=d) 
   model <- coxme(fm,data=d) 
   model
}

# function that creates models for each force and its synthetic control
# for each imputed copy of the data
# d - row IDs of force data and their matched counterparts
# ittFormula - the formula to use in the GLM model
createPPModelsReoffending <- function(cohortGroup,ppFormula,ddss=F) {
      # iterate over imputations
      results <- lapply(1:numImputations,function(imputationIndex) {
         # join the current imputation to it
         modelData <- pfda_imputed[[imputationIndex]]
         modelData <- modelData[CohortGroup==cohortGroup]
         
         # merge in the historical offence data and reoffending data from the PNC
         modelData <- merge(modelData,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
         modelData <- merge(modelData,reoffending,by="RandomGroupedIncidentID",all.x=T)
         modelData <- merge(modelData,drugReoffending,by="RandomGroupedIncidentID",all.x=T) 
         
         # if we are doing the DDSS analysis we need to create the exposure
         if(ddss) {
            modelData[,DisposalCategory:="Other"]
            # fill in the imputed outcome (we ignore other as its already the default)
            modelData[!is.na(ImputedHOOutcome)&!(ImputedHOOutcome=="Other"),HOOutcomeCodeLong:=ImputedHOOutcome]
            modelData[HOOutcomeCodeLong %in% c("OC08","OC22"),DisposalCategory:="Diverted"]
            modelData[HOOutcomeCodeLong %in% c("OC01","OC1A"),DisposalCategory:="Charged"]
            # 2 and 2A shouldn't strictly be in here but they are
            modelData[HOOutcomeCodeLong %in% c("OC02","OC2A"),DisposalCategory:="Cautioned"]
            modelData[HOOutcomeCodeLong %in% c("OC03","OC3A"),DisposalCategory:="Cautioned"]
            modelData[,DisposalCategory:=factor(DisposalCategory,
               levels = c("Diverted","Charged", "Cautioned",  "Other"))]
         }
         
         # drop unused levels of the offence
         modelData <- droplevels(modelData)
         
         # create the model
         mode
         print(paste0("Creating PP model for imputation ",imputationIndex))
         m <- createReoffendingModel(modelData,ppFormula)
         m
      })
      
      results
}

# function to extract the effect coefficient and variances 
# from a set of models
combineCoxModels <- function(models) {
   betas <- MIextract(models, fun = function(x) coef(x))
   variances <- MIextract(models, fun = function(x) vcov(x))
   MIcombine(betas, variances)
}

# extract robust stats
# pr - pooled results
extractPooledStatsReoffending <- function(pr,rowName="WasDivertedOrOC22TRUE") {
   
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
   
   d
}

# G1 analysis
modelsExist <- exists("ppG1ReoffendingModels")
if(!modelsExist & loadFinalModelsFromDisk) {
   print("Loading ppG1ReffoendingModels from disk")
   modelFN <- paste0(moj_data_path,"/models/pp_reoffending_g1_final_models_ENCRYPTED.rds")
   ppG1ReoffendingModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
} else if (!modelsExist) {
   print("Recreating ppG1ReoffendingModels")
   ppG1ReoffendingModels <- createPPModelsReoffending(1,ppCoxFormulaReoffending)
}

makePPReoffendingSummary <- function(models,groupName,rowName="WasDivertedOrOC22TRUE") {
   betas <- MIextract(models, fun = function(x) coef(x))
   variances <- MIextract(models, fun = function(x) vcov(x))
   m <- MIcombine(betas, variances)
   ms <- summary(m)
   logHR <- ms[rowName,"results"]
   hr <- exp(logHR)
   se <- ms[rowName,"se"]
   lowerCI=exp(logHR - qnorm(0.975)*se)
   upperCI=exp(logHR + qnorm(0.975)*se)
   z <- logHR / se
   p <- 2 * pnorm(abs(z),lower.tail = F)
   
   data.table(
      Group=groupName,
      HR=hr,
      LowerCI=lowerCI,
      UpperCI=upperCI,
      p=p
   )
}

ppG1ReoffendingSummary <- makePPReoffendingSummary(ppG1ReoffendingModels,"G1 : Simple possession")

# G2 analysis
modelsExist <- exists("ppG2ReoffendingModels")
if(!modelsExist & loadFinalModelsFromDisk) {
   print("Loading ppG2ReffoendingModels from disk")
   modelFN <- paste0(moj_data_path,"/models/pp_reoffending_g2_final_models_ENCRYPTED.rds")
   ppG2ReoffendingModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
} else if (!modelsExist) {
   print("Recreating ppG2ReoffendingModels")
   ppG2ReoffendingModels <- createPPModelsReoffending(2,ppCoxFormulaReoffending)
}

ppG2ReoffendingSummary <- makePPReoffendingSummary(ppG2ReoffendingModels,"G2 : (Theft | Assault | Damage) + Drugs flag")


ppReoffendingSummary <- rbind(ppG1ReoffendingSummary,ppG2ReoffendingSummary)

## do the DDSS models

ddssCoxFormulaReoffending <- "
   Surv(ExposureTimeDays, DidReoffend) ~ DisposalCategory +
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


# G1 and G2 analyses for disposal cat
modelsExist <- exists("ddssG1ReoffendingModels")
if(!modelsExist & loadFinalModelsFromDisk) {
   print("Loading ddssG1ReffoendingModels from disk")
   modelFN <- paste0(moj_data_path,"/models/ddss_reoffending_g1_final_models_ENCRYPTED.rds")
   ddssG1ReoffendingModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
} else if (!modelsExist) {
   print("Recreating ddssG1ReoffendingModels")
   ddssG1ReoffendingModels <- createPPModelsReoffending(1,ddssCoxFormulaReoffending,ddss = T)
}
if(!modelsExist & loadFinalModelsFromDisk) {
   print("Loading ddssG2ReffoendingModels from disk")
   modelFN <- paste0(moj_data_path,"/models/ddss_reoffending_g2_final_models_ENCRYPTED.rds")
   ddssG2ReoffendingModels <- readEncryptedMultiPlatform(modelFN,gENCRYPTION_KEY)
} else if (!modelsExist) {
   print("Recreating ddssG2ReoffendingModels")
   ddssG2ReoffendingModels <- createPPModelsReoffending(2,ddssCoxFormulaReoffending,ddss=T)
}

combineResults <- function(models) {
   # 'fixef' is the coef method for glmmTMB
   betas <- MIextract(models, fun = function(x) fixef(x))
   # 'vcov' is the vcov method for glmmTMB
   variances <- MIextract(models, fun = function(x) vcov(x))
   combined_results <- MIcombine(betas, variances)
   combined_results
}

ddssG1R <- combineResults(ddssG1ReoffendingModels)
#g2R <- combineResults(g2Models)
#exp(g1R["WasDivertedOrOC22TRUE",c("results","(lower","upper)")])
#exp(g2R["WasDivertedOrOC22TRUE",c("results","(lower","upper)")])


ddssG1Summary <- summary(ddssG1R)
ddssG1ResultsTable <- ddssG1Summary[grep("DisposalCategory", rownames(ddssG1Summary)), ]
setDT(ddssG1ResultsTable,keep.rownames=T)
ddssG1ResultsTable[rn=="DisposalCategoryCharged","DisposalCategory":="Charged"]
ddssG1ResultsTable[rn=="DisposalCategoryCautioned","DisposalCategory":="Cautioned"]
ddssG1ResultsTable[rn=="DisposalCategoryOther","DisposalCategory":="Other"]
ddssG1ResultsTable <- rbind(
   ddssG1ResultsTable,
   data.table(
      rn="DisposalCategoryDiverted",
      results=NA,
      se=NA,
      `(lower`=NA,
      `upper)`=NA,
      missInfo=NA,
      DisposalCategory="Diverted"
   )
)


# determine N for each category by averaging over imputations

getCategoryCounts <- function(cohortGroup) {
   
   perImputationCounts <- rbindlist(lapply(1:numImputations,function(imputationIndex) {
      # join the current imputation to it
      modelData <- pfda_imputed[[imputationIndex]]
      modelData <- modelData[CohortGroup==cohortGroup]
      
      modelData[,DisposalCategory:="Other"]
      # fill in the imputed outcome (we ignore other as its already the default)
      modelData[!is.na(ImputedHOOutcome)&!(ImputedHOOutcome=="Other"),HOOutcomeCodeLong:=ImputedHOOutcome]
      modelData[HOOutcomeCodeLong %in% c("OC08","OC22"),DisposalCategory:="Diverted"]
      modelData[HOOutcomeCodeLong %in% c("OC01","OC1A"),DisposalCategory:="Charged"]
      # 2 and 2A shouldn't strictly be in here but they are
      modelData[HOOutcomeCodeLong %in% c("OC02","OC2A"),DisposalCategory:="Cautioned"]
      modelData[HOOutcomeCodeLong %in% c("OC03","OC3A"),DisposalCategory:="Cautioned"]
      modelData[,DisposalCategory:=factor(DisposalCategory,
                                          levels = c("Diverted","Charged", "Cautioned",  "Other"))]
      
      modelData[,list(Imputation=imputationIndex,.N),by=DisposalCategory]
   }))
   
   averageCounts <- perImputationCounts[,list(`N (avg)`=round(mean(N))),by=DisposalCategory]
   averageCounts[,DisposalCategory:=factor(DisposalCategory,levels=c("Diverted","Charged","Cautioned","Other"))]
   setorder(averageCounts,DisposalCategory)
   averageCounts
   
}

ddssG1CategoryCounts <- getCategoryCounts(cohortGroup=1)

ddssG1ResultsTable[ddssG1CategoryCounts,N_avg:=`N (avg)`,on="DisposalCategory"]

makeDDSSPrettyTable <- function(d) {
   
   # transform log to HR
   d[,Disposal:=DisposalCategory]
   d[, HR := ifelse(Disposal== "Diverted", 1.00, exp(results))]
   d[, L_CI := exp(`(lower`)]
   d[, U_CI := exp(`upper)`)]
   
   # format strings
   d[, `Hazard Ratio` := round(HR, 2)]
   d[, `95% CI` := ifelse(Disposal== "Diverted", 
                           "-", 
                           paste0("[", sprintf("%.2f", L_CI), ", ", sprintf("%.2f", U_CI), "]"))]
   
   # order factor
   level_order <- c("Diverted", "Charged", "Cautioned", "Other")
   d[, Disposal:= factor(Disposal, levels = level_order)]
   setorder(d, Disposal)
   
   final <- d[, .(Disposal, `N (avg)` = N_avg, `Hazard Ratio`, `95% CI`)]
   
   return(final)
}

ddssG1PrettyTable <- makeDDSSPrettyTable(ddssG1ResultsTable)

ddssG2R <- combineResults(ddssG2ReoffendingModels)
ddssG2Summary <- summary(ddssG2R)
ddssG2ResultsTable <- ddssG2Summary[grep("DisposalCategory", rownames(ddssG2Summary)), ]
setDT(ddssG2ResultsTable,keep.rownames=T)
ddssG2ResultsTable[rn=="DisposalCategoryCharged","DisposalCategory":="Charged"]
ddssG2ResultsTable[rn=="DisposalCategoryCautioned","DisposalCategory":="Cautioned"]
ddssG2ResultsTable[rn=="DisposalCategoryOther","DisposalCategory":="Other"]
ddssG2ResultsTable <- rbind(
   ddssG2ResultsTable,
   data.table(
      rn="DisposalCategoryDiverted",
      results=NA,
      se=NA,
      `(lower`=NA,
      `upper)`=NA,
      missInfo=NA,
      DisposalCategory="Diverted"
   )
)
ddssG2CategoryCounts <- getCategoryCounts(cohortGroup=2)
ddssG2ResultsTable[ddssG2CategoryCounts,N_avg:=`N (avg)`,on="DisposalCategory"]
ddssG2PrettyTable <- makeDDSSPrettyTable(ddssG2ResultsTable)

fwrite(ddssG1PrettyTable,"safe_outputs/ddss_g1_reoffending_diversion_relative.csv")
fwrite(ddssG2PrettyTable,"safe_outputs/ddss_g2_reoffending_diversion_relative.csv")

ddssG1PrettyTable[,Cohort:="Simple possession"]
ddssG2PrettyTable[,Cohort:="Wider offending"]
ddssPretty <- rbind(ddssG1PrettyTable,ddssG2PrettyTable)
setcolorder(ddssPretty,"Cohort")

## pool pairwise emmeans contrasts across a list of models

poolContrastsReoffending <- function(models, specs, level = 0.95, ...) {
   
   # extract the contrast pairs per model
   pair_list <- lapply(models, function(m) {
      emm <- emmeans(m, specs = specs, ...)  # e.g. specs = ~ DisposalCategory
      # use type lp for Log Hazard
      data.table(summary(pairs(emm,type="lp")))
   })
   
   # 2) Extract contrast betas + vcovs for each imputation
   betas <- MIextract(pair_list, fun = function(x) setNames(x$estimate,x$contrast))
   vars  <- MIextract(pair_list, fun = function(x) setNames(x$SE^2,x$contrast))
   
   # 3) Rubin pooling at contrast level
   pooled <- MIcombine(betas, vars)
   
   # 4) extract and convert back to HRs
   logHR <- pooled$coefficients
   HR    <- exp(logHR)
   ci_log <- confint(pooled, level = level)
   ci_HR  <- exp(ci_log)
   SE     <- sqrt(diag(pooled$variance))
   z      <- logHR / SE
   df     <- pooled$df
   
   out <- data.frame(
      contrast = names(logHR),
      logHR    = logHR,
      SE       = SE,
      HR       = HR,
      CI_low   = ci_HR[, 1],
      CI_high  = ci_HR[, 2],
      df       = df,
      z        = z,
      p        = 2 * pt(abs(z), df = df, lower.tail = FALSE),
      stringsAsFactors = FALSE
   )
   
   out
}

ddssG1ReoffendingContrasts <- poolContrastsReoffending(ddssG1ReoffendingModels,specs = ~ DisposalCategory)
ddssG2ReoffendingContrasts <- poolContrastsReoffending(ddssG2ReoffendingModels,specs = ~ DisposalCategory)

# save these locally (safe outputs)
g1FN <- "safe_outputs/DDSS_Reoffending_G1.csv"
if(!file.exists(g1FN)) { write.csv(ddssG1ReoffendingContrasts,g1FN,row.names=F) }
g2FN <- "safe_outputs/DDSS_Reoffending_G2.csv"
if(!file.exists(g2FN)) { write.csv(ddssG2ReoffendingContrasts,g2FN,row.names=F) }

# drugs only model

ppDrugsOnlyCoxFormula <- "
   Surv(ExposureTimeDaysDrugs, DidReoffendDrugs) ~ WasDivertedOrOC22 +
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
ppG1DrugsReoffendingModels <- createPPModelsReoffending(1,ppDrugsOnlyCoxFormula,ddss=F)
ppG1DrugsReoffendingSummary <- makePPReoffendingSummary(ppG1DrugsReoffendingModels,"G1 : Simple possession")
ppG2DrugsReoffendingModels <- createPPModelsReoffending(2,ppDrugsOnlyCoxFormula,ddss=F)
ppG2DrugsReoffendingSummary <- makePPReoffendingSummary(ppG2DrugsReoffendingModels,"G2 : Complex needs")

# save models, deliberately control this as we want to "freeze" our real models
if(F) {
   print("Writing G1 PP models")
   modelFN <- paste0(moj_data_path,"/models/pp_reoffending_g1_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ppG1ReoffendingModels,modelFN,gENCRYPTION_KEY) 
   print("Writing G2 PP models")
   modelFN <- paste0(moj_data_path,"/models/pp_reoffending_g2_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ppG2ReoffendingModels,modelFN,gENCRYPTION_KEY) 
   print("Writing G1 DDSS models")
   modelFN <- paste0(moj_data_path,"/models/ddss_reoffending_g1_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ddssG1ReoffendingModels,modelFN,gENCRYPTION_KEY) 
   print("Writing G2 DDSS models")
   modelFN <- paste0(moj_data_path,"/models/ddss_reoffending_g2_final_models_ENCRYPTED.rds")
   writeEncryptedMultiPlatform(ddssG2ReoffendingModels,modelFN,gENCRYPTION_KEY) 
}



# write full imputed models
groups <- 1:2
outcomes <- c("Reoffending","DrugsReoffending")
lapply(groups,function(currentGroup){
   lapply(outcomes,function(currentOutcome) {
      # get variable name and output file name
      varName <- paste0("ppG",currentGroup,currentOutcome,"Models")
      print(paste0("Processing G",currentGroup,", ",currentOutcome))
      fn <- paste0("safe_outputs/g",currentGroup,"_pp_",currentOutcome,"_all_imputed_coefs.csv")
      
      # impute the coefficients
      models <- get(varName)
      betas <- MIextract(models,fun = function(x) coef(x))
      variances <- MIextract(models,fun = function(x) vcov(x))
      m <- MIcombine(betas,variances)
      x <- as.data.table(summary(m),keep.rownames = "variable")
      print(paste0("Writing ",fn))
      fwrite(x,fn)
   })
})

ppG1ReoffendingZPHModel <- createReoffendingModel(droplevels(pfda[CohortGroup==1]),ppCoxFormulaReoffending)
ppG2ReoffendingZPHModel <- createReoffendingModel(droplevels(pfda[CohortGroup==2]),ppCoxFormulaReoffending)

ppG1DrugReoffendingZPHModel <- createReoffendingModel(droplevels(pfda[CohortGroup==1]),ppDrugsOnlyCoxFormula)
ppG2DrugReoffendingZPHModel <- createReoffendingModel(droplevels(pfda[CohortGroup==2]),ppDrugsOnlyCoxFormula)

ppZPHModels <- list(
   G1=list(
      Reoffending=ppG1ReoffendingZPHModel,
      DrugReoffending=ppG1DrugReoffendingZPHModel 
   ),
   G2=list(
      Reoffending=ppG2ReoffendingZPHModel,
      DrugReoffending=ppG2DrugReoffendingZPHModel 
   )
)

# make a table of the coeffs
library(devEMF)
lapply(names(ppZPHModels),function(group){
   models <- ppZPHModels[[group]]
   lapply(names(models),function(modelName){
      model <- get(paste0("pp",group,modelName,"ZPHModel"))
      zph <- cox.zph(model)
      fn <- paste0("figures/",tolower(group),"_pp_",modelName,"_proportionality.emf")
      emf(fn,width=8,height=6,units="in")
      plot(zph[1])
      dev.off()
   })
})

# make a table
lapply(names(ppZPHModels),function(group){
   models <- ppZPHModels[[group]]
   lapply(names(models),function(modelName){
      model <- get(paste0("pp",group,modelName,"ZPHModel"))
      zph <- cox.zph(model)
      fn <- paste0("safe_outputs/",tolower(group),"_pp_",modelName,"_zph.csv")
      zphDT <- as.data.table(zph$table, keep.rownames = "variable")
      fwrite(zphDT,fn)
   })
})
   
   


ppCountFormulaReoffending <- "
   PNCReoffenceCount ~ WasDivertedOrOC22 +
   offset(log(ExposureTimeDays)) +
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
ppCountFormulaReoffending <- as.formula(gsub("\n"," ",ppCountFormulaReoffending))


ppCCCountModelG1 <- glmmTMB(ppCountFormulaReoffending,  data = pfda[CohortGroup==1],  family = nbinom2)
ppCCCountModelG2 <- glmmTMB(ppCountFormulaReoffending,  data = pfda[CohortGroup==2],  family = nbinom2)
ppCCCountModelZIG2 <- glmmTMB(
   ppCountFormulaReoffending,
   data = pfda[CohortGroup==2],  
   ziformula = ~WasDivertedOrOC22,
   family = truncated_nbinom2
)
   
   
   
   