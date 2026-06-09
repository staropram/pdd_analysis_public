source("../../common/r/file_paths.R")
library(lme4)
library(glmmTMB)
library(ResourceSelection)
library(pROC)
library(sjPlot)

source("EquityFormula.R")
# function to con

# Analysis code for the equity analysis
gForceReload <- T
if(!exists("pfda")|gForceReload==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
}

# do it only for Group 1
pfda1 <- pfda[CohortGroup==1]

# create complete-case subset
pfda1CC <- pfda1[
   !is.na(Sex) & # sex is present
   !is.na(EthnicityCoarse) & # ethnicity is present AND
   (!is.na(HOOutcomeCodeLong) | WasDivertedOrOC22==T)
]


# age plot
# fit model just with age and age^2
ageModel <- glmmTMB(
   ageFormula,
   pfda1CC[!is.na(EthnicityCoarse)],
   family=binomial
)

## Ethnicity only
print("Creating ethnicity-only diversion model")
diversionModelEthnicityOnly <- glmmTMB(
   diversionFormulaEthnicityOnly,
   pfda1CC,
   family=binomial
)

# save the model
print("Saving ethnicity-only diversion-outcome model")
modelOF <- paste0(analysis_outputs,"DiversionModel_EthnicityOnly.RData")
save(diversionModelEthnicityOnly,file=modelOF)

# equity model with all covariates but no police intercept
print("Creating full diversion-outcome model but with no random effects")
diversionModelFullNoRandom <- glmmTMB(
   diversionFormulaFullNoRandom,
   pfda1CC,
   family=binomial
)

# save the model
print("Saving full diversion-outcome model but with no random effects")
modelOF <- paste0(analysis_outputs,"DiversionModel_Full_NoRandom.RData")
save(diversionModelFullNoRandom,file=modelOF)

## FULL MODEL
print("Creating full diversion-outcome model")
diversionModelFull <- glmmTMB(
   diversionFormulaFull,
   pfda[CohortGroup==1],
   family=binomial
)

dmf <- glmer(diversionFormulaFull,pfda1CC,family=binomial)

# save the model
print("Saving full diversion-outcome model")
modelOF <- paste0(analysis_outputs,"DiversionModel_Full.RData")
save(diversionModelFull,file=modelOF)

## FULL MODEL PLUS SLOPE
#print("Creating full diversion-outcome model plus slope")
#diversionModelFullPlusSlope <- glmmTMB(
#   diversionFormulaFullPlusSlope,
#   pfda1CC,
#   family=binomial
#)

# save the model
print("Saving full diversion-outcome model")
modelOF <- paste0(analysis_outputs,"DiversionModel_FullPlusSlope.RData")
save(diversionModelFullPlusSlope,file=modelOF)

#profile_cis <- confint(diversionModelFull,method="Wald")

# write some docs for convenient examination
print("Write convenience documents for diversion model")
tab_model(diversionModelEthnicityOnly,file="doc/DiversionModel_EthnicityOnly_NO_IMPUTATION.doc")
tab_model(diversionModelFullNoRandom,file="doc/DiversionModel_Full_NoRandom_NO_IMPUTATIOn.doc")
tab_model(diversionModelFull,file="doc/DiversionModel_Full_NO_IMPUTATION.doc")

x <- tab_model(diversionModelFull)

# are non diversion forces allowed the flag?
if(F) {
   ageVSDiversion <- data.table(
      AgeInDays=18:50*365
   )
   ageVSDiversion[,AgeInDaysSquared:=AgeInDays^2]
   ageVSDiversion[,AgeAtContactInDays_CenterStandard:=(AgeInDays-mean(AgeInDays))/sd(AgeInDays)]
   ageVSDiversion[,AgeAtContactInDaysSquared_CenterStandard:=(AgeInDaysSquared-mean(AgeInDaysSquared))/sd(AgeInDaysSquared)]
   
   ageVSDiversion[,PredictedDiversion:=predict(ageModel,newdata=ageVSDiversion,type="response")]
   
   avdReal <- pfda[CohortGroup==1,mean(WasDivertedOrOC22,na.rm=T),by=AgeAtLastBirthday]
   
   plot(ageVSDiversion$AgeInDays/365,ageVSDiversion$PredictedDiversion)
   points(avdReal$AgeAtLastBirthday,avdReal$V1,col="blue",pch=2) 
}

# Hosmer-Lemeshow test
if(F) {
   observed <- model.frame(diversionModelFull)$WasDivertedOrOC22
   predicted <- predict(diversionModelFull,type="response")
   roc_curve <- roc(observed, predicted)
   auc_value <- auc(roc_curve)
   hl_test <- hoslem.test(observed, predicted, g = 10)
}

# Create models for criminalised outcome
# criminalisation with only ethnicity

pfda1CCCrim <- pfda1[
   !is.na(Sex) & # sex is present
   !is.na(EthnicityCoarse) & # ethnicity is present AND
   !is.na(HOOutcomeCodeLong) # ho outcome is present
]

## Ethnicity only
print("Creating ethnicity-only criminalisation model")
criminalisationModelEthnicityOnly <- glmmTMB(
   criminalisationFormulaEthnicityOnly,
   pfda1CCCrim,
   family=binomial
)

# save the model
print("Saving ethnicity-only criminalisation model")
modelOF <- paste0(analysis_outputs,"criminalisationModel_EthnicityOnly.RData")
save(criminalisationModelEthnicityOnly,file=modelOF)

# criminalisation model with all covariates but no police intercept
print("Creating full criminalisation model but with no random effects")
criminalisationModelFullNoRandom <- glmmTMB(
   criminalisationFormulaFullNoRandom,
   pfda1CCCrim,
   family=binomial
)

# save the model
print("Saving full criminalisation model but with no random effects")
modelOF <- paste0(analysis_outputs,"criminalisationModel_Full_NoRandom.RData")
save(criminalisationModelFullNoRandom,file=modelOF)

## FULL MODEL
print("Creating full criminalisation model")
criminalisationModelFull <- glmmTMB(
   criminalisationFormulaFull,
   pfda1CCCrim,
   family=binomial
)

# save the model
print("Saving full criminalisation model")
modelOF <- paste0(analysis_outputs,"criminalisationModel_Full.RData")
save(criminalisationModelFull,file=modelOF)

# write some docs for convenient examination
tab_model(criminalisationModelEthnicityOnly,file="doc/CriminalisationModel_EthnicityOnly_NO_IMPUTATION.doc")
tab_model(criminalisationModelFullNoRandom,file="doc/CriminalisationModel_Full_NoRandom_NO_IMPUTATION.doc")
tab_model(criminalisationModelFull,file="doc/CriminalisationModel_Full_NO_IMPUTATION.doc")
