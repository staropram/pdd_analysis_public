source("../../common/r/file_paths.R")
library(lme4)
library(glmmTMB)
library(mice)
library(Amelia)
library(mitools)
library(broom.mixed)
library(sjPlot)
library(qs)

# load the equity formulae
source("EquityFormula.R")

# recompute options
forceReloadPFDA <- T
forceRecomputeModels <- T
forceReloadImputedData <- F

imputationStrategy <- "Bayes"

# Analysis code for the equity analysis
if(!exists("pfda")|forceReloadPFDA==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
}

# load in the long form imputed data
if(!exists("imputedDataLong")|forceReloadImputedData==T) {
   imputedLongInputFN <- paste0(analysis_path,"PFD_Plus_Imputed_Long_",imputationStrategy,".feather")
   print(paste0("Reading long from ",imputedLongInputFN)) 
   imputedDataLong <- arrow::read_feather(imputedLongInputFN)
   
   # check for multi events
   m <- pfda[,list(.N,DivertCount=sum(WasDiverted)),by=IncidentID]
   m <- m[N!=1&DivertCount==1&DivertCount!=2]
   ids <- m$IncidentID
   
   imputedDataLong[IncidentID %in% ids,WasDiverted:=T]
   imputedDataLong[IncidentID %in% ids,WasDivertedOrOC22:=T]
   
   # we only care about group 1
   imputedDataLong <- imputedDataLong[CohortGroup==1]
   
   #ids <- c("PFD_d298d843e93b41c2df82ce7ab697bd3d_2021-11-17","PFD_93a8e6b028ae3a652252fa58d2c7d963_2022-08-26",
   #"PFD_85da7242af5c97ffd67e8836c4d0a358_2021-11-21","PFD_89256ddb3994d97b51cd915f7fef5277_2022-05-22")
   #imputedDataLong[IncidentID %in% ids,WasDiverted:=T]
   #imputedDataLong[IncidentID %in% ids,WasDivertedOrOC22:=T]
   
   
   #  convert the long data back to data format to be used in modelling
   # Note: Although we use the `mids` format from the `mice` package for compatibility 
   # with `with()` and `pool()`, our imputations were generated externally using a 
   # Bayesian model rather than the `mice()` function. We constructed the long-format 
   # dataset manually, including `.imp == 0` (original data with missing values) for 
   # diagnostic comparison only. As per the `mids` specification, `.imp == 0` is not 
   # used in model fitting or pooling — only `.imp >= 1` are treated as imputations.
   imputedPFDA <- as.mids(imputedDataLong)
}

# load existing models if not been asked to force recompute
modelsLoaded <- F
if(forceRecomputeModels==F) {
   diversionModelOF <- paste0(analysis_outputs,imputationStrategy,"DiversionModel_Raw.RData")
   criminalisationModelOF <- paste0(analysis_outputs,imputationStrategy,"CriminalisationModel_Raw.RData")
   if(file.exists(diversionModelOF)&&file.exists(criminalisationModelOF)) {
      print("Loading models from file")
      load(diversionModelOF)
      load(criminalisationModelOF)
      modelsLoaded <-T 
   } else {
      print("Unable to load models, will have to recompute")
   }
}

# recompute models if asked to
if(forceRecomputeModels==T|modelsLoaded==F) {
   print("Running models for imputed data")
   # diverted outcome models
   diversionModels <- list(
      EthnicityOnly = with(data=imputedPFDA, glmmTMB(diversionFormulaEthnicityOnly,family = binomial),
                           parallel="snow",n.core=2),
      FullNoRandom  = with(data=imputedPFDA, glmmTMB(diversionFormulaFullNoRandom,family = binomial),
                           parallel="snow",n.core=2),
      Full          = with(data=imputedPFDA, glmmTMB(diversionFormulaFull,family = binomial),
                           parallel="snow",n.core=2)
   )
   
   # criminalisation outcome models
   criminalisationModels <- list(
      EthnicityOnly = with(data=imputedPFDA, glmmTMB(criminalisationFormulaEthnicityOnly,family = binomial),
                           parallel="snow",n.core=2),
      FullNoRandom  = with(data=imputedPFDA, glmmTMB(criminalisationFormulaFullNoRandom,family = binomial),
                           parallel="snow",n.core=2),
      Full          = with(data=imputedPFDA, glmmTMB(criminalisationFormulaFull,family = binomial),
                           parallel="snow",n.core=2)
   )
}

# save the raw models (unless they were just loaded)
if(modelsLoaded==F) {
   print("Saving models to file")
   #modelOF <- paste0(analysis_outputs,imputationStrategy,"DiversionModel_Raw.rds")
   #saveRDS(diversionModels,file=modelOF)
   #modelOF <- paste0(analysis_outputs,imputationStrategy,"CriminalisationModel_Raw.rds")
   #saveRDS(criminalisationModels,file=modelOF)
   
   modelOF <- paste0(analysis_outputs,imputationStrategy,"DiversionModel_Raw.qs")
   qsave(diversionModels,file=modelOF)
   modelOF <- paste0(analysis_outputs,imputationStrategy,"CriminalisationModel_Raw.qs")
   qsave(criminalisationModels,file=modelOF)
}

# pool the results
print("Pooling results")
diversionPooledResults <- lapply(diversionModels,pool)
criminalisationPooledResults <- lapply(criminalisationModels,pool)

# save these pooled results
print("Saving pooled results to file")
modelOF <- paste0(analysis_outputs,imputationStrategy,"DiversionModel_Pooled.rds")
saveRDS(diversionPooledResults,file=modelOF)
modelOF <- paste0(analysis_outputs,imputationStrategy,"CriminalisationModel_Pooled.rds")
saveRDS(criminalisationPooledResults,file=modelOF)

source("TablingFunctions.R")
# create table summaries
print("Creating table summaries")
createDocxSummaries <- function(pooledResults,experimentName,imputationStrategy) {
   lapply(names(pooledResults),function(prName) {
      pr <- pooledResults[[prName]]
      createPooledModelSummaryDoc(pr,experimentName,imputationStrategy,prName)
   })
}

createDocxSummaries(diversionPooledResults,"DiversionOutcome",imputationStrategy)
createDocxSummaries(criminalisationPooledResults,"CrimininalisationOutcome",imputationStrategy)


# create summary stats

headers <- c("Ethnicity","Model 1","Model 2\n(adjusted for age, sex, drug)","Model 3 - primary analysis\n(adjusted for age, sex, drug, police force)")
eths <- c("ASIAN","BLACK","MIXED","OTHER")
whiteRow <- data.table(Ethnicity="WHITE",M1="1 (ref)",M2="1 (ref)",M3="1 (ref)")
names(whiteRow) <- headers
extractCIFromPool <- function(pooledResult,eths,exponentiate) {
  # extract the relevant data from each row
  ethSelectors <- paste0("EthnicityCoarse",eths)
  sDat <- data.table(summary(pooledResult))
  
  estimates <- sDat[term %in% ethSelectors,estimate]
  errors <- sDat[term %in% ethSelectors,std.error]
  
  # need df for confint
  df <- sDat[term %in% ethSelectors,df]
  # 95% confidence level
  criticalValues <- qt(0.975, df = df)
  
  # calculate confidence interval
  ciLower <- estimates - criticalValues * errors
  ciUpper <- estimates + criticalValues * errors
  
  data.table(
     Ethnicity=eths,
     Estimates=if(exponentiate==T) {
       sprintf("%.2f (%.2f, %.2f)",exp(estimates),exp(ciLower),exp(ciUpper))
     } else {
       sprintf("%.2f (%.2f, %.2f)",estimates,ciLower,ciUpper)
     }
  )
}

x <- lapply(diversionPooledResults,
   function(pooledResults) { extractCIFromPool(pooledResults,eths,T)  }
)
combined <- Reduce(function(a,b) {merge(a,b,by="Ethnicity",all.x=T)},x)
names(combined) <- headers
combinedDiversion <- rbindlist(list(whiteRow,combined))

# save this 
write_feather(combinedDiversion,"safeoutputs/DiversionCombined.feather")

x <- lapply(criminalisationPooledResults,
   function(pooledResults) { extractCIFromPool(pooledResults,eths,T)  }
)
combined <- Reduce(function(a,b) {merge(a,b,by="Ethnicity",all.x=T)},x)
names(combined) <- headers
combinedCriminalisation <- rbindlist(list(whiteRow,combined))
write_feather(combinedCriminalisation,"safeoutputs/CriminalisationCombined.feather")
