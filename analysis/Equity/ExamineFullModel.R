# load in the full model
source("../../common/r/file_paths.R")
library(lme4)
library(glmmTMB)
library(ResourceSelection)
library(pROC)
library(gt)
library(qs)

# OPTIONS
# experimentVariant is either "AllForces" "DiversionForcesOnly"
experimentVariant <- "Bayes"

# load in the equityModels
print("Loading equity models")
load(paste0(analysis_outputs,experimentVariant,"DiversionModel_Pooled.RData"))
load(paste0(analysis_outputs,experimentVariant,"DiversionModel_Raw.RData"))
load(paste0(analysis_outputs,experimentVariant,"CriminalisationModel_Pooled.RData"))
load(paste0(analysis_outputs,experimentVariant,"CriminalisationModel_Raw.RData"))
diversionPooledResults <- readRDS(paste0(analysis_outputs,experimentVariant,"DiversionModel_Pooled.rds"))
#diversionModels <- readRDS(paste0(analysis_outputs,experimentVariant,"DiversionModel_Raw.rds"))
#criminalisationPooledResults <- readRDS(paste0(analysis_outputs,experimentVariant,"CriminalisationModel_Pooled.rds"))
#criminalisationModels <- readRDS(paste0(analysis_outputs,experimentVariant,"CriminalisationModel_Raw.rds"))

#diversionPooledResults <- qload(paste0(analysis_outputs,experimentVariant,"DiversionModel_Pooled.rds"))
#diversionModels <- qload(paste0(analysis_outputs,experimentVariant,"DiversionModel_Raw.qs"))
#criminalisationPooledResults <- qload(paste0(analysis_outputs,experimentVariant,"CriminalisationModel_Pooled.rds"))
#criminalisationModels <- qload(paste0(analysis_outputs,experimentVariant,"CriminalisationModel_Raw.qs"))

# create some pointers to named models
diversionModelsEthnicityOnly <- diversionModels[["EthnicityOnly"]]
diversionModelsFullNoRandom <- diversionModels[["FullNoRandom"]]
diversionModelsFull <- diversionModels[["Full"]]
criminalisationModelsEthnicityOnly <- criminalisationModels[["EthnicityOnly"]]
criminalisationModelsFullNoRandom <- criminalisationModels[["FullNoRandom"]]
criminalisationModelsFull <- criminalisationModels[["Full"]]

# load the raw pfda
gForceReload <- T
if(!exists("pfda")|gForceReload==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
   pfda1 <- pfda[CohortGroup==1]
   pfda2 <- pfda[CohortGroup==2]
}

# load the ethnicity maps between SDE <-> Coarse and IC <-> Coarse
ethnicityMapSDEtoCoarse <- data.table(read_excel("../../common/data/internal/EthnicityMap.xlsx",sheet="SDEtoCoarse"))
ethnicityMapICtoCoarse <- data.table(read_excel("../../common/data/internal/EthnicityMap.xlsx",sheet="ICtoCoarse"))

generate_extra_stats_glmmTMB <- function(model) {
   # Check if model is a glmmTMB model
   if (!inherits(model, "glmmTMB")) {
      stop("The provided model is not a glmmTMB model.")
   }
   
   # Get model family
   model_family <- family(model)$family
   
   # Compute residual variance (σ²) based on model family
   residual_variance <- switch(
      model_family,
      "gaussian" = round(VarCorr(model)$cond$residual, 2),
      "binomial" = round(pi^2 / 3, 2),  # Logistic distribution variance
      "poisson" = 1,                   # Variance for Poisson
      NA_real_                          # Default for unsupported families
   )
   
   # Extract random effects variance
   random_effects <- as.data.table(VarCorr(model)$cond)[
      , .(Effect = names(.SD), Variance = round(as.numeric(.SD), 2))
   ]
   
   # Number of levels for each random effect
   random_effect_levels <- sapply(
      lapply(ranef(model)$cond, rownames),
      function(x) length(unique(x))
   )
   
   # Calculate Intraclass Correlation Coefficient (ICC) and R2
   model_stats <- model_performance(model)
   icc <- round(model_stats$ICC, 2)
   r2_marginal <- round(model_stats$R2_marginal, 3)
   r2_conditional <- round(model_stats$R2_conditional, 3)
   
   # Number of observations
   num_obs <- nobs(model)
   
   # Combine all into a data.table
   extra_stats <- data.table(
      Metric = c(
         "σ² (residual)", 
         paste0("τ00 ", random_effects$Effect), 
         paste0("N ", random_effects$Effect), 
         "ICC", 
         "Observations", 
         "Marginal R² / Conditional R²"
      ),
      Value = c(
         residual_variance,
         random_effects$Variance,
         random_effect_levels,
         icc,
         num_obs,
         paste0(r2_marginal, " / ", r2_conditional)
      )
   )
   
   return(extra_stats)
}

generate_extra_stats <- function(model) {
   # Check if model is a mixed-effects model
   if (!inherits(model, "merMod")) {
      stop("The provided model is not a mixed-effects model.")
   }
   
   # Extract random effects variance
   random_effects <- as.data.table(VarCorr(model))[
      , .(Effect = grp, Variance = round(vcov, 2))
   ]
   
   # Calculate residual variance
   residual_variance <- round(attr(VarCorr(model), "sc")^2, 2)
   
   # Calculate Intraclass Correlation Coefficient (ICC) and R2
   model_stats <- model_performance(model)
   icc <- round(model_stats$ICC, 2)
   r2_marginal <- round(model_stats$R2_marginal, 3)
   r2_conditional <- round(model_stats$R2_conditional, 3)
   
   # Number of observations
   num_obs <- nobs(model)
   
   # Combine all into a data.table
   extra_stats <- data.table(
      Metric = c(
         "σ²", 
         paste0("τ00 ", random_effects$Effect[1]), 
         "ICC", 
         "Observations", 
         "Marginal R² / Conditional R²"
      ),
      Value = c(
         residual_variance,
         random_effects$Variance[1],
         icc,
         num_obs,
         paste0(r2_marginal, " / ", r2_conditional)
      )
   )
   
   return(extra_stats)
}