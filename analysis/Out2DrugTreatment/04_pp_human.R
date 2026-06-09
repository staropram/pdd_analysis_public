assessCounterFactualsTreatment <- function(cohortGroup, models, numSamples = 0, cycleDays = 30) {
   
   results <- lapply(1:numImputations, function(imputationIndex) {
      
      print(paste0("Constructing counterfactuals for imputation ", imputationIndex))
      
      # get the input data
      modelData <- pfda_imputed[[imputationIndex]]
      modelData <- modelData[CohortGroup == cohortGroup]
      
      # merge historical PNC offending data
      modelData <- merge(modelData,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
      
      # merge in the drug treatment variables
      # remove the old variable first
      modelData[,EnteredIntoDrugTreatment:=NULL]
      modelData[,WasInTreatmentWithin28DaysPriorToContact:=NULL]
      modelData <- merge(modelData,drugTreatment,by="RandomGroupedIncidentID",all.x=T)
      
      # exclude those in treatment within 28 days before
      modelData <- modelData[WasInTreatmentWithin28DaysPriorToContact==F]
      
      # drop unused levels
      modelData <- droplevels(modelData)
      
      # sample if requested
      if(numSamples != 0) {
         set.seed(1000 + imputationIndex)
         modelData <- modelData[sample.int(nrow(modelData), numSamples)]
      }
      
      # make counterfactual datasets
      modelDataDiverted    <- copy(modelData)
      modelDataNotDiverted <- copy(modelData)
      
      modelDataDiverted[,    WasDivertedOrOC22 := TRUE]
      modelDataNotDiverted[, WasDivertedOrOC22 := FALSE]
      
      # set follow-up to 30 days (IMPORTANT: assumes LogFollowupDuration = log(days))
      modelDataDiverted[,    LogDrugTreatmentExposureTimeDays := log(cycleDays)]
      modelDataNotDiverted[, LogDrugTreatmentExposureTimeDays := log(cycleDays)]
      
      # get current model
      m <- models[[imputationIndex]]
      
      # predict population-average probabilities (random effects set to 0)
      pDiverted_i <- predict(m, newdata = modelDataDiverted,
                             type = "response", re.form = NA)
      
      pNotDiverted_i <- predict(m, newdata = modelDataNotDiverted,
                                type = "response", re.form = NA)
      
      # marginalise
      pDivertedTimeWindow   <- mean(pDiverted_i, na.rm = TRUE)
      pNotDivertedTimeWindow <- mean(pNotDiverted_i, na.rm = TRUE)
      
      data.table(
         ImputationIndex = imputationIndex,
         TreatProbDivertedTimeWindow   = pDivertedTimeWindow,
         TreatProbNotDivertedTimeWindow = pNotDivertedTimeWindow
      )
   })
   
   rbindlist(results)
}

poolMarginals <- function(marginals,
                             prob0Name = "TreatProbNotDivertedTimeWindow",
                             prob1Name = "TreatProbDivertedTimeWindow",
                             exposure_name = "WasDivertedOrOC22",
                             return_ci = FALSE) {
   
   # number of imputations
   M <- nrow(marginals)
   
   # extract pooled probabilities
   p0_bar <- mean(marginals[[prob0Name]], na.rm = TRUE)
   p1_bar <- mean(marginals[[prob1Name]], na.rm = TRUE)
   
   # point estimates
   RD_hat <- p1_bar - p0_bar
   RR_hat <- p1_bar / p0_bar
   
   # build final Markov table
   final_tab <- data.table(
      WasDivertedOrOC22 = c(FALSE, TRUE),
      p_30d = c(p0_bar, p1_bar)
   )
   
   # optionally compute Rubin-style CI for RD and RR
   if(return_ci) {
      
      # per-imputation RD and log-RR
      RD_m <- marginals[[prob1Name]] - marginals[[prob0Name]]
      logRR_m <- log(marginals[[prob1Name]] / marginals[[prob0Name]])
      
      # between-imputation variances
      B_RD <- var(RD_m, na.rm = TRUE)
      B_logRR <- var(logRR_m, na.rm = TRUE)
      
      # Rubin total variance (W = 0, only MI uncertainty)
      Tvar_RD <- B_RD * (1 + 1/M)
      Tvar_logRR <- B_logRR * (1 + 1/M)
      
      se_RD <- sqrt(Tvar_RD)
      se_logRR <- sqrt(Tvar_logRR)
      
      # 95% Wald CIs
      RD_ci <- RD_hat + c(-1, 1) * 1.96 * se_RD
      RR_ci <- exp(log(RR_hat) + c(-1, 1) * 1.96 * se_logRR)
      
      attr(final_tab, "summary") <- list(
         p0 = p0_bar,
         p1 = p1_bar,
         RD = RD_hat,
         RD_ci = RD_ci,
         RR = RR_hat,
         RR_ci = RR_ci,
         M = M
      )
   }
   
   return(final_tab)
}


#marginalsG1 <- assessCounterFactualsTreatment(1,ppG1DrugTreatmentModels,numSamples = 0,cycleDays = 30)
#marginalsG2 <- assessCounterFactualsTreatment(2,ppG2DrugTreatmentModels,numSamples = 0,cycleDays = 30)

#finalG1 <- poolMarginals(marginalsG1)
#finalG1[,Group:=1]
#finalG2 <- poolMarginals(marginalsG2)
#finalG2[,Group:=2]

#final <- rbind(finalG1,finalG2)

assessCounterFactualsTreatment <- function(cohortGroup, models, numSamples = 0, cycleDays = 30) {
   
   results <- lapply(1:numImputations, function(imputationIndex) {
      
      print(paste0("Constructing counterfactuals for imputation ", imputationIndex))
      
      # get the input data
      modelData <- pfda_imputed[[imputationIndex]]
      modelData <- modelData[CohortGroup == cohortGroup]
      
      # merge historical PNC offending data
      modelData <- merge(modelData,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
      
      # merge in the drug treatment variables
      # remove the old variable first
      modelData[,EnteredIntoDrugTreatment:=NULL]
      modelData[,WasInTreatmentWithin28DaysPriorToContact:=NULL]
      modelData <- merge(modelData,drugTreatment,by="RandomGroupedIncidentID",all.x=T)
      
      # exclude those in treatment within 28 days before
      modelData <- modelData[WasInTreatmentWithin28DaysPriorToContact==F]
      
      # drop unused levels
      modelData <- droplevels(modelData)
      
      # sample if requested
      if(numSamples != 0) {
         set.seed(1000 + imputationIndex)
         modelData <- modelData[sample.int(nrow(modelData), numSamples)]
      }
      
      # make counterfactual datasets
      modelDataDiverted    <- copy(modelData)
      modelDataNotDiverted <- copy(modelData)
      
      modelDataDiverted[,    WasDivertedOrOC22 := TRUE]
      modelDataNotDiverted[, WasDivertedOrOC22 := FALSE]
      
      # set follow-up to 30 days (IMPORTANT: assumes LogFollowupDuration = log(days))
      modelDataDiverted[,    LogDrugTreatmentExposureTimeDays := log(cycleDays)]
      modelDataNotDiverted[, LogDrugTreatmentExposureTimeDays := log(cycleDays)]
      
      # get current model
      m <- models[[imputationIndex]]
      
      # predict population-average probabilities (random effects set to 0)
      pDiverted_i <- predict(m, newdata = modelDataDiverted,
                             type = "response", re.form = NA)
      
      pNotDiverted_i <- predict(m, newdata = modelDataNotDiverted,
                                type = "response", re.form = NA)
      
      # marginalise
      pDivertedTimeWindow   <- mean(pDiverted_i, na.rm = TRUE)
      pNotDivertedTimeWindow <- mean(pNotDiverted_i, na.rm = TRUE)
      
      data.table(
         ImputationIndex = imputationIndex,
         TreatProbDivertedTimeWindow   = pDivertedTimeWindow,
         TreatProbNotDivertedTimeWindow = pNotDivertedTimeWindow
      )
   })
   
   rbindlist(results)
}

poolMarginals <- function(marginals,
                             prob0Name = "TreatProbNotDivertedTimeWindow",
                             prob1Name = "TreatProbDivertedTimeWindow",
                             exposure_name = "WasDivertedOrOC22",
                             return_ci = FALSE) {
   
   # number of imputations
   M <- nrow(marginals)
   
   # extract pooled probabilities
   p0_bar <- mean(marginals[[prob0Name]], na.rm = TRUE)
   p1_bar <- mean(marginals[[prob1Name]], na.rm = TRUE)
   
   # point estimates
   RD_hat <- p1_bar - p0_bar
   RR_hat <- p1_bar / p0_bar
   
   # build final Markov table
   final_tab <- data.table(
      WasDivertedOrOC22 = c(FALSE, TRUE),
      p_30d = c(p0_bar, p1_bar)
   )
   
   # optionally compute Rubin-style CI for RD and RR
   if(return_ci) {
      
      # per-imputation RD and log-RR
      RD_m <- marginals[[prob1Name]] - marginals[[prob0Name]]
      logRR_m <- log(marginals[[prob1Name]] / marginals[[prob0Name]])
      
      # between-imputation variances
      B_RD <- var(RD_m, na.rm = TRUE)
      B_logRR <- var(logRR_m, na.rm = TRUE)
      
      # Rubin total variance (W = 0, only MI uncertainty)
      Tvar_RD <- B_RD * (1 + 1/M)
      Tvar_logRR <- B_logRR * (1 + 1/M)
      
      se_RD <- sqrt(Tvar_RD)
      se_logRR <- sqrt(Tvar_logRR)
      
      # 95% Wald CIs
      RD_ci <- RD_hat + c(-1, 1) * 1.96 * se_RD
      RR_ci <- exp(log(RR_hat) + c(-1, 1) * 1.96 * se_logRR)
      
      attr(final_tab, "summary") <- list(
         p0 = p0_bar,
         p1 = p1_bar,
         RD = RD_hat,
         RD_ci = RD_ci,
         RR = RR_hat,
         RR_ci = RR_ci,
         M = M
      )
   }
   
   return(final_tab)
}


#marginalsG1 <- assessCounterFactualsTreatment(1,ppG1DrugTreatmentModels,numSamples = 0,cycleDays = 30)
#marginalsG2 <- assessCounterFactualsTreatment(2,ppG2DrugTreatmentModels,numSamples = 0,cycleDays = 30)

#finalG1 <- poolMarginals(marginalsG1)
#finalG1[,Group:=1]
#finalG2 <- poolMarginals(marginalsG2)
#finalG2[,Group:=2]

#final <- rbind(finalG1,finalG2)

assessCounterFactualsTreatment_DeltaRubin <- function(
      cohortGroup,
      models,
      cycleDays = 30,
      exposure_name = "WasDivertedOrOC22TRUE",
      M = length(models),
      h = 1e-4
) {
   
   results_list <- lapply(seq_len(M), function(imputationIndex) {
      print(paste0("Generating marginals for model",imputationIndex))
      
      m <- models[[imputationIndex]]
      
      # --- rebuild modelData exactly as before ---
      modelData <- pfda_imputed[[imputationIndex]]
      modelData <- modelData[CohortGroup == cohortGroup]
      
      modelData <- merge(modelData, historicalOffences,
                         by="RandomGroupedIncidentID", all.x=TRUE)
      
      modelData[,EnteredIntoDrugTreatment:=NULL]
      modelData[,WasInTreatmentWithin28DaysPriorToContact:=NULL]
      
      modelData <- merge(modelData, drugTreatment,
                         by="RandomGroupedIncidentID", all.x=TRUE)
      
      modelData <- modelData[WasInTreatmentWithin28DaysPriorToContact==FALSE]
      modelData <- droplevels(modelData)
      
      # --- counterfactual datasets ---
      modelDataDiverted    <- copy(modelData)
      modelDataNotDiverted <- copy(modelData)
      
      modelDataDiverted[,    WasDivertedOrOC22 := TRUE]
      modelDataNotDiverted[, WasDivertedOrOC22 := FALSE]
      
      modelDataDiverted[,    LogDrugTreatmentExposureTimeDays := log(cycleDays)]
      modelDataNotDiverted[, LogDrugTreatmentExposureTimeDays := log(cycleDays)]
      
      # --- linear predictors (population-average) ---
      lp_div <- predict(m, newdata = modelDataDiverted,
                        type="link", re.form=NA)
      
      lp_not <- predict(m, newdata = modelDataNotDiverted,
                        type="link", re.form=NA)
      
      p1 <- mean(plogis(lp_div))
      p0 <- mean(plogis(lp_not))
      
      RD  <- p1 - p0
      RR  <- p1 / p0
      logRR <- log(RR)
      
      # --- extract beta + SE(beta) ---
      coef_table <- summary(m)$coefficients$cond
      idx <- which(rownames(coef_table) == exposure_name)
      
      if(length(idx)==0)
         stop("Exposure variable not found in coefficient table")
      
      beta    <- coef_table[idx, "Estimate"]
      se_beta <- coef_table[idx, "Std. Error"]
      
      # --- numerical derivative wrt beta ---
      # Only diverted arm depends on beta in logistic model
      
      p1_plus  <- mean(plogis(lp_div + h))
      p1_minus <- mean(plogis(lp_div - h))
      
      deriv_p1 <- (p1_plus - p1_minus)/(2*h)
      deriv_p0 <- 0
      deriv_RD <- deriv_p1
      deriv_logRR <- deriv_p1 / p1   # since p0 constant
      
      # --- within-imputation variances (delta) ---
      U_p1    <- (deriv_p1^2)    * se_beta^2
      U_p0    <- 0
      U_RD    <- (deriv_RD^2)    * se_beta^2
      U_logRR <- (deriv_logRR^2) * se_beta^2
      
      data.table(
         p1 = p1,
         p0 = p0,
         RD = RD,
         logRR = logRR,
         U_p1 = U_p1,
         U_p0 = U_p0,
         U_RD = U_RD,
         U_logRR = U_logRR
      )
   })
   
   dt <- rbindlist(results_list)
   
   # --- Rubin pooling helper ---
   pool_one <- function(Q, U) {
      
      Qbar <- mean(Q)
      W    <- mean(U)
      B    <- var(Q)
      Tvar <- W + (1 + 1/M)*B
      SE   <- sqrt(Tvar)
      
      data.table(
         Estimate = Qbar,
         SE       = SE,
         LowerCI  = Qbar - 1.96*SE,
         UpperCI  = Qbar + 1.96*SE,
         RIV      = ifelse(W>0, (1 + 1/M)*B/W, NA_real_)
      )
   }
   
   out_p1    <- pool_one(dt$p1, dt$U_p1)
   out_p0    <- pool_one(dt$p0, dt$U_p0)
   out_RD    <- pool_one(dt$RD, dt$U_RD)
   out_logRR <- pool_one(dt$logRR, dt$U_logRR)
   
   # transform logRR CI back
   out_RR <- copy(out_logRR)
   out_RR[, Estimate := exp(Estimate)]
   out_RR[, LowerCI := exp(LowerCI)]
   out_RR[, UpperCI := exp(UpperCI)]
   
   # final table
   final <- rbindlist(list(
      cbind(Metric="ProbDiverted",    out_p1),
      cbind(Metric="ProbNotDiverted", out_p0),
      cbind(Metric="RiskDifference",  out_RD),
      cbind(Metric="RiskRatio",       out_RR)
   ))
   
   final[, Outcome := "DrugTreatment"]
   final[, Cohort := cohortGroup]
   
   setcolorder(final,
               c("Outcome","Cohort","Metric","Estimate","SE","LowerCI","UpperCI","RIV"))
   
   return(final)
}

finalG1 <- assessCounterFactualsTreatment_DeltaRubin(
   cohortGroup=1,
   models=ppG1DrugTreatmentModels,
   cycleDays=30
)

finalG2 <- assessCounterFactualsTreatment_DeltaRubin(
   cohortGroup=2,
   models=ppG2DrugTreatmentModels,
   cycleDays=30
)

finalDrugTreatment <- rbind(finalG1, finalG2)

print(finalDrugTreatment)

finalG1365 <- assessCounterFactualsTreatment_DeltaRubin(
   cohortGroup=1,
   models=ppG1DrugTreatmentModels,
   cycleDays=365
)

finalG2365 <- assessCounterFactualsTreatment_DeltaRubin(
   cohortGroup=2,
   models=ppG2DrugTreatmentModels,
   cycleDays=365
)

finalDrugTreatment <- rbind(finalG1365, finalG2365)

print(finalDrugTreatment)