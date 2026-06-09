ppReoffendingGetBaselineCumHazAtTime_Efron <- function(model, modelData, timeOffsetDays=0) {
   # LP includes random effects
   lp <- predict(model)
   risk <- exp(lp)
   
   time_vec <- modelData$ExposureTimeDays
   event_vec <- modelData$DidReoffend
   
   dt <- data.table(time = time_vec, event = event_vec, risk = risk)
   
   # sort by time ascending
   setorder(dt, time)
   
   # rolling risk-set sum S_R(t) per row (risk set = time >= t)
   dt[, SR_row := rev(cumsum(rev(risk)))]
   
   # For each tied-time block:
   # - SR is constant for that time (use last SR_row in the block to be safe)
   # - d_t is number of events in the block
   # - SD is sum of risk among events in the block
   agg <- dt[, .(
      d = sum(event),
      SR = SR_row[.N], # risk set sum at that time
      SD = sum(risk[event == 1L]) # sum risk among those who fail at that time
   ), by = time]
   
   setorder(agg, time)
   
   # Efron hazard increment at each time
   # ΔH0(t) = sum_{l=0}^{d-1} 1 / (SR - (l/d)*SD)
   agg[, haz_inc := {
      if (d == 0L) {
         0
      } else if (d == 1L) {
         1 / SR
      } else {
         # l = 0...(d-1)
         l <- 0:(d-1L)
         sum(1 / (SR - (l / d) * SD))
      }
   }, by = time]
   
   agg[, cumhaz := cumsum(haz_inc)]
   
   # Extract last cumhaz on/before offset
   idx <- max(which(agg$time <= timeOffsetDays))
   if (length(idx) == 0L || !is.finite(idx)) return(0)
   
   agg$cumhaz[idx]
}

# gets the baseline hazard from a specific model using specific data
# to do the calculation at the given time offset
ppReoffendingGetBaselineHazardAtTime <- function(model,modelData,timeOffsetDays=0) {
   # get the linear predictors (LP) which include the force random effects
   lp <- predict(model)
   risk_score <- exp(lp)
   
   # get time and event variables note these match the models data order
   # because the merge operations in their construction (prior to being passed into
   # this function order by the key
   time_vec   <- modelData$ExposureTimeDays
   event_vec  <- modelData$DidReoffend
   
   # --- 2. Sort everything by time (Breslow requirement) ---
   ord        <- order(time_vec)
   time_s     <- time_vec[ord]
   event_s    <- event_vec[ord]
   risk_s     <- risk_score[ord]
   
   # aggregate by unique time t because you can have more than one event
   # per day
   d <- data.table(time=time_s,event=event_s,risk=risk_s)
   # risk set denominator
   d[,denom_row:=rev(cumsum(rev(risk)))]
   d[,denom_time := denom_row[.N],by=time]
   agg <- d[,.(
      d = sum(event),
      denom = denom_time[1]
   ),by=time][order(time)]
   
   agg[,haz_inc := d /denom] 
   agg[,cumhaz := cumsum(haz_inc)]
   
   
   # --- 5. Extract the Baseline Hazard at exactly 1 Year (365 days) ---
   # We find the last recorded cumulative hazard on or before Day 365
   idx <- max(which(agg$time <= timeOffsetDays))
   hazAtOffset <- if(is.finite(idx)) agg$cumhaz[idx] else 0
   
   #print(paste("Baseline Cumulative Hazard at ",timeOffsetDays," days:", round(hazAtOffset, 4)))
   
   hazAtOffset
}

ppReoffendingGetAdjustedTreatmentEffectAtTimeOffset <- function(
   model,
   modelData,
   timeOffsetDays,
   treatmentVariable="WasDivertedOrOC22TRUE",
   betaOverride=NULL # allow to override beta for delta estimation of SE
   ) {
   
   # get the baseline hazard for the required time offset
   baselineHazardAtOffset <- ppReoffendingGetBaselineHazardAtTime(model,modelData,timeOffsetDays)
   #baselineHazardAtOffset <- ppReoffendingGetBaselineCumHazAtTime_Efron(model,modelData,timeOffsetDays)
   
   # 1. Get the Treatment coefficient from the coxme model
   if(is.null(betaOverride)) {
      betaTreatment <- fixef(model)[treatmentVariable]
   } else {
      betaTreatment <- betaOverride
   }
   
   # 2. Extract the linear predictor vector
   lp <- predict(model)
   
   # 3. Create 'Clean' LP (Subtract the effect of their ACTUAL treatment)
   # This leaves you with their base risk + their force's random effect
   colName <- gsub("TRUE$", "", treatmentVariable)
   lpBase <- lp - (modelData[[colName]]* betaTreatment)
   
   # 4. Scenario A: Everyone is Treated
   lpAllTreated <- lpBase + (1 * betaTreatment)
   probTreated <- 1 - exp(-baselineHazardAtOffset * exp(lpAllTreated))
   
   # 5. Scenario B: Everyone is Control
   lpAllControl <- lpBase + (0 * betaTreatment)
   probControl <- 1 - exp(-baselineHazardAtOffset * exp(lpAllControl))
   
   # 6. The Result: Average Treatment Effect at 1 Year
   averageTreatmentEffect <- mean(probTreated) - mean(probControl)
   
   data.table(
      ProbTreated= mean(probTreated),
      ProbControl= mean(probControl),
      AverageTreatmentEffect=averageTreatmentEffect
   )
}

ppReoffendingPoolAdjustedTreatmentEffectsAtTimeOffset <- function(
      cohortGroup,
      models,
      timeOffsetDays,
      treatmentVariable="WasDivertedOrOC22TRUE"
      ) {
   results <- rbindlist(lapply(1:20, function(imputationIndex) {
      # get the current model data
      modelData <- pfda_imputed[[imputationIndex]]
      modelData <- modelData[CohortGroup==cohortGroup]
      
      # merge in the historical offence data and reoffending data from the PNC
      modelData <- merge(modelData,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
      modelData <- merge(modelData,reoffending,by="RandomGroupedIncidentID",all.x=T)
      
      # get the current model
      model <- models[[imputationIndex]]
      
      adjustedTreatment <- ppReoffendingGetAdjustedTreatmentEffectAtTimeOffset(model,modelData,timeOffsetDays,treatmentVariable)
   }))
   results
}

ppG1AdjustedTreatmentEffectReoffending365 <- ppReoffendingPoolAdjustedTreatmentEffectsAtTimeOffset(cohortGroup=1,ppG1ReoffendingModels,365,"WasDivertedOrOC22TRUE")
ppG2AdjustedTreatmentEffectReoffending365 <- ppReoffendingPoolAdjustedTreatmentEffectsAtTimeOffset(cohortGroup=2,ppG2ReoffendingModels,365,"WasDivertedOrOC22TRUE")
ppG1AdjustedTreatmentEffectReoffending30 <- ppReoffendingPoolAdjustedTreatmentEffectsAtTimeOffset(cohortGroup=1,ppG1ReoffendingModels,30,"WasDivertedOrOC22TRUE")
ppG2AdjustedTreatmentEffectReoffending30 <- ppReoffendingPoolAdjustedTreatmentEffectsAtTimeOffset(cohortGroup=2,ppG2ReoffendingModels,30,"WasDivertedOrOC22TRUE")

ppReoffendingSummarisePooledTreatmentEffects <- function(pooledResultsDT) {
   # 1. Calculate the Pooled Mean (the average across all 20 imputations)
   pooledProbTreated <- mean(pooledResultsDT$ProbTreated)
   pooledProbControl <- mean(pooledResultsDT$ProbControl)
   pooledATE         <- mean(pooledResultsDT$AverageTreatmentEffect)
   
   # 2. Calculate the Standard Deviation across imputations (between-imputation variance)
   sdATE <- sd(pooledResultsDT$AverageTreatmentEffect)
   
   # 3. Calculate 95% Confidence Intervals using the variation between models
   # Note: This represents the stability of the result across the imputed datasets
   critValue <- qnorm(0.975)
   ciLower <- pooledATE - (critValue * sdATE)
   ciUpper <- pooledATE + (critValue * sdATE)
   
   # 4. Create the final summary table
   summaryTable <- data.table(
      Metric = c("ProbTreated (Pooled)", "ProbControl (Pooled)", "AverageTreatmentEffect (Pooled)"),
      Estimate = c(pooledProbTreated, pooledProbControl, pooledATE),
      LowerCI = c(NA, NA, ciLower),
      UpperCI = c(NA, NA, ciUpper),
      StabilitySD = c(NA, NA, sdATE)
   )
   
   return(summaryTable)
}

# Apply to your results
ppG1ReoffendingTreatmentFinalSummary365 <- ppReoffendingSummarisePooledTreatmentEffects(ppG1AdjustedTreatmentEffectReoffending365)
ppG2ReoffendingTreatmentFinalSummary365 <- ppReoffendingSummarisePooledTreatmentEffects(ppG2AdjustedTreatmentEffectReoffending365)
ppG1ReoffendingTreatmentFinalSummary30 <- ppReoffendingSummarisePooledTreatmentEffects(ppG1AdjustedTreatmentEffectReoffending30)
ppG2ReoffendingTreatmentFinalSummary30 <- ppReoffendingSummarisePooledTreatmentEffects(ppG2AdjustedTreatmentEffectReoffending30)

# Print results
print("--- G1 Pooled Results 365 ---")
print(ppG1ReoffendingTreatmentFinalSummary365)
print("--- G2 Pooled Results 365 ---")
print(ppG2ReoffendingTreatmentFinalSummary365)
print("--- G1 Pooled Results 30 ---")
print(ppG1ReoffendingTreatmentFinalSummary30)
print("--- G2 Pooled Results 30 ---")
print(ppG2ReoffendingTreatmentFinalSummary30)

ppReoffendingSummariseWithRubinsRules <- function(
      cohortGroup,
      modelsList,
      timeOffsetDays,
      treatmentVar="WasDivertedOrOC22TRUE",
      M=20,
      h=1e-4
) {
   
   results_list <- lapply(seq_len(M), function(i) {
      
      model <- modelsList[[i]]
      
      # Recreate modelData exactly as before
      modelData <- pfda_imputed[[i]]
      modelData <- modelData[CohortGroup==cohortGroup]
      modelData <- merge(modelData,historicalOffences,
                         by="RandomGroupedIncidentID",all.x=TRUE)
      modelData <- merge(modelData,reoffending,
                         by="RandomGroupedIncidentID",all.x=TRUE)
      
      # Extract beta and SE(beta)
      coef_table <- summary(model)$coefficients
      idx <- which(rownames(coef_table) == treatmentVar)
      
      if(length(idx)==0)
         stop("Treatment variable not found in coefficient table")
      
      beta <- coef_table[idx,"coef"]
      se_beta <- coef_table[idx,"se(coef)"]
      
      # --- Evaluate function at beta, beta+h, beta-h ---
      
      f0 <- ppReoffendingGetAdjustedTreatmentEffectAtTimeOffset(
         model, modelData, timeOffsetDays,
         treatmentVariable=treatmentVar,
         betaOverride=beta
      )
      
      f_plus <- ppReoffendingGetAdjustedTreatmentEffectAtTimeOffset(
         model, modelData, timeOffsetDays,
         treatmentVariable=treatmentVar,
         betaOverride=beta + h
      )
      
      f_minus <- ppReoffendingGetAdjustedTreatmentEffectAtTimeOffset(
         model, modelData, timeOffsetDays,
         treatmentVariable=treatmentVar,
         betaOverride=beta - h
      )
      
      # Numerical derivatives
      deriv_treated <- (f_plus$ProbTreated  - f_minus$ProbTreated)  / (2*h)
      deriv_control <- (f_plus$ProbControl  - f_minus$ProbControl)  / (2*h)
      deriv_ate     <- (f_plus$AverageTreatmentEffect -
                           f_minus$AverageTreatmentEffect) / (2*h)
      
      # Within-imputation variances (delta)
      U_treated <- (deriv_treated^2) * se_beta^2
      U_control <- (deriv_control^2) * se_beta^2
      U_ate     <- (deriv_ate^2)     * se_beta^2
      
      data.table(
         ProbTreated = f0$ProbTreated,
         ProbControl = f0$ProbControl,
         ATE         = f0$AverageTreatmentEffect,
         U_treated   = U_treated,
         U_control   = U_control,
         U_ate       = U_ate
      )
   })
   
   dt <- rbindlist(results_list)
   
   # --- Rubin pooling ---
   
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
         RIV      = ifelse(W>0, (1 + 1/M)*B / W, NA_real_)
      )
   }
   
   treated_summary <- pool_one(dt$ProbTreated, dt$U_treated)
   control_summary <- pool_one(dt$ProbControl, dt$U_control)
   ate_summary     <- pool_one(dt$ATE,         dt$U_ate)
   
   rbindlist(list(
      cbind(Metric="ProbTreated", treated_summary),
      cbind(Metric="ProbControl", control_summary),
      cbind(Metric="ATE",         ate_summary)
   ))
}


finalG130Reoffending <- ppReoffendingSummariseWithRubinsRules(
   cohortGroup = 1,
   modelsList=ppG1ReoffendingModels,
   timeOffsetDays = 30
)
finalG130Reoffending[,Cohort:=1]
finalG230Reoffending <- ppReoffendingSummariseWithRubinsRules(
   cohortGroup = 2,
   modelsList=ppG2ReoffendingModels,
   timeOffsetDays = 30
)
finalG230Reoffending[,Cohort:=2]
final30Reoffending <- rbind(finalG130Reoffending,finalG230Reoffending)
final30Reoffending[,Outcome:="Reoffending"]
setcolorder(final30Reoffending,c("Outcome","Cohort"))

finalG1365Reoffending <- ppReoffendingSummariseWithRubinsRules(
   cohortGroup = 1,
   modelsList=ppG1ReoffendingModels,
   timeOffsetDays = 365
)
finalG1365Reoffending[,Cohort:=1]
finalG2365Reoffending <- ppReoffendingSummariseWithRubinsRules(
   cohortGroup = 2,
   modelsList=ppG2ReoffendingModels,
   timeOffsetDays = 365
)
finalG2365Reoffending[,Cohort:=2]
final365Reoffending <- rbind(finalG1365Reoffending,finalG2365Reoffending)
final365Reoffending[,Outcome:="Reoffending"]
setcolorder(final365Reoffending,c("Outcome","Cohort"))

testCalcs <- F
if(testCalcs) {
   coxPHTestFormula <- "
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
      frailty(ForceName,distribution='gaussian')
   "
   coxPHTestFormula <- as.formula(gsub("\n"," ",coxPHTestFormula))
   testModelG1 <- ppG1ReoffendingModels[[1]]
   testDataG1 <- pfda_imputed[[1]]
   testDataG1 <- testDataG1[CohortGroup==1]
   testDataG1 <- merge(testDataG1,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
   testDataG1 <- merge(testDataG1,reoffending,by="RandomGroupedIncidentID",all.x=T)
   testDataG1 <- droplevels(testDataG1)
   testFormula <- as.formula(gsub("\n"," ",ppCoxFormulaReoffending))
   testRemodelG1 <- coxme(testFormula,data=testDataG1)
   
   testDataG2 <- pfda_imputed[[1]]
   testDataG2 <- testDataG2[CohortGroup==2]
   testDataG2 <- merge(testDataG2,historicalOffences,by="RandomGroupedIncidentID",all.x=T)
   testDataG2 <- merge(testDataG2,reoffending,by="RandomGroupedIncidentID",all.x=T)
   testDataG2 <- droplevels(testDataG2)
   testRemodelG2 <- coxme(testFormula,data=testDataG2,ties="breslow")
   
   testCoxPHModel <- coxph(coxPHTestFormula,testData)
   bh_table <- basehaz(testCoxPHModel,centered = F)
   h0_coxph_365 <- bh_table$hazard[max(which(bh_table$time <= 365))]
}