library(pROC)

# ==========================================
# 1. FOR THE DIVERSION MODEL
# ==========================================

# Step A: Generate predicted probabilities (on the response scale: 0 to 1)
# Note: re.form = NULL ensures random effects (Force & Participant) are included in the prediction
prob_diversion <- predict(
   diversionModelFullPlusOffending, 
   type = "response", 
   re.form = NULL
)

# Step B: Compute the ROC object and extract AUC
# Replace 'your_complete_case_data' with the actual dataframe used for this specific model fit
roc_diversion <- roc(pfda1CCDiversion$WasDivertedOrOC22, prob_diversion)
auc(roc_diversion)


# ==========================================
# 2. FOR THE CRIMINALISATION MODEL
# ==========================================

prob_crim <- predict(
   criminalisationModelFullPlusOffending, 
   type = "response", 
   re.form = NULL
)

# Replace 'Outcome_Column' with your binary criminalisation variable name
roc_crim <- roc(pfda1CCCrim[!is.na(WasCriminalised)]$WasCriminalised, prob_crim)
auc(roc_crim)


# Create the model adequacy table
model_adequacy_dt <- data.table(
   Specification      = c("Diversion Model (Primary)", "Criminalisation Model"),
   Marginal_R2        = c(0.030, 0.185),
   Conditional_R2     = c(0.786, 0.312),
   Conditional_AUC    = c(0.907, 0.7758)
)