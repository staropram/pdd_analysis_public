source("EquityFormula.R")
library(sjPlot)
library(performance)
pfda1 <- pfda[CohortGroup==1]

pfda1CCDiversion <- pfda1[
   !is.na(Sex) & # sex is present
   !is.na(EthnicityCoarse) & # ethnicity is present AND
   (
      !is.na(HOOutcomeCodeLong) | # ho outcome is present
      WasDivertedOrOC22==T # OR they were diverted (police flag)
   )
]

# diversion
print("Creating ethnicity only diversion model")
diversionModelEthnicityOnly <- glmmTMB(
   diversionFormulaEthnicityOnly,
   pfda1CCDiversion,
   family=binomial
)

print("Creating controlled diversion model without random force intercept")
diversionModelFullNoRandomPlusOffending <- glmmTMB(
   diversionFormulaFullNoRandomPlusOffending,
   pfda1CCDiversion,
   family=binomial
)

print("Creating controlled diversion model with random force intercept")
diversionModelFullPlusOffending <- glmmTMB(
   diversionFormulaFullPlusOffending,
   pfda1CCDiversion,
   family=binomial
)

# person level clustering
diversionModelFullNoRandomPlusOffendingPlusPersonClustering <- glmmTMB(
   diversionFormulaFullNoRandomPlusOffendingPlusPersonClustering,
   pfda1CCDiversion,
   family=binomial
)
tab_model(diversionModelFullNoRandomPlusOffendingPlusPersonClustering,file="doc/DiversionModel_Full_NO_IMPUTATION_no_random_history_and_person_cluster.doc")
diversionModelFullPlusOffendingPlusPersonClustering <- glmmTMB(
   diversionFormulaFullPlusOffendingPlusPersonClustering,
   pfda1CCDiversion,
   family=binomial
)
tab_model(diversionModelFullPlusOffendingPlusPersonClustering,file="doc/DiversionModel_Full_NO_IMPUTATION_history_and_person_cluster.doc")

pfda1CCCrim <- pfda1[
   !is.na(Sex) & # sex is present
   !is.na(EthnicityCoarse) & # ethnicity is present AND
   (
      !is.na(HOOutcomeCodeLong) | # ho outcome is present
      WasDivertedOrOC22==T # OR they were diverted (police flag)
   )
]

# crim
print("Creating ethnicity only criminalisation model")
criminalisationModelEthnicityOnly <- glmmTMB(
   criminalisationFormulaEthnicityOnly,
   pfda1CCCrim,
   family=binomial
)

print("Creating controlled criminalisation model without random force intercept")
criminalisationModelFullNoRandomPlusOffending <- glmmTMB(
   criminalisationFormulaFullNoRandomPlusOffending,
   pfda1CCCrim,
   family=binomial
)
print("Creating controlled criminalisation model with random force intercept")
criminalisationModelFullPlusOffending <- glmmTMB(
   criminalisationFormulaFullPlusOffending,
   pfda1CCCrim,
   family=binomial
)

# person level clustering
criminalisationModelFullNoRandomPlusOffendingPlusPersonClustering <- glmmTMB(
   criminalisationFormulaFullNoRandomPlusOffendingPlusPersonClustering,
   pfda1CCCrim,
   family=binomial
)
criminalisationModelFullPlusOffendingPlusPersonClustering <- glmmTMB(
   criminalisationFormulaFullPlusOffendingPlusPersonClustering,
   pfda1CCCrim,
   family=binomial
)
tab_model(criminalisationModelFullNoRandomPlusOffendingPlusPersonClustering,file="doc/CrimModel_Full_NO_IMPUTATION_no_random_history_and_person_cluster.doc")
tab_model(criminalisationModelFullPlusOffendingPlusPersonClustering,file="doc/CrimModel_Full_NO_IMPUTATION_history_and_person_cluster.doc")

library(broom.mixed)
library(dplyr)

# 1. Define a helper function to format the OR (95% CI)
get_stats <- function(model) {
   tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
      filter(effect == "fixed" & term != "(Intercept)") %>%
      mutate(
         formatted = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
      ) %>%
      select(term, formatted)
}

# 2. Extract stats for the Diversion models
m1_stats <- get_stats(diversionModelEthnicityOnly)
m2_stats <- get_stats(diversionModelFullNoRandomPlusOffending)
m3_stats <- get_stats(diversionModelFullPlusOffending)

# 3. Join them together
results_table <- m1_stats %>%
   rename(`Model 1 (Unadjusted)` = formatted) %>%
   left_join(m2_stats %>% rename(`Model 2 (Adjusted + Offending)` = formatted), by = "term") %>%
   left_join(m3_stats %>% rename(`Model 3 (Primary + Random Effects)` = formatted), by = "term")

print(results_table)

# 1. Clean up the names and terms for the table
df_for_word <- results_table
df_for_word$term <- c("Asian or Asian British", 
                      "Black, African, Caribbean or Black British", 
                      "Mixed or multiple ethnic groups", 
                      "Other ethnic groups")

# Add the reference row for 'White' at the top
ref_row <- data.frame(
   term = "White",
   `Model 1 (Unadjusted)` = "1 (ref)",
   `Model 2 (Adjusted + Offending)` = "1 (ref)",
   `Model 3 (Primary + Random Effects)` = "1 (ref)",
   check.names = FALSE
)
df_for_word <- rbind(ref_row, df_for_word)

# 2. Create the flextable
ft <- flextable(df_for_word) %>%
   # Set header labels to match your image exactly
   set_header_labels(
      term = "Ethnicity",
      `Model 1 (Unadjusted)` = "Model 1\n(unadjusted)",
      `Model 2 (Adjusted + Offending)` = "Model 2\n(adjusted for age,\nsex, drug, offending)",
      `Model 3 (Primary + Random Effects)` = "Model 3 - primary\nanalysis\n(adjusted for age,\nsex, drug, offending, force)"
   ) %>%
   
   # Styling
   font(fontname = "Arial", part = "all") %>%
   fontsize(size = 10, part = "all") %>%
   bold(part = "header") %>%
   bg(bg = "#D9E1F2", part = "header") %>% # Matches the light blue shade
   align(align = "left", j = 1, part = "all") %>%
   align(align = "left", j = 2:4, part = "all") %>%
   
   # Borders
   border_remove() %>%
   border_outer(border = fp_border_default(color = "black", width = 1)) %>%
   border_inner_h(border = fp_border_default(color = "black", width = 0.5)) %>%
   border_inner_v(border = fp_border_default(color = "black", width = 0.5)) %>%
   
   # Cell padding and width
   padding(padding = 5, part = "all") %>%
   autofit()


ftDiversion <- ft %>%
   # Add specific left padding to create an indent (e.g., 10 points)
   padding(j = 1:4, padding.left = 10, part = "all") %>%
   # You might also want a bit of right padding for symmetry
   padding(j = 1:4, padding.right = 5, part = "all") %>%
   # Keep your existing vertical padding
   padding(padding.top = 5, padding.bottom = 5, part = "all")


# 2. Extract stats for the Diversion models
m1_stats <- get_stats(criminalisationModelEthnicityOnly)
m2_stats <- get_stats(criminalisationModelFullNoRandomPlusOffending)
m3_stats <- get_stats(criminalisationModelFullPlusOffending)

# 3. Join them together
results_table <- m1_stats %>%
   rename(`Model 1 (Unadjusted)` = formatted) %>%
   left_join(m2_stats %>% rename(`Model 2 (Adjusted + Offending)` = formatted), by = "term") %>%
   left_join(m3_stats %>% rename(`Model 3 (Primary + Random Effects)` = formatted), by = "term")

print(results_table)

# 1. Clean up the names and terms for the table
df_for_word <- results_table
df_for_word$term <- c("Asian or Asian British", 
                      "Black, African, Caribbean or Black British", 
                      "Mixed or multiple ethnic groups", 
                      "Other ethnic groups")

# Add the reference row for 'White' at the top
ref_row <- data.frame(
   term = "White",
   `Model 1 (Unadjusted)` = "1 (ref)",
   `Model 2 (Adjusted + Offending)` = "1 (ref)",
   `Model 3 (Primary + Random Effects)` = "1 (ref)",
   check.names = FALSE
)
df_for_word <- rbind(ref_row, df_for_word)

# 2. Create the flextable
ft <- flextable(df_for_word) %>%
   # Set header labels to match your image exactly
   set_header_labels(
      term = "Ethnicity",
      `Model 1 (Unadjusted)` = "Model 1\n(unadjusted)",
      `Model 2 (Adjusted + Offending)` = "Model 2\n(adjusted for age,\nsex, drug, offending)",
      `Model 3 (Primary + Random Effects)` = "Model 3 - primary\nanalysis\n(adjusted for age,\nsex, drug, offending, force)"
   ) %>%
   
   # Styling
   font(fontname = "Arial", part = "all") %>%
   fontsize(size = 10, part = "all") %>%
   bold(part = "header") %>%
   bg(bg = "#D9E1F2", part = "header") %>% # Matches the light blue shade
   align(align = "left", j = 1, part = "all") %>%
   align(align = "left", j = 2:4, part = "all") %>%
   
   # Borders
   border_remove() %>%
   border_outer(border = fp_border_default(color = "black", width = 1)) %>%
   border_inner_h(border = fp_border_default(color = "black", width = 0.5)) %>%
   border_inner_v(border = fp_border_default(color = "black", width = 0.5)) %>%
   
   # Cell padding and width
   padding(padding = 5, part = "all") %>%
   autofit()


ftCrim <- ft %>%
   # Add specific left padding to create an indent (e.g., 10 points)
   padding(j = 1:4, padding.left = 10, part = "all") %>%
   # You might also want a bit of right padding for symmetry
   padding(j = 1:4, padding.right = 5, part = "all") %>%
   # Keep your existing vertical padding
   padding(padding.top = 5, padding.bottom = 5, part = "all")