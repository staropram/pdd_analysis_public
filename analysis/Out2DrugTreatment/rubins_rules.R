# function to take a data table of effects and SEs 
# and pool the results with Rubin's rules
rubinsPool <- function(d) {
   
   # number of imputations (i.e. models)
   m <- nrow(d)
   
   # pooled point estimate (mean of estimates)
   Q_bar <- mean(d$est)
   
   # mean within-imputation variance (SE squared)
   U_bar <- mean(d$se^2)
   
   # between-imputation variance (across-imputation variance of estimates)
   B <- var(d$est)
   
   # total variance (Rubin 1987)
   # combines within- and between-imputation variance
   T_var <- U_bar + (1 + 1/m) * B
   
   # total standard error
   se_total <- sqrt(T_var)
   
   # degrees of freedom (Rubin 1987)
   # note: if B ~ 0 (identical estimates), df -> Inf
   if (B <= .Machine$double.eps) {
      nu <- Inf
   } else {
      nu <- (m - 1) * (1 + U_bar / ((1 + 1/m) * B))^2
   }
   
   # critical value for 95% CI (t with df=nu or normal if nu=Inf)
   crit <- if (is.finite(nu)) qt(0.975, df = nu) else qnorm(0.975)
   
   # confidence intervals using df as calculated above
   ci_low <- Q_bar - crit * se_total
   ci_high <- Q_bar + crit * se_total
   
   # diagnostics (optional but useful)
   # RIV = relative increase in variance due to missing data
   # FMI = fraction of missing information
   RIV <- if (T_var > 0) ((1 + 1/m) * B) / U_bar else NA_real_
   FMI <- if (is.finite(nu)) (RIV + 2/(nu + 3)) / (RIV + 1) else RIV / (RIV + 1)
   
   # bring this all together
   data.table(
      est_pooled = Q_bar,       # pooled estimate
      se_pooled = se_total,     # pooled standard error
      ci_low = ci_low,          # lower 95% CI bound
      ci_high = ci_high,        # upper 95% CI bound
      df = nu,                  # degrees of freedom
      m = m,                    # number of imputations
      U_bar = U_bar,            # within-imputation variance
      B = B,                    # between-imputation variance
      T_var = T_var,            # total variance
      RIV = RIV,                # relative increase in variance
      FMI = FMI                 # fraction of missing information
   )
}