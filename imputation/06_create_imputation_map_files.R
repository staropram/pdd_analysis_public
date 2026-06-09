source("../common/r/file_paths.R")

print("Loading ethnicity imputations")
bayesG1EthFN <- paste0(analysis_path,"/BayesEthnicityImputation_G1.rds")
bayesG2EthFN <- paste0(analysis_path,"/BayesEthnicityImputation_G2.rds")
dEthG1 <- readRDS(bayesG1EthFN)
dEthG2 <- readRDS(bayesG2EthFN)
# this isn't strictly necessary as groupedIncidentID is row-unique
# but its useful for sanity checking
dEthG1$CohortGroup <- 1
dEthG2$CohortGroup <- 2

# load in the home office outcomes predicted for each of the above ethnicity imputations
print("Loading the outcome code imputations")
combinedHOImputationsG1 <- paste0(analysis_path,"/BayesHOOutcomeImputation_G1_Combined.rds")
combinedHOImputationsG2 <- paste0(analysis_path,"/BayesHOOutcomeImputation_G2_Combined.rds")
dHOG1 <- readRDS(combinedHOImputationsG1)
dHOG2 <- readRDS(combinedHOImputationsG2)
dHOG1$CohortGroup <- 1
dHOG2$CohortGroup <- 2

print("Binding grouped mappings into single files")
dHO <- rbindlist(list(dHOG1,dHOG2))
dEth <- rbindlist(list(dEthG1,dEthG2))

print("Save single imputation files")
write_feather(dHO,bayes_ho_outcome_imputation_fn)
write_feather(dEth,bayes_ethnicity_imputation_fn)