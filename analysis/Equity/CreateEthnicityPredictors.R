# this file creates predictors for ethnicity based on name likelihood
# load the original PFD data

source("../../common/r/file_paths.R")
print("Creating predictors for ethnicity based on name")
forceReload <- T
if(!exists("pfd")|forceReload) {
   pfd <- data.table(arrow::read_feather(PFD_complete_fn_FEATHER))
}

# for each FirstName, determine the most likely ethnicity
firstNameVSEthnicity <- pfd[,.N,by=.(FirstName,EthnicityCoarse)]
firstNameEthnicityMap <- firstNameVSEthnicity[,.SD[which.max(N)],by=FirstName]
setnames(firstNameEthnicityMap,"EthnicityCoarse","FirstNameEthnicity")

# and for SurName
lastNameVSEthnicity <- pfd[,.N,by=.(LastName,EthnicityCoarse)]
lastNameEthnicityMap <- lastNameVSEthnicity[,.SD[which.max(N)],by=LastName]
setnames(lastNameEthnicityMap,"EthnicityCoarse","LastNameEthnicity")

# lets put these into pfda
pfd <- merge(pfd,firstNameEthnicityMap[,.(FirstName,FirstNameEthnicity)],by="FirstName",all.x=T)
pfd <- merge(pfd,lastNameEthnicityMap[,.(LastName,LastNameEthnicity)],by="LastName",all.x=T)

# create a combined variable taking first name eth as priority and then lastname if missing
pfd[,NameEthnicity:=fifelse(is.na(FirstNameEthnicity),LastNameEthnicity,FirstNameEthnicity)]

# merge these back into pfda
pfda <- merge(pfda,unique(pfd[,.(PseudoID,NameEthnicity,FirstNameEthnicity,LastNameEthnicity)]),by="PseudoID",all.x=T)

# make sure these predictor variables tell us something
pfda[is.na(LastNameEthnicity),LastNameEthnicity:=FirstNameEthnicity]
pfda[is.na(FirstNameEthnicity),FirstNameEthnicity:=LastNameEthnicity]
pfda[is.na(LastNameEthnicity),LastNameEthnicity:="UNKNOWN"]
pfda[is.na(FirstNameEthnicity),FirstNameEthnicity:="UNKNOWN"]