# this file creates predictors for ethnicity based on name likelihood
# load the original PFD data

source("../common/r/file_paths.R")


createEthnicityPredictors <- function(d,forceReloadPoliceData=F,mergeColumns=F) {
   print("Creating predictors for ethnicity based on name")
   if(!exists("pfd")|forceReloadPoliceData==T) {
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
   
   # lets put these into d
   pfd <- merge(pfd,firstNameEthnicityMap[,.(FirstName,FirstNameEthnicity)],by="FirstName",all.x=T)
   pfd <- merge(pfd,lastNameEthnicityMap[,.(LastName,LastNameEthnicity)],by="LastName",all.x=T)
   
   # create a combined variable taking first name eth as priority and then lastname if missing
   pfd[,NameEthnicity:=fifelse(is.na(FirstNameEthnicity),LastNameEthnicity,FirstNameEthnicity)]
   
   # merge these back into d
   d <- merge(d,unique(pfd[,.(PseudoID,NameEthnicity,FirstNameEthnicity,LastNameEthnicity)]),by="PseudoID",all.x=T)
   
   # make sure these predictor variables tell us something
   if(mergeColumns) {
      d[is.na(LastNameEthnicity),LastNameEthnicity:=FirstNameEthnicity]
      d[is.na(FirstNameEthnicity),FirstNameEthnicity:=LastNameEthnicity]
   }
   d[is.na(LastNameEthnicity),LastNameEthnicity:="UNIQUE_NAME"]
   d[is.na(FirstNameEthnicity),FirstNameEthnicity:="UNIQUE_NAME"]
   
   d
}