source("../../common/r/file_paths.R")

# This script performs a preliminarly cleaning of each force derived data file
# What this means is that:
# 1. All column names are normalised
# 2. DrugType field is created and normalised
# 3. Each force data is then saved to "clean" directory

# Some cleaning actions make more sense to perform when all part-cleaned data
# is aggregated because the cleaning process needs to see all possible values
# of say the "Ethnicity" column in order to ensure nothing is missed

# Norfolk and Suffolk need to be separated out as they 
# were for some reason delivered as a single file

norfolkAndSuffolk <-  data.table(read_excel(fileNames$NorfolkAndSuffolkDirtyIn,sheet=1))
# first 3 rows were example rows
norfolkAndSuffolk <- norfolkAndSuffolk[-c(1:3),]
# the prefix of the LocalID determines force, 36 for Norfolk and 37 for Suffolk
norfolkAndSuffolk[,LocalIDPrefix:=substr(LocalID,1,2)]
norfolk <- norfolkAndSuffolk[LocalIDPrefix=="36"]
suffolk <- norfolkAndSuffolk[LocalIDPrefix=="37"]
# output the split data
write_xlsx(norfolk,fileNames$NorfolkDirtyOut)
write_xlsx(suffolk,fileNames$SuffolkDirtyOut)

# WestYorks needs to be separated out into Wakefield and the rest of WestYorks
westYorksAll <- data.table(read_excel(fileNames$WestYorksDirtyIn,sheet=2))
# there are some individuals supposedly not in Wakefield but they
# have been offered diversion so it makes sense to include these in the diversion
# scheme
westYorksExWakefield <- westYorksAll[pilot_eligible=="N"&referral=="N"]
wakeField <- westYorksAll[pilot_eligible=="Y"|referral=="Y"]
write_xlsx(westYorksExWakefield,fileNames$WestYorksExWakefieldDirtyOut)
write_xlsx(wakeField,fileNames$WakefieldDirtyOut)

# some useful functions

partCleanColumns <- c(
   "ForceName",
   "FirstName",
   "LastName",
   "DOB",
   "Sex",
   "EthnicityAsSuppliedIC",
   "EthnicityAsSuppliedSDE",
   "PNCNumber",
   "ContactDate",
   "ContactReason",
   "WasDiverted",
   "WasArrested",
   "OutcomeType"
)

savePartCleanForceFile <- function(force) {
   data <- get(force)
   write.table(data,file=forceDataPaths[[force]]$PartClean$outTSV,row.names=F,sep="\t")
}

saveCleanForceFile <- function(force) {
   data <- get(force)
   write.table(data,file=forceDataPaths[[force]]$outTSV,row.names=F,sep="\t")
   write_xlsx(data,forceDataPaths[[force]]$outXLSX)
}

# source all of the scripts
for(f in forcesWithData) {
   print(paste0("Sourcing ",f))
   source(paste0("force_scripts/",f,".R"))
}
  
# put the below in "finalise_cleaning" or something
# need to "derive" the following
#Age
#BaselineReoffendingRate