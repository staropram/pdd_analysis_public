source("../../common/r/file_paths.R")

# load the analysis data in
print("Loading in analysis data")
if(!exists("data_loaded")) {
   currentWD <- getwd()
   setwd("../../import_scripts/")
   source("00_process_all.R")
   setwd(currentWD)
   data_loaded <- T
}

# This gives us:

# pfda - The original police force analysis data, joined to NDTMS with added fields,
# - ReoffendingCensorDate - specific censor date computed for reoffending

# pfda_imputed - 20 imputed versions of the above with correctly constructed
# ethnicity, outcome prediction etc

# reoffending - data about reoffending per indicent
#  - DidReoffend - did this incident result in reoffending
#  - ExposureTimeDays - the exposure time for the model for "DidReoffend" to be relevant
#  - WasImprisoned - were they imprisoned
#  - CensorReason - the reason their exposure time ends when it does

# historicalOffences
# - HistoricalOffenceCount - All cause count of offences in the last 5 years
# - HistoricalPossessionCount - Count of possession offences in last 5 years
# - HistoricalOtherDrugsCount - Count of other drugs offences in last 5 years
# - HistoricalTheftCount - Count of theft offences in last 5 years
# - HistoricalViolenceCount - Count of violent offences in last 5 years
