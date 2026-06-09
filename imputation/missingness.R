# the purpose of this script is to understand how missing our data is

source("../common/r/file_paths.R")
# flag to control whether pfd data should be reloaded with every execution of the script
forceReload <- F

# read the pfd data in
if(!exists("pfda")|forceReload) {
   print("Reading in police force data analysis data (pfda)")
   pfda <- data.table(arrow::read_feather(PFD_analysis_full))
}


missingness <- rbindlist(lapply(names(pfda),function(colName) {
   m <- data.table(
      Name=colName,
      MissingCount=pfda[is.na(get(colName)),.N]
   )
   if(m$MissingCount==0) {
      return(NULL)
   }
   m[,MissingPercent:=round(MissingCount/nrow(pfda)*100,digits=2)]
   m
}))

columnsWeWant <- c(
   "Sex",
   "EthnicityCoarse",
   "HOOutcomeCodeLong",
   "WasArrested",
   "WasCriminalised",
   "WasDivertedOrOC22"
)

missingness2 <- missingness[Name %in% columnsWeWant,]
