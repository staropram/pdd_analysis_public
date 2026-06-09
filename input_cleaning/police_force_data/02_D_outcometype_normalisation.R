# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# load in the clean data
forceReload <- F
if(!exists("pfd")|forceReload) {
   pfd <- data.table(arrow::read_feather(PFD_complete_fn_FEATHER))
}

pfd[,OutcomeType:=tolower(OutcomeType)]

# obtain the unique outcome type strings
uniqueOutcomeTypes <- unique(tolower(pfd$OutcomeType))
uniqueOutcomeTypes <- uniqueOutcomeTypes[!is.na(uniqueOutcomeTypes)]

# load in the HO outcome descriptions
hoOutcomes <- fread("../../common/data/internal/HomeOfficeOutcomes.tsv")

# load in map between police codes and HO codes
outcomeTypeMap <- fread("../../common/data/internal/OutcomeTextToHOOutcome.tsv",na.strings="")

# join the outcome descriptions to this
outcomeTypeMap <- merge(outcomeTypeMap,hoOutcomes,by="HOOutcomeCodeLong",all.x=T)

# see if any HOCodes are missing
missingHOCodes <- which(outcomeTypeMap$HOOutcomeCodeLong=="Unknown")
if(length(missingHOCodes>0)) {
   print(outcomeTypeMap[missingHOCodes,"OutcomeType"])
   warning("HOCodes are missing")
}

# see if any of the outcome types in the pfd data are not mapped
notYetMapped <- uniqueOutcomeTypes[!uniqueOutcomeTypes %in% outcomeTypeMap$OutcomeType]
if(length(notYetMapped)!=0) {
   print("The following outcome types have not been mapped:")
   print(notYetMapped)
   warning("HOCodes are missing")
}

