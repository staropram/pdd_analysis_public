# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

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


library(DT)
library(lubridate)

load_part_clean_data <- function(minimalColumns=F) {
  pfd <- lapply(forcesWithData,function(forceName) {
     print(paste0("Loading ",forceName))
     fn <- forceDataPaths[[forceName]]$PartClean$outTSV
     d <- fread(fn,sep="\t")
     if(minimalColumns) {
        d <- d[,..partCleanColumns]
     }
     d
  })
  names(pfd) <- forcesWithData
  pfd
}

load_clean_data <- function(minimalColumns=F) {
  pfd <- lapply(forcesWithData,function(forceName) {
     print(paste0("Loading ",forceName))
     fn <- forceDataPaths[[forceName]]$Clean$outTSV
     d <- fread(fn,sep="\t")
     if(minimalColumns) {
        d <- d[,..correctColumnOrder]
     }
     d
  })
  names(pfd) <- forcesWithData
  pfd
}

forceLoad <- T
print("Loading police force data")
if(!exists("pfdList")|forceLoad) {
   pfdList <- load_part_clean_data(minimalColumns=T)
}
print("Merging police force data")
if(!exists("pfd")|forceLoad) {
  pfd <- rbindlist(pfdList,use.names=T)
  # make sure first and last names are all in caps
  pfd[,FirstName:=toupper(FirstName)]
  pfd[,LastName:=toupper(LastName)]
}

# create PseudoID
#pseudoIDs <- unlist(lapply(pfd[,paste0(PNCNumber,FirstName,LastName,DOB,Sex,ForceName)],function(x) {
#   digest(x,algo="spookyhash")
#}))
pseudoIDs <- sapply(pfd[,paste0(PNCNumber,FirstName,LastName,DOB,Sex,ForceName)],digest,algo="spookyhash")
pfd[,PseudoID:=paste0("PFD_",pseudoIDs)]

# normalise ethnicity, note this has to be sourced here since it uses
# the loaded data
print("Normalising ethnicities")
# this creates the map
source("02_A_ethnicity_normalisation.R")
# filter out partially missing ethnicities
source("02_A2_ethnicity_partially_missing.R")

# remove debug variables
pfd[,EthCorrectedReason:=NULL]

# try and fill in missing ethnicities

#source("02_A2_ethnicity")


print("Normalising offences")
source("02_B_offence_normalisation.R")
pfd <- merge(pfd,offenceMap,by="ContactReason",all.x=T)
# any offence not mapped, map as unknown
pfd[is.na(OffenceGroup),`:=` (
   OffenceType="Not known",
   OffenceGroup="Not known",
   Offence="Not known",
   OffenceCode="07602",
   OffenceDetailed="07602 - Not known"
)]
browser()
# remove offences that shouldn't be in there
forceReload <- T
if(!exists("ValidOffences")|forceReload) {
   validOffences <- fread("../../common/data/internal/ValidOffences.tsv",
                          colClasses="character")
}


# do a join on this
pfd <- merge(pfd,validOffences[,c("OffenceCode","OurOffenceGroup","ValidOffence")],by="OffenceCode",all.x=T)
offencePrevalence <- createOffencePrevalanceTable()
setnames(pfd,"OurOffenceGroup","CohortGroup")
# remove the invalid offences
pfd <- pfd[ValidOffence=="Y"]

# (create) and normalise drugtype column (from contact reason)
print("Normalising drugs")
source("02_C_drugtype_normalisation.R")
pfd <- merge(pfd,drugMap[,c("OffenceDetailed","DrugType","DrugClass")],by="OffenceDetailed",all.x=T)


# create and normalise outcome type column
print("Normalising outcome types")
source("02_D_outcometype_normalisation.R")

pfd <- merge(pfd,outcomeTypeMap,by="OutcomeType",all.x=T)

# create some derived variables like age etc
# calculate ages
pfd[,AgeAtContactInDays:=time_length(interval(DOB,ContactDate),unit="days")]
pfd[,AgeAtContactInYears:=time_length(interval(DOB,ContactDate),unit="years")][]
pfd[,AgeAtLastBirthday:=floor(AgeAtContactInYears)]
# remove individuals under 18
pfd <- pfd[AgeAtContactInYears>=18,]

# remove duplicate rows
pfd <- unique(pfd)

# add an identifier column
# we will sort by a specific and arbitrary ordering of pf
# use the order as layed out in the pf table
pfd[,ForceNameFactor:=factor(ForceName,levels=forcesWithData)]
setorder(pfd,ForceNameFactor,ContactDate)


# deal with same day offences and multi-crime offences
# this modifies pfd
print("Normalising multi-crime offences")
#browser()
# make an incident ID which is the PseudoID + ContactDate
pfd[,IncidentID:=paste0(PseudoID,'_',ContactDate)]
# make an ID which also separates incidents by group
pfd[,GroupedIncidentID:=paste0(IncidentID,'_',CohortGroup)]
#browser()
source("02_E_handle_multi_crime_offences.R")
pfd[is.na(MultiOffenceIncident),MultiOffenceIncident:=F]

# remove dates that shouldn't be in there (this also removes those with NA dates)
startWindow <- as.Date("01/10/2021",format="%d/%m/%Y")
endWindow <- as.Date("30/09/2022",format="%d/%m/%Y")
#x<- pfd[,list(Before=.N),by=ForceName]
pfd <- pfd[ContactDate >= startWindow & ContactDate <= endWindow]
#y<- pfd[,list(After=.N),by=ForceName]
#fDiff <- x[y,on="ForceName"]

# create censorship window for each individual
# this modifies pfd
print("Creating censorship windows")
source("02_F_create_censorship_windows.R")

# Rename some columns to make it clear
setnames(pfd,"ContactReason","ContactReasonAsSupplied")

setnames(pfd,"Offence","HOOffence")
setnames(pfd,"OffenceCode","HOOffenceCode")
setnames(pfd,"OffenceGroup","HOOffenceGroup")
setnames(pfd,"OffenceType","HOOffenceType")
setnames(pfd,"OffenceDetailed","HOOffenceDetailed")

setnames(pfd,"OutcomeType","OutcomeTypeAsSupplied")

# remove unnecessary columns that were used along
# the way and are no longer required
unnecessaryColumns <- c("ForceNameFactor","ValidOffence","OffenceReferralProbability","OffenceProbability","Index","SameDayPath","ToBeRemoved","IsSameDayOffence","IsMultiOffence")
pfd <- pfd[,.SD,.SDcols = !unnecessaryColumns]

# compute FollowupDuration for NDTMS and PNC

# merge in force-level variables
source("02_G_merge_force_level_variables.R")

# add some derived variables
source("02_H_add_derived_variables.R")

# order the columns in a sensible manner
variableDescriptions <- read_excel("../../common/data/internal/VariableDescriptions.xlsx",sheet=1)
setcolorder(pfd,variableDescriptions$Variable)

# save the combined, clean data to a single flat file
print("Writing merged police force data")
#print("Writing FST")
#fst::write_fst(pfd,policeforce_data_complete_fn_FST)
print("Writing Feather")
arrow::write_feather(pfd,PFD_complete_fn_FEATHER)