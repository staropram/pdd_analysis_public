# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Humberside
Humberside <- data.table(read_excel(HumbersidePath,sheet=1))

# normalise column names
setnames(Humberside,"Forename","FirstName")
setnames(Humberside,"Surname","LastName")
setnames(Humberside,"Date Of Birth","DOB")
setnames(Humberside,"Gender","Sex")
setnames(Humberside,"Ethnic Appearance","EthnicityAsSuppliedIC")
setnames(Humberside,"PNC ID","PNCNumber")
setnames(Humberside,"Incident Created Date","ContactDate")
setnames(Humberside,"HO description","ContactReason")
setnames(Humberside,"Detection Type","OutcomeType")

# force name
Humberside$ForceName <- "Humberside"

# force uses IC ethnicity
Humberside[,EthnicityAsSuppliedSDE:=NA]

# Obtain diversion force status from master info
#Humberside$DiversionForce <- policeForceInfo[ForceName=="Humberside",DiversionForce]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
Humberside[,DOB:=as.Date(DOB)]
Humberside[,ContactDate:=as.Date(ContactDate)]

# create DrugType column
#Humberside[,DrugType:=offenceToDrugType(ContactReason)]

# diversion status (it's not a diversion force)
Humberside$WasDiverted <- F

# normalise sex column, Humberside has either Male, Female or "Unknown"
# first two are compatible, "Unknown" should be NA
Humberside[Sex=="Unknown",Sex:=NA]

# arrest status
Humberside[,WasArrested:=fifelse(Arrested=="Yes",T,F)]

# They use 0 instead of NA for empty PNC number
Humberside[PNCNumber==0,PNCNumber:=NA]

# drop columns we are done with
Humberside[,Category:=NULL]
Humberside[,Arrested:=NULL]

# Set order of columns
setcolorder(Humberside,partCleanColumns)

# keep only these columns
Humberside <- Humberside[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("Humberside")