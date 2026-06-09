# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Hampshire
Hampshire <- data.table(read_excel(HampshirePath,sheet=1,skip=4))

# Rename columns to be consistent with other forces
setnames(Hampshire,"Participant First Name","FirstName")
setnames(Hampshire,"Participant Last Name","LastName")
setnames(Hampshire,"Date of Birth","DOB")
setnames(Hampshire,"Officer Defined Ethnicity","EthnicityAsSuppliedIC")
setnames(Hampshire,"Self-Defined Ethnicity","EthnicityAsSuppliedSDE")
setnames(Hampshire,"PNC Number","PNCNumber")
setnames(Hampshire,"Date of Police Contact","ContactDate")
setnames(Hampshire,"Offence Description","ContactReason")
setnames(Hampshire,"Home Office Outcome Type","OutcomeType")

# Hampshire uses the string "NOT STATED" for an empty PNCNumber
Hampshire[PNCNumber=="NOT STATED",PNCNumber:=NA]

# Create the drug type column
#Hampshire[,DrugType:=offenceToDrugType(ContactReason)]

# Create the column for diversion: this is a control force, nobody was diverted
Hampshire$WasDiverted <- F

# Create the column for arrest status
Hampshire[,WasArrested:=fifelse(`Was the participant Arrested` %like% "Y",T,F)]

# Create the Sex column, note this implicity sets
# "NOT STATED" and "UNKNOWN" to NA
Hampshire[`Sex/Gender` %like% "M",Sex:="Male"]
Hampshire[`Sex/Gender` %like% "F",Sex:="Female"][]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
Hampshire[,DOB:=as.Date(DOB)]
Hampshire[,ContactDate:=as.Date(ContactDate)]

# Add in the force name for later merging
Hampshire$ForceName <- "Hampshire"

# Obtain diversion force status from master info
#Hampshire$DiversionForce <- policeForceInfo[ForceName=="Hampshire",DiversionForce]

# Set order of columns
setcolorder(Hampshire,partCleanColumns)

# keep only these columns
Hampshire <- Hampshire[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("Hampshire")