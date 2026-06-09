# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Merseyside
Merseyside <- data.table(read_excel(MerseysidePath,sheet=1))

# Rename columns for consistency
setnames(Merseyside,"Surname","LastName")
setnames(Merseyside,"DateOfBirth","DOB")
setnames(Merseyside,"RlvDate","ContactDate")
setnames(Merseyside,"Offence","ContactReason")
setnames(Merseyside,"Home Office Outcome of the Offence","OutcomeType")

# Merseyside uses self defined ethnicity
setnames(Merseyside,"Ethnicity","EthnicityAsSuppliedSDE")
Merseyside[,EthnicityAsSuppliedIC:=NA]

# change Sex M/F to Male/Female
# I and U become NA
Merseyside[Sex %like% "M",Sex:="Male"]
Merseyside[Sex %like% "F",Sex:="Female"]
Merseyside[Sex %like% "I",Sex:=NA]
Merseyside[Sex %like% "U",Sex:=NA]

# Create diverted column
Merseyside[,WasDiverted:=fifelse(`Referred to diversion scheme` %like% "Yes",T,F)]

# Create arrested status column
Merseyside[,WasArrested:=fifelse(Arrested %like% "Yes",T,F)]

# For the drugs column, Merseyside has provided multiple drugs
# we can start just by deriving from the offence field
#Merseyside[,DrugType:=offenceToDrugType(ContactReason)]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
Merseyside[,DOB:=as.Date(DOB)]
Merseyside[,ContactDate:=as.Date(ContactDate)]

# Add in the force name for later merging
Merseyside$ForceName <- "Merseyside"

# Obtain diversion force status from master info
#Merseyside$DiversionForce <- policeForceInfo[ForceName=="Merseyside",DiversionForce]

# Set order of columns
setcolorder(Merseyside,partCleanColumns)

# keep only these columns
Merseyside <- Merseyside[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("Merseyside")