# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# GreaterManchester
GreaterManchester <- data.table(read_excel(GreaterManchesterPath,sheet=1))

# normalise column names
setnames(GreaterManchester,"Crime Creation Date","ContactDate")
setnames(GreaterManchester,"Crime Latest Classification : HO 5 Digit Code","ContactReason")
setnames(GreaterManchester,"Crime Outcome","OutcomeType")
setnames(GreaterManchester,"Name / Date of Birth (offender)","LastNameFirstNameDOB")
setnames(GreaterManchester,"Offender Self Defined Ethnicity","EthnicityAsSuppliedSDE")
setnames(GreaterManchester,"Offender Police Defined Ethnicity","EthnicityAsSuppliedIC")
setnames(GreaterManchester,"Crime Person : Identity Reference Number","PNCNumber")
setnames(GreaterManchester,"Crime Person Citizen : Date Of Birth","DOBAlt")
# note: DOBAlt turns out to be identical to that in the composite field

# create Sex column, `Offender Gender` has values
# Male, Female, "Missing value", and Unknown
# We will make it so everything is NA except Male and Female
GreaterManchester[`Offender Gender` %like% "Male",Sex:="Male"]
GreaterManchester[`Offender Gender` %like% "Female",Sex:="Female"]

# Derive the FirstName, LastName, and DOB from LastNameFirstNameDOB
GreaterManchester[,c("FirstName","LastName","DOB"):= {
  x <- transpose(strsplit(LastNameFirstNameDOB,"- "))
  firstName <- trimws(x[[2]])
  lastName <- trimws(x[[1]])
  dob <- as.Date(x[[3]],format="%Y-%m-%d")
  .(FirstName=firstName,LastName=lastName,DOB=dob)
}]

# both SDE and IC coded ethnicities are provided
GreaterManchester[EthnicityAsSuppliedSDE=="Not Provided",EthnicityAsSuppliedSDE:=NA]
GreaterManchester[EthnicityAsSuppliedSDE=="Not Stated",EthnicityAsSuppliedSDE:=NA]
GreaterManchester[EthnicityAsSuppliedIC=="not recorded/not known",EthnicityAsSuppliedIC:=NA]

# Arrest status is provided via a field called "Linked to Custody"
# which is either "Y" meaning arrested or blank meaning not arrested
GreaterManchester[,WasArrested:=F]
GreaterManchester[`Linked to Custody`=="Y",WasArrested:=T]

# Group membership, GM provides two columns but we have
# decided that a group 1 person is not a group 2 person by default
GreaterManchester[,Group:=fifelse(`Group 1`=="Y",1,2)]

# Create diversion column, GM is a control force so it is always F
GreaterManchester[,WasDiverted:=F]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
GreaterManchester[,DOB:=as.Date(DOB)]
GreaterManchester[,ContactDate:=as.Date(ContactDate)]

# Add in the force name for later merging
GreaterManchester$ForceName <- "GreaterManchester"

# Obtain diversion force status from master info
#GreaterManchester$DiversionForce <- policeForceInfo[ForceName=="GreaterManchester",DiversionForce]

# Set order of columns
setcolorder(GreaterManchester,partCleanColumns)

# keep only these columns
GreaterManchester <- GreaterManchester[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("GreaterManchester")