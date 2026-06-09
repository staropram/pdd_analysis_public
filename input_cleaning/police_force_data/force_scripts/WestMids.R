# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# WestMids
WestMids <- data.table(read_excel(WestMidsPath,sheet=2))

# normalise column names
setnames(WestMids,"Date Record Created","ContactDate")
setnames(WestMids,"[Short Offence Title]","ContactReason")
setnames(WestMids,"Outcome","OutcomeType")
setnames(WestMids,"PNC ID","PNCNumber")

# West mids uses self-assessed ethnicity
setnames(WestMids,"Suspect Ethnic Group (Self Assessed)","EthnicityAsSuppliedSDE")
WestMids[,EthnicityAsSuppliedIC:=NA]

# PNC number uses "NULL" to denote NA
WestMids[PNCNumber=="NULL",PNCNumber:=NA]

# create Sex column, `Suspect Gender` has values
# Male, Female, NA, Unknown, and Unspecified
# We will make it so everything is NA except Male and Female
WestMids[`Suspect Gender` %like% "M",Sex:="Male"]
WestMids[`Suspect Gender` %like% "F",Sex:="Female"]

# Derive the FirstName and LastName fields from their combined name field
WestMids[,c("FirstName","LastName"):={
     nameList <- strsplit(.SD[[1]]," ")
     .(
        FirstName=unlist(lapply(nameList,first)),
        LastName=unlist(lapply(nameList,last))
     )
  },
  .SDcols="Suspect Name"
][]

# create DrugType column
#WestMids[,DrugType:=offenceToDrugType(ContactReason)]

# West mids arrest status is either "Yes" or "No" with no NA
WestMids[,WasArrested:=fifelse(ArrestMade=="Yes",T,F)]

# Create diversion column, Referral is either "Yes" or "No"
WestMids[,WasDiverted:=fifelse(Referral=="Yes",T,F)]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
WestMids[,DOB:=as.Date(DOB)]
WestMids[,ContactDate:=as.Date(ContactDate)]

# Add in the force name for later merging
WestMids$ForceName <- "WestMids"

# Obtain diversion force status from master info
#WestMids$DiversionForce <- policeForceInfo[ForceName=="WestMids",DiversionForce]

# Set order of columns
setcolorder(WestMids,partCleanColumns)

# keep only these columns
WestMids <- WestMids[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("WestMids")