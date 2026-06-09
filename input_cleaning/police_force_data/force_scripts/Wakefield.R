# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Wakefield
Wakefield <- data.table(read_excel(WakefieldPath,sheet=1))

# normalise column names
setnames(Wakefield,"first_name","FirstName")
setnames(Wakefield,"surname","LastName")
setnames(Wakefield,"dob","DOB")
setnames(Wakefield,"pnc_id","PNCNumber")
setnames(Wakefield,"date_first_crimed","ContactDate")
setnames(Wakefield,"ho_offence","ContactReason")
setnames(Wakefield,"outcome","OutcomeType")

# force name
Wakefield$ForceName <- "Wakefield"

# Obtain diversion force status from master info
#Wakefield$DiversionForce <- policeForceInfo[ForceName=="Wakefield",DiversionForce]

# group assignment
Wakefield[,Group1:=fifelse(group %like% "1",T,F)]
Wakefield[,Group2:=fifelse(group %like% "2",T,F)]

# both officer and self defined ethnicity are provided
setnames(Wakefield,"offender_ea_self","EthnicityAsSuppliedSDE")
setnames(Wakefield,"offender_ea","EthnicityAsSuppliedIC")

# Create the Sex column from their "gender" column, this
# can take the values "F", "I", "M" and "U" but U and I are
# occur in insignificant numbers and semantically are equivalent
# to NA
Wakefield[gender=="F",Sex:="Female"]
Wakefield[gender=="M",Sex:="Male"]

# create DrugType column
#Wakefield[,DrugType:=offenceToDrugType(tolower(ContactReason))]

# arrest status, arrest_flag is either "Y" or "N" with 0 NAs
Wakefield[,WasArrested:=fifelse(arrest_flag=="Y",T,F)]

# diversion status, isn't clear yet what the different categories are
# for now just use the referral column
# referral is either "Y" or "N"
Wakefield[,WasDiverted:=fifelse(referral=="Y",T,F)]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
Wakefield[,DOB:=as.Date(DOB)]
Wakefield[,ContactDate:=as.Date(ContactDate)]

# Set order of columns
setcolorder(Wakefield,partCleanColumns)

# keep only these columns
Wakefield <- Wakefield [,.SD,.SDcols=partCleanColumns]

# save as a tab separated file and an XLSX
savePartCleanForceFile("Wakefield")