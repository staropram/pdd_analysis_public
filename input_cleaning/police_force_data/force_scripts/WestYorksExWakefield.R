# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# WestYorksExWakefield
WestYorksExWakefield <- data.table(read_excel(WestYorksExWakefieldPath,sheet=1))

# normalise column names
setnames(WestYorksExWakefield,"first_name","FirstName")
setnames(WestYorksExWakefield,"surname","LastName")
setnames(WestYorksExWakefield,"dob","DOB")
setnames(WestYorksExWakefield,"pnc_id","PNCNumber")
setnames(WestYorksExWakefield,"date_first_crimed","ContactDate")
setnames(WestYorksExWakefield,"ho_offence","ContactReason")
setnames(WestYorksExWakefield,"outcome","OutcomeType")

# force name
WestYorksExWakefield$ForceName <- "WestYorksExWakefield"

# Obtain diversion force status from master info
#WestYorksExWakefield$DiversionForce <- policeForceInfo[ForceName=="WestYorksExWakefield",DiversionForce]

# group assignment
WestYorksExWakefield[,Group1:=fifelse(group %like% "1",T,F)]
WestYorksExWakefield[,Group2:=fifelse(group %like% "2",T,F)]

# both officer and self defined ethnicity are provided
setnames(WestYorksExWakefield,"offender_ea_self","EthnicityAsSuppliedSDE")
setnames(WestYorksExWakefield,"offender_ea","EthnicityAsSuppliedIC")

# Create the Sex column from their "gender" column, this
# can take the values "F", "I", "M" and "U" but U and I are
# occur in insignificant numbers and semantically are equivalent
# to NA
WestYorksExWakefield[gender=="F",Sex:="Female"]
WestYorksExWakefield[gender=="M",Sex:="Male"]

# create DrugType column
#WestYorksExWakefield[,DrugType:=offenceToDrugType(tolower(ContactReason))]

# arrest status, arrest_flag is either "Y" or "N" with 0 NAs
WestYorksExWakefield[,WasArrested:=fifelse(arrest_flag=="Y",T,F)]

# diversion status, isn't clear yet what the different categories are
# for now just use the referral column
# referral is either "Y" or "N"
WestYorksExWakefield[,WasDiverted:=fifelse(referral=="Y",T,F)]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
WestYorksExWakefield[,DOB:=as.Date(DOB)]
WestYorksExWakefield[,ContactDate:=as.Date(ContactDate)]

# Set order of columns
setcolorder(WestYorksExWakefield,partCleanColumns)

# keep only these columns
WestYorksExWakefield <- WestYorksExWakefield [,.SD,.SDcols=partCleanColumns]

# save as a tab separated file and an XLSX
savePartCleanForceFile("WestYorksExWakefield")