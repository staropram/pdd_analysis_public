# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Bedfordshire
Bedfordshire <- data.table(read_excel(BedfordshirePath,sheet=1))

# normalise column names
setnames(Bedfordshire,"Gender","Sex")
setnames(Bedfordshire,"ReferredToDiversion","WasDiverted")
setnames(Bedfordshire,"DateOfContact","ContactDate")
setnames(Bedfordshire,"Offence","ContactReason")
setnames(Bedfordshire,"HomeOfficeOutcome","OutcomeType")
setnames(Bedfordshire,"Ethnicity","EthnicityAsSuppliedIC")
setnames(Bedfordshire,"Self Defined Ethnicity","EthnicityAsSuppliedSDE")

# force name
Bedfordshire$ForceName <- "Bedfordshire"

# PNCNumber column uses "NULL" as a string to represent NA
Bedfordshire[PNCNumber=="NULL",PNCNumber:=NA]

# Sex column is already good just need to replace "NULL" with NA
Bedfordshire[Sex=="NULL",Sex:=NA]

# arrest status isn't available for Bedfordshire
Bedfordshire[,WasArrested:=NA]

# nobody in bedfordshire was diverted
Bedfordshire[,WasDiverted:=F]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default but Bedfordshire uses a different format for DOB
Bedfordshire[,DOB:=as.Date(DOB,format="%d/%m/%Y")]
Bedfordshire[,ContactDate:=as.Date(ContactDate)]

# keep only the columns we want
Bedfordshire <- Bedfordshire[,.SD,.SDcols=partCleanColumns]

# Set order of columns
setcolorder(Bedfordshire,partCleanColumns)

# save as a tab separated file and an XLSX
savePartCleanForceFile("Bedfordshire")