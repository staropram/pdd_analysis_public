# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Cleveland

# define column types and skip columns we don't need
colTypes <- c(
  "skip", # LocalID
  "text", # FirstName
  "text", # LastName
  "date", # DateOfBirth
  "text", # Sex
  "text", # Gender
  "text", # Ethnicity	
  "date", # Crime valid date
  "skip", # Crime disposal date
  "text", # Offence HO group
  "text", # Offence HO class
  "numeric", # Group
  "skip", # Drug	(its empty)
  "text", # ReferredToDiversion	
  "text", # Arrested
  "text", # HomeOfficeOutcome	
  "text", # PNCNumber	
  "skip", # Drug offence in previous 5 years
  "skip", # Drugs flag on crime	
  "skip"  # Positive drugs test in custody
)
# it's faster if you spec col types and reduces chances for mistakes
Cleveland <- data.table(read_excel(ClevelandPath,sheet=1,col_types=colTypes))

# normalise column names
setnames(Cleveland,"DateOfBirth","DOB")
setnames(Cleveland,"Crime valid date","ContactDate")
setnames(Cleveland,"Offence HO class","ContactReason")
setnames(Cleveland,"HomeOfficeOutcome","OutcomeType")


# Cleveland uses self defined ethnicity
setnames(Cleveland,"Ethnicity","EthnicityAsSuppliedSDE")
Cleveland[,EthnicityAsSuppliedIC:=NA]

# Sex is already set and is already binary Male/Female

# make the DrugType column
#Cleveland[,DrugType:=offenceToDrugType(ContactReason)]

# diversion status, need to check this with the force
Cleveland[,WasDiverted:=F]
Cleveland[ReferredToDiversion=="Yes",WasDiverted:=T]

# arrest status, Arrested is either "Yes" or "No"
Cleveland[,WasArrested:=fifelse(Arrested=="Yes",T,F)]

# drop unused columns
Cleveland[,`Gender`:=NULL][]
Cleveland[,`Offence HO group`:=NULL][]
Cleveland[,`ReferredToDiversion`:=NULL][]
Cleveland[,`Group`:=NULL][]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
Cleveland[,DOB:=as.Date(DOB)]
Cleveland[,ContactDate:=as.Date(ContactDate)]

# add in meta columns for merging
Cleveland$ForceName <- "Cleveland"

# Set order of columns
setcolorder(Cleveland,partCleanColumns)

# keep only the columns we want
Cleveland <- Cleveland[,.SD,.SDcols=partCleanColumns]

# save as TSV and excel
savePartCleanForceFile("Cleveland")