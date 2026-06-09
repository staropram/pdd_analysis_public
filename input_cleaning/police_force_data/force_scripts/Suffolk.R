# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Suffolk
Suffolk <- data.table(read_excel(SuffolkPath,sheet=1))

# normalise column names
setnames(Suffolk,"DateOfBirth","DOB")
setnames(Suffolk,"HomeOfficeOutcome","OutcomeType")
setnames(Suffolk,"Offense","ContactReason")
setnames(Suffolk,"DateOfContact","ContactDate")

# Suffolk uses officer defined ethnicity
setnames(Suffolk,"Ethnicity","EthnicityAsSuppliedIC")
Suffolk[,EthnicityAsSuppliedSDE:=NA]

# Suffolk has provided a bunch of offences that should not be in there
# for other forces there are so few we actually map them and then remove
# them but in this case there are so many we remove them in bulk
Suffolk[,ContactReason:=tolower(ContactReason)]

# luckily they prefix everything
Suffolk <- Suffolk[ContactReason %like% "criminal damage -" |
                  ContactReason %like% "drugs -" |
                  ContactReason %like% "assault -" |
                  ContactReason %like% "theft -" |
                  ContactReason %like% "twoc -" |
                  ContactReason %like% "drunk and disorderly"]

# Suffolk uses a unicode dash instead of an ASCII hypen in some outcomes
Suffolk[, OutcomeType := gsub("\u2013", "-", OutcomeType)]

# get rid of some specifics
Suffolk <- Suffolk[!ContactReason %like% "gbh"]
Suffolk <- Suffolk[!ContactReason %like% "abh"]

# No PNC Numbers are provided
Suffolk[,PNCNumber:=NA]

# Suffolk doesn't provide any group categories so we derive
# them from offence: all their drug offences have "Drugs" in the
# name
Suffolk[,Group:=fifelse(ContactReason %like% "Drugs",1,2)]

# Sex is either "Female", "Male" with one "Unspecified", the rest is NA
Suffolk[Sex=="Unspecified",Sex:=NA]

# Arrested is only "Yes" at the moment
Suffolk[,WasArrested:=Arrested=="Yes"]

# Suffolk is a control force so nobody is diverted
Suffolk[,WasDiverted:=F]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
Suffolk[,DOB:=as.Date(DOB)]
Suffolk[,ContactDate:=as.Date(ContactDate)]

# Add in the force name for later merging
Suffolk$ForceName <- "Suffolk"

# Obtain diversion force status from master info
#Suffolk$DiversionForce <- policeForceInfo[ForceName=="Suffolk",DiversionForce]

# Set order of columns
setcolorder(Suffolk,partCleanColumns)

# keep only these columns
Suffolk <- Suffolk[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("Suffolk")