# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Norfolk
Norfolk <- data.table(read_excel(NorfolkPath,sheet=1))

# normalise column names
setnames(Norfolk,"DateOfBirth","DOB")
setnames(Norfolk,"HomeOfficeOutcome","OutcomeType")
setnames(Norfolk,"Offense","ContactReason")
setnames(Norfolk,"DateOfContact","ContactDate")

# Norfolk uses officer defined ethnicity
setnames(Norfolk,"Ethnicity","EthnicityAsSuppliedIC")
Norfolk[,EthnicityAsSuppliedSDE:=NA]

# Norfolk has provided a bunch of offences that should not be in there
# for other forces there are so few we actually map them and then remove
# them but in this case there are so many we remove them in bulk
Norfolk[,ContactReason:=tolower(ContactReason)]

# luckily they prefix everything
Norfolk <- Norfolk[ContactReason %like% "criminal damage -" |
                  ContactReason %like% "drugs -" |
                  ContactReason %like% "assault -" |
                  ContactReason %like% "theft -" |
                  ContactReason %like% "twoc -" |
                  ContactReason %like% "drunk and disorderly"]

# Norfolk uses a unicode dash instead of an ASCII hypen in some outcomes
Norfolk[, OutcomeType := gsub("\u2013", "-", OutcomeType)]

# get rid of some specifics
Norfolk <- Norfolk[!ContactReason %like% "gbh"]
Norfolk <- Norfolk[!ContactReason %like% "abh"]

#ol <- unique(Norfolk$ContactReason) %>% sort
#print(ol)

# No PNC Numbers are provided
Norfolk[,PNCNumber:=NA]

# Norfolk doesn't provide any group categories so we derive
# them from offence: all their drug offences have "Drugs" in the
# name
Norfolk[,Group:=fifelse(ContactReason %like% "Drugs",1,2)]

# Sex is either "Female", "Male" with no NAs or unknowns, so this is done

# Arrested is only "Yes" at the moment
Norfolk[,WasArrested:=Arrested=="Yes"]

# Norfolk is a control force so nobody is diverted
Norfolk[,WasDiverted:=F]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
Norfolk[,DOB:=as.Date(DOB)]
Norfolk[,ContactDate:=as.Date(ContactDate)]

# Add in the force name for later merging
Norfolk$ForceName <- "Norfolk"

# Obtain diversion force status from master info
#Norfolk$DiversionForce <- policeForceInfo[ForceName=="Norfolk",DiversionForce]

# Set order of columns
setcolorder(Norfolk,partCleanColumns)

# keep only these columns
Norfolk <- Norfolk[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("Norfolk")