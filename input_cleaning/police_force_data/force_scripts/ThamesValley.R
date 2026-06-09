# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# ThamesValley
ThamesValley <- data.table(read_excel(ThamesValleyPath,sheet=1))

# normalise column names
setnames(ThamesValley,"DateOfBirth","DOB")
setnames(ThamesValley,"DateOfContact","ContactDate")
setnames(ThamesValley,"Offense","ContactReason")
setnames(ThamesValley,"HomeOfficeOutcome","OutcomeType")

# ThamesValley  uses self defined ethnicity
setnames(ThamesValley,"Ethnicity","EthnicityAsSuppliedSDE")
ThamesValley[,EthnicityAsSuppliedIC:=NA]

# create Sex column, their `Sex` has values
# Male, Female, Indeterminate, Unknown, 
# We will make it so everything is NA except Male and Female
ThamesValley[Sex=="Male",Sex:="Male"]
ThamesValley[Sex=="Female",Sex:="Female"]
ThamesValley[Sex=="Indeterminate",Sex:=NA]
ThamesValley[Sex=="Unknown",Sex:=NA]

# arrest status
ThamesValley[,WasArrested:=fifelse(Arrested=="Y",T,F)] 

# Create diversion column, ReferredToDiversion is either "Y" or "N" with no NAs
ThamesValley[,WasDiverted:=ReferredToDiversion=="Y"]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
ThamesValley[,DOB:=as.Date(DOB)]
ThamesValley[,ContactDate:=as.Date(ContactDate)]

# Add in the force name for later merging
ThamesValley$ForceName <- "ThamesValley"

# Obtain diversion force status from master info
#ThamesValley$DiversionForce <- policeForceInfo[ForceName=="ThamesValley",DiversionForce]

# Set order of columns
setcolorder(ThamesValley,partCleanColumns)

# keep only these columns
ThamesValley <- ThamesValley[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("ThamesValley")