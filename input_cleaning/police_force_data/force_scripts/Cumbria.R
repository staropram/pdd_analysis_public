# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Cumbria
Cumbria <- data.table(read_excel(CumbriaPath,sheet=1))

# normalise column names
setnames(Cumbria,"DateOfBirth","DOB")
setnames(Cumbria,"HomeOfficeOutcome","OutcomeType")
setnames(Cumbria,"Offense","ContactReason")
setnames(Cumbria,"DateOfContact","ContactDate")

# Cumbria uses self defined ethnicity
setnames(Cumbria,"Ethnicity","EthnicityAsSuppliedSDE")
Cumbria[,EthnicityAsSuppliedIC:=NA]

# for some weird reason the date of birth is loaded as "excel" even though
# this happens for no other file. I think an NA triggers it: the solution 
# is to rebase the date using excel's baseline of "December 30, 1899."
Cumbria[,DOB:=as.Date(as.numeric(DOB), origin="1899-12-30")]

# PNC number uses "n/a" to denote NA
Cumbria[PNCNumber=="n/a",PNCNumber:=NA]
Cumbria[PNCNumber=="N/A",PNCNumber:=NA]

# Sex is either M, F, or W we will use just M or F
Cumbria[Sex=="M",Sex:="Male"]
Cumbria[Sex=="F",Sex:="Female"]
Cumbria[Sex=="W",Sex:=NA]

# WasArrested, this is either N, Y, or n so use Y as dichotomy
Cumbria[,WasArrested:=fifelse(toupper(Arrested)=="Y",T,F)]

# WasDiverted, either Y or N
Cumbria[,WasDiverted:=fifelse(toupper(ReferredToDiversion)=="Y",T,F)]

# Force name
Cumbria$ForceName <- "Cumbria"

# Set order of columns
setcolorder(Cumbria,partCleanColumns)

# keep only these columns
Cumbria <- Cumbria[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("Cumbria")