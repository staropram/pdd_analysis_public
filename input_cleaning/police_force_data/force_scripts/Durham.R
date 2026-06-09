# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Durham
# there are three sheets and they all use different names for some cols
# and have some other differences that need normalising

# SHEET 1 - "group 1 notifiable offences"
d1 <- data.table(read_excel(DurhamPath,sheet=1))

# rename columns
setnames(d1,"Group","GroupText")

setnames(d1,"surname","LastName")
setnames(d1,"Date of Birth","DOB")
setnames(d1,"PNC ID ref","PNCNumber")
setnames(d1,"Created Date","ContactDate")
setnames(d1,"referred to diversion","WasDivertedText")
setnames(d1,"arrested","WasArrestedText")
setnames(d1,"Outcome","OutcomeType")
# we need to keep crime urn to deal with Durham oddity (see below)
setnames(d1,"Crime URN","CrimeURN")
setnames(d1,"Offender URN","OffenderURN")

setnames(d1,"SD ethnicity code and desc","EthnicityAsSuppliedSDE")

# remove useless columns
#d1[,`Crime URN`:=NULL]
#d1[,`Offender URN`:=NULL]
d1[,`age at time of offence`:=NULL][]

# SHEET 2 - "Group 2 notifiable offences"
d2 <- data.table(read_excel(DurhamPath,sheet=2))
# rename inconsistent columns
setnames(d2,"Group","GroupText")

setnames(d2,"surname","LastName")
setnames(d2,"Date of Birth","DOB")
setnames(d2,"SD ethnicity code and desc","EthnicityAsSuppliedSDE")
setnames(d2,"PNC ID","PNCNumber")
setnames(d2,"Date of police contact","ContactDate")
setnames(d2,"referred to diversion scheme","WasDivertedText")
setnames(d2,"arrested","WasArrestedText")
setnames(d2,"HO Outcome","OutcomeType")
# we need to keep crime urn to deal with something odd Durham do, see below
setnames(d2,"crime URN","CrimeURN")
setnames(d2,"Offender URN","OffenderURN")

# remove useless columns
d2[,`Age (at time of contact)`:=NULL][]

# SHEET 3 - "Group 2 - D&D arrests"
d3 <- data.table(read_excel(DurhamPath,sheet=3))

# rename inconsistent columns
setnames(d3,"Offence (HO Class)","HO Class")
setnames(d3,"FORENAMES","Forenames")
setnames(d3,"GENDER","Gender")
setnames(d3,"Group","GroupText")

setnames(d3,"SURNAME","LastName")
setnames(d3,"SD ethnicity code and desc","EthnicityAsSuppliedSDE")
setnames(d3,"PNCID","PNCNumber")
setnames(d3,"ARREST_DATE","ContactDate")
setnames(d3,"referred to diversion scheme","WasDivertedText")
setnames(d3,"arrested","WasArrestedText")
setnames(d3,"OffenceDecision","OutcomeType")

# remove useless columns
#d3[,CUSTODY_RECORD_NUMBER:=NULL]
#d3[,`offender reference number`:=NULL]

# merge all three sheets
Durham <- rbindlist(list(d1,d2,d3),use.names=T,fill=T) 

# Durham uses SDE ethnicity
Durham[,EthnicityAsSuppliedIC:=NA]

# Don't need time with arrest
Durham[,ContactDate:=as.Date(ContactDate)][]

# Contact reason as a mix of HO offence code data
Durham[,ContactReason:=paste0(`HO Sub Code`," - ",`HO Sub Code Text (ie the drug in question)`)]

# make the drug column
#Durham[,DrugType:=offenceToDrugType(ContactReason)]

# Fix some other column entries
Durham[PNCNumber=="Not recorded on PNC",PNCNumber:=NA]

# arrest status has 3 possibilities
Durham[WasArrestedText=="yes",WasArrested:=T]
Durham[WasArrestedText=="no - is not recorded on PNC",WasArrested:=F]
Durham[WasArrestedText=="unable to verify",WasArrested:=NA]

# Fix Sex column
Durham[Gender %like% "M",Sex:="Male"]
Durham[Gender %like% "F",Sex:="Female"][]

# was diverted column
Durham[,WasDiverted:=(WasDivertedText=="yes"&!is.na(WasDivertedText))][]

# We only need first name of forename list
Durham[,FirstName:=unlist(Map(function(x){strsplit(x," ")[[1]][1]},Durham$Forenames),use.names=F)]

# drop unused columns
# explicitly drop each one so it is clear in reading what is dropped
Durham[,`age at time of arrest`:=NULL][]
Durham[,`Forenames`:=NULL][]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
Durham[,DOB:=as.Date(DOB)]
Durham[,ContactDate:=as.Date(ContactDate)]

# Durham put crimes in both sheets if they are group 1 AND group 2 which means
# we have duplicates, remove duplicates here
Durham <- unique(Durham,by=c("CrimeURN","OffenderURN"))
   
# add in meta columns for merging
Durham$ForceName <- "Durham"

# Set order of columns
setcolorder(Durham,partCleanColumns)

# keep only these columns
Durham <- Durham[,.SD,.SDcols=partCleanColumns]

# save
savePartCleanForceFile("Durham")