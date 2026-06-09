# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# sheet 1 is simple possession (group 1)
# sheet 2 is any other eligible offence that involved drugs (group 2)

# load sheet 1
AvonAndSomersetS1 <- data.table(read_excel(AvonAndSomersetPath,sheet=1))
# rename columns in sheet 1
setnames(AvonAndSomersetS1,"Offender Given Name 1","FirstName")
setnames(AvonAndSomersetS1,"Offender Surname/Organisation Name","LastName")
setnames(AvonAndSomersetS1,"Offender DOB","DOB")
setnames(AvonAndSomersetS1,"Offender - Merged Latest Ethnicity IC Code and Desc","EthnicityAsSuppliedIC")
setnames(AvonAndSomersetS1,"Offender - Occurrence Self Defined Ethnicity","EthnicityAsSuppliedSDE")

setnames(AvonAndSomersetS1,"Offender PNC ID","PNCNumber")
setnames(AvonAndSomersetS1,"Occurrence Created Date","ContactDate")
setnames(AvonAndSomersetS1,"Current Offence Description","ContactReason")
setnames(AvonAndSomersetS1,"Current Classification HO Outcome Code and Desc","OutcomeType")

# diversion status, not clear yet if this is correct (are there other indivs classified as diverted)
AvonAndSomersetS1[,WasDiverted:=F]
AvonAndSomersetS1[`Drug educatiom programme`=="Drugs education programme",WasDiverted:=T]

# create DrugType column
#AvonAndSomersetS1[,DrugType:=offenceToDrugType(ContactReason)]

# arrested status, this col is either "Arrested" or NA
AvonAndSomersetS1$WasArrested <- NA
AvonAndSomersetS1[Arrest=="Arrested",WasArrested:=T]

# load sheet 2, annoyingly need to specify column types here
# otherwise read_excel thinks a sparse string col is a logical for some reason
# can guess everything except the "Drug educatiom programme" col
colTypes <- c(
  "guess", # Occurrence Number	
  "guess", # Offender Given Name 1
  "guess", # Offender Surname/Organisation Name	
  "guess", # Offender DOB	
  "guess", # Offender Gender	
  "guess", # Offender - Merged Latest Ethnicity IC Code and Desc
  "guess", # Offender - Occurrence Self Defined Ethnicity
  "guess", # Occurrence Created Date
  "guess", # Current Offence Description
  "skip", # for some reason there is a blank column here
  "text",  # Drug educatiom programme
  "guess", # Arrest
  "guess", # Offender Custody/VA Number
  "guess", # Current Classification HO Outcome Code and Desc
  "guess", # Offender PNC ID	
  "guess"  # Offender Warning Type (Concat)
)
AvonAndSomersetS2 <- data.table(read_excel(AvonAndSomersetPath,sheet=2,col_types=colTypes))

# rename columns in sheet 2
# note this is duplication as the columns are the same for Sheet 2 as for sheet 1
# however this is not always the case for other files
setnames(AvonAndSomersetS2,"Offender Given Name 1","FirstName")
setnames(AvonAndSomersetS2,"Offender Surname/Organisation Name","LastName")
setnames(AvonAndSomersetS2,"Offender DOB","DOB")
setnames(AvonAndSomersetS2,"Offender - Merged Latest Ethnicity IC Code and Desc","EthnicityAsSuppliedIC")
setnames(AvonAndSomersetS2,"Offender - Occurrence Self Defined Ethnicity","EthnicityAsSuppliedSDE")
setnames(AvonAndSomersetS2,"Offender PNC ID","PNCNumber")
setnames(AvonAndSomersetS2,"Occurrence Created Date","ContactDate")
setnames(AvonAndSomersetS2,"Current Offence Description","ContactReason")
setnames(AvonAndSomersetS2,"Current Classification HO Outcome Code and Desc","OutcomeType")


# diversion, again this isn't clear
# is everyone in Group 2 for this force is a "drugs warning diversion"?
#AvonAndSomersetS2$WasDiverted <- T 
AvonAndSomersetS2[,WasDiverted:=F]
AvonAndSomersetS2[`Drug educatiom programme`=="Drugs education programme",WasDiverted:=T]

# arrested status, this is either "Arrested" or empty
AvonAndSomersetS2$WasArrested <- NA
AvonAndSomersetS2[Arrest=="Arrested",WasArrested:=T]

# merge the two excel sheets into a single table
AvonAndSomerset <- rbindlist(list(AvonAndSomersetS1,AvonAndSomersetS2),
                             use.names=T,fill=T)
# drop cols we don't care about
AvonAndSomerset[,`Occurrence Number`:=NULL][]
#AvonAndSomerset[,`Offender Warning Type (Concat)`:=NULL][]
AvonAndSomerset[,`Offender Custody/VA Number`:=NULL][]

# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
AvonAndSomerset[,DOB:=as.Date(DOB)]
AvonAndSomerset[,ContactDate:=as.Date(ContactDate)]

# create Sex column, "Offender Gender" is either Female,Male,Inteterminate, 
# or Unknown but the latter two are very rare so we will treat these as NA
AvonAndSomerset[`Offender Gender` %like% "F",Sex:="Female"]
AvonAndSomerset[`Offender Gender` %like% "M",Sex:="Male"][]

# Add in the force name for later merging
AvonAndSomerset$ForceName <- "AvonAndSomerset"

# Set order of columns
setcolorder(AvonAndSomerset,partCleanColumns)

# keep only these columns
AvonAndSomerset <- AvonAndSomerset[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file and as an excel file
savePartCleanForceFile("AvonAndSomerset")