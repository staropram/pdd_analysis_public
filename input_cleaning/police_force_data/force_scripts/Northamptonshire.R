# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Northamptonshire
Northamptonshire <- data.table(read_excel(NorthamptonshirePath,sheet=1))

# normalise column names
setnames(Northamptonshire,"Occurrence Reported Date","ContactDate")
setnames(Northamptonshire,"Person Date of Birth","DOB")
setnames(Northamptonshire,"Stats Classification Desc","ContactReason")
setnames(Northamptonshire,"Stats Class Status","OutcomeType")
setnames(Northamptonshire,"Person Gender","Sex")
setnames(Northamptonshire,"Person ID-PNC","PNCNumber")


# Northamtonshire provides both self and officer defined ethnicity
setnames(Northamptonshire,"Ethnicity","EthnicityAsSuppliedIC")
setnames(Northamptonshire,"Self Defined Ethnicity","EthnicityAsSuppliedSDE")

# Northamptonshire has 3 categories for Group assignment
# i) Group 1 only
# ii) Group 2 only
# iii) Both
# We want an exclusive membership, but posession takes priority so
# if someone is in Both we change them to being Group 1 only
Northamptonshire[,Group:=fifelse(`Offence Group`=="GROUP 2 ONLY",2,1)]

# Gender is either "Female", "Male", "Unknown", or NA
# We reduce this to M,F, and NA
Northamptonshire[Sex!="Female"&Sex!="Male"|is.na(Sex),Sex:=NA]

# Person Arrested is either "YES" or "NO" with no NA values
Northamptonshire[,WasArrested:=`Person Arrested`=="YES"]

# Derive the FirstName and LastName fields from their combined name field
Northamptonshire[,c("FirstName","LastName"):={
     nameList <- strsplit(.SD[[1]]," ")
     .(
        FirstName=unlist(lapply(nameList,first)),
        LastName=unlist(lapply(nameList,last))
     )
  },
  .SDcols="Person Full Name"
][]

# Northamptonshire does have a column which seems to imply diversion
# a variable called "Disclosure Scheme Used" which is either NA or Yes
Northamptonshire[,WasDiverted:=F]
Northamptonshire[!is.na(`Disclosure Scheme Used`),WasDiverted:=T]


# change datetimes into dates and ensure dates are parsed correctly
# %Y-%m-%d is the default and this police force uses it
Northamptonshire[,DOB:=as.Date(DOB)]
Northamptonshire[,ContactDate:=as.Date(ContactDate)]


# Add in the force name for later merging
Northamptonshire$ForceName <- "Northamptonshire"

# Obtain diversion force status from master info
#Northamptonshire$DiversionForce <- policeForceInfo[ForceName=="Northamptonshire",DiversionForce]

# Set order of columns
setcolorder(Northamptonshire,partCleanColumns)

# keep only these columns
Northamptonshire <- Northamptonshire[,.SD,.SDcols=partCleanColumns]

# save as a tab separated file
savePartCleanForceFile("Northamptonshire")