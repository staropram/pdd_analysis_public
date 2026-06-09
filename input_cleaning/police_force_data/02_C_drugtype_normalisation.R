# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# load in the clean data
forceReload <- F
if(!exists("pfd")|forceReload) {
   pfd <- fread(policeforce_data_complete_fn_TSV)
}

# we also need the offence group classification
hoOffences <- data.table(read_excel("../../common/data/external/offence_group_classification_june_2023.xlsx",sheet=2))
hoOffences[,OffenceDetailed:=tolower(`Detailed offence`)]

# create a map between offence and drug type
drugMap <- data.table(OffenceDetailed=unique(pfd$OffenceDetailed))
setkey(drugMap,OffenceDetailed)
setkey(hoOffences,OffenceDetailed)
drugMap <- drugMap[hoOffences[,c("OffenceDetailed","Offence group")],nomatch=0]

# only drug offences need to be mapped
drugMap[`Offence group`!="Drug offences",DrugType:="Non-drug-offence"]

oCheckDrug <- function(include=NA,exclude=NA) {
   includes <- ifelse(!is.na(include),lapply(include,function(x){grepl(x,drugMap$OffenceDetailed,fixed=T)}),T)
   excludes <- ifelse(!is.na(exclude),lapply(exclude,function(x){!grepl(x,drugMap$OffenceDetailed,fixed=T)}),T)
   combinedI <- Reduce(`&`,includes)
   combinedE <- Reduce(`&`,excludes)
   drugMap[combinedI&combinedE,OffenceDetailed]
}

oMapDrug <- function(include=NA,exclude=NA,drugName,drugClass,test=F) {
   if(test==T)  {
      return(oCheckDrug(include,exclude)) 
   }
   includes <- ifelse(!is.na(include),lapply(include,function(x){grepl(x,drugMap$OffenceDetailed,fixed=T)}),T)
   excludes <- ifelse(!is.na(exclude),lapply(exclude,function(x){!grepl(x,drugMap$OffenceDetailed,fixed=T)}),T)
   combinedI <- Reduce(`&`,includes)
   combinedE <- Reduce(`&`,excludes)
   drugMap[combinedI&combinedE,`:=` (DrugType=drugName,DrugClass=drugClass)]
}

# map drug offences to drug type (extract the specific drug)
oMapDrug(c("cannabis"),NA,"Cannabis","B")
oMapDrug(c("crack cocaine"),NA,"Crack Cocaine","A")
oMapDrug(c("cocaine"),c("crack"),"Cocaine","A")
oMapDrug(c("heroin"),NA,"Heroin","A")
oMapDrug(c("amphetamine"),c("methyl"),"Amphetamine","B")
oMapDrug(c("methylamphetamine"),NA,"Methylamphetamine","A")
oMapDrug(c("cannabinoid"),NA,"Synthetic cannabinoid","B")
oMapDrug(c("ketamine"),NA,"Ketamine","B")
oMapDrug(c("psychoactive"),NA,"Psychoactive substance","Unknown")
oMapDrug(c("mdma"),NA,"MDMA","A")
oMapDrug(c("khat"),NA,"Khat","C")
oMapDrug(c("methadone"),NA,"Methadone","A")
oMapDrug(c("mephedrone"),NA,"Mephedrone","B")
oMapDrug(c("ghb"),NA,"GHB","C")
oMapDrug(c("lsd"),NA,"LSD","A")
oMapDrug(c("bzp"),NA,"BZP","C")
oMapDrug(c("unspecified"),NA,"Unspecified","Unknown")
oMapDrug(c("steroids"),NA,"Anabolic steroids","C")
oMapDrug(c("class a","other"),NA,"Class A (other)","A")
oMapDrug(c("class b","other"),NA,"Class B (other)","B")
oMapDrug(c("class c","other"),NA,"Class C (other)","C")
oMapDrug(c("09201"),NA,"Unspecified","Unknown")
oMapDrug(c("09203"),NA,"Class A (other)","A")
oMapDrug(c("09204"),NA,"Class B (other)","B")
oMapDrug(c("09207"),NA,"Class B (other)","B")

# some drug offences do not map a drug
# obstructing a pc in search related to temporary class drug
oMapDrug(c("09389"),NA,"Unspecified","Unknown")
oMapDrug(c("09330"),NA,"Unspecified","Unknown")
# other indicatble or triable offence relating to drugs
oMapDrug(c("09340"),NA,"Unspecified","Unknown")
# permitting use of premises for supply or production (temporary class drug)
oMapDrug(c("09387"),NA,"Psychoactive substance","Unknown")

# print out which ones are left
unmappedDrugs <- drugMap[is.na(DrugType),OffenceDetailed]
if(length(unmappedDrugs)>0) {
  print("Some drugs have not been mapped, this must be rectified before continuing")
  print("----------------------------------------------------------------------------")
  print(sort(unmappedDrugs))
  print("----------------------------------------------------------------------------")
  print(paste(nrow(drugMap)-length(unmappedDrugs),"done ",length(unmappedDrugs),"to go"))
}