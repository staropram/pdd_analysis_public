# This script takes an NDTMS journey file that has already had  discharge 
# reasons added, and adds in all the things required for linkage
# PseudoID
# Police force name (corresponding to individual's location)
# Initials,Sex,DOB

# load file paths and libraries
source("../../common/r/file_paths.R")
library(data.table)
library(readr)
library(stringr)

print("Loading NDTMS file with journey discharge reasons")
ndtms<-data.table(read_rds(file=NDTMS_journey_with_discharge_reasons_fn_ndtms2))

# need to create PseudoIDs but this uses the ForceName which hasn't been
# created yet. Let's create this now.
print("Mapping postcode sectors to PFAs")
# load in the map between postcode sector and PFA
sector_to_pfa<-fread("../../common/data/internal/sector_to_pfa.csv")
# ensure that this map has PartialPostcode in the same format as NDTMS
sector_to_pfa[,PartialPostcode:=toupper(paste(str_sub(Sector,1,str_length(Sector)-1),str_sub(Sector,str_length(Sector))))]
# We have to temporarily rename the column in NDTMS for the merge
setnames(ndtms,"pc","PartialPostcode")
ndtms<-merge(ndtms,sector_to_pfa,by="PartialPostcode",all.x=T)
setnames(ndtms,"PartialPostcode","pc")

# Now any row that is missing PartialPostcode will not have a PFA mapped so 
# the next step is to use the NDTMS DAT code and map that to a PFA
print("Mapping missing postcode sectors from DATs to PFAs")
source("02b_dat_code_to_pfa_ndtms2.R")
setnames(ndtms,"dat","AgencyDATCode")
ndtms<-merge(ndtms,dat_to_pfa,by="AgencyDATCode",all.x=T)
setnames(ndtms,"AgencyDATCode","dat")

# use the DAT area when the sector based location is NA
ndtms[is.na(ForceName),ForceName:=DATForceName]

# extract initials: initial of FirstName and LastName are stored in the field "
# attrbdat" as the 1st and 2nd character respectively
print("Creating FirstNameInitial and LastNameInitial columns")
ndtms[,`:=`(FirstNameInitial=str_sub(attrbdat,1,1),LastNameInitial=str_sub(attrbdat,2,2))]

# sex is also extracted from attrbdat and is stored in character position 11
print("Creating Sex column")
ndtms[,`:=`(Sex=str_sub(attrbdat,11,11))]

# DOB
ndtms[,DOB:=dob]

# Now that everyone has a Forcename we can make the PseudoID for NDTMS2 data
print("Adding in PseudoIDs")
pseudoIDs<-unlist(lapply(ndtms[,paste0(FirstNameInitial,LastNameInitial,DOB,Sex,ForceName)],function(x) {
  digest(x,algo="spookyhash")
}))
ndtms[,PseudoID_NDTMS:=paste0("NDTMS_",pseudoIDs)]

# make ethnicity column
# load the ethnicity code descriptions in
# no longer use ethnicity
includeEthnicity<-F
if(includeEthnicity) {
   ethnicityCodeMap<-fread("../../common/data/internal/NDTMS_ethnicity_codes.csv")
   setnames(ndtms,"ethnic","EthnicityCode")
   # create Ethnicity column that's compatible with Police Force Data for linkage
   print("Remapping ethnicities")
   ndtms<-ndtms[ethnicityCodeMap[,c("EthnicityCode","EthCoarse")],on="EthnicityCode"]
   setnames(ndtms,"EthnicityCode","ethnic")
}

# now output this as another intermediate NDTMS file
print("Writing output")
arrow::write_feather(ndtms,NDTMS_journey_with_linkage_fields_fn_FEATHER_ndtms2)
