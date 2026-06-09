library(data.table)
library(readxl)

source('../../common/r/file_paths.R')

# load in postcode to PFA map
postcode_to_pfa<-fread("../../common/data/internal/pc_to_pfa.csv")

# load in DAT code to postcode map
dat_to_postcode<-data.table(read_excel(DAT_to_PFA_map_fn,sheet=1))
# make sure postcode is capitalised
dat_to_postcode[,postcode:=toupper(postcode)]
setnames(dat_to_postcode,"postcode","Postcode")
setnames(dat_to_postcode,"agency_dat_code","AgencyDATCode")

# merge on postcode to get PFA for each DAT
dat_to_postcode<-merge(dat_to_postcode,postcode_to_pfa,by="Postcode",all.x=T)

setnames(dat_to_postcode,"ForceName","DATForceName")
dat_to_pfa<-dat_to_postcode[,c("AgencyDATCode","DATForceName")]

# remove NA DATs and PFAs
dat_to_pfa<-dat_to_pfa[!is.na(AgencyDATCode)]
dat_to_pfa<-dat_to_pfa[!is.na(DATForceName)]

# some DATs map to more than one PFA but in these cases it is always a majority
# in one area, count the number of PFA per DAT
pfa_per_dat<-dat_to_pfa[,list(N=uniqueN(DATForceName)),by=AgencyDATCode]

# for each DAT code with more than one PFA, only keep the most common
lapply(pfa_per_dat[N>1,AgencyDATCode],function(datCode) {
   # find the most commonly occuring force name
   pfaRanks<-dat_to_pfa[AgencyDATCode==datCode,.N,by=DATForceName]
   rankingPFA<-pfaRanks[which.max(pfaRanks$N),DATForceName]
   print(paste0(rankingPFA," ranks highest for ",datCode))
   # and change everything to this
   dat_to_pfa[AgencyDATCode==datCode,DATForceName:=rankingPFA]

})


# we only want unique info
dat_to_pfa<-unique(dat_to_pfa)


