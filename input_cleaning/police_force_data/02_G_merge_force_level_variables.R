source('../../common/r/file_paths.R')
# This script merges force-level variables into the PFD
print("Merging force-level variables")

# load the force-level variables
forceMetadata <- data.table(read_excel("../../common/data/internal/force_metadata.xlsx",sheet="summary"))
setnames(forceMetadata,"Police force name","ParentForceLong")
setnames(forceMetadata,"All cause reoffending rate, Oct 2018-Sep 2021","BaselineReoffendingRate")
setnames(forceMetadata,"Budget per person","ForceFunding")
setnames(forceMetadata,"Treatment penetration","BaselineDrugTreatment")

# put wakefield in the same category as WestYorks for the join
pfd[,ParentForce:=ForceName]
pfd[ForceName=="Wakefield",ParentForce:="WestYorks"]
pfd[ForceName=="WestYorksExWakefield",ParentForce:="WestYorks"]

# we need the long names of the police forces
pfd <- merge(pfd,unique(policeForceInfo[,c("ParentForce","ParentForceLong")]),by="ParentForce",all.x=T)

# now join with the metadata
pfd <- merge(pfd,forceMetadata[,c(
   "ParentForceLong",
   "BaselineReoffendingRate",
   "ForceFunding",
   "BaselineDrugTreatment"
   # xxx select force-level metadata
)],by="ParentForceLong",all.x=T)

# let's rename some of these
setnames(pfd,"BaselineReoffendingRate","ForceReoffendingRate")
setnames(pfd,"BaselineDrugTreatment","ForceOCUPenetration")

# temporary until we fill the rest in
pfd[,HistoricalPossession:=1]
pfd[,HistoricalOtherDrugs:=1]
pfd[,HistoricalTheft:=1]
pfd[,HistoricalViolence:=1]

# merge in the diversion status too
pfd <- merge(pfd,policeForceInfo[HaveData==T,c(
   "ForceName",
   "G1DiversionForce",
   "G2DiversionForce",
   "DiversionForce"
)],by="ForceName")