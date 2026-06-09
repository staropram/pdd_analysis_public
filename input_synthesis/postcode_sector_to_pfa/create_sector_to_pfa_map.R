library(data.table)
pc_to_pfa_fn <-  "../../common/data/internal/pc_to_pfa.csv"
sector_to_pfa_fn <-  "../../common/data/internal/sector_to_pfa.csv"

# takes the postcode to PFA map and creates a postcode sector to PFA map
# load in the pc to pfa map 
pc_to_pfa <- fread(pc_to_pfa_fn)

# go through each sector and find the majority vote for each
unique_sectors <- unique(pc_to_pfa$Sector)
nSectors <- length(unique_sectors)
sector_to_pfa <- rbindlist(lapply(unique_sectors,function(sector){
   sI <- which(unique_sectors==sector)
   if(sI%%100==0) {
      print(paste("Sector ",sI,"of",nSectors," (",format(sI/nSectors*100,digits=2),"% )"))
   }
   # get the rows for this sector
   sectorRows <- pc_to_pfa[Sector==sector]
   # if all the PFA for this sector are NA then so should the map
   sectorCount <- nrow(sectorRows)
   naCount <- sum(is.na(sectorRows$PFA))
   if(sectorCount==naCount) {
      return(data.table(Sector=sector,PFA=NA))
   }
   
   # otherwise count the PFAs for all postcodes in the sector
   pfaRanks <- pc_to_pfa[Sector==sector,.N,by=PFA]
   # choose the most popular UTLA so long as it gets more than
   # 75% of the vote
   sumVotes <- sum(pfaRanks$N)
   whichMaxVotesI <- which.max(pfaRanks$N)
   whichMaxVotesN <- pfaRanks[whichMaxVotesI,N]
   whichMaxVotesPFA <- pfaRanks[whichMaxVotesI,PFA]
   fractionMaxVotes <- whichMaxVotesN/sumVotes
   if(fractionMaxVotes>0.75) {
      return(data.table(Sector=sector,PFA=whichMaxVotesPFA))
   } else {
      # nobody has more than 75%, record this for posterity as "multiple - N"
      return(data.table(Sector=sector,PFA=paste0("MULTI - ",nrow(pfaRanks))))
   }
}))

# examine the multis here
# remove the mutlis
sector_to_pfa[PFA %like% "MULTI",PFA:=NA]

setnames(sector_to_pfa,"PFA","ForceNameLong")
sector_to_pfa <- merge(sector_to_pfa,policeForceInfo[,c("ForceNameLong","ForceName")],by="ForceNameLong",all.x=T)

fwrite(sector_to_pfa,sector_to_pfa_fn,na=NA)