# this is for sanity checking, we want to check that our
# data extraction makes sense by manually examining it for a few people
# lets call this person Pete (made up name) to ground our reasoning
#petesID <- 37181
petesID <- 35548
pfdaPete <- pfda[RandomParticipantID==petesID&CohortGroup==2,
   .(RandomParticipantID,
     Source="Police",
     OffenceDate=ContactDate,
     OffenceDescription=HOOffenceDetailed,
     DidReoffend,
     ReoffendingCensorDate,
     PNCReoffenceCount,
     PNCFirstOffenceDate,
     PNCFirstCustodyDate
    )
]
pncPete <- pncOffencesUnique[RandomParticipantID==petesID,
   .(RandomParticipantID,
     Source="PNC",
     OffenceDate=OffenceStartDate,
     OffenceDescription=HOOffenceCode,
     DidReoffend=NA,
     ReoffendingCensorDate=NA,
     PNCReoffenceCount=NA,
     PNCFirstOffenceDate=NA,
     PNCFirstCustodyDate=NA
    )
] 
# i want to create a view that makes it easy to see the whole interleaved
# sequence, we will put each datasets columns
peteAll <- rbindlist(list(pfdaPete,pncPete))

# check that all force areas match
a <- pfda[,.(RandomParticipantID,ForceName)]
a[ForceName=="WestYorksExWakefield",ForceName:="WestYorks"]
a[ForceName=="Wakefield",ForceName:="WestYorks"]
b <- unique(pncOffences[,.(RandomParticipantID,ForceDescription)])
b[,ForceDescription:=str_replace_all(ForceDescription," ","")]
b[ForceDescription=="WestYorkshire",ForceDescription:="WestYorks"]
b[ForceDescription=="AvonandSomerset",ForceDescription:="AvonAndSomerset"]
b[ForceDescription=="WestMidlands",ForceDescription:="WestMids"]

d <- merge(a,b,by="RandomParticipantID",all.x=T)

allForcesMatch <- d[!is.na(ForceDescription),all(ForceName==ForceDescription)]
if(allForcesMatch) {
   print("All forces match between police and PNC data linkage!")
}
