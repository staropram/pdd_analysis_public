forceList <- c("Durham","ThamesValley","WestMids")
tables <- lapply(1:2,function(group){
   d <- copy(pfda[CohortGroup==group&ForceName %in% forceList])
   d[is.na(HOOutcomeCodeLong),HOOutcomeCodeLong:="NA"]
   
   r <- d[,list(
      .N,
      OC01=sum(HOOutcomeCodeLong=="OC01"),
      OC03=sum(HOOutcomeCodeLong=="OC03"),
      OC08=sum(HOOutcomeCodeLong=="OC08"),
      OC22=sum(HOOutcomeCodeLong=="OC22"),
      Other=sum(! HOOutcomeCodeLong %in% c("OC01","OC03","OC08","OC22"))
   ),by=ForceName]
   
   setorder(r,ForceName)
   r
})