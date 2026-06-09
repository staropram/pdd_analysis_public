
# extract relevant vars
# we can produce stats for incidents or individuals, the flag below controls this
produceIndividualStats <- F
if(produceIndividualStats==T) {
   pd <- unique(pfda[,.(
      AgeAtLastBirthday,
      Sex,
      EthnicityCoarse,
      WasDiverted=any(WasDivertedOrOC22),
      WasCriminalised=any(WasCriminalised),
      OffenceSimplified2
   ),by=PseudoID_PFD])
} else {
   pd <- pfda[,.(
      CohortGroup,
      AgeAtLastBirthday,
      Sex,
      EthnicityCoarse,
      WasDiverted,
      WasCriminalised,
      OffenceSimplified2
   )]
}

setnames(pd,"AgeAtLastBirthday","Age")

pd[is.na(Sex),Sex:="NA"]
#pd[is.na(WasDiverted),WasDiverted:="NA"]
pd[is.na(EthnicityCoarse),EthnicityCoarse:="NA"]
pd[,EthnicityCoarse:=factor(EthnicityCoarse,levels=c("WHITE","ASIAN","BLACK","MIXED","OTHER","NA"))]

# create age categories
pd[, AgeCategory :=  cut(
   Age,
   breaks = c(18,25, 35, 45, 55, 65, 100),
   right = T,  # Exclude the right boundary
   include.lowest=T,
   ordered_result=T
)]



getStat <- function(colName,matchVal) {
   pd[,list(
     Value=sprintf(
        "%.f (%.1f %%)",
         sum(get(colName)==matchVal,na.rm=T),
         mean(get(colName)==matchVal,na.rm=T)*100
     )
   )]
}

# sex stats
sexStats <- pd[,.N,by=Sex]
sexStats[,`%`:=round(100*N/sum(N),digits=1)]
sexStats[,Variable:="Sex"]
setnames(sexStats,"Sex","Category")

# age stats
ageStats <- pd[,.N,by=AgeCategory]
ageStats[,`%`:=round(100*N/sum(N),digits=1)]
ageStats[,Variable:="Age"]
setorder(ageStats,AgeCategory)
setnames(ageStats,"AgeCategory","Category")
# add in median and IQR
AgeQ1 <- quantile(pd$Age,0.25)
AgeQ2 <- quantile(pd$Age,0.75)
AgeMedian <- median(pd$Age)
ageStats <- rbindlist(list(
   pd[,list(Category="Median [IQR]",N=sprintf("%.1f [%.1f-%.1f]",AgeMedian,AgeQ1,AgeQ2),`%`="",Variable="Age")],
   ageStats
))

# eth stats
ethStats <- pd[,.N,by=EthnicityCoarse]
ethStats[,`%`:=round(100*N/sum(N),digits=1)]
ethStats[,Variable:="Ethnicity"]
setorder(ethStats,EthnicityCoarse)
setnames(ethStats,"EthnicityCoarse","Category")

# diverted stats
divStats <- pd[,.N,by=WasDiverted]
divStats[,`%`:=round(100*N/sum(N),digits=1)]
divStats[,Variable:="Diverted"]
setnames(divStats,"WasDiverted","Category")

# crim stats
crimStats <- pd[,.N,by=WasCriminalised]
crimStats[,`%`:=round(100*N/sum(N),digits=1)]
crimStats[,Variable:="WasCriminalised"]
setnames(crimStats,"WasCriminalised","Category")

# drug stats
drugStats <- pd[,.N,by=OffenceSimplified2]
drugStats[,`%`:=round(100*N/sum(N),digits=1)]
drugStats[,Variable:="Drug"]
setnames(drugStats,"OffenceSimplified2","Category")
setorder(drugStats,-N)

categoryStats <- rbindlist(list(
   sexStats,
   ageStats,
   ethStats,
   divStats,
   crimStats,
   drugStats
))
setcolorder(categoryStats,c("Variable"))


print(categoryStats)