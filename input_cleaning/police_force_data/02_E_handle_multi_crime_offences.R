# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

forceReload <- F
if(!exists("pfd")|forceReload) {
   pfd <- data.table(arrow::read_feather(PFD_complete_fn_FEATHER))
}

if(!"OffenceReferralProbability" %in% names(pfd)) {
   # compute the prior probability of a given offence receiving a referral
   # as we will use this to differentiate relevant offences
   offenceReferralProbability <- pfd[,list(OffenceReferralProbability=sum(WasDiverted,na.rm=T)/.N),by=OffenceCode]
   pfd <- merge(pfd,offenceReferralProbability,by="OffenceCode",all.x=T)
}

if(!"OffenceProbability" %in% names(pfd)) {
   # compute the prior probability of a given offence receiving a referral
   # as we will use this to differentiate relevant offences
   offenceProbability <- pfd[,list(OffenceProbability=.N/nrow(pfd)),by=OffenceCode]
   pfd <- merge(pfd,offenceProbability,by="OffenceCode",all.x=T)
}

# the issue here is that fairly often someone will commit multiple crimes
# in a given "event", imagine for example they are caught with goods from
# multiple shops and each is logged as a separate crime, or if someone
# assaults a police officer while being arrested for another crime

# we want to be able to deal with these cases in some sensible manner
# the suggestion is that we take the most serious/relevant offence in each case
# and delete the others

# we might also want to give preference to diverted individuals since they
# are important in the secondary analysis and the IV analysis

# first of all we need to identify these multi-crime events, this is where
# the PseudoID and ContactDate match


# we don't want to examine all columns as some are correlated exactly like
# the home office outcome code and the home office outcome code description
# so compare a subset
columnsToExamine <- names(pfd)
# this is an exclusionary list
columnsToExamine <- columnsToExamine[!columnsToExamine %in% c(
   "Index",
   "ContactReason",
   "Ethnicity",
   "EthCoarse",
   "IC",
   "SDE",
   "OutcomeType",
   "HOOutcomeCodeDetailed",
   "Offence",
   "OffenceGroup",
   "OffenceType",
   "OffenceDetailed",
   "DrugClass",
   "DrugType",
   "Group1",
   "Group2",
   "OffenceReferralProbability",
   "OffenceProbability",
   "SameDayPath",
   "CensorDate"
)]

# given a set of rows, show which columns they differ by (or empty string if they do not)
# columnSet is the set of columns to use, default is to use all names
whichColumnsDiffer <- function(d,columnSet=names(d)) {
   differingCols <- unlist(lapply(columnSet,function(colName) {
      # if a column has more than one unique value then it differs
      if(uniqueN(d[,..colName])==1) {
         return(NULL)
      }
      colName
   }))
   differingCols
}


# mark which rows are part of multi-offence clusters
pfd[,IsMultiOffence:=F]
# have a column which will indicate which offences to remove
pfd[,ToBeRemoved:=F]
multi_offence_mask <- 
   duplicated(pfd,by=c("IncidentID")) |
   duplicated(pfd,by=c("IncidentID"),fromLast=T)
pfd[multi_offence_mask,IsSameDayOffence:=T]

# give each row an index for the purpose of the algorithm that 
# removes rows so it can act directly on PFD
pfd[,Index:=.I]

# create a view of the data that is just the same day offences
sameDayOffences <- pfd[IsSameDayOffence==T]

# create a variable thats a concatenation of offences present in each cluster
sameDayOffences[,OffenceList:=paste0(OffenceDetailed,collapse=" <br/> "),by=IncidentID]
sameDayOffences[,OffenceListShort:=paste0(OffenceCode,collapse="|"),by=IncidentID]

sameDayOffencesG1 <- sameDayOffences[CohortGroup==1,c("ContactDate","OffenceListShort","FirstName","OffenceDetailed","OffenceCode","WasDiverted","WasArrested","HOOutcomeCodeDetailed","ForceName")]
sameDayOffencesG2 <- sameDayOffences[CohortGroup==2,c("ContactDate","OffenceListShort","FirstName","OffenceDetailed","OffenceCode","WasDiverted","WasArrested","HOOutcomeCodeDetailed","ForceName")]

# anyone record that is in a same day offence cluster gets a flag "HasAnotherOffence"
sameDayOffences[,HasAnotherOffence:=T]


outcomeRank <- data.table(
   Outcome=c("OC01","OC1A","OC02","OC2A","OC03","OC3A",
                            paste0("OC",str_pad(4:22,2,pad="0"))),
   Rank=1:25
)

# takes a list of outcome codes and returns the most relevant
mostRelevantOutcome <- function(outcomeList) {
   ranks <- outcomeRank[Outcome %in% outcomeList]
   unique(ranks[which.max(ranks$Rank),Outcome])
}

# takes a list of offence codes and referral probabilities 
# returns the most relevant offence
mostRelevantOffence <- function(offences) {
   # if the offences are all the same just return the 1st offence
   if(uniqueN(offences$OffenceCode,na.rm=T)==1) {
      return(unique(offences$OffenceCode))
   }
   
   # if the offence referral probabilities differ, pick the one with
   # highest offence referral probability
   if(uniqueN(offences$OffenceReferralProbability)!=1) {
      offences[,Rank:=rank(OffenceReferralProbability,ties="min")]
      return(unique(offences[Rank==max(Rank),OffenceCode]))
   }
   
   # if the offence probabilities differ
   if(uniqueN(offences$OffenceProbability)!=1) {
      offences[,Rank:=rank(OffenceProbability,ties="min")]
      return(unique(offences[Rank==max(Rank),OffenceCode]))
   }
   
   # otherwise choose randomly?
   browser()
}
pfd[,SameDayPath:=""]

## XXX what we are going to do is just combine everything by the semantics
## of an "incident" which means (after splitting into G1,G2)
## 1. WasArrested and WasDiverted "Propogate" to other all other rows
## 2. Offence becomes the most relevant offence for diversion
## 3. Outcome becomes the most severe outcome 
sameDayResolverV3 <- function(d) {
   # if this is called with just one offence, the issue is automatically resolved
   if(nrow(d)==1) {
      #print("Only 1 offence left, keeping it")
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"A")]
      return()
   }
   
   # if this is not just one offence, then make sure to set the flag
   # HasAnotherOffence for every offence in the incident
   pfd[d$Index,MultiOffenceIncident:=T]
   
   # if the offences are mix of group 1 and group 2 
   # rows then deal with these separately
   if(uniqueN(d$CohortGroup)!=1) {
      #print("Splitting up group 1 and group 2 offences")
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"B")]
      sameDayResolverV3(d[CohortGroup==1])
      sameDayResolverV3(d[CohortGroup==2])
      return()
   }
   
   # propagate WasDiverted to all rows of the incident
   if(any(d$WasDiverted,na.rm=T)) {
      d[,WasDiverted:=T]
   }
   
   # propagate WasArrested to all rows of the incident
   if(any(d$WasArrested,na.rm=T)) {
      d[,WasArrested:=T]
   }
   
   # choose the most relevant offence,
   mrOffence <- mostRelevantOffence(d[,c("OffenceCode","OffenceDetailed","OffenceReferralProbability","OffenceProbability")])
   
   # choose the most relevant outcome
   mrOutcome <- mostRelevantOutcome(d$HOOutcomeCodeLong)
   
   # now we remove all the offences except one and then change 
   # the remaining offence to reflect the most relevant from
   # the incident chosen above
   indicesToRemove <-  d[1:(nrow(d)-1),Index]
   pfd[indicesToRemove,ToBeRemoved:=T]
   
   # copy the relevant offence and outcome measures
   indexToKeep <- d[nrow(d),Index]
   
   #x <- pfd[indexToKeep]
   # copy offence data of offence being kept
   if(length(mrOffence)==0) {
      browser()
   } else {
      newOffenceType = unique(d[OffenceCode==mrOffence,OffenceType])
      newOffence = unique(d[OffenceCode==mrOffence,Offence])
      newOffenceDetailed = unique(d[OffenceCode==mrOffence,OffenceDetailed])
      newOffenceGroup  = unique(d[OffenceCode==mrOffence,OffenceGroup])
      pfd[indexToKeep,OffenceCode:=mrOffence]
      pfd[indexToKeep,OffenceDetailed:=newOffenceDetailed]
      pfd[indexToKeep,Offence:=newOffence]
      pfd[indexToKeep,OffenceType:=newOffenceType]
      pfd[indexToKeep,OffenceGroup:=newOffenceGroup]
      pfd[indexToKeep,OffenceReferralProbability:=unique(d[OffenceCode==mrOffence,OffenceReferralProbability])]
      pfd[indexToKeep,OffenceProbability:=unique(d[OffenceCode==mrOffence,OffenceProbability])]
   }
   
   # copy outcome code info into offence being kept
   if(length(mrOutcome)==0) {
      pfd[indexToKeep,HOOutcomeCodeLong:=NA]
      pfd[indexToKeep,HOOutcomeCodeDetailed:=NA]
   } else {
      pfd[indexToKeep,HOOutcomeCodeLong:=mrOutcome]
      pfd[indexToKeep,HOOutcomeCodeDetailed:=unique(d[HOOutcomeCodeLong==mrOutcome,HOOutcomeCodeDetailed])]
   }
   #y <- pfd[indexToKeep]
   
   # check things are actually changed
   #if(x$OffenceCode!=y$OffenceCode) {
   #   browser()
   #}
   
}

# recursive resolver for same day offences (version 2)
# we only need to keep track of those offences that need to be removed
# otherwise just leave them alone
# in this case we treat each group as an "incident"
sameDayResolverV2 <- function(d) {
   
   ## XXX what we are going to do is just combine everything by the semantics
   ## of an "incident" which means (after splitting into G1,G2)
   ## 1. WasArrested and WasDiverted "Propogate" to other all other rows
   ## 2. Offence becomes the most relevant offence for diversion
   ## 3. Outcome becomes the most severe outcome 
   
   # if this is called with just one offence, the issue is automatically resolved
   if(nrow(d)==1) {
      #print("Only 1 offence left, keeping it")
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"A")]
      return()
   }
   
   # if the offences are mix of group 1 and group 2 
   # rows then deal with these separately
   if(uniqueN(d$CohortGroup)!=1) {
      #print("Splitting up group 1 and group 2 offences")
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"B")]
      sameDayResolverV2(d[CohortGroup==1])
      sameDayResolverV2(d[CohortGroup==2])
      return()
   }
   
   # propagate WasDiverted to all rows of the incident
   if(any(d$WasDiverted,na.rm=T)) {
      d[,WasDiverted:=T]
   }
   
   # propagate WasArrested to all rows of the incident
   if(any(d$WasArrested,na.rm=T)) {
      d[,WasArrested:=T]
   }
   
   
   ### XXX check if one outcome is NA, it looks like AvonAndSomerset might 
   ### leave it blank if one row resulted in diversion
   
   # which columns do the rows differ by (sorted into alphabetical order)
   differingCols <- sort(whichColumnsDiffer(d,columnsToExamine))
   differingColsString <- paste(differingCols,collapse=",")
   
   # if we get here we are dealing with multiple offences of either
   # group 1 or group 2
   
   # are the offences all of the same type
   if(uniqueN(d$OffenceCode)==1) {
      #print("Identical offences")
      
      # if no columns are different this means the rows are duplicates
      # this can happen because some police forces have categorise
      # offences internally in a way that is not consistent with HO codes
      # so we just pick the first offence in this case
      if(is.null(differingCols)) {
         #print("No columns are different, just keep one offence")
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"C")]
         indicesToRemove <- d[1:(nrow(d)-1),Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         return()
      }
      
      # rows that differ by only one column
      if(length(differingCols)==1) {
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"D")]
         # do they differ only by outcome type?
         if(differingCols=="HOOutcomeCodeLong") {
            #print("Rows differ only by outcome type, choosing most relevant")
            mro <- mostRelevantOutcome(d$HOOutcomeCodeLong)
            # delete the rows that do not correspond to the most relevant outcome
            # and recurse on the remaining rows
            indicesToRemove <- d[HOOutcomeCodeLong!=mro,Index]
            pfd[indicesToRemove,ToBeRemoved:=T]
            pfd[d$Index,SameDayPath:=paste0(SameDayPath,"1")]
            return(sameDayResolverV2(d[HOOutcomeCodeLong==mro]))
         }
         browser()
      }
   
      browser()
   }
   
   # Offences are different
   
   # rows that differ by only one column
   if(length(differingCols)==1) {
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"F")]
      # only the offence is different, same outcome
      if(differingCols=="OffenceCode") {
         #print("Rows differ only by outcome type, choosing most relevant")
         mro <- mostRelevantOffence(d[,c("OffenceCode","OffenceDetailed","OffenceReferralProbability","OffenceProbability")])
         # remove the less relevant offences
         indicesToRemove <- d[OffenceCode!=mro,Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         # recurse on the remainder
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"1")]
         return(sameDayResolverV2(d[OffenceCode==mro]))
      }
      browser()
   }
   
   # rows differ by more than once column
   pfd[d$Index,SameDayPath:=paste0(SameDayPath,"G")]
   
   # if OffenceCode is one of those columns, then arbitrate based on it
   if("OffenceCode" %in% differingCols) {
      #print("Rows differ only by outcome type, choosing most relevant")
      mro <- mostRelevantOffence(d[,c("OffenceCode","OffenceDetailed","OffenceReferralProbability","OffenceProbability")])
      # remove the less relevant offences
      indicesToRemove <- d[OffenceCode!=mro,Index]
      pfd[indicesToRemove,ToBeRemoved:=T]
      # recurse on the remainder
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"1")]
      return(sameDayResolverV2(d[OffenceCode==mro]))
   }
   browser()
}

# recursive resolver for same day offences
# we only need to keep track of those offences that need to be removed
# otherwise just leave them alone
sameDayResolverV1 <- function(d) {
   
   # if this is called with just one offence, the issue is automatically resolved
   if(nrow(d)==1) {
      #print("Only 1 offence left, keeping it")
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"A")]
      return()
   }
   
   # if the offences are mix of group 1 and group 2 
   # rows then deal with these separately
   if(uniqueN(d$CohortGroup)!=1) {
      #print("Splitting up group 1 and group 2 offences")
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"B")]
      sameDayResolverV1(d[CohortGroup==1])
      sameDayResolverV1(d[CohortGroup==2])
      return()
   }
   
   # which columns do the rows differ by (sorted into alphabetical order)
   differingCols <- sort(whichColumnsDiffer(d,columnsToExamine))
   differingColsString <- paste(differingCols,collapse=",")
   #print("Rows differ by")
   #print(differingCols)
   
   # if we get here we are dealing with multiple offences of either
   # group 1 or group 2
   
   # are the offences all of the same type
   if(uniqueN(d$OffenceCode)==1) {
      #print("Identical offences")
      
      # if no columns are different this means the rows are duplicates
      # this can happen because some police forces have categorise
      # offences internally in a way that is not consistent with HO codes
      # so we just pick the first offence in this case
      if(is.null(differingCols)) {
         #print("No columns are different, just keep one offence")
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"C")]
         indicesToRemove <- d[1:(nrow(d)-1),Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         return()
      }
      
      # rows that differ by only one column
      if(length(differingCols)==1) {
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"D")]
         # do they differ only by outcome type?
         if(differingCols=="HOOutcomeCodeLong") {
            #print("Rows differ only by outcome type, choosing most relevant")
            mro <- mostRelevantOutcome(d$HOOutcomeCodeLong)
            # delete the rows that do not correspond to the most relevant outcome
            # and recurse on the remaining rows
            indicesToRemove <- d[HOOutcomeCodeLong!=mro,Index]
            pfd[indicesToRemove,ToBeRemoved:=T]
            pfd[d$Index,SameDayPath:=paste0(SameDayPath,"1")]
            return(sameDayResolverV1(d[HOOutcomeCodeLong==mro]))
         }
         
         # do they differ only by diversion status?
         if(differingCols=="WasDiverted") {
            # delete the rows that are not diverted
            indicesToRemove <- d[WasDiverted==F,Index]
            pfd[indicesToRemove,ToBeRemoved:=T]
            # recurse on the remaining rows
            pfd[d$Index,SameDayPath:=paste0(SameDayPath,"2")]
            return(sameDayResolverV1(d[WasDiverted==T]))
         }
      }
      
      # rows differ by more than one column so priorities need to be implemented
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"E")]
      
      # Most relevant event is a diversion
      if("WasDiverted" %in% differingCols) {
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"1")]
         #print("Keeping diversion row")
         # remove rows that are not diverted
         indicesToRemove <- d[WasDiverted==F,Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         # recurse on the remaining rows
         return(sameDayResolverV1(d[WasDiverted==T]))
      }
      
      # next is whether they were arrested
      if("WasArrested" %in% differingCols) {
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"2")]
         #print("Keeping arrested row")
         # remove rows that are not arrested
         indicesToRemove <- d[WasArrested==F,Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         # recurse on the remaining rows
         return(sameDayResolverV1(d[WasArrested==T]))
      }
      
      # need to add a row id to modify the data outside of the lapply-function env
      
   }
   
   # Offences are different
   
   # rows that differ by only one column
   if(length(differingCols)==1) {
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"F")]
      # only the offence is different, same outcome
      if(differingCols=="OffenceCode") {
         #print("Rows differ only by outcome type, choosing most relevant")
         mro <- mostRelevantOffence(d[,c("OffenceCode","OffenceDetailed","OffenceReferralProbability","OffenceProbability")])
         # remove the less relevant offences
         indicesToRemove <- d[OffenceCode!=mro,Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         # recurse on the remainder
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"1")]
         return(sameDayResolverV1(d[OffenceCode==mro]))
      }
      browser()
   }
   
   
   # is was diverted one of the differing cols
   if(grepl("WasDiverted",differingColsString,fixed=T)) {
      # is WasArrested also one of the differing cols?
      if(grepl("WasArrested",differingColsString,fixed=T)) {
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"L")]
         # if suspect is arrested AND diverted then this is more relevant
         arrestedAndDiverted <- d[WasArrested==T&WasDiverted==T]
         if(nrow(arrestedAndDiverted)!=0) {
            pfd[d$Index,SameDayPath:=paste0(SameDayPath,"1")]
            return(sameDayResolverV1(arrestedAndDiverted))
         }
         
         # otherwise the diversion offence is more relevant
         indicesToRemove <- d[WasDiverted==F,Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"2")]
         return(sameDayResolverV1(d[WasDiverted==T]))
      }
      # we always choose the diverted case but
      # differentiate below to count the different pathways
      
      # OffenceCode and WasDiverted differ
      if(differingColsString=="OffenceCode,WasDiverted") {
            pfd[d$Index,SameDayPath:=paste0(SameDayPath,"J")]
         # choose the WasDiverted offences as more relevant
         indicesToRemove <- d[WasDiverted==F,Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         return(sameDayResolverV1(d[WasDiverted==T]))
      }
      
      # HOOutcomeCodeLong, OffenceCode, and WasDiverted differ
      if(differingColsString=="HOOutcomeCodeLong,OffenceCode,WasDiverted") {
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"K")]
         indicesToRemove <- d[WasDiverted==F,Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         # choose the WasDiverted offences as more relevant
         return(sameDayResolverV1(d[WasDiverted==T]))
      }
   }
   
   # was arrested one of the differing cols?
   if(grepl("WasArrested",differingColsString,fixed=T)) {
      
      # otherwise we don't care what else differs, choose the was
      # arrested column, 
      # will differentiate each case here but only to count
      # different combinations
   
      # OffenceCode and WasArrested differ
      if(differingColsString=="OffenceCode,WasArrested") {
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"H")]
         # flag the non arrested offences for removal
         
         # choose the WasArrested offences as more relevant
         indicesToRemove <- d[WasArrested==F,Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         return(sameDayResolverV1(d[WasArrested==T]))
      }
      
      # HOOutcomeCodeLong, OffenceCode, and WasArrested differ
      if(differingColsString=="HOOutcomeCodeLong,OffenceCode,WasArrested") {
          pfd[d$Index,SameDayPath:=paste0(SameDayPath,"I")]
         # choose the WasArrested offences as more relevant
         indicesToRemove <- d[WasArrested==F,Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         return(sameDayResolverV1(d[WasArrested==T]))
      }
      browser()
   }
   
   # both offence and outcome differ
   if(differingColsString=="HOOutcomeCodeLong,OffenceCode") {
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"G")]
      #print("Rows differ by offence and outcome type")
      # if any of the outcomes are 22 (diversion) choose those
      if("OC22" %in% d$HOOutcomeCodeLong) {
         pfd[d$Index,SameDayPath:=paste0(SameDayPath,"1")]
         # remove the outcomes that are not OC22
         indicesToRemove <- d[HOOutcomeCodeLong!="OC22",Index]
         pfd[indicesToRemove,ToBeRemoved:=T]
         return(sameDayResolverV1(d[HOOutcomeCodeLong=="OC22"]))
      }
      
      # otherwise choose the most relevant offence
      mro <- mostRelevantOffence(d[,c("OffenceCode","OffenceDetailed","OffenceReferralProbability","OffenceProbability")])
      # remove the less relevant offences
      indicesToRemove <- d[OffenceCode!=mro,Index]
      pfd[indicesToRemove,ToBeRemoved:=T]
      # recurse on the remainder
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"2")]
      return(sameDayResolverV1(d[OffenceCode==mro]))
   }
   
   # there is a rare case where WestYorks people can be classified
   # as in a diversion force and not which doesn't make sense
   # so this is an error correction, choose DiversionForce==T
   if(differingColsString=="DiversionForce,OffenceCode") {
      pfd[d$Index,SameDayPath:=paste0(SameDayPath,"M")]
      return(sameDayResolverV1(d[DiversionForce==T]))
   }
   
   browser()
}

lapply(unique(sameDayOffences$IncidentID),function(id){
   sameDayResolverV3(sameDayOffences[IncidentID==id])
})


#PFD_69bdfab68bf578c127b342bd3ad4f2f0

#examineSameDayOffences <- function() {
sameDayOffencePrevalenceG1 <- sameDayOffences[CohortGroup==1,.N,by=c("OffenceList","OffenceListShort")]
setorder(sameDayOffencePrevalenceG1,-N)
sameDayOffencePrevalenceG1[,NPercent:=round(N/sum(N)*100,digits=2)]
sameDayOffencePrevalenceG1[,NPercentSum:=round(cumsum(NPercent),digits=2)]
#}


findClusterDifferences <- function() {
   
   # iterate over these clusters to find out which columns differ
   # for each cluster
   
   columnDifferences <- rbindlist(lapply(pfd[IsMultiOffence==T,IncidentID],function(id) {
      currentCluster <- pfd[IncidentID==id]
      differingCols <- unlist(lapply(columnsToExamine,function(colName) {
         # if a column has more than one unique value then it differs
         if(uniqueN(currentCluster[,..colName])==1) {
            return(NULL)
         }
         colName
      }))
      if(length(differingCols)>1) {
      }
      x <- data.table(IncidentID=id,DifferingCols=do.call(purrr::partial(paste,sep=","),as.list(differingCols)))
   }))
   
   # now create a summary table
   summaryColumnDifferences <- columnDifferences[,.N,by=DifferingCols]
   setorder(summaryColumnDifferences,-N)
   
   examineColDiffCategory <- function(colDiff)  {
      entryIDs <- columnDifferences[DifferingCols==colDiff,IncidentID]
      selection <- pfd[IncidentID %in% entryIDs]
      setcolorder(selection,c("ContactDate","FirstName","LastName","OffenceDetailed","HOOutcomeCodeDetailed","WasArrested","WasDiverted"))
      View(selection)
   }
}

# remove the rows we are supposed to remove
pfd <- pfd[ToBeRemoved==F]