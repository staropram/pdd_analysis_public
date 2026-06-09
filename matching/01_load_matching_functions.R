# this script contains the definitions of the matching functions
# and allows us to construct matched controls in the later scripts

source("../common/r/file_paths.R")
library(lubridate)

# we don't use all the columns to match on
columnsUsedInMatch <- c("AgeAtLastBirthday","Sex","ContactDate","HOOffenceGroup")

# gets k matches from the donorPool for 
# participant with ID interventionGroupedIncidentID
getMatches <- function(
   basePFDA, # the base data to use for matching (can use imputed versions)
   interventionGroupedIncidentID, # intervention ID to get matches for
   donorPool, # pool to obtain matches from
   k) {
   
   interventionParticipant <- unique(basePFDA[GroupedIncidentID==interventionGroupedIncidentID,..columnsUsedInMatch])
   if(nrow(interventionParticipant)!=1) {
      stop("Error, participant is not unique")
   }
   
   # this is for when we need to return an empty match
   emptyMatch <-  data.table(
      InterventionGroupedIncidentID=interventionGroupedIncidentID,
      ControlGroupedIncidentID=NA
   )
   
   # start matching
   
   # get age matches (+/- 3 years)
   ageMin <- interventionParticipant$AgeAtLastBirthday - 3
   ageMax <- interventionParticipant$AgeAtLastBirthday + 3
   controlMatches <- donorPool[ ageMin<=AgeAtLastBirthday & AgeAtLastBirthday<=ageMax]
   #print(paste0(nrow(controlMatches)," matches after matching on Age"))
   
   # sex matches
   controlMatches <- controlMatches[Sex==interventionParticipant$Sex]
   #print(paste0(nrow(controlMatches)," matches after matching on Sex"))
   
   # Contact date matches
   contactDateMin <- interventionParticipant$ContactDate - days(30)
   contactDateMax <- interventionParticipant$ContactDate + days(30)
   controlMatches <- controlMatches[contactDateMin<=ContactDate & ContactDate<=contactDateMax]
   #print(paste0(nrow(controlMatches)," matches after matching on ContactDate"))
   
   # HO offence group matches
   controlMatches <- controlMatches[HOOffenceGroup==interventionParticipant$HOOffenceGroup]
   #print(paste0(nrow(controlMatches)," matches after matching on HOOffenceGroup"))
   
   # select k matches randomly 
   # check if we can actually take the full sample
   if(k>nrow(controlMatches)) {
      #print(paste0("Cannot get ",k," matches, got ",nrow(controlMatches)))
      # if we can't, return an empty match
      return(emptyMatch)
   }
   finalMatches <- sample(controlMatches$GroupedIncidentID,k)
   
   data.table(
      InterventionGroupedIncidentID=interventionGroupedIncidentID,
      ControlGroupedIncidentID=list(finalMatches)
      #ControlGroupedIncidentID=paste0(finalMatches,collapse=":")
   )
}

getMatchedControls <- function(
      basePFDA, # the base analysis data to operate on (can be imputed version)
      forceList, # the list of forces to produce matches for
      cohortGroup, # the cohort group to produce matches for
      k, # the match ratio: number of matches per individual in the target force
      writeOutput=F, # save the output (if saved will not regen unless
                     # below flag is set
      forceRegen=F,# force-regeneration do not reload existing
      imputationStrategyName # used to name the files, can be anything
      # but the idea is to use it to differentiate different imputation strats
      ) {
   
   # determine the donor pool
   # ITT case: any same group incident from a control force
   if(cohortGroup==1) {
      donorPool <- basePFDA[G1DiversionForce==F&CohortGroup==1]
   } else {
      donorPool <- basePFDA[G2DiversionForce==F&CohortGroup==2]
   }
   
   matchedControls <- lapply(forceList,function(interventionForce) {
      print(paste0("Creating matched controls for ",interventionForce," group ",cohortGroup,", and k=",k," and imputation strategy ",imputationStrategyName))
      
      # check if they have already been generated
      if(writeOutput) {
         outFN <-  analysisDataPaths[[interventionForce]]$PlusMatchedControl[[cohortGroup]]
         outFNStats <- paste0(outFN,"_MR",k,"_Imputation-",imputationStrategyName,"_STATS.feather")
         outFN <- paste0(outFN,"_MR",k,"_Imputation-",imputationStrategyName,".feather")
         if(!forceRegen) {
            if(file.exists(outFN)&file.exists(outFNStats)) {
               print("Matched controls already exist, loading")
               print(outFN)
               return(list(
                  data = read_feather(outFN),
                  stats = read_feather(outFNStats)
               ))
            }
         }
      }
      
      # get the current intervention force individuals for the cohort group of 
      # interest, note we use the rowID here because we care about each incident
      forceIncidentIDs <- basePFDA[ForceName==interventionForce&CohortGroup==cohortGroup,unique(GroupedIncidentID)]
      
      # match each individual in the current intervention force
      print("Getting matches")
      forceMatches <- rbindlist(lapply(forceIncidentIDs,function(interventionID) {
         getMatches(basePFDA,interventionID,donorPool,k)
      }))
      
      
      # make a note of the individuals that could be matched and those that could not
      matchableInterventionIDs <- forceMatches[!is.na(ControlGroupedIncidentID),InterventionGroupedIncidentID]
      unmatchableInterventionIDs <- forceMatches[is.na(ControlGroupedIncidentID),InterventionGroupedIncidentID]
      
      # for the ones that did match, make a note of who they matched to
      matchedControlToInterventionMap <- forceMatches[
         !is.na(ControlGroupedIncidentID),
         list(ControlGroupedIncidentID=unlist(ControlGroupedIncidentID)),
         by=InterventionGroupedIncidentID]
      # rename these things so that it makes sense for a join
      setnames(matchedControlToInterventionMap,"ControlGroupedIncidentID","GroupedIncidentID")
      setnames(matchedControlToInterventionMap,"InterventionGroupedIncidentID","MatchedTo")
      setcolorder(matchedControlToInterventionMap,"GroupedIncidentID")
      
      
      # convert the table of control matches to a list of individuals 
      matchedControlIncidentIDs <- unlist(
         forceMatches[InterventionGroupedIncidentID %in% matchableInterventionIDs,ControlGroupedIncidentID]
      )
      
      # include both the intervention individuals and their control matches in the final selection
      #finalIncidentIDTable <- data.table(GroupedIncidentID=c(matchableInterventionIDs,matchedControlIncidentIDs))
      
      # finalMatch
      fakeInterventionTable <- data.table(
         GroupedIncidentID=matchableInterventionIDs,
         MatchedTo=NA
      )
      finalIncidentIDTable2 <- rbindlist(list(
         fakeInterventionTable,
         matchedControlToInterventionMap
      ))
      
      # make a version of the data for analysis that includes individuals from this 
      # force that have matches and those matches
      # note we have to use a join otherwise we lose rows
      #newForceData <- basePFDA[finalIncidentIDTable,on="GroupedIncidentID",nomatch=0]
      newForceData <- basePFDA[finalIncidentIDTable2,on="GroupedIncidentID",nomatch=0]
      
      # create a stats object to return also
      statsObject <- data.table(
         ForceName=interventionForce,
         K=k,
         CohortGroup=cohortGroup,
         ForceN=nrow(basePFDA[ForceName==interventionForce&CohortGroup==cohortGroup]),
         DonorPoolN=nrow(donorPool),
         Unmatchable=length(unmatchableInterventionIDs),
         `%`=round(length(unmatchableInterventionIDs)/nrow(basePFDA[ForceName==interventionForce])*100,digits=2),
         `Control reuse %`=round((1-uniqueN(matchedControlIncidentIDs)/length(matchedControlIncidentIDs))*100,digits=2),
         NewSize=nrow(newForceData)
      )
      
      # save this for analysis
      if(writeOutput==T) {
         #write.table(newForceData,file=outFN,row.names=F,sep="\t")
         print("Writing output")
         write_feather(newForceData,outFN)
         write_feather(statsObject,outFNStats)
      }
      
      list(
         data = newForceData,
         stats = statsObject
      )
   })
   
   names(matchedControls) <- forceList
   matchedControls
}