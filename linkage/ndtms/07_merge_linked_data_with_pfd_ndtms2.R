# this script takes a linkage model and merges the relevant NDTMS variables into
# the pfd data

## ---- Setup
# required libs
library(data.table)
library(knitr)
library(DT)
library(readxl)
library(fst)
library(digest)
library(lubridate)

# flag to control whether pfd data should be reloaded with every execution of the script
forceReload<-T

# load in the paths of all required input and output files
source("../../common/r/file_paths.R")

# load in the OHID drug code to drug map
drugCodeToDrug<-data.table(readxl::read_xlsx("../../common/data/internal/ohid_drug_codes.xlsx"))

whatDrug<-function(drugCode) {
   drugCodeToDrug[code==drugCode,value]
}

whatDrugGroup<-function(drugCode) {
   if(is.na(drugCode))  {
      return("None")
   }
   drugCodeToDrug[code==drugCode,drug_group_value]
}

# read the pfd data in
if(!exists("pfd")|forceReload) {
   print("Reading in clean police force data (pfd)")
   pfd<-data.table(arrow::read_feather(PFD_complete_fn_FEATHER))
   pfd[,PseudoID_PFD:=PseudoID]
   
   # make sure we have censor dates correct before we do this join
   drugTreatmentFollowupDate <- as.Date("2025-09-30")
   
   ## 1. ensure ordering, note we want to oder also by CohortGroup
   # because we set the exposure window per contact group
   #setorder(pfd, PseudoID_PFD, ContactDate)
   setorder(pfd,CohortGroup,PseudoID_PFD,ContactDate)
   pfd[, NextContactDate := shift(ContactDate, type = "lead"), by = .(CohortGroup,PseudoID_PFD)]
   pfd[, DrugTreatmentCensorDate := fifelse(!is.na(NextContactDate), NextContactDate, drugTreatmentFollowupDate)]
   
   ## 2. Contact-level table (one row per contact per person)
   #contacts <- unique(pfd[, .(PseudoID_PFD, ContactDate)])
   #setorder(contacts, PseudoID_PFD, ContactDate)
   
   ## 3. Next contact date of ANY type (or admin censor if last)
   #contacts[, NextContactDate :=
   #            shift(ContactDate, type = "lead", fill = projectEndDate),
   #         by = PseudoID_PFD]
   
   ## 4. Join baseline censor date back to all offences at that contact
   #pfd[contacts,
   #     on = .(PseudoID_PFD, ContactDate),
   #     CensorDate := i.NextContactDate]
}

forceReload<-F
# load in NDTMS data (only those linked)
if(!exists("ndtms_linked_only")|forceReload) {
   ndtms_linked_only<-data.table(arrow::read_feather(NDTMS_with_only_PFD_participants_fn_FEATHER_ndtms2))
   # we only care about certain columns in the merge
   columnsOfInterest<-c("PseudoID_NDTMS","PseudoID_PFD",
                          "attrbdat","dat","agncy","la","pc","dob",
                          "ethnic","triaged_jy","n_jy","disd_jy",
                          "disrsn_ep","disrsn_jy","triaged","drug1","drug2","drug3")
   ndtms_linked_only<-ndtms_linked_only[,..columnsOfInterest]
   # rename some columns
   setnames(ndtms_linked_only,"attrbdat","PatientID")
   setnames(ndtms_linked_only,"agncy","Agency")
   setnames(ndtms_linked_only,"dat","AgencyDATCode")
   setnames(ndtms_linked_only,"la","LocalAuthority")
   setnames(ndtms_linked_only,"dob","DOB")
   setnames(ndtms_linked_only,"pc","PartialPostcode")
   setnames(ndtms_linked_only,"ethnic","EthnicityCode")
   setnames(ndtms_linked_only,"n_jy","JourneyIndex")
   setnames(ndtms_linked_only,"triaged_jy","JourneyStartDate")
   setnames(ndtms_linked_only,"disd_jy","JourneyEndDate")
   setnames(ndtms_linked_only,"disrsn_jy","JourneyDischargeReason")
   setnames(ndtms_linked_only,"disrsn_ep","EpisodeDischargeReason")
   setnames(ndtms_linked_only,"triaged","EpisodeTriagedDate")
   setnames(ndtms_linked_only,"drug1","Drug1Code")
   setnames(ndtms_linked_only,"drug2","Drug2Code")
   setnames(ndtms_linked_only,"drug3","Drug3Code")
}

# takes a 1-1 link prediction and creates a data.table merging PFD with
# NDTMS on these links
createLinkedDataForNDTMS<-function(linkPredictions,ndtmsExtract) {
   print("Extracting drug treatment status from NDTMS for linked PFD rows")
   # we want to know for each pfd row, do they have an NDTMS journey
   # that starts within the censorship window
   
   # add the NDTMS links into PFD
   pfd<-merge(pfd,linkPredictions[,c("PseudoID_PFD","PseudoID_NDTMS")],by="PseudoID_PFD",all.x=T)
   
   # create a new key which combines the pseudoID and the contact date
   #pfd[,IDAndDate:=paste0(PseudoID_PFD,ContactDate)]
   # no we want to iterate over all rows that are in NDTMS
   
   # set a variable for PFD indicating NDTMS presence
   pfd[,IsInNDTMS:=ifelse(!is.na(PseudoID_NDTMS),T,F)]
   
   # iterate over the rows of pfd that have an entry in NDTMS
   lapply(pfd[IsInNDTMS==T,GroupedIncidentID],function(gid){
      currentPFDRow <- pfd[GroupedIncidentID==gid]
      
      # if there is more than one row then this is a "same day offence" and
      # so the censor date etc will be the same
      if(nrow(currentPFDRow)>1) {
         browser()
      }
      
      # does there exist a row in NDTMS with a journeystart date within the window
      pfdCensorDate <- currentPFDRow$DrugTreatmentCensorDate
      pfdContactDate <- currentPFDRow$ContactDate
      pfdSuspectID <- currentPFDRow$PseudoID_PFD
      ndtmsLinkedID <- currentPFDRow$PseudoID_NDTMS
      
      # get the ndtms rows that correspond to this pseudoID_NDTMS
      ndtmsEntries <- unique(ndtmsExtract[PseudoID_NDTMS==ndtmsLinkedID])
      
      pfd[GroupedIncidentID==gid, DrugTreatmentJourneyCount:=uniqueN(ndtmsEntries$JourneyIndex)]
      
      # map the drugs they are in NDTMS for, this can change over time
      # at the individual level lets just use the latest drug we see
      # for the incident we can extract whatever the relevant journey says
      latestNDTMSI <- which.max(ndtmsEntries$JourneyStartDate)
      if(length(latestNDTMSI)>1) {
         browser()
      }
      drugs<-sapply(ndtmsEntries[latestNDTMSI,.(Drug1Code,Drug2Code,Drug3Code)],whatDrugGroup)
      pfd[GroupedIncidentID==gid,c("NDTMSDrugLatest1","NDTMSDrugLatest2","NDTMSDrugLatest3"):=as.list(drugs)]
      
      # record the first entry following their index date for later
      entriesFollowingIndex <- ndtmsEntries[JourneyStartDate>pfdContactDate]
      if(nrow(entriesFollowingIndex!=0)) {
         firstDate <- min(entriesFollowingIndex$JourneyStartDate)
         pfd[GroupedIncidentID==gid,FirstPostIndexDrugEntryDate:=firstDate]
      } 
      
      # compute the mean journey duration for this individual
      indexedJourneys <- ndtmsEntries[,list(
         EpisodeID = paste0(PseudoID_NDTMS,"_",EpisodeTriagedDate,"_",AgencyDATCode),
         Duration = JourneyEndDate-JourneyStartDate
      ),by=PseudoID_NDTMS]
      indexedJourneysUniqueDurations<-indexedJourneys[,list(Duration=first(Duration)),by=EpisodeID]
      pfd[GroupedIncidentID==gid,DrugTreatmentMeanJourneyDurationDays:=as.integer(mean(indexedJourneysUniqueDurations$Duration,na.rm=T))]
      
      # set data for individuals who are in a journey at the point of contact with the police
      withinContactData <- ndtmsEntries[
         # started a journey before and ended after
         (JourneyStartDate < pfdContactDate & JourneyEndDate >= pfdContactDate) |
         # started a journey before contact and have no registered end date
         (JourneyStartDate < pfdContactDate & is.na(JourneyEndDate) & is.na(EpisodeDischargeReason))
      ]
      inTreatmentAtPointOfContact <- F
      if(nrow(withinContactData)!=0) {
         inTreatmentAtPointOfContact <- T
         pfd[GroupedIncidentID==gid,IsInTreatmentAtPointOfContact:=T]
         # extract the drugs they were being treated for at the point of contact
         firstNDTMSI <- which.min(withinContactData$JourneyStartDate)
         drugs<-sapply(withinContactData[firstNDTMSI,.(Drug1Code,Drug2Code,Drug3Code)],whatDrugGroup)
         pfd[GroupedIncidentID==gid,c("NDTMSDrugIncident1","NDTMSDrugIncident2","NDTMSDrugIncident3"):=as.list(drugs)]
      }
      
      # we also want to know who had a journey in the last 28 days
      # we only need to look at end date as a journey that spans the contact
      # date will be captured by the other flag IsInTreatmentAtPointOfContact
      # or they enter treatment before 28 days prior and never exit
      dateOf28DaysPrior <- pfdContactDate - days(28)
      contactInLast28DaysData<-ndtmsEntries[
         # ended a journey within 28 days before index (can end on index)
         (JourneyEndDate>=dateOf28DaysPrior & JourneyEndDate<=pfdContactDate) |
         # start a journey within the last 28 days
         (JourneyStartDate>=dateOf28DaysPrior & JourneyStartDate<pfdContactDate) |
         # started before 28 days but never left
         (JourneyStartDate<dateOf28DaysPrior & is.na(JourneyEndDate)& is.na(EpisodeDischargeReason))
      ]
      inTreatmentInLast28Days <- F
      if(nrow(contactInLast28DaysData)>0) {
         inTreatmentInLast28Days <- T
         pfd[GroupedIncidentID==gid,WasInTreatmentWithin28DaysPriorToContact:=T]
         pfd[GroupedIncidentID==gid,Within28DaysPriorTreatmentEndInDays:=as.integer(pfdContactDate-max(contactInLast28DaysData$JourneyEndDate))/86400]
      }
      
      # set data for the individuals with journeys that begin after contact with the police
      # note that an individual isn't deemed to have entered into treatment if they
      # were already in treatment at the point of contact
      # nor are they in treatment 28 days before
      if(!inTreatmentAtPointOfContact & !inTreatmentInLast28Days) {
         postContactData <- ndtmsEntries[
            # start a journey strictly within the window
            (JourneyStartDate >= pfdContactDate) & (JourneyStartDate < pfdCensorDate)
         ]
         
         if(nrow(postContactData)!=0) {
            
            # if a person has more than one journey we only care about the first
            # journey that happens within the censorship window
            firstNDTMSI <- which.min(postContactData$JourneyStartDate)
            
            pfd[GroupedIncidentID==gid,
                DateEnteredIntoDrugTreatment:=postContactData[firstNDTMSI,JourneyStartDate]]
            pfd[GroupedIncidentID==gid,EnteredIntoDrugTreatment:=T]
            pfd[GroupedIncidentID==gid,
                DrugTreatmentDuration:=postContactData[firstNDTMSI,JourneyEndDate-JourneyStartDate]]
            pfd[GroupedIncidentID==gid,
                DrugTreatmentDischargeReason:=postContactData[firstNDTMSI,JourneyDischargeReason]]
            
            # extract the drugs they were in for at the point of contact
            drugs<-sapply(postContactData[firstNDTMSI,.(Drug1Code,Drug2Code,Drug3Code)],whatDrugGroup)
            pfd[GroupedIncidentID==gid,c("NDTMSDrugIncident1","NDTMSDrugIncident2","NDTMSDrugIncident3"):=as.list(drugs)]
         }
      }
      
      
      
      # we also want to know who had a journey in the last 5 years
      dateOf5YearsPrior<-pfdContactDate - years(5)
      # we check both start and end of journeys, all we care about was were
      # they treated
      preContactData<-ndtmsEntries[
         # ended a journey in the last 5 years (inc contact date)
         ( JourneyEndDate>=dateOf5YearsPrior & JourneyEndDate<=pfdContactDate ) |
         # started a journey in the last 5 years
         (JourneyStartDate>=dateOf5YearsPrior & JourneyStartDate<pfdContactDate ) |
         # were triaged in the last 5 years
         ( EpisodeTriagedDate >=dateOf5YearsPrior & EpisodeTriagedDate <pfdContactDate ) |
         # or started more than 5 years go and never ended
         (JourneyStartDate<dateOf5YearsPrior & is.na(JourneyEndDate)& is.na(EpisodeDischargeReason))
      ]
      
      if(nrow(preContactData)>0) {
         pfd[GroupedIncidentID==gid,TreatedForDrugsInLastFiveYears:=T]
         # Also record number of journeys that occurred prior to contact
         pfd[GroupedIncidentID==gid,DrugTreamentHistoricalJourneyCount:=uniqueN(preContactData$JourneyIndex)]
      }
      
   })
   return(pfd)
}

# the link predictions are inherent in the NDTMS linked-only data
link_predictions<-unique(ndtms_linked_only[,c("PseudoID_PFD","PseudoID_NDTMS")])

# create the linked data
linked_data_full<-createLinkedDataForNDTMS(link_predictions,ndtms_linked_only)

browser()

# merge in the drugs

# clean up some variables
linked_data_full[,IsInNDTMS:=F]
linked_data_full[!is.na(PseudoID_NDTMS),IsInNDTMS:=T]
linked_data_full[is.na(EnteredIntoDrugTreatment),EnteredIntoDrugTreatment:=F]
linked_data_full[is.na(TreatedForDrugsInLastFiveYears),TreatedForDrugsInLastFiveYears:=F]
linked_data_full[is.na(IsInTreatmentAtPointOfContact),IsInTreatmentAtPointOfContact:=F]
linked_data_full[is.na(WasInTreatmentWithin28DaysPriorToContact),WasInTreatmentWithin28DaysPriorToContact:=F]


# strip out identifiers and unnecessary variables
linked_data_full[,FirstName:=NULL]
linked_data_full[,LastName:=NULL]
linked_data_full[,DOB:=NULL]
linked_data_full[,PNCNumber:=NULL]

# adjust the follow-up duration for  people who entered into drug treatment
# save the previous followup-duration in case we need it, call it "observation window"
linked_data_full[,ObservationWindow:=FollowupDuration]
linked_data_full[,LogObservationWindow:=log(ObservationWindow)]

## Follow-up and log
linked_data_full[EnteredIntoDrugTreatment==T, DrugTreatmentCensorDate := pmin(DrugTreatmentCensorDate, DateEnteredIntoDrugTreatment)]
linked_data_full[, FollowupDuration := as.numeric(DrugTreatmentCensorDate - ContactDate, units = "days")]
linked_data_full[, FollowupDuration := pmax(FollowupDuration, 1)]
linked_data_full[, LogFollowupDuration := log(FollowupDuration)]

# save the linked and merged data
print("Writing linked data (full)")
arrow::write_feather(linked_data_full,NDTMS_to_PFD_deterministic_linked_full_fn_FEATHER_ndtms2)
# this is currently also the analsysis data
# We can drop PseudoID_PFD for the analysis data
linked_data_full[,PseudoID_PFD:=NULL]
arrow::write_feather(linked_data_full,PFD_analysis_full_ndtms2)

## XXX TODO
## what I want to do here is prove that deterministic linkage makes sense
## 1. count how many more people you could get from prob join by summing non-id based differences
## 2. when pnc number is the same, how many times are identifiers different and what are those diffs

## Also capture: 
## 1. main abuse substance
## 2. time from contact until entry into NDTMS

