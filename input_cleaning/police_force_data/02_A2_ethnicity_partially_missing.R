print("Determining missing ethnicities in cases where we have partial information")

# add in some helper columns and track the reasons
pfd[, `:=`( Index = .I, EthCorrectedReason = "None") ]

# load the ethnicity maps between SDE <-> Coarse and IC <-> Coarse
ethnicityMapSDEtoCoarse <- data.table(read_excel("../../common/data/internal/EthnicityMap.xlsx",sheet="SDEtoCoarse"))
ethnicityMapICtoCoarse <- data.table(read_excel("../../common/data/internal/EthnicityMap.xlsx",sheet="ICtoCoarse"))

# only process IDs that more than one row and more than one ethnicity
ids_to_process<- pfd[, .N > 1 && uniqueN(EthnicityCoarse) != 1, by = PseudoID][V1 == TRUE, PseudoID]
      
pfd[PseudoID %in% ids_to_process, {
   num_ethnicities <- uniqueN(EthnicityCoarse)
   num_nas <- sum(is.na(EthnicityCoarse))
   
   # this gets a bit complicated and I want to avoid too much nesting
   # without losing the efficiency of the .SD subgroup processing so
   # we will use a flag to say when we're done
   caseDone <- F
   
   # 1. If we have a mix of NAs and a one real ethnicity
   # we can fill the NAs with the real ethnicity
   onlyOneEthnicityAndRestNAs <- (num_nas >= 1 && num_ethnicities == 2)
   if(onlyOneEthnicityAndRestNAs) {
      
      # get the NA row indices
      na_indices <- Index[is.na(EthnicityCoarse)]
      # choose one of the filled in ethnicity rows
      fill_index <- Index[!is.na(EthnicityCoarse)][1]
      
      # update the NA rows
      # note that it is reasonable to assume that the IC and SDE assessments
      # are also the same since they must have been NA too
      pfd[Index %in% na_indices, `:=`(
         EthnicityCoarse = pfd[Index==fill_index, EthnicityCoarse],
         EthnicityIC = pfd[Index==fill_index, EthnicityIC],
         EthnicitySDE = pfd[Index==fill_index, EthnicitySDE],
         EthCorrectedReason = "NA"
      )]
      # mark us as done
      caseDone <- T
   } 
   
   # if we get here then there are multiple ethnicities in the group
   
   # 2.0: Can we use SDE or IC to differentiate?
   
   # Do any of the rows have self-defined ethnicities (not including NS)?
   sdes <- .SD[!is.na(EthnicitySDE) & EthnicitySDE!="NS - NOT STATED",.N,by=EthnicitySDE]
   someRowsHaveSDEs <- (nrow(sdes)!=0)
   
   # 2.1: Is there a majority SDE or only one SDE?
   thereIsAMajoritySDEOrOnlyOneSDE <- someRowsHaveSDEs && (sdes[,uniqueN(N)]!=1|nrow(sdes)==1)
   if(!caseDone && thereIsAMajoritySDEOrOnlyOneSDE) {
      # pick the majority (in the case of one SDE this is the majority also)
      mostCommonEthnicitySDE <- unique(sdes[which.max(sdes$N),EthnicitySDE])
      mostCommonEthnicityCoarse <- ethnicityMapSDEtoCoarse[SDE==mostCommonEthnicitySDE,Coarse]
      # map the majority SDE to a coarse ethnicity
      replacementIndices <- Index[EthnicityCoarse!=mostCommonEthnicityCoarse]
      # update the other rows to match the new determination
      pfd[Index %in% replacementIndices, `:=`(
         EthnicityCoarse = mostCommonEthnicityCoarse,
         EthCorrectedReason = "MostCommonObservedSDE"
      )]
      # mark this case as done
      caseDone <- T
   }
      
   # 2.2: Is there a majority IC or only one IC?
   ics <- .SD[!is.na(EthnicityIC)&EthnicityIC!="IC9 - UNKNOWN",.N,by=EthnicityIC]
   someRowsHaveICs <- (nrow(ics)!=0)
   thereIsAMajorityICOrOnlyOneIC <- someRowsHaveICs && (ics[,uniqueN(N)]!=1|nrow(ics)==1)
   
   if(!caseDone && thereIsAMajorityICOrOnlyOneIC) {
      # pick the majority (in the case of one SDE this is the majority also)
      mostCommonEthnicityIC <- unique(ics[which.max(ics$N),EthnicityIC])
      mostCommonEthnicityCoarse <- ethnicityMapICtoCoarse[IC==mostCommonEthnicityIC,Coarse]
      # map the majority SDE to a coarse ethnicity
      replacementIndices <- Index[EthnicityCoarse!=mostCommonEthnicityCoarse]
      # update the other rows to match the new determination
      pfd[Index %in% replacementIndices, `:=`(
         EthnicityCoarse = mostCommonEthnicityCoarse,
         EthCorrectedReason = "MostCommonObservedIC"
      )]
      # mark this case as done
      caseDone <- T
   }
   
   # if we are not done then there was neither a majority SDE nor a majority IC
   
   # 3.0: Choose most recent ethnicity
   contactDatesWithCoarseEthnicities <- .SD[!is.na(EthnicityCoarse),ContactDate]
   if(!caseDone) {
      # get the most recent SDE ethnicity
      mostRecentDate <- max(contactDatesWithCoarseEthnicities)
      # get the ethnicities that this corresponds to that are stated
      mostRecentEthnicitiesCoarse <- .SD[
         !is.na(EthnicityCoarse) &
         ContactDate==mostRecentDate
      ,EthnicityCoarse]
      
      # if there is only one unique ethnicity use this, otherwise we defer to random
      onlyOneEthnicity <- (uniqueN(mostRecentEthnicitiesCoarse)==1)
      if(onlyOneEthnicity) {
         # pick the most recent SDE ethnicity
         mostRecentEthnicityCoarse <- unique(mostRecentEthnicitiesCoarse)
         # replace the other ethnicities
         replacementIndices <- Index[EthnicityCoarse!=mostRecentEthnicityCoarse]
         # update the other rows to match the new determination
         pfd[Index %in% replacementIndices, `:=`(
            EthnicityCoarse = mostRecentEthnicityCoarse,
            EthCorrectedReason = "MostRecentObservedEthnicity"
         )]
         # mark this case as done
         caseDone <- T
      } 
   }
   
   # 3.2: If we are still not done, choose randomly
   if(!caseDone) {
      # get list of non-na ethnicities
      ethnicitiesCoarse <- unique(.SD[!is.na(EthnicityCoarse),EthnicityCoarse])
      randomEthnicity <- sample(ethnicitiesCoarse,1)
      # replace the other ethnicities
      replacementIndices <- Index[EthnicityCoarse!=randomEthnicity]
      # update the other rows to match the new determination
      pfd[Index %in% replacementIndices, `:=`(
         EthnicityCoarse = mostRecentEthnicityCoarse,
         EthCorrectedReason = "RandomEthnicity"
      )]
      # mark this case as done
      caseDone <- T
   }
   
   if(!caseDone) {
      stop("Error, all cases should be handled")
   }
      
   NULL  # Return NULL since updates are in-place
}, by = PseudoID]


# make some stats
correctionStats <- data.table(
   N=sum(pfd$EthCorrectedReason!="None"),
   `NA replacement`=sum(pfd$EthCorrectedReason=="NA"),
   `Most common SDE`=sum(pfd$EthCorrectedReason=="MostCommonObservedSDE"),
   `Most common IC`=sum(pfd$EthCorrectedReason=="MostCommonObservedIC"),
   `Most recent`=sum(pfd$EthCorrectedReason=="MostRecentObservedEthnicity"),
   `Random`=sum(pfd$EthCorrectedReason=="RandomEthnicity")
)