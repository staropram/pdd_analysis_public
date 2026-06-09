# This script runs matching on the complete case data to determine
# what the matching ratios are for each police force.
#
# Matching ratio = TargetForce:DonorPool, for example 1:5 means each individual
# in TargetForce is matched with 5 individuals in the donor pool 
# (with replacement), and we say the matching ratio is 1:5 or "5" for short.
# We refer this matching ratio as k
#
# The maximum ratio we are interested in is 5: this is an accepted heuristic 
# where the point which diminishing returns follows.
#
# But we also want to minimise "reuse" of individuals from the donor pool
# so that these individuals don't bias the result. There's a balance between
# the extra power that increasing the ratio brings, and the possibility of
# introducing bias.

# We can measure this and say 'we never want the "reuse" of individuals in the
# donor pool to exceed 30%' and we mean that no more than 30% of the data
# in the matched control should be constructed from individuals that appear
# more than once.
#
# But the forces we are matching for change in size: AvonAndSomerset has only
# 415 Group 1 offences, whereas Merseyside has 6971.
#
# We can see that it probably much easier to find 5*415 = 2075 unique
# individuals from a fixed size donor pool than 5*6971 = 34855.  The problem 
# compounds for larger forces since they are taken out of the donor pool
# for the matching, so the donor pool shrinks even more.
# 
# So the purpose of this script is to determine for each force the match ratio 
# to use with the constraint that the constructed matched control has no more
# than 30% of its data obtained from individuals sampled more than once.
#
# The empirical way to do this is to start with k=1 for a given force, perform
# a match, and then compute the reuse measurement, then set k=2, etc etc until 
# either we hit k=5 or we hit our reuse threshold. 
# The k we arrive at is then the k to use for the given target force
# 
# Since we will match multiple times, for imputed data too, we only really
# want to compute this k-table once, and then keep it fixed for all the
# other matches. Of course the k will be slighty different for the imputed
# data as that has no missing values, but not by a lot, and as each imputation
# differs, we might in theory get diffrent k (unlikely). The most sensible
# approach then is to use the base data, with its missing values to estimate k
# and go with that

# we need the matching functions
source("01_load_matching_functions.R")

# options
gForceReloadPFDA <- T
gForceRecomputeMatchedControls <- F

# load in the base data
if(!exists("pfda")|gForceReloadPFDA==T) {
   print("Loading final PFD analysis dataset")
   pfda <-  data.table(read_feather(PFD_analysis_full))
}

interventionForces <- list(
   G1=pfda[G1DiversionForce==T,unique(ForceName)],
   G2=pfda[G2DiversionForce==T,unique(ForceName)]
)

#library(future.apply)
#plan(multisession, workers = 4) # Adjust workers as needed

# get the matched controls for each offender group and matching ratio
print("Iterating through all matched control k variants")
if(!exists("matchedControls")|gForceRecomputeMatchedControls==T) {
   matchedControls <- lapply(1:5,function(k) {
      groupMatches <- lapply(1:2,function(cohortGroup){
         matches <- getMatchedControls(
            pfda,
            interventionForces[[cohortGroup]],
            cohortGroup,
            k,
            writeOutput=T,
            gForceRecomputeMatchedControls,
            "NO_IMPUTATION"
         )
         stats <- rbindlist(lapply(matches,function(sc){ sc$stats }))
         list(matches=matches,stats=stats)
      })
      names(groupMatches) <- as.character(1:2)
      groupMatches
   })
   names(matchedControls) <- as.character(1:5)
}

# make the k table

# make some stats for each force (either G1 or G2)
allInterventionForces <- unique(unlist(interventionForces))

print("Computing stats")
allForceStats <- rbindlist(lapply(allInterventionForces,function(forceName){
   rbindlist(lapply(matchedControls,function(currentMatches){
      stats <- currentMatches[[1]]$stats
      if(length(currentMatches)==2) {
         statsG2 <- currentMatches[[2]]$stats
         stats <- rbindlist(list(stats,statsG2))
      }
      forceStats <- stats[ForceName==forceName]
      setcolorder(forceStats,c("ForceName","CohortGroup"))
      
   }))
}))
setorder(allForceStats,ForceName,CohortGroup,K)

# save the stats locally
write.table(allForceStats,file="outputs/all_force_stats.tsv",sep="\t",row.names = F)


# we can compare each synthetic force with its intervention force
# use each row of allForceStats to obtain the force names and indices
# instead of multiple nested loops
comparisonStats <- rbindlist(lapply(1:nrow(allForceStats),function(i) {
   # get the data indices
   forceName <- allForceStats[i,ForceName]
   cohortGroup <- allForceStats[i,CohortGroup]
   k <- allForceStats[i,K]
   
   # get the data
   d <- matchedControls[[k]][[cohortGroup]][["matches"]][[forceName]][["data"]]
   
   # compute comparison stats for each of the matching variables
   # age is only the really relevant variable that might be different
   
   # for Intervention (dI) and control (dC) groups
   dI <- d[ForceName==forceName]
   dC <- d[ForceName!=forceName]
   
   # compute variance ratio of Intervention (I) and control (C)
   varIAge <- var(dI$AgeAtLastBirthday)
   varCAge <- var(dC$AgeAtLastBirthday)
   varAgeRatio <- varIAge/varCAge
   # diff between means in abs
   meanAgeI <- mean(dI$AgeAtLastBirthday)
   meanAgeC <- mean(dC$AgeAtLastBirthday)
   meanDiffAgeAbs <- abs(meanAgeI - meanAgeC)
   # t-test p
   tAge <- t.test(dI$AgeAtLastBirthday,dC$AgeAtLastBirthday)
   tAgeP <- tAge$p.value
   
   uniqueAgeI <- uniqueN(dI$AgeAtLastBirthday)
   uniqueAgeC <- uniqueN(dC$AgeAtLastBirthday)
   
   # return a data.table so we can add these stats onto the other stats
   data.table(
      ForceName=forceName,
      CohortGroup=cohortGroup,
      K=k,
      InterventionN=nrow(dI),
      VarAgeRatio=round(varAgeRatio,digits=4),
      MeanAgeDiffAbs=round(meanDiffAgeAbs,digits=4),
      UniqueAgeI=uniqueAgeI,
      UniqueAgeC=uniqueAgeC,
      TTestAgeP=round(tAgeP,digits=3)
   )
}))

# merge in the other force stats
comparisonStats <- merge(
   comparisonStats,
   allForceStats[,c("ForceName","CohortGroup","K","Control reuse %")],
   by=c("ForceName","CohortGroup","K"),
   all.x=T
)
write.table(comparisonStats,file="outputs/comparisonStats.tsv",sep="\t",row.names = F)


# some plot testing stuff
if(F) {
   comparisonForce <- "WestMids"
   comparisonData <- data.table(AgeAtLastBirthday=pfda[ForceName==comparisonForce&CohortGroup==1,AgeAtLastBirthday],Force=comparisonForce)
   otherData  <- data.table(AgeAtLastBirthday=pfda[ForceName!=comparisonForce&CohortGroup==1,AgeAtLastBirthday],Force="Rest")
   allData <- rbindlist(list(comparisonData,otherData))
   comparisonPlot <- ggplot(allData, aes(x = AgeAtLastBirthday,color=Force)) +
      geom_density(fill = "lightblue", alpha = 0.5) +  # Kernel density plot
      labs(title = "Smoothed Age Histogram", x = "Age", y = "Density") +
      theme_minimal()
   print(comparisonPlot)
}

# for each intervention force find max k which keeps control 
# group reuse below 30%
ivForces <- unique(allForceStats[,c("ForceName","CohortGroup")])
ivForcesPlusK <- rbindlist(lapply(1:nrow(ivForces),function(i){
   forceName <- ivForces[i,ForceName]
   cohortGroup <- ivForces[i,CohortGroup]
   
   d <- allForceStats[ForceName==forceName&CohortGroup==cohortGroup]
   # exclude any reuse above 30 %
   d[`Control reuse %`>30,`Control reuse %`:=NA]
   # then choose the k that maximises reuse
   optimalK <-  d[which.max(d$`Control reuse %`),K]
   return(data.table(ForceName=forceName,CohortGroup=cohortGroup,K=optimalK))
}))

# save this information about which matches to use for canonical
write.table(ivForcesPlusK,file="outputs/optimal_iv_force_k_selection.tsv",sep="\t",row.names = F)

# write the ones that are chosen as the "canonical" data
x <- lapply(1:nrow(ivForcesPlusK),function(i){
   forceName <- ivForcesPlusK[i,ForceName]
   cohortGroup <- ivForcesPlusK[i,CohortGroup]
   k <- ivForcesPlusK[i,K]
   print(paste0("Writing canonical data for ",forceName,", G",cohortGroup," and K=",k))
   # get the data
   d <- matchedControls[[k]][[cohortGroup]][["matches"]][[forceName]][["data"]]
   # set outfile and write
   outFN <-  paste0(analysisDataPaths[[forceName]]$PlusMatchedControl[[cohortGroup]],"_NO_IMPUTATION.feather")
   write_feather(d,outFN)
   T
})

# join them all together into a single datafile

## TEST
testMe <- F
if(testMe==T) {
   source('01_load_matching_functions.R')
   testMatches <- getMatchedControls(
      pfda,
      c("WestMids"),
      1,
      3,
      writeOutput=F,
      T,
      "TEST"
   )
}