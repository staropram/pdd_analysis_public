library(sf)
library(data.table)
library(dplyr)
library(stringr)
library(purrr)
library(reshape2)
library(DT)

# load the data we need
force_reload_data <- F
if(!exists("data_loaded")|force_reload_data) {
   source("reoffending_baseline_load_data.R")
   data_loaded <- T
}
# - pf contains the list of participating police forces
# - pfa is a spatial dataframe of Police Force Area boundaries
# - utla is a spatial dataframe of all UTLA boundaries
# - reoff reoffending statistics at the level of UTLA

# remove Juveniles from the reoffending statistics as our
# cohort are 18+
reoff <- reoff %>% filter(`Adult / Juvenile`=="Adult")

# load the PFA to UTLA map
utla_to_pfa <- fread("../pfa_to_utla/outputs/utla_to_pfa.csv")
pfa_to_utla <- fread("../pfa_to_utla/outputs/pfa_to_utla.tsv")

# now we want to compute the reoffending statistics for the PFAs
# by using the stats which are provided at the level of UTLA

# convert the pfa map to a list as this makes things easier
pfa_to_utla_list <- lapply(pfa_to_utla$UTLAs,function(utlas) {
   utlas <- str_replace_all(utlas,"'","")
   utlas <- str_split(utlas,"\t")
   utlas[[1]]
})
names(pfa_to_utla_list) <- pfa_to_utla$PoliceForce

# the data is provided for lots of different time periods
# it is useful to have a way to select a range of years
# to create statistics for, these functions enable this
cohortsAvailable <- unique(reoff$Cohort)

yearRangeToCohortList <- function(yrRange) {
   endPoints <- paste("Mar",yrRange)
   matches <- lapply(endPoints,function(ep) { cohortsAvailable %like% ep })
   cohortSelect <- Reduce("|",matches)
   cohortsAvailable[cohortSelect]
}

# this will create reoffending statistics for a given year range
# by summing the actual
createStatsForYearRange <- function(yrRange) {
   cohortsToUse <- yearRangeToCohortList(yrRange)
   # ensure we are using adults and our selected cohorts
   reoff_restricted <- reoff[Cohort %in% cohortsToUse & `Adult / Juvenile`=="Adult"]

   reoff_by_region <- reoff_restricted[,
      list(
         offenders=sum(offenders,na.rm=T),
         reoffenders=sum(reoffenders,na.rm=T)
      ),
      by=Geography,
   ]
   reoff_by_region[,ReoffendingProportion:=reoffenders/offenders*100][]

   # reoffending by PFA
   reoffending_by_pfa <- rbindlist(lapply(names(pfa_to_utla_list),function(pfn) {
      # for some reason the Northamptonshire UTLAs (West and North)
      # don't exist in the stats and instead Northamptonshire 
      # is listed under Northamptonshire
      if(pfn=="Northamptonshire") {
         utlas <- "Northamptonshire"
      } else {
         # get list of all UTLAs associated to this PFA
         utlas <- pfa_to_utla_list[[pfn]]
      }

      counts_per_utla <- rbindlist(lapply(utlas,function(utla) {
         regionStats <- reoff_by_region[Geography==utla,]
         data.table(
            "Geography"=regionStats$Geography,
            "offenders"=regionStats$offenders,
            "reoffenders"=regionStats$reoffenders,
            "propReoffenders"=regionStats$reoffenders/regionStats$offenders*100
         )
      }))

      counts_per_pfa <- data.table(
         "PoliceForce"=pfn,
         "offenders"=sum(counts_per_utla$offenders),
         "reoffenders"=sum(counts_per_utla$reoffenders)
      )
      counts_per_pfa[,propReoffenders:=reoffenders/offenders*100][]
   }))

   list(
      by_utla=reoff_by_region,
      by_pfa= reoffending_by_pfa
   )
}

reoffendingTrendList <- lapply(2011:2022,function(yr) {
   stats <- createStatsForYearRange(yr)
   by_pfa <- stats$by_pfa
   names(by_pfa)[4] <- paste0(yr)
   by_pfa[,c(1,4)]
})

reoffendingTrends <- Reduce(function(x,y) merge(x,y,by="PoliceForce"),reoffendingTrendList)
write.table(reoffendingTrends,"outputs/reoffending_trends.csv",sep=",",row.names=F)

# the stats we need for the study: reoffending rates between
# April 2018 and March 2020
reoffendingStats <- createStatsForYearRange(2019:2020)
write.table(reoffendingStats$by_pfa,"outputs/reoffending_Apr18_to_Mar20.csv",sep=",",row.names=F)