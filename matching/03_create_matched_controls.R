# purpose of this script is to create matched controls for each
# intervention force
source("../common/r/file_paths.R")

library(lubridate)

# set the random seed so we can run again without changing the results
set.seed(259)

# load the matching functions
source("01_load_matching_functions.R")

# load the imputation strategies
source("../imputation/00a_imputation_strategies.R")

# options
gForceReloadPFDA <- F
forceRematch <- F

# we need the pfda data
if(!exists("pfda")|gForceReloadPFDA==T) {
   print("Loading final PFD analysis dataset")
   pfda <- read_feather(PFD_analysis_full)
}

# load in the optimal matching ratio table
optimal_k_table <- data.table(read.table(file="outputs/optimal_iv_force_k_selection.tsv",sep="\t",header=T))

# create matches for each imputation and each imputation strategy
for(imputationStrategyName in names(imputationStrategies)) {
   print(paste0("Generating matched for imputation strategy ",imputationStrategyName))
   
   # load imputed data for current strategy
   imputedLongInputFN <- paste0(analysis_path,"PFD_Plus_Imputed_Long_",imputationStrategyName,".feather")
   print(paste0("Reading imputed data ",imputedLongInputFN))
   imputedDataLong <- arrow::read_feather(imputedLongInputFN)

   masterMatchFilename <- paste0(
      analysis_path,
      "/PlusMatchedControl/",
      imputationStrategyName,
      "/PFD_Matched_From_Imputation_MASTER.feather"
   )
   
   # check the directory exists
   mmDir <- dirname(masterMatchFilename)
   if(!dir.exists(mmDir)) {
      dir.create(mmDir)
   }

   # for the current imputation strategy, for each imputation within that
   # strategy create a set of matches
   maxImputationIndex <- imputedDataLong[,max(.imp)]
   
   masterMatches <- rbindlist(lapply(1:maxImputationIndex,function(imputationIndex) {
      print(paste0("Doing matching for imputation index ",imputationIndex))
      
      currentImputedData <- imputedDataLong[.imp==imputationIndex]
      
      # file name for this match
      matchedControlFilename <- paste0(
         analysis_path,
         "/PlusMatchedControl/",
         imputationStrategyName,
         "/PFD_Matched_From_Imputation_With_Index",
         imputationIndex,
         ".feather"
      )
      
      # skip files that already exist
      if(file.exists(matchedControlFilename)&!forceRematch) {
         print(paste0("Loading",matchedControlFilename))
         print("As it already exists and forceRematch==F")
         allMatches <- read_feather(matchedControlFilename)
         return(allMatches)
      }
   
      allMatches  <- rbindlist(lapply(1:nrow(optimal_k_table),function(kTableIndex) {
         # do the matches one by one so each can have unique k
         forceName <- optimal_k_table[kTableIndex,ForceName]
         cohortGroup <- optimal_k_table[kTableIndex,CohortGroup]
         k <- optimal_k_table[kTableIndex,K]
         
         matches <- getMatchedControls(
            currentImputedData,
            forceName,
            cohortGroup,
            k,
            writeOutput=F, # don't write output, this is for something else
            T, # always recompute
            imputationStrategyName
         )[[1]]
         
         # extract data
         data <- matches$data 
         
         # add identifying columns as we are going to make this a "long" dataset
         data[,MatchedForce:=forceName]
         data[,MatchedCohort:=cohortGroup]
         data[,MatchedK:=k]
         data[,ImputationStrategy:=imputationStrategyName]
         data[,ImputationIndex:=imputationIndex]
         
         data
      }))
      
      # save the long data
      print(paste0("Writing ",matchedControlFilename))
      write_feather(allMatches,matchedControlFilename)
   }))
   
   # save the master match data
   print(paste0("Writing master match filename",masterMatchFilename))
   write_feather(masterMatches,masterMatchFilename)
}
