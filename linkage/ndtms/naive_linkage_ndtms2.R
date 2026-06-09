# this script examines the possibility of doing a naive linkage where we
# just use an exact match on {FirstNameInitial,LastNameInitial,DOB,Sex,EthCoarse}
# or a subset

# interestingly, the PFD data has PNC numbers so we can use these to examine
# how often a naive linkage would work within PFD and use this as an estimate
# of the linkage match with NDTMS based on this methodology

## ---- Setup
# required libs
library(data.table)
library(knitr)
library(DT)
library(readxl)
library(fst)
library(digest)

# flag to control whether pfd data should be reloaded with every execution of the script
forceReload <- F

# load in the list of forces
source("../../common/r/force_list.R")

# load in the paths of all required input and output files
source("../../common/r/file_paths.R")

# read the pfd data in
if(!exists("pfd")|forceReload) {
   print("Reading in clean police force data (pfd)")
   pfd <- data.table(arrow::read_feather(PFD_complete_fn_FEATHER))
}

# source the file with functions that make a "NDTMS linkable" version of pfd
source("../../input_cleaning/police_force_data/linkage_dataset_functions.R")

# add the columns that you would link on here
#pfd_linkable <- makePFDLinkableWithNDTMS(pfd,keepPNC=T)

# get first name initial and last name initial
pfd[,FirstNameInitial:=str_sub(FirstName,1,1)]
pfd[,LastNameInitial:=str_sub(LastName,1,1)]
# drop rows without a PNC number
pfd <- pfd [!is.na(PNCNumber)]

# see how unique our combo is
maximalLinkColumns <- c("FirstNameInitial","LastNameInitial","Sex","DOB","ForceName","EthCoarse")

# a function which tells us the internal link percentage for a given set of columns
# when using PNCNumber as the canonical ID
computeInternalLinkPercentage <- function(linkColumns) {
   
   # set a column name which is a composite of the linkColumns
   pfd[,NDTMSLinkCols:=do.call(paste0, .SD), .SDcols = linkColumns]
   
   # for each unique column name, count the unique number of PNC numbers that match to it
   pfd_link_map_uniqueness_by_NDTMSLinkCols <- pfd[,list(NDTMSLinkColCount=uniqueN(PNCNumber)),by=NDTMSLinkCols]
   # those with a count of 1 are promising for a 1 <=> 1 mapping but it can also be the
   # case that two different NDTMSLinkCols map to the same PNCNumber so we have to do the
   # reverse map too
   pfd_link_map_uniqueness_by_PNCNumber <- pfd[,list(NDTMSLinkColCount=uniqueN(NDTMSLinkCols)),by=PNCNumber]
   # this tells us, for each PNC number how many unique NDTMSLinkCols they map to
   
   # The number of 1 <=> 1 mappings is the number of PNCNumbers with exactly one NDTMSLinkCols
   # minus the number of NDTMSLinkCols with more than one PNCNumber

   A <- nrow(pfd_link_map_uniqueness_by_PNCNumber[NDTMSLinkColCount==1])
   B <- sum(pfd_link_map_uniqueness_by_NDTMSLinkCols[NDTMSLinkColCount>1,NDTMSLinkColCount])
   # this can be less than zero
   uniqueLinks <- max(A-B,0)
   linkPercent <- round(uniqueLinks/uniqueN(pfd$PNCNumber)*100,digits=2)
   
   data.table(Columns=paste(linkColumns,collapse=", "),UniqueCombinations=nrow(pfd_link_map_uniqueness_by_NDTMSLinkCols),NumberLinked=uniqueLinks,PercentLinked=linkPercent)
}

linkPercentages <- rbindlist(lapply(1:length(maximalLinkColumns),function(upperI) {
   computeInternalLinkPercentage(maximalLinkColumns[1:upperI]) 
}))
fwrite(linkPercentages,"outputs/link_percentages.tsv",sep="\t")

# we can examine the ethnicity differences
#pfd[,CompoundID:=paste0(FirstNameInitial, LastNameInitial, Sex, DOB, ForceName)]
pfd[,CompoundID:=PNCNumber]
pfd[,CompoundIDPlusEth:=paste0(FirstNameInitial, LastNameInitial, Sex, DOB, ForceName,EthCoarse)]
# when we add in eth and it reduces specificity what is going on
x <- pfd[,uniqueN(CompoundIDPlusEth),by=CompoundID]
multiEthIDs <- x[V1>1,CompoundID]
multiEth <- pfd[CompoundID %in% multiEthIDs]
# what are the unique combos
ethCombos <- rbindlist(lapply(multiEthIDs,function(id){
   d <- pfd[CompoundID==id]
   eths <- unique(d$EthCoarse)
   if(length(eths)==1) {
      return(NULL)
   }
   eths[is.na(eths)] <- "NA"
   eths <- sort(eths)
   data.table(ID=id,Eths=paste0(eths,collapse=","))
}))

ethComboCounts <- ethCombos[,.N,by=Eths]
setorder(ethComboCounts,-N)

ethComboCountsSafe <- ethComboCounts[N>=10]
names(ethComboCountsSafe) <- c("Ethnicity combination","Count")
fwrite(ethComboCountsSafe,"outputs/eth_combos.tsv",sep="\t")

# multi force
forcePerPNCNumber <- pfd[,uniqueN(ForceName),by=PNCNumber]

# 
#print(paste("Percentage of PNC numbers that match one-to-one with the NDTMS link cols:",computeInternalLinkPercentage(maximalLinkColumns)))

# load in the NDTMS data

# load linkable ndtms data
#if(!exists("ndtms_linkable_post_sept21")|forceReload) {
#   ndtms_linkable_post_sept21 <- data.table(arrow::read_feather(NDTMS_linkable_with_PFD_post_sept21_fn_FEATHER))
#}

#if(!exists("ndtms_linkable_full")|forceReload) {
#   ndtms_linkable_full <- data.table(arrow::read_feather(NDTMS_linkable_with_PFD_full_fn_FEATHER))
#}
