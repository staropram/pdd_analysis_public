library(data.table)
library(sf)
library(dplyr)
library(stringr)
library(readxl)
source("../../common/r/file_paths.R")
source("postcode_sector_to_pfa_load_data.R")

# load utla to pfa map
if(!file.exists(UTLA_to_PFA_full_fn)) {
   stop("In order to continue you need to generate the pfa_to_utla.tsv file \n
        by running the code in the input_synthesis/pfa_to_utla directory")
} else {
   utla_to_pfa <- fread(UTLA_to_PFA_full_fn)
}

# load in the postcode centroids
#pcs <- st_read("data/NSPL_Online_Centroids.gpkg")

# load in the sector to UTLA data provided by DHSC
# this isn't correct in the sense that not all of the data in here are UTLAs
# to take an example, Dudley (E08000027) is in this dataset but is a 
# Metropolitan Borough of the Metropolitan County West Midlands (E11000005)
# so in that case "West Midlands" is the UTLA, not Dudley

# probably the easiest thing to do here is to manually change the
# Metropolitan Boroughs in this dataset into the actual UTLA
sector_to_utla <- fread(fileNames$Sector_UTLA_fn)
sector_to_utla[,UTLACode:=toupper(utla)]

# make a single list of "UTLA" codes from this
sector_to_utla_codes <- unique(unlist(lapply(sector_to_utla$UTLACode,function(utlaCodes) {
   str_split(utlaCodes,"\\|")[[1]]
})))

# find out which police force area UTLAs are not in it
unmappableUTLA <- rbindlist(lapply(utla_to_pfa$UTLACode,function(utlaCode) {
   if(!utlaCode %in% sector_to_utla_codes) {
      utla_to_pfa[UTLACode==utlaCode,]
   }
}))

# find out which LADS fall in each of these and label them
lad_centroids <- st_centroid(lad)
lad_to_utla <- rbindlist(lapply(unmappableUTLA$UTLACode,function(uc) {
   currentUTLA <- utla[utla$UTLA22CD==uc,]
   # find LADs that fall within
   childLADs <- st_intersection(currentUTLA,lad_centroids)
   data.table(childLADs[,c("UTLA22CD","UTLA22NM","LAD23CD","LAD23NM")])
}))

# check all LADs were mapped
if(any(!lad_to_utla$LAD23CD %in% sector_to_utla$UTLACode)) {
   stop("Some LADs could not be mapped to UTLAs")
}

# now in "their" UTLA file, update the LAD based UTLAs to use the correct
# "UTLA" names
sector_to_utla$LAD23CD <- sector_to_utla$UTLACode
sector_to_utla_extended <- merge(sector_to_utla,lad_to_utla,by=c("LAD23CD"),all.x=T)
utla_to_update <- !is.na(sector_to_utla_extended$UTLA22CD)
sector_to_utla_extended$UTLACode[utla_to_update] <- sector_to_utla_extended$UTLA22CD[utla_to_update]

# now do a bunch of joins/maps
# utla name -> police force area
#sector_to_pfa <- sector_to_utla[utla_to_pfa,on="UTLACode"]

# each sector can have multiple UTLAs so process everything manually
sector_to_pfa <- rbindlist(lapply(1:nrow(sector_to_utla_extended),function(i) {
   # get the current sector
   sector <- sector_to_utla_extended$sector[i]
   
   # define the default result, or just return NULL
   emptyReturn <- data.table(Sector=sector,UTLAName=NA,UTLACode=NA,PFAName=NA,PFACode=NA)
   
   # get the list of utla that correspond to this sector
   utlaCodes <- sector_to_utla_extended$UTLACode[i]
   
   utlaName <- ""
   
   # for straddling sectors, lookup the actual postcodes
   if(str_length(utlaCodes)>9) {
      # count the UTLAs for all postcodes in the sector
      utlaRanks <- postcode_to_utla[Sector==sector,.N,by=utla22cd]
      # choose the most popular UTLA so long as it gets more than
      # 75% of the vote
      sumVotes <- sum(utlaRanks$N)
      whichMaxVotesI <- which.max(utlaRanks$N)
      whichMaxVotesN <- utlaRanks[whichMaxVotesI,N]
      whichMaxVotesUTLA <- utlaRanks[whichMaxVotesI,utla22cd]
      fractionMaxVotes <- whichMaxVotesN/sumVotes
      if(fractionMaxVotes>0.75) {
         utlaName <- utla_codes_to_names[UTLACode==whichMaxVotesUTLA,UTLAName]
         utlaCodes <- whichMaxVotesUTLA
      } else {
         #print(utlaRanks)
         #print(paste0("For sector ",sector," utla ",whichMaxVotesUTLA," got the most votes at ",fractionMaxVotes))
         # cannot resolve this straddled sector so ignore it
         return(emptyReturn)
      }
   } else {
      # get the utla name
      utlaName <- utla_codes_to_names[UTLACode==utlaCodes,UTLAName]
   }
   
   if(length(utlaName)==0) {
      print(paste0("Unable to get UTLA for ",utlaCodes))
      return(emptyReturn)
   }
   
   if(str_length(utlaName)==0) {
      print(paste0("Unable to get UTLA for ",utlaCodes))
      return(emptyReturn)
   }
   
   # get the associated PFA into
   pfa <- utla_to_pfa[UTLAName==utlaName,]
   if(nrow(pfa)==0) {
      print(paste0("Unable to get PFA for ",utlaName))
      return(emptyReturn)
   }
   
   return(pfa[,Sector:=sector])
   
}),use.names=T)

# save this map
setcolorder(sector_to_pfa,"Sector")
fwrite(sector_to_pfa,"outputs/sector_to_pfa_full.csv")