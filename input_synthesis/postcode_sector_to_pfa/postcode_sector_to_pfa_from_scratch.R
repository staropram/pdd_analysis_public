library(data.table)
library(sf)
library(dplyr)
library(stringr)
library(readxl)
library(tictoc)

library(future.apply)
options(future.globals.maxSize = 2 * 1024^3)
tic()
plan(multicore,workers=availableCores())
toc()

#source("../../common/r/file_paths.R")
#source("postcode_sector_to_pfa_load_data.R")

if(!exists("pfa")) {
   pfa <- st_read("../../common/data/external/boundaries/Police_Force_Areas_December_2022_EW_BFC.gpkg")
   #pfa <- st_read("../../common/data/external/boundaries/Police_Force_Areas_December_2022_EW_BGC.gpkg")
}

# load in the postcode centroids
if(!exists("pcs")) {
   pcs <- st_read("../../common/data/external/NSPL_LATEST_UK.shp")
   pcsdt <- data.table(pcs)
}

testIndices <- c(1,544101)
tic()
pfaList <- rbindlist(lapply(1:100,function(i){
   pcrow <- pcs[i,]
   pcode <- pcrow$PCDS
   
   # which PFA does this pcode intersect
   pfa_within <- st_within(pcrow,pfa)
   nIntersecting <- length(pfa_within[[1]])
   if(nIntersecting==0) {
      return(data.table(Postcode=pcode,PFA=NA))
   }
   if(nIntersecting>1) {
      browser()
   }
   return(data.table(Postcode=pcode,PFA=pfa$PFA22NM[32]))
}))
print(pfaList)
toc()

# extract sector
#pcs_split <- str_split_fixed(pcs$PCDS," ",n=2)
#pcs$sector <- paste0(pcs_split[,1],substr(pcs_split[,2],1,1))

#pcmap <- fread("../../common/data/external/PCD_OA21_LSOA21_MSOA21_LTLA22_UTLA22_CAUTH22_NOV23_UK_LU_v2.csv")

# for each postcode determine the PFA it falls within
