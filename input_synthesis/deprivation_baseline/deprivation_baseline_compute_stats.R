# load in the data
source("deprivation_baseline_load_data.R")

# relevant here
# imd19 - Index of Multiple Derivation 2019 dataset


library(data.table)
library(stringr)

# load the previously computed pfa_to_lsoa map
pfa_to_lsoa <- fread("outputs/pfa_to_lsoa.tsv",sep="\t")

# convert the pfa map to a list as this makes things easier
pfa_to_lsoa_list <- lapply(pfa_to_lsoa$LSOAs,function(lsoas) {
   lsoas <- str_split(lsoas,", ")
   lsoas <- lsoas[[1]]
})
names(pfa_to_lsoa_list) <- pfa_to_lsoa$`Force Name`

# count the number of LSOA in the bottom quintile
pfa_deprivation <- rbindlist(lapply(names(pfa_to_lsoa_list),function(pfan) {
   lsoas <- pfa_to_lsoa_list[[pfan]]
   imd_entries <- imd19 %>% filter(`LSOA code (2011)` %in% lsoas)

   out <- data.table(
      `Police Force`=pfan,
      `LSOA Count`=nrow(imd_entries),
      `Bottom Quintile Count`=sum(imd_entries$`Index of Multiple Deprivation (IMD) Decile`<=2),
      `Bottom Decile Count`=sum(imd_entries$`Index of Multiple Deprivation (IMD) Decile`==1)
   )
   out$`Bottom Quintile Percent` <- out$`Bottom Quintile Count` / out$`LSOA Count` *100
   out$`Bottom Decile Percent` <- out$`Bottom Decile Count` /
      out$`LSOA Count` * 100
   out
}))

