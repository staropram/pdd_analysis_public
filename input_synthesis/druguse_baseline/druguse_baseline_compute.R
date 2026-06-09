library(sf)
library(data.table)
library(dplyr)
library(stringr)
library(purrr)
library(reshape2)
library(DT)

# load the data we need
force_reload_data <- T
if(!exists("data_loaded")|force_reload_data) {
   source("druguse_baseline_load_data.R")
   data_loaded <- T
}

# Data loaded
# ocu - opiate and crack use data
# pf - list of participating police forces

# convert the pfa map to a list as this makes things easier
pfa_to_utla_list <- lapply(pfa_to_utla$UTLAs,function(utlas) {
   utlas <- str_replace_all(utlas,"'","")
   utlas <- str_split(utlas,"\t")
   utlas <- utlas[[1]]
   # remove entries that have ", City of" postfix
   # as the OCU names omit this
   city_postfixers <- utlas %like% ", City of"
   utlas <- utlas[!city_postfixers]
})
names(pfa_to_utla_list) <- pfa_to_utla$PoliceForce

# some of the police forces are not listed with their
# UTLAS in the OCU stats, but with local authorities instead,
# these need to be manually adjusted
# also some of the names like Stockton-on-Tees is called
# "Stockton" in this document
pfa_to_utla_list$Merseyside <- c("Liverpool","Wirral","Knowsley","St Helens","Sefton")
pfa_to_utla_list$`Greater Manchester` <- c("Wigan","Salford","Trafford","Manchester","Stockport","Tameside","Oldham","Rochdale","Bury","Bolton")
pfa_to_utla_list$`Durham` <- c("Durham","Darlington")
pfa_to_utla_list$`West Yorkshire` <- c("Bradford","Leeds","Wakefield","Calderdale","Kirklees")
pfa_to_utla_list$Cleveland <- c("Hartlepool","Middlesbrough","Redcar and Cleveland","Stockton")
pfa_to_utla_list$Northamptonshire <- c("Northamptonshire")
# NOTE - wales isn't in the OCU dataset
pfa_to_utla_list$`North Wales` <- NULL

pfa_to_la_table <- data.table(
   `Police Force`=names(pfa_to_utla_list),
   `UTLAsAndLAs`= unlist(lapply(pfa_to_utla_list,function(l) { paste(l,collapse=", ")}),use.names=F)
)
write.table(pfa_to_la_table,"outputs/pfa_to_utla_and_la_map.tsv",row.names=F,sep="\t")

all_relevant_utla <- unlist(pfa_to_utla_list,use.names=F)

ocu_la <- ocu$`Local authority`
# how many of the utla list is present
la_present <- unlist(lapply(all_relevant_utla,function(utla) {
   utla %in% ocu_la
}))

#missing_la <- all_relevant_utla[!la_present]
#print(missing_la)

# now create the counts for each pfa
pfa_ocu_count <- rbindlist(lapply(names(pfa_to_utla_list),function(pfan) {
   utla_list <- pfa_to_utla_list[pfan][[1]]
   relevant_rows <- ocu[ocu$`Local authority` %in% utla_list,]
   ocuCount <- round(sum(relevant_rows$`OCU estimate`))
   data.table(PoliceForce=pfan,OCUCount=ocuCount)
}))

write.table(pfa_ocu_count,file="outputs/pfa_ocu_count.csv",row.names=F,sep=",")