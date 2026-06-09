# function to load a gpkg into the global environment
relative_data_path <- "../../common/data/external/"
load_gpkg <- function(params) {
   if(!exists(params$rname)) {
      d <- st_read(paste0(relative_data_path,params$fn,".gpkg"))
      assign(params$rname, d, envir = .GlobalEnv)
   }
}

lapply(list(
   list(fn="boundaries/Local_Authority_Districts_December_2023_Boundaries_UK_BGC",rname="lad_lowres"),
   list(fn="boundaries/Local_Authority_Districts_December_2023_Boundaries_UK_BFC",rname="lad"),
   list(fn="boundaries/Police_Force_Areas_December_2022_EW_BGC",rname="pfa_lowres"),
   list(fn="boundaries/Police_Force_Areas_December_2022_EW_BFC",rname="pfa"),
   list(fn="boundaries/Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BFC",rname="utla"),
   list(fn="boundaries/Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BGC",rname="utla_lowres")
),load_gpkg)

# the name fields in the pfa_lowres are not trimmed
pfa_lowres$PFA22NM <- trimws(pfa_lowres$PFA22NM)
lad_lowres$PFA23NM <- trimws(lad_lowres$LAD23NM)

# get a list of utla names and codes
utla_codes_to_names <- data.table(UTLACode=utla$UTLA22CD,UTLAName=utla$UTLA22NM)

if(!exists("postcode_to_utla")) {
   postcode_to_utla <- fread("../../common/data/external/PCD_OA21_LSOA21_MSOA21_LTLA22_UTLA22_CAUTH22_NOV23_UK_LU_v2.csv")
   # make a postcode sector column
   postcode_to_utla[,Sector:=tolower(gsub("(.*)\\s(.).*","\\1\\2",pcds))]
}