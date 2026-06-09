library(sf)
# a .gitignore in the data/ directory stops large files from
# being pushed to the repo

# Opiate and Crack Use prevalence estimates from
# https://www.gov.uk/government/publications/opiate-and-crack-cocaine-use-prevalence-estimates
# Download the 2018-2019 and 2019-2020 ODS files

# GPKG data for the boundaries needs to be obtained from
# https://geoportal.statistics.gov.uk/
#
# i) Boundaries -> Administrative Boundaries -> UTLAs
# obtain the BFC and BGC files and save as
# Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BFC.gpkg
# Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BGC.gpkg
#
# ii) Boundaries -> Other Boundaries -> Police Force Areas
# obtain the BFC and BGC files and save as
# Police_Force_Areas_December_2022_EW_BFC.gpkg
# Police_Force_Areas_December_2022_EW_BGC.gpkg
#
# iii) Boundaries -> Administrative Boundaries -> Local Authority Districts
# obtain the BGC file and save as:
# Local_Authority_Districts_December_2022_UK_BGC_V2.gpkg
#
# list of participating police forces is called pf.csv
# should be updated to reflect actual participation as needed

library(data.table)
library(readODS)

# Opiate and Crack use prevalence estimates
xlfn19 <- "../../common/data/external/OCU-prevalence-estimates-England_2018-2019.ods"
ocu <- read_ods(xlfn19,sheet=3,skip=4)

# function to load a gpkg into the global environment
load_gpkg <- function(params) {
   if(!exists(params$rname)) {
      d <- st_read(paste0("../../common/data/external/boundaries/",params$fn,".gpkg"))
      assign(params$rname, d, envir = .GlobalEnv)
   }
}

lapply(list(
   list(fn="Local_Authority_Districts_December_2022_UK_BGC_V2",rname="lad_lowres"),
   list(fn="Police_Force_Areas_December_2022_EW_BGC",rname="pfa_lowres")
),load_gpkg)

# load list of participating police forces
pf <- fread("../../common/data/internal/pf.csv",sep="\t")

# load the map between police force area and local authority
pfa_to_utla_fn <- "../pfa_to_utla/outputs/pfa_to_utla.tsv"
if(!file.exists(pfa_to_utla_fn)) {
   stop("In order to continue you need to generate the pfa_to_utla.tsv file \n
        by running the code in the input_synthesis/pfa_to_utla directory")
} else {
   pfa_to_utla <- fread(pfa_to_utla_fn)
}

# miscellaneous, options for DT datatable outputs
tableOptions <- list(
   pageLength = -1, # Show all rows
   paging = F,
   info = F,
   searching = F,
   ordering = F,
   columnDefs = list(list(visible=FALSE, targets=0)) # hide row numbering
)
