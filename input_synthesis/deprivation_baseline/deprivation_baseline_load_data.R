library(sf)
# a .gitignore in the data/ directory stops large files from
# being pushed to the repo

# Population Weighted Centroids (PWC) of LSOA (2011)
# https://ckan.publishing.service.gov.uk/dataset/lower-layer-super-output-areas-december-2011-ew-population-weighted-centroids
# The 2011 LSOA are used in the IMD19 stats

# GPKG data for the boundaries needs to be obtained from
# https://geoportal.statistics.gov.uk/
#
# i) Boundaries -> Other Boundaries -> Police Force Areas
# obtain the BFC and BGC files and save as
# Police_Force_Areas_December_2022_EW_BFC.gpkg
# Police_Force_Areas_December_2022_EW_BGC.gpkg
#
# list of participating police forces is called pf.csv
# should be updated to reflect actual participation as needed

library(data.table)
library(readODS)
library(readxl)

# imd 19
#https://assets.publishing.service.gov.uk/media/5d8b3abded915d0373d3540f/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx
xlfn <- "data/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
imd19 <- read_excel(xlfn,sheet=2)

# load in the 2011 LSOA PWCs
lsoa_pwc <- st_read("data/LSOA_Dec_2011_PWC_in_England_and_Wales_2022.geojson")

# function to load a gpkg into the global environment
load_gpkg <- function(params) {
   if(!exists(params$rname)) {
      d <- st_read(paste0("./data/",params$fn,".gpkg"))
      assign(params$rname, d, envir = .GlobalEnv)
   }
}

lapply(list(
   list(fn="Police_Force_Areas_December_2022_EW_BGC",rname="pfa_lowres"),
   list(fn="Police_Force_Areas_December_2022_EW_BFC",rname="pfa")
),load_gpkg)

# load list of participating police forces
pf_participating <- fread("../../common/data/pf.csv",sep="\t")

# miscellaneous, options for DT datatable outputs
tableOptions <- list(
   pageLength = -1, # Show all rows
   paging = F,
   info = F,
   searching = F,
   ordering = F,
   columnDefs = list(list(visible=FALSE, targets=0)) # hide row numbering
)
