library(sf)
source("../../common/r/file_paths.R")
# a .gitignore in the ../../common/data/boundaries/ directory stops large files from
# being pushed to the repo

# GPKG data for the boundaries needs to be obtained from
# https://geoportal.statistics.gov.uk/
# Note that BFC and BGC stand for Boundary Full Clipped and 
# Boundary Generalised Clipped, these are both clipped to mean high water mark
# but the generalised boundary uses a 20m grid to make it lower resolution
#
# i) Boundaries -> Administrative Boundaries -> UTLAs
# obtain the BFC and BGC files and save as
# Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BFC.gpkg
# Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BGC.gpkg
#
# ii) Boundaries-> Other Boundaries -> Police Force Areas
# obtain the BFC and BGC files and save as
# Police_Force_Areas_December_2022_EW_BFC.gpkg
# Police_Force_Areas_December_2022_EW_BGC.gpkg
#
# list of participating police forces is called pf.csv
# should be updated to reflect actual participation as needed
#
# Proven reoffending stats are obtained from 
# https://www.gov.uk/government/collections/proven-reoffending-statistics
# click on "Proven reoffending statistics: January to March 2022"
# obtain the file "CSV format of data tools" and extract the
# following file from the zip:
# PRSQ_Geographic_data_April 2011 to March 2022.csv'

# function to load a gpkg into the global environment
load_gpkg <- function(params) {
   if(!exists(params$rname)) {
      d <- st_read(paste0("../../common/data/external/boundaries/",params$fn,".gpkg"))
      assign(params$rname, d, envir = .GlobalEnv)
   }
}

# load the boundary gpkg files
lapply(list(
   list(fn="Counties_and_Unitary_Authorities_December_2022_Boundaries_UK_BFC",rname="auths"),
   list(fn="Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BFC",rname="utla"),
   list(fn="Police_Force_Areas_December_2022_EW_BFC",rname="pfa"),
   # also load in low res data for plots
   list(fn="Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BGC",rname="utla_lowres"),
   list(fn="Police_Force_Areas_December_2022_EW_BGC",rname="pfa_lowres")
),load_gpkg)

# load the list of participating police forces
policeForceInfo <- data.table(read_excel("../../common/data/internal/PoliceForceInfo.xlsx",sheet=1,
                                            col_types=c("text","text","text","logical","logical")))

# miscellaneous, options for DT datatable outputs
tableOptions <- list(
   pageLength = -1, # Show all rows
   paging = F,
   info = F,
   searching = F,
   ordering = F,
   columnDefs = list(list(visible=FALSE, targets=0)) # hide row numbering
)
