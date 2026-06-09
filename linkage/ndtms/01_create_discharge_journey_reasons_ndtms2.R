print("Creating discharge journey reasons")
# This script takes an NDTMS "journey" file from DHSC and runs the
# mod.discharges.R snippet on it. This ensures that correctly coded
# journey discharge reasons are added to each entry.

source("../../common/r/file_paths.R")

library(tidyr)
library(dplyr)
library(readr)
if(!exists("journey")) {
  journey <- read_rds(file=NDTMS_journey_file_ndtms2)
}

# now run DHSC code which extracts the discharge reason for each journey
if(!exists("discharges_normalised")) {
  source("mod.discharges_ndtms2.R")
   discharges_normalised <- T
}

# save as journey merged for further work
write_rds(journey,NDTMS_journey_with_discharge_reasons_fn_ndtms2)
