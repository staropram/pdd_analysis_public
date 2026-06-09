# set the random seed so we can run again without changing the results
# change this if you want a different matching
set.seed(259)

source("01_load_matching_functions.R")
source("02_determine_match_ratios.R")
source("03_create_matched_controls.R")
source("04_rejoin_matched_rows.R")
