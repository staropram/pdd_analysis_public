# check dependencies
source("../../common/r/check_dependencies.R")

# make the output directory if it doesn't exist
if(!dir.exists("outputs")) {
   dir.create("outputs")
}

# process all the scripts in this directory in the correct order
source("druguse_baseline_compute.R")
source("druguse_baseline_plots.R")

# render the docs
source("druguse_baseline_docs.R")
