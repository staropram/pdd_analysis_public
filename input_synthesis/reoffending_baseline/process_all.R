# check dependencies
source("../../common/r/check_dependencies.R")

# make the output directory if it doesn't exist
if(!dir.exists("outputs")) {
   dir.create("outputs")
}

# process all the scripts in this directory in the correct order
source("reoffending_baseline_compute.R")
source("reoffending_baseline_plots.R")

# render the docs
source("reoffending_baseline_docs.R")
