source("../../common/r/file_paths.R")
library(data.table)
pc_to_pfa_out_fn <-  "../../common/data/internal/pc_to_pfa.csv"
pc_to_pfa_base_path <-  "../../common/data/external/pc_to_pfa/"
pc_to_pfa_files <- list.files(pc_to_pfa_base_path)

# load and merge all pc to pfa individual files
pc_to_pfa <- rbindlist(lapply(pc_to_pfa_files,function(fn) {
   fread(paste0(pc_to_pfa_base_path,fn))
}))

# add in a sector
pc_to_pfa[,Sector:=gsub("(.*) (.).*","\\1\\2",Postcode)]
pc_to_pfa[PFA=="",PFA:=NA]
setnames(pc_to_pfa,"PFA","ForceNameLong")
# we also need our short force names
pc_to_pfa <- merge(pc_to_pfa,policeForceInfo[,c("ForceNameLong","ForceName")],by="ForceNameLong",all.x=T)


fwrite(pc_to_pfa,pc_to_pfa_out_fn,na=NA)