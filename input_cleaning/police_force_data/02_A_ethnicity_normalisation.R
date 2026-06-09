
# load the ethnicity maps
ethnicityMapIC <- read_excel("../../common/data/internal/EthnicityMap.xlsx",sheet="ICMap")
ethnicityMapSDE <- read_excel("../../common/data/internal/EthnicityMap.xlsx",sheet="SDEMap")

# make the supplied text uppercase to reduce trivial discrepancies
pfd[,EthnicityAsSuppliedIC:=toupper(EthnicityAsSuppliedIC)]
pfd[,EthnicityAsSuppliedSDE:=toupper(EthnicityAsSuppliedSDE)]

# merge the IC map in
pfd <- merge(pfd,ethnicityMapIC,by=c("EthnicityAsSuppliedIC"),all.x=T)
# merge the SDE map in
pfd <- merge(pfd,ethnicityMapSDE,by=c("EthnicityAsSuppliedSDE"),all.x=T)
#pfd <- merge(pfd,ethnicityMap,by=c("ForceName","EthnicityAsSuppliedSDE","EthnicityAsSuppliedIC"),all.x=T)

# determine EthnicityCoarse using both derived ethnicities, preferring SDE 
pfd[,EthnicityCoarse:=fifelse(
   is.na(EthnicityAsSuppliedSDEToCoarse),
   EthnicityAsSuppliedICToCoarse,
   EthnicityAsSuppliedSDEToCoarse
)]