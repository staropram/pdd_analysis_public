print("Adding in derived variables")

# Create categories for each diversion type based on the index offence
pfd[,G1DiversionIncident:=F]
pfd[,G2DiversionIncident:=F]
pfd[WasDiverted==T&CohortGroup==1,G1DiversionIncident:=T]
pfd[WasDiverted==T&CohortGroup==2,G2DiversionIncident:=T]

# merge in the simplified offence type
offenceSimple <- data.table(read_excel("../../common/data/internal/OffenceSimplification.xlsx",sheet=1))
pfd <- merge(pfd,offenceSimple[,c("HOOffenceDetailed","OffenceSimplified","OffenceSimplified2")],by="HOOffenceDetailed",all.x=T)
pfd[,DrugTypeSimplified:=OffenceSimplified2]

# determine if the incident was criminalised
pfd[!is.na(HOOutcomeCodeLong),WasCriminalised:=HOOutcomeCodeLong %in% c("OC01","OC1A","OC03","OC3A")]

# add in some other useful variables
pfd[,AgeAtContactInDaysSquared:=AgeAtContactInDays^2]
pfd[,AgeAtContactInDays_CenterStandard :=(AgeAtContactInDays-mean(AgeAtContactInDays))/sd(AgeAtContactInDays)]
pfd[,AgeAtContactInDaysSquared_CenterStandard :=(AgeAtContactInDaysSquared-mean(AgeAtContactInDaysSquared))/sd(AgeAtContactInDaysSquared)]

pfd[,WasDivertedOrOC22:=WasDiverted]
pfd[!is.na(HOOutcomeCodeLong)&HOOutcomeCodeLong=="OC22",WasDivertedOrOC22:=T]

pfd[,LogFollowupDuration:=log(FollowupDuration)]

# set the reference categories for analysis
pfd[,EthnicityCoarse:=as.factor(EthnicityCoarse)]
pfd[,EthnicityCoarse:=relevel(EthnicityCoarse,"WHITE")]
pfd[,Sex:=as.factor(Sex)]
pfd[,Sex:=relevel(Sex,"Male")]
pfd[,DrugTypeSimplified:=as.factor(DrugTypeSimplified)]
pfd[,DrugTypeSimplified:=relevel(DrugTypeSimplified,"Cannabis")]
pfd[,OffenceSimplified:=as.factor(OffenceSimplified)]
pfd[,OffenceSimplified:=relevel(OffenceSimplified,"Possession Cannabis")]
pfd[,OffenceSimplified2:=as.factor(OffenceSimplified2)]
pfd[,OffenceSimplified2:=relevel(OffenceSimplified2,"Cannabis")]