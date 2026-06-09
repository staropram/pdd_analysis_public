library(survival)

ppCoxFormula <- "
Surv(ExposureTimeDays, DidReoffend) ~ DiversionForce +
OffenceSimplified2 +
AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard +
Sex + EthnicityCoarse +
ForceReoffendingRate + ForceOCUPenetration + ForceFunding +
TreatedForDrugsInLastFiveYears +
HistoricalPossessionCount +
HistoricalOtherDrugsCount +
HistoricalTheftCount +
HistoricalViolenceCount +
cluster(ForceName)
"
ppCoxFormula <- as.formula(gsub("\n"," ", ppCoxFormula))

testData <- pfda[CohortGroup==1]
# do a complete case analysis
testData <- testData[!is.na(EthnicityCoarse)]
testData <- testData[!is.na(HOOutcomeCodeLong)]



#p30_div <- with(basehaz(fit, centered=FALSE),
#                mean(1 - exp(-hazard[max(which(time<=30))] *
#                                exp(predict(fit, newdata=transform(pfda, WasDiverted=1),
#                                            type="lp")))))


# assume the cannabis cohort was diverted and compute the expected survival
# at 30 days
fit <- coxph(ppCoxFormula,data = testData)
bh <- basehaz(fit, centered = FALSE)
H0_30 <- bh$hazard[max(which(bh$time <= 30))]

nd <- testData[OffenceSimplified2=="Cannabis"]
nd$WasDivertedOrOC22 <- T
eta <- predict(fit, newdata = nd, type = "lp")
p30_div <- mean(1 - exp(-H0_30 * exp(eta)),na.rm=T)
print(p30_div)

# assume the cannabis cohort was not-diverted and compute the expected survival
nd$WasDivertedOrOC22 <- F
#nd$OffenceSimplified2 <- "Cannabis"
eta <- predict(fit, newdata = nd, type = "lp")
p30_div <- mean(1 - exp(-H0_30 * exp(eta)),na.rm=T)
print(p30_div)

# check if in each OffenceSimplified2 category we actually have enough people
# to make a robust prediction
offenceCountsG1 <- pfda[CohortGroup==1,
   list(
      .N,
      NumReoffenders=sum(ExposureTimeDays<30&DidReoffend==T),
      NumDiverted=sum(WasDivertedOrOC22),
      NumNotDiverted=sum(!WasDivertedOrOC22),
      Reoffended30DayDiverted=sum(ExposureTimeDays <30 & DidReoffend & WasDivertedOrOC22),
      Reoffended30DayNotDiverted=sum(ExposureTimeDays <30 & DidReoffend & !WasDivertedOrOC22)
   ),
by=OffenceSimplified2]

offenceCountsG2 <- pfda[CohortGroup==2,
   list(
      .N,
      NumReoffenders=sum(ExposureTimeDays<30&DidReoffend==T),
      NumDiverted=sum(WasDivertedOrOC22),
      NumNotDiverted=sum(!WasDivertedOrOC22),
      Reoffended30DayDiverted=sum(ExposureTimeDays <30 & DidReoffend & WasDivertedOrOC22),
      Reoffended30DayNotDiverted=sum(ExposureTimeDays <30 & DidReoffend & !WasDivertedOrOC22)
   ),
by=OffenceSimplified2]