d <- copy(pfda)

d <- createExposureVariablePP(d,"EnteredDrugTreatmentAnte28DaysPost365Days")
d <- createExposureVariablePP(d,"EnteredDrugTreatmentAnte28DaysPost28Days")
d <- createExposureVariablePP(d,"EnteredDrugTreatmentAnte28DaysPostNone")

fit <- coxph(Surv(FollowupDuration,EnteredDrugTreatmentAnte28DaysPostNone) ~ WasDivertedOrOC22 + 
             + OffenceSimplified2
             + AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard + Sex + EthnicityCoarse
             + ForceReoffendingRate + ForceOCUPenetration + ForceFunding
             + TreatedForDrugsInLastFiveYears + cluster(ForceName),
        data=d[CohortGroup==1])
fitG2 <- coxph(Surv(FollowupDuration,EnteredDrugTreatmentAnte28DaysPostNone) ~ WasDivertedOrOC22 + 
             + OffenceSimplified2
             + AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard + Sex + EthnicityCoarse
             + ForceReoffendingRate + ForceOCUPenetration + ForceFunding
             + TreatedForDrugsInLastFiveYears + cluster(ForceName),
        data=d[CohortGroup==2])


baseFormula <- str_replace(ppBaseFormula,"EXPOSURE","EnteredDrugTreatmentAnte28DaysPostNone")
fit2 <- glmmTMB(as.formula(baseFormula),data=d[CohortGroup==1])
