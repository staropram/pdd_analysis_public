
# various models to examine
ageFormula <- "WasDivertedOrOC22 ~ AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard"
ageFormula <- as.formula(ageFormula)


# DIVERSION OUTCOME

# only ethnicity
diversionFormulaEthnicityOnly <- "WasDivertedOrOC22 ~ EthnicityCoarse"
diversionFormulaEthnicityOnly <- as.formula(diversionFormulaEthnicityOnly)

# full formula without random effects
diversionFormulaFullNoRandom <- "WasDivertedOrOC22 ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse 
   + OffenceSimplified2"
diversionFormulaFullNoRandom <-  as.formula(gsub("\n","",diversionFormulaFullNoRandom))

diversionFormulaFullNoRandomPlusOffending <- "WasDivertedOrOC22 ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse  +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   + OffenceSimplified2"
diversionFormulaFullNoRandomPlusOffending <-  as.formula(gsub("\n","",diversionFormulaFullNoRandomPlusOffending))

diversionFormulaFullNoRandomPlusOffendingPlusPersonClustering <- "WasDivertedOrOC22 ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse  +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   + OffenceSimplified2 + (1|RandomParticipantID)"
diversionFormulaFullNoRandomPlusOffendingPlusPersonClustering <-
   as.formula(gsub("\n","",diversionFormulaFullNoRandomPlusOffendingPlusPersonClustering))

# full formula
diversionFormulaFull <- "WasDivertedOrOC22 ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse 
   + OffenceSimplified2 
   + (1|ForceName)"
diversionFormulaFull <-  as.formula(gsub("\n","",diversionFormulaFull))

diversionFormulaFullPlusOffending <- "WasDivertedOrOC22 ~ 
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse 
   + OffenceSimplified2 
   + (1|ForceName)"
diversionFormulaFullPlusOffending <-  as.formula(gsub("\n","",diversionFormulaFullPlusOffending))

diversionFormulaFullPlusOffendingPlusPersonClustering <- "WasDivertedOrOC22 ~ 
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse 
   + OffenceSimplified2 
   + (1|ForceName) + (1|RandomParticipantID)"
diversionFormulaFullPlusOffendingPlusPersonClustering <-  as.formula(gsub("\n","",diversionFormulaFullPlusOffendingPlusPersonClustering))

diversionFormulaFullPlusSlope <- "WasDivertedOrOC22~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse 
   + OffenceSimplified2 
   + (EthnicityCoarse|ForceName)"
diversionFormulaFullPlusSlope <-  as.formula(gsub("\n","",diversionFormulaFullPlusSlope))

# CRIMINALISATION OUTCOME
criminalisationFormulaEthnicityOnly <- "WasCriminalised ~ EthnicityCoarse"
criminalisationFormulaEthnicityOnly <- as.formula(criminalisationFormulaEthnicityOnly)

# full formula without random effects
criminalisationFormulaFullNoRandom <- "WasCriminalised ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse 
   + OffenceSimplified2"
criminalisationFormulaFullNoRandom <-  as.formula(gsub("\n","",criminalisationFormulaFullNoRandom))

criminalisationFormulaFullNoRandomPlusOffending <- "WasCriminalised ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex +
   EthnicityCoarse +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   OffenceSimplified2"
criminalisationFormulaFullNoRandomPlusOffending <-  as.formula(gsub("\n","",criminalisationFormulaFullNoRandomPlusOffending))

criminalisationFormulaFullNoRandomPlusOffendingPlusPersonClustering <- "WasCriminalised ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex +
   EthnicityCoarse +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   OffenceSimplified2 + (1|RandomParticipantID)"
criminalisationFormulaFullNoRandomPlusOffendingPlusPersonClustering <-  as.formula(gsub("\n","",criminalisationFormulaFullNoRandomPlusOffendingPlusPersonClustering))

# criminalisation full formula
criminalisationFormulaFull <- "WasCriminalised ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex 
   + EthnicityCoarse 
   + OffenceSimplified2 
   + (1|ForceName)"
criminalisationFormulaFull <-  as.formula(gsub("\n","",criminalisationFormulaFull))

criminalisationFormulaFullPlusOffending <- "WasCriminalised ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   EthnicityCoarse +
   OffenceSimplified2 +
   (1|ForceName)"
criminalisationFormulaFullPlusOffending <-  as.formula(gsub("\n","",criminalisationFormulaFullPlusOffending))

criminalisationFormulaFullPlusOffendingPlusPersonClustering <- "WasCriminalised ~ 
   AgeAtContactInDays_CenterStandard + AgeAtContactInDaysSquared_CenterStandard  + Sex +
   TreatedForDrugsInLastFiveYears +
   HistoricalPossessionCount +
   HistoricalOtherDrugsCount +
   HistoricalTheftCount +
   HistoricalViolenceCount +
   EthnicityCoarse +
   OffenceSimplified2 +
   (1|ForceName) + (1|RandomParticipantID)"
criminalisationFormulaFullPlusOffendingPlusPersonClustering <-  as.formula(gsub("\n","",criminalisationFormulaFullPlusOffendingPlusPersonClustering))