fit <- glmer(
   (HOOutcomeCodeLong=="OC15") ~ EthnicityCoarse +  AgeAtContactInDays_CenterStandard +  AgeAtContactInDaysSquared_CenterStandard +  Sex + OffenceSimplified2 + 
   (1|ForceName),
   data=pfda[CohortGroup==1],
   family=binomial,
   control=glmerControl(optimizer="bobyqa")
)

confs <- confint(fit)