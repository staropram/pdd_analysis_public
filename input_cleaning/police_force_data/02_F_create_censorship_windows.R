# this modifies pfd so that each offence has an appropriate censorship window
# for follow-up, defined as follows:
# A) For individuals with one offence, the first of:
#    i) The date they died if they died
#   ii) The analysis endpoint
# B) For individuals with more than one offence, the first of:
#    i) The date of the next offence
#   ii) The date they died if they died
#  iii) The analysis endpoint

# this is actually straightforward to do using data.table's shift function
# we need to sort by PseudoID and contact date and then use the next 
# contact date as the censor window except when there is no more

# note that the project end date is obtained from 
projectEndDate <- as.Date("2024-03-20")
setorderv(pfd,c("PseudoID","ContactDate"))
# XXX we probably need to do this for each group
#pfd[,CensorDate := shift(ContactDate,type="lead",fill=projectEndDate),by=PseudoID]

pfd[CohortGroup==1,CensorDate := shift(ContactDate,type="lead",fill=projectEndDate),by=PseudoID]
pfd[CohortGroup==2,CensorDate := shift(ContactDate,type="lead",fill=projectEndDate),by=PseudoID]
pfd[,FollowupDuration := CensorDate-ContactDate]