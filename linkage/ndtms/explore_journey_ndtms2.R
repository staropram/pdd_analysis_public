# examine the pathway of a given person

# find someone likely to have a lot of journeys
# people born before 1980
#oldies <- journey %>% filter(dob < as.Date("1980-01-01"))

# count by number of rows, 

# lets use data.table, and we only care about certain columns
#j <- data.table(journey)
colsOfInterest <- c("attrbdat","agncy","pc","ethnic","triaged_jy","triaged","n_jy","disd","disd_ep","disd_jy","disrsn","disrsn_ep","disrsn_jy","dob")
js <- j[,..colsOfInterest]
#j <- data.table(journey %>% select(attrbdat,agncy,triaged_jy,triaged,n_jy,disd_ep,disd_jy,disrsn,dob))

# primary key is attrbdat,agncy
js[,id:=paste0(attrbdat,agncy)]

# find people born before 1980 and count number of records
oldiesCount <- js[dob < as.Date("1985-01-01"),.N,by=list(id)]
setorder(oldiesCount,-N)
oldieToExamine <- oldiesCount[N==30][2,]
personRows <- js[id==oldieToExamine$id]
setorder(personRows,-triaged)
View(personRows)


# find people born before 1980 and count number of journeys
#journeyCount <- j[dob < as.Date("1980-01-01"),list(JourneyCount=max(n_jy)),by="id"]
setorder(journeyCount,-JourneyCount)

# pick the first person with 5 journeys
#personToExamine <- journeyCount[JourneyCount==5]
#personRows <- j[attrbdat==personToExamine$attrbdat&agncy==personToExamine$agncy]
#view(person)
# primary key is attrbdat,agncy
# 
