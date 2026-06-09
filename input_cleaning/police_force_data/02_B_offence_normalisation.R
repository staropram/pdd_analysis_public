# note we always source this relative to the working directory
source("../../common/r/file_paths.R")

# Function to escape all special regex characters in a pattern
escape_special_chars <- function(pattern) {
   str_replace_all(pattern, "([\\[\\]{}()*+?^$.|\\\\])", "\\\\\\1")
}

# load in the clean data
forceReload <- F
if(!exists("pfd")|forceReload) {
   pfd <- arrow::read_feather(PFD_complete_fn_FEATHER)
}

# make contact reason lowercase for consistent matching
pfd[,ContactReason:=tolower(ContactReason)]

# we also need the offence group classification
hoOffences <- data.table(read_excel("../../common/data/external/offence_group_classification_june_2023.xlsx",sheet=2))
hoOffences[,`Detailed offence`:=tolower(`Detailed offence`)]


# Make a map between all the different offence codes and our normalised codes
offenceMap <- data.table(ContactReason=unique(pfd$ContactReason))
offenceMap$Offence <- ""
offenceMap$Offence <- NA

# function to check which offence strings in the map a regex matches
# this checks strings that include everything in "include", but
# exclude everything in "exclude"
oCheckRE <- function(include=NA,exclude=NA) {
   includes <- ifelse(!is.na(include),lapply(include,function(x){grepl(x,offenceMap$ContactReason,fixed=T)}),T)
   excludes <- ifelse(!is.na(exclude),lapply(exclude,function(x){!grepl(x,offenceMap$ContactReason,fixed=T)}),T)
   combinedI <- Reduce(`&`,includes)
   combinedE <- Reduce(`&`,excludes)
   offenceMap[combinedI&combinedE,ContactReason]
}

# check home office "Detailed offence" field for offences that match
# all include strings and do not match any exclude strings
oCheckHO <- function(include=NA,exclude=NA) {
   includes <- ifelse(!is.na(include),lapply(include,function(x){grepl(x,hoOffences$`Detailed offence`,fixed=T)}),T)
   excludes <- ifelse(!is.na(exclude),lapply(exclude,function(x){!grepl(x,hoOffences$`Detailed offence`,fixed=T)}),T)
   combinedI <- Reduce(`&`,includes)
   combinedE <- Reduce(`&`,excludes)
   hoOffences[combinedI&combinedE,`Detailed offence`]
}

# check just unmapped offences
oCheckUM <- function(include=NA,exclude=NA) {
   includes <- ifelse(!is.na(include),lapply(include,function(x){grepl(x,unmappedOffences,fixed=T)}),T)
   excludes <- ifelse(!is.na(exclude),lapply(exclude,function(x){!grepl(x,unmappedOffences,fixed=T)}),T)
   combinedI <- Reduce(`&`,includes)
   combinedE <- Reduce(`&`,excludes)
   unmappedOffences[combinedI&combinedE]
}


oMapExact <- function(exactString,hoCode) {
   # check for a match first
   if(!any(str_detect(offenceMap$ContactReason,paste0("^",escape_special_chars(exactString),"$")),na.rm=T)) {
      stop(paste0("Could not find \"",exactString,"\", cannot continue"))
   }
   offenceMap[ContactReason==exactString,
      `:=`(
          OffenceType=hoOffences[`Offence code`==hoCode,`Offence type`],
          OffenceGroup=hoOffences[`Offence code`==hoCode,`Offence group`],
          Offence=hoOffences[`Offence code`==hoCode,`Offence`],
          OffenceCode=hoCode,
          OffenceDetailed=hoOffences[`Offence code`==hoCode,`Detailed offence`]
       )
   ]
}

oMapRE <- function(include,exclude,hoCode,checkOnly=F) {
   if(checkOnly) {
      return(oCheckRE(include,exclude))
   }
   hoOffence <- hoOffences[`Offence code`==hoCode,`Detailed offence`]
   #print("-----------------------------------------------------")
   #print(paste("Obtaining matches for",hoOffence,"matches: "))
   #print(oCheckRE(include,exclude))
   #print("-----------------------------------------------------")
   
   includes <- ifelse(!is.na(include),lapply(include,function(x){grepl(x,offenceMap$ContactReason,fixed=T)}),T)
   excludes <- ifelse(!is.na(exclude),lapply(exclude,function(x){!grepl(x,offenceMap$ContactReason,fixed=T)}),T)
   combinedI <- Reduce(`&`,includes)
   combinedE <- Reduce(`&`,excludes)
   selection <- combinedI & combinedE

   offenceMap[selection,
      `:=`(
          OffenceType=hoOffences[`Offence code`==hoCode,`Offence type`],
          OffenceGroup=hoOffences[`Offence code`==hoCode,`Offence group`],
          Offence=hoOffences[`Offence code`==hoCode,`Offence`],
          OffenceCode=hoCode,
          OffenceDetailed=hoOffence
       )
   ]
}


# this map is going to be populated manually, search strings should be
# checked that they do not over-capture before setting

# have to be careful here when we get new data that existing strings don't
# overmatch, check the output of oMapRE and others

# put this in the order of offence frequency to make matching faster

# note we only need to address the offences in the map: not all possible offences

## oMapRE INCLUDES EXCLUDES HOCODE

# Drugs
# cannabis possession 09261
oMapRE(c("cannabis","possess"),c("supply"),"09261")
oMapRE(c("092/61"),NA,"09261")
# cannabis possession with intent to supply  09281
oMapRE(c("cannabis","supply"),c("premises"),"09281")
# cannabis production 09221
oMapRE(c("cannabis","production"),NA,"09221")
oMapExact("drugs - cult. cannabis","09221")
oMapExact("drugs - prod. cannabis","09221")
# cannabis, permit premises to be used 09321
oMapRE(c("09321"),NA,"09321")
# crack cocaine possession 09254
oMapRE(c("crack","possess"),c("supply"),"09254")
oMapExact("092/54","09254")
# cocaine possession 09250
oMapRE(c("cocaine","possess"),c("supply","crack"),"09250")
oMapExact("092/50","09250")
# cocaine supply 09270
oMapRE(c("cocaine","supply"),c("crack"),"09270")
# heroin possession 09251
oMapRE(c("heroin","possess"),c("supply"),"09251")
oMapExact("092/51","09251")
# heroin, permit use of premises for supply  09231
oMapRE(c("heroin","supply","premises"),NA,"09231")
# amphetamine possession 09260
oMapRE(c("amphetamine","possess"),c("supply","meth"),"09260")
oMapExact("092/60","09260")
# methampetamine possession, 09367
oMapRE(c("crystal meth","possess"),c("supply"),"09367")
# cannabinoid possession 09262
oMapRE(c("cannabinoid","possess"),c("supply"),"09262")
# ketamine possession 09372
oMapRE(c("ketamine","possess"),c("supply"),"09372")
oMapExact("093/72","09372")
# psychoactive, possession with intent to supply 09344
oMapRE(c("psychoactive","supply"),NA,"09344")
# psychoactive, possession in a custodial institute 09347
oMapRE(c("psychoactive","possess","custodial"),NA,"09347")
# mdma, possession 09253
oMapRE(c("mdma","possess"),c("supply"),"09253")
oMapExact("092/53","09253")
# methadone, possession 09255
oMapRE(c("methadone","possess"),c("supply"),"09255")
oMapExact("092/55","09255")
# cathinone derivative, possession 09263
oMapRE(c("cathinone","possess"),c("supply"),"09263")
# GHB, possession 09371
oMapRE(c("ghb","possess"),c("supply"),"09371")
# LSD, possession 09252
oMapRE(c("lsd","possess"),c("supply"),"09252")
# bzp (a piperazine), possession 09370
oMapRE(c("bzp","possess"),c("supply"),"09370")
oMapExact("093/70","09370")
# unspecified, possession 09269
oMapRE(c("unspecified","possess"),c("supply"),"09269")
oMapExact("drugs - possess class b or c","09269")
# unspecified, supplying of offering to supply 09249
oMapRE(c("unspecified","supply","offer"),NA,"09249")
oMapExact("drugs - supplying controlled drug","09249")
oMapExact("drugs - concerned in supplying controlled drug","09249")
# other class A, possession 09259
oMapRE(c("class a","other","possess"),c("supply"),"09259")
oMapExact("092/59","09259")
oMapExact("drugs - possess class a","09259")
# other class A, possession with intent to supply 09279
oMapRE(c("class a","other","supply"),c("offer"),"09279")
oMapExact("possession of a controlled drug with intent to supply - class a","09289")
oMapExact("drugs - possession with intent to supply  - class a","09289")
# other class A, supply or offer to supply 09249
oMapRE(c("class a","other","supply","offer"),NA,"09249")
# other class B, possession 09265
oMapRE(c("class b","other","possess"),c("supply"),"09265")
# other class B, posession with intent to supply
oMapRE(c("class b","other","possess"),c("supply"),"09265")
oMapExact("possession of a controlled drug with intent to supply - class b","09265")
# other class B, production 09225
oMapRE(c("class b","other","production"),NA,"09225")
# other class C, possession 09268
oMapRE(c("class c","other","possess"),c("supply"),"09268")
oMapExact("possess a class c controlled drug (recordable)","09268")
oMapExact("092/68","09268")
# khat, possession, 09374
oMapRE(c("khat","possess"),c("supply"),"09374")
# anabolic steroids, 09267
oMapRE(c("steroids","possess"),c("supply"),"09267")
# methylamphetamine, possession 09367 
oMapRE(c("methylamphetamine","possess"),c("supply"),"09367")
# permit use of premises for drug under temporary order, 09387
oMapRE(c("permit","temporary","drug"),NA,"09387")
# permit premises to be used - other class A, 09319
oMapRE(c("09319"),NA,"09319")
# import psychoactive substance
oMapRE(c("09345"),NA,"09345")
oMapExact("drugs - importing controlled drug","09345")
# other drugs offences, 09340
oMapRE(c("09340"),NA,"09340")
# production of a drug class unspecified, 09229
oMapExact("drugs - producing controlled drug","09229")

# OTHER CRIMES

# robbery 03401
oMapRE(c("robbery"),NA,"03401")

# THEFT OFFENCES
# theft OF a motor vehicle 04801
oMapRE(c("theft","motor","of"),c("other"),"04801")
# theft of conveyance other than motor or pedal cycle, 04912
oMapRE(c("theft","motor","of","other"),NA,"04912")
# theft FROM a motor vehicle 04510
oMapRE(c("theft","motor","from"),c("other"),"04510")
oMapExact("theft - from vehicle","04510")
# theft FROM a vehicle other than a motor vehicle, 04511
oMapRE(c("theft","motor","from","other"),NA,"04511")
# theft of a pedal cycle, 04400
oMapRE(c("theft","pedal"),c("motor"),"04400")
# theft from a shop, 04600
oMapRE(c("theft","shop"),NA,"04600")
oMapRE(c("04600"),NA,"04600")
# theft from an automatic machine or meter, 04700
oMapRE(c("theft","automatic"),c("other"),"04700")
oMapExact("theft - from machine/meter","04700")
# theft in a dwelling, other than from automatic machine or meter, 04000
oMapRE(c("theft","dwelling","other"),NA,"04000")
# theft of mail bag or postal packet, 04200
oMapRE(c("theft","mail"),NA,"04000")
# theft - not classified elsewhere, 04910
oMapRE(c("theft","not classified"),NA,"04910")
oMapExact("theft - other","04910")
# theft from the person of another (called stealing from in HO), 03900
oMapRE(c("theft","person"),NA,"03900")
oMapRE(c("stealing","person"),NA,"03900")
oMapExact("039/00","03900")
# theft by an employee, 04100
oMapRE(c("theft","employee"),NA,"04100")
oMapExact("theft - from employer","04100")
# theft by "finding", not a real offence, classify as "other" 04910
oMapRE(c("theft","finding"),NA,"04910")
# theft of a fixture by tenant, other...  04910
oMapRE(c("theft","fixture"),NA,"04910")
# theft by "sneak-in", ... other ... 04910
oMapRE(c("theft","sneak-in"),NA,"04910")
# going equipped for theft 03300
oMapRE(c("theft","equipped"),NA,"03300")
# use to check for uncovered theft offences
#oCheckRE(c("theft"),c("motor","pedal","shop","dwelling","automatic","mail","not classified","person","employee","finding","fixture","equipped","sneak-in"))
# receiving stolen goods 05401
oMapRE(c("receiving stolen goods"),NA,"05401")
# helping with stolen goods 05402
oMapRE(c("stolen goods","assisting"),NA,"05402")
# unauthorised taking of a motor vehicle 13001
oMapRE(c("unauthorised taking","motor"),c("conveyance"),"13001")
oMapExact("take a motor vehicle without the owners consent","13001")
oMapExact("twoc - take a motor vehicle without the owners consent","13001")
# unauthorised taking of a conveyance other than a motor vehicle 13002
oMapRE(c("unauthorised taking","conveyance"),NA,"13002")
oMapRE(c("conveyance","driv"),NA,"13002")
oMapExact("twoc - take a conveyance (not motor vehicle/pedal cycle) without the owners consent","13002")
# theft of a pedal cycle, 04400
oMapRE(c("unauthorised taking","pedal"),c("conveyance"),"04400")
# taking or riding a pedal cycle without consent... 13718
oMapRE(c("taking","pedal","consent"),c("conveyance"),"13718")
# making off without payment, 05325
oMapRE(c("mak","off","payment"),NA,"05325")
# going equipped for theft, 03300
oMapRE(c("steal","equipped"),NA,"03300")
oMapExact("conspire to steal from another (recordable)","03300")
oMapExact("033/00","03300")

# CRIMINAL DAMAGE AND ARSON
# criminal damage to property valued under 5000 GBP, 14900
oMapRE(c("criminal damage","property","under"),NA,"14900")
oMapExact("criminal damage - to a dwelling","14900")
# whole bunch of other criminal damage under 5000 which falls under 14900
oMapRE(c("criminal damage","other","under"),c("property"),"14900")
oMapExact("criminal damage to a dwelling - value under £5000","14900")
oMapExact("criminal damage to a vehicle - value under £5000","14900")
oMapRE(c("criminal damage","building","under"),c("property"),"14900")
oMapRE(c("criminal damage","149"),NA,"14900")
oMapRE(c("other criminal damage"),c("under","over","149","058"),"14900")
oMapRE(c("damage","unknown"),NA,"14900")
oMapExact("criminal damage - to a vehicle","14900")
oMapExact("criminal damage - other","14900")
oMapExact("criminal damage - to a building other than a dwelling","14900")
# criminal damage to property (over 5000), classed as other 05800
oMapRE(c("criminal damage","property","over"),NA,"05800")
oMapRE(c("criminal damage","058"),c("racial"),"05800")
oMapExact("criminal damage to a vehicle - value over £5000","05800")
# other criminal damage over 5000 which falls under 05800
oMapRE(c("criminal damage","other","over"),c("property"),"05800")
# criminal damage endangering life (dwelling or vehicle), 05700
# this is an odd crime, it actually talks about causing an explosion, but
# this is the code used by some of the forces...
oMapRE(c("damage","endanger"),NA,"05700")
oMapRE(c("explosion","endanger"),NA,"05700")
# racially and religiously aggravated damage, odd categories 05801,05803,05804
oMapRE(c("criminal damage","racially","religiously"),NA,"05804")
oMapRE(c("05804"),NA,"05804")
# threat to commit criminal damage, 05911
oMapRE(c("damage","threat"),NA,"05911")
# checker
#oCheckRE(c("criminal damage"),c("endangering","property","other","racially","building","threat"))
# arson endangering life, 05601
oMapRE(c("arson","endangering"),c("not"),"05601")
# arson NOT endangering life, 05602
oMapRE(c("arson","not endangering"),NA,"05602")

# ASSAULT

# assault on a constable, 10423
oMapRE(c("assault","constable"),c("73"),"10423")
oMapExact("assault - on police","10423")
oMapExact("assault - with intent to resist arrest","10423")
# merseyside uses 00873 and has two categories one which means 10423
oMapRE(c("008/73"),c("emergency"),"10423")
# assault on an emergency worker, 00873
oMapRE(c("assault","emergency","worker"),NA,"00873")
# common assault, 10501
oMapRE(c("assault","common"),c("racial","emergency"),"10501")
oMapRE(c("assault by beating (recordable)"),c("racial","emergency"),"10501")
oMapRE(c("assault (vatp)"),c("racial","emergency"),"10501")
# assault occassioning actual bodily harm, 00806
oMapRE(c("assault","actual"),c("racial","emergency","constable"),"00806")
# assault on a custody officer, 10504
oMapRE(c("assault","prison","custody officer"),NA,"10504")
# racial or religiously aggrevated assault, 00857
oMapRE(c("assault","racial","religious"),NA,"00857")
oMapExact("008/57","00857")
# assault with intent to resist apprehension, 00820
oMapRE(c("00820"),NA,"00820")
# assault with intent to resist arrest, 00820
oMapExact("assault with intent to resist arrest","00820")
# gbh without intent, 00801
oMapExact("assault - s20 - gbh grievous bodily harm without intent","00801")
# administering poison with intent to injure or annoy, 00802
oMapExact("cause administer poison with intent to injure / aggrieve / annoy","00802")

#oCheckRE(c("assault"),c("common","constable","emergency worker","actual","assault by beating (recordable)","assault (vatp)","prisoner custody officer"))

# OTHER

# drunk and disorderly (in public), 14101
oMapRE(c("drunk","disorderly","public"),NA,"14101")
oMapExact("drunk and disorderly","14101")
# blackmail
oMapRE(c("blackmail"),NA,"03500")
# racially or religiously aggravated harrassment or alarm or distress, 00855
oMapRE(c("harass","racial"),c("stalking"),"00855")
# racially or religiously aggravated stalking without violence, 00856
oMapRE(c("harass","racial","stalking"),NA,"00855")
# harassment, alarm or distress, 12512
oMapRE(c("harass","alarm","distress"),c("racial","intentional","words"),"12512")
oMapExact("harassment","12512")
# causing intentional harassment harassment (sec 4a), 12509
oMapRE(c("harass","alarm","distress","intentional"),c("racial"),"12509")
# abstracting electricity 04300
oMapRE(c("electricity"),NA,"04300")
# breach of ASBO, 00832
oMapRE(c("anti-social","breach"),NA,"00832")
# interference with a motor vehicle, 12600
oMapRE(c("interference","motor"),NA,"12600")
oMapExact("vehicle interference - trailer","12600")
# tampering with a motor vehicle, 82506
oMapRE(c("tampering","motor"),NA,"12600")
# stalking in breach of sec 1 which amounts to stalking, 19512
oMapRE(c("stalking","breach","conduct"),NA,"19512")
# stalking involving fear and violence, 00865
oMapRE(c("stalking","fear","violence"),c("without"),"00865")
# stalking involving serious alarm/distress, 00866
oMapRE(c("stalking","alarm","distress"),NA,"00866")
# racially or religious aggravated stalking without violence, 00856
oMapRE(c("stalking","racial","without violence"),NA,"00856")
# threats to kill, 00301
oMapRE(c("threats","kill"),NA,"00301")
# violent disorder, 06500
oMapRE(c("violent disorder"),NA,"06500")
# public nuisance, 06622
oMapRE(c("public nuisance"),c("recklessly"),"06622")
# wounding with intent to do GBH, 00501
oMapRE(c("wounding"),c("racial","constable"),"00501")
# racially or religiously aggravated wounding, 00859
oMapRE(c("wounding","racial"),NA,"00859")
# fear or provocation of violence, 12511
oMapRE(c("fear","provocation"),c("racial","relig"),"12511")
oMapExact("putting people in fear of violence","12511")
# out of control dog that injures someone, 00821
oMapRE(c("dog","control","injur"),c("without","no injury"),"00821")
oMapExact("dog causing injury to a person or assistance dog","00821")
# obstruct a constable in execution of duty 10433
oMapRE(c("obstruct","constable","execution"),NA,"10433")
# resisting an accredited person, 10509
oMapRE(c("resist","accredited"),NA,"10509")
# aggravated vehicle taking, damage under 5k, 13101
oMapRE(c("aggravated", "vehicle","under"),NA,"13101")
# aggravated vehicle taking, damage or injury, 03702
oMapRE(c("aggravated","vehicle"),c("under","theft"),"03702")
# intentional strangulation, 00877
oMapExact("non-fatal strangulation and suffocation","00877")
# hold person in slavery, 03606
oMapRE(c("slavery","servitude"),c("knowingly"),"03606")
# forced labour, 03607
oMapRE(c("compulsory","labour"),c("knowingly"),"03607")
# false imprisonment, 03603
oMapRE(c("false","imprisonment"),c("or"),"03603")
# breach of a restraining order, 00831
oMapRE(c("breach","restraining"),c("sex","acquittal"),"00831")
# breach of a non-molestation order, 06639
oMapRE(c("breach","molestation"),NA,"06639")
# breach conditions of injunction against harassment, 00829
oMapRE(c("breach","harass","conditions"),NA,"00829")
# kidnapping, 03601
oMapRE(c("kidnap"),c("travel"),"03601")
# engaging in controlling behaviour in a relationship, 00867
oMapRE(c("controlling","family"),NA,"00867")
# being carried in a stolen vehicle, 13003
oMapRE(c("conveyance","carried"),NA,"13003")
# obstructing a search, 09389
oMapRE(c("obstruct","23","search"),c("terror"),"09389")
# disclose private sexual protographs, 00871
oMapRE(c("sex","disclose"),NA,"00871")
# cruelty to or neglect of children, 01103
oMapRE(c("child","neglect"),c("safety","food"),"01103")
# affray, 06601
oMapRE(c("affray"),NA,"06601")
# threatening words or behaviour, 06825
oMapRE(c("harass","behaviour"),c("anti-social"),"06825")
# abduction of a child by parent, 01301
oMapRE(c("abduction","child","parent"),NA,"01301")
# sending letters with intent to cause distress, 00872
oMapRE(c("letters","distress"),NA,"00872")
# there's 145 offences in the HO codes which are "not-known" as the descriptor
# 07602 is used when nothing is known at all, even the category
oMapRE(c("not known"),NA,"07602")
# arrange travel for person for exploitation, 03608
oMapRE(c("travel","exploit","person"),c("sex"),"03608")
# commiting or conspiring to comit an act outraging public decency, 06621
oMapRE(c("public decency"),NA,"06621")
# obstructing powers of search 09330
oMapRE(c("09330"),NA,"09330")
# this shouldn't be in the dataset (offender under 16), it's impossible
# for that to be the offence so we can map to Not Known
oMapRE(c("offender under 16"),NA,"07602")

# murder over 1 years old 00101
oMapRE(c("murder","over"),NA,"00101")
# attempted murder 00200
oMapRE(c("murder","attempt"),NA,"00200")
#oMapRE(c("do not use"),NA,"07602")

# non-notifable offence, this isn't an offence so put Not Known
# as we do not know what the police are intending here
oMapRE(c("non notifiable"),NA,"07602")

# some police forces provide the codes exactly, so deal with this all together
exactCodes <- unique(pfd[ContactReason %like% "^.../..$",ContactReason])
lapply(exactCodes,function(code){
   oMapExact(code,paste0(substr(code,1,3),substr(code,5,6)))
})

# if we have mapped all offences then the following statement will return an empty result
unmappedOffences <- offenceMap[is.na(Offence),ContactReason]
unmappedOffences <- unmappedOffences[!is.na(unmappedOffences)]
if(length(unmappedOffences)>0) {
  print("Some offences have not been mapped, this must be rectified before continuing")
  print("----------------------------------------------------------------------------")
  print(sort(unmappedOffences))
  print("----------------------------------------------------------------------------")
  print(paste(nrow(offenceMap)-length(unmappedOffences),"done ",length(unmappedOffences),"to go"))
}

#test <- normaliseOffence(pfd$ContactReason)

# prevalance table
createOffencePrevalanceTable <- function() {
   offencePrevalenceLong <- pfd[,.N,by=c("ForceName","OffenceDetailed")]
   offencePrevalence <- dcast(offencePrevalenceLong,OffenceDetailed~ForceName,value.var="N",fill=0)
   offencePrevalence[, N := rowSums(.SD, na.rm = TRUE), .SDcols = -"OffenceDetailed"]
   
   # merge in our offence grouping and validity columns
   offencePrevalence <- merge(offencePrevalence,unique(pfd[,c("OffenceDetailed","OurOffenceGroup","ValidOffence")]),by="OffenceDetailed")
   setnames(offencePrevalence,"OurOffenceGroup","Group")
   setnames(offencePrevalence,"ValidOffence","Valid")
   
   # Add an "All" offence to see sums
   forceSums <- offencePrevalence[, lapply(.SD, sum, na.rm = TRUE), .SDcols = -c("OffenceDetailed","Group","Valid")]
   forceSums[, OffenceDetailed := "All"]
   offencePrevalence <- rbindlist(list(offencePrevalence,forceSums),use.names=T,fill=T)
   offencePrevalence[, NForces := rowSums(.SD!=0, na.rm = TRUE), .SDcols = -c("OffenceDetailed","N","Group","Valid")]
   
   # order rows and columns appropriately
   setorder(offencePrevalence,-N)
   setcolorder(offencePrevalence,c("OffenceDetailed","N","NForces","Group","Valid"))
   
   datatable(offencePrevalence,tableOptions) %>% 
      formatStyle(
         columns = names(offencePrevalence),
         valueColumns = names(offencePrevalence),
         backgroundColor = styleEqual(0, '#ffcccc')
      )  %>% 
      formatStyle(
         columns = c("Valid"),
         valueColumns = c("Valid"),
         backgroundColor = styleEqual("Y", '#ccffcc')
      ) %>% 
      formatStyle(
         columns = c("Valid"),
         valueColumns = c("Valid"),
         backgroundColor = styleEqual("N", '#ffcccc')
      ) %>%
      formatStyle(
         columns = c("Valid"),
         valueColumns = c("Valid"),
         backgroundColor = styleEqual("U", '#ffcc99')
      ) %>%
      formatStyle(
         columns = c("Group"),
         valueColumns = c("Group"),
         backgroundColor = styleEqual(1, '#ccccff')
      ) %>% 
      formatStyle(
         columns = c("Group"),
         valueColumns = c("Group"),
         backgroundColor = styleEqual(2, '#ffffcc')
      )
}
#createOffencePrevalanceTable()