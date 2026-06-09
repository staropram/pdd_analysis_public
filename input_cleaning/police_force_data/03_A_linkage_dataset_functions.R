library(stringr)

# function which takes the police force data and
# makes a version to be used in linkage with NDTMS
makePFDLinkableWithNDTMS <- function(pfd,keepPNC=F) {
   # save the "linkable" data
   # Note that we don't want to keep Wakefield as a force name since its not
   # a real PFA and won't have an area mapped for the NDTMS people
   # pull in ParentForce and then rename it
   linkable_columns <- c("PseudoID","PNCNumber","FirstName","LastName","DOB","Sex","ParentForce")
   pfd_linkable <- unique(pfd[,..linkable_columns])
   setnames(pfd_linkable,"ParentForce","ForceName")
   
   
   # linkable data for NDTMS uses different fields
   pfd_linkable_with_NDTMS <- copy(pfd_linkable)
   if(!keepPNC) {
      pfd_linkable_with_NDTMS[,PNCNumber:=NULL]
   }
   
   # get first name initial and last name initial
   pfd_linkable_with_NDTMS[,FirstNameInitial:=toupper(str_sub(FirstName,1,1))]
   pfd_linkable_with_NDTMS[,LastNameInitial:=toupper(str_sub(LastName,1,1))]
   # NDTMS uses M and F for sex
   pfd_linkable_with_NDTMS[Sex=="Male",Sex:="M"]
   pfd_linkable_with_NDTMS[Sex=="Female",Sex:="F"]
   
   # drop fields not used in NDTMS linkage
   pfd_linkable_with_NDTMS[,FirstName:=NULL]
   pfd_linkable_with_NDTMS[,LastName:=NULL]
   
   pfd_linkable_with_NDTMS
}

# function which takes the police force data and
# makes a version to be used in linkage with PNC
makePFDLinkableWithPNC <- function(pfd) {
   # save the "linkable" data
   linkable_columns <- c("PseudoID","PNCNumber","FirstName","LastName","DOB","Sex","EthnicityCoarse","ForceName")
   pfd_linkable_with_PNC <- unique(pfd[,..linkable_columns])
   pfd_linkable_with_PNC
}