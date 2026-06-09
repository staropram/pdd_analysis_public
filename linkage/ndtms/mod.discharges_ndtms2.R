# Discharges from treatment=====================================================

# Creates discharge reasons at episode and journey level.
# NB Look to replace with a csv lookup table rather than manual lookups

# If ethnicity already exists in the master journey file, all ethnicity
# variables are deleted and a warning is given
if(exists("disrsn_jy", journey))
{ warning("Journey level discharge reason already exist: all variables will be overwritten")
  journey <- select(journey, -starts_with("disrsn_"))
}

# Select only variables needed for discharge creation
discharges <- journey %>%
  select(attrb, dat, agncy, triaged, n_jy, disrsn, disd, disd_ep, disd_jy)

# Recodes old PbR discharge reasons to standard codes for successful completion
discharges <- discharges %>%
  mutate(disrsn = case_when(disrsn == 90 ~ 80,
                            disrsn == 91 ~ 82,
                            disrsn == 92 ~ 81,
                            is.na(disrsn) & !is.na(disd) ~ 0,
                            TRUE ~ as.double(disrsn)))

# Discharge reason of the episode-----------------------------------------------

# If discharge date and reason present, along with episode discharge date,
# and the latter matches the discharge date in that record -
# creates episode discharge reason from that record
discharges <- discharges %>%
  mutate(disrsn_ep = ifelse(is.na(disd) | is.na(disrsn) |
                              disd != disd_ep | is.na(disd_ep),
                            NA,
                            disrsn))

# Groups discharge reasons into smaller groups for inconsistency checking
discharges <- discharges %>%
  mutate(disrsn_ep = case_when(disrsn_ep %in% c(2,22:28) ~ 82,
                               disrsn_ep %in% c(1,15:21,80,81) ~ 80,
                               disrsn_ep == 9 ~ 89,
                               disrsn_ep %in% c(6,43:46,48,49) ~ 85,
                               disrsn_ep %in% c(50:53,55,56) ~ 7,
                               disrsn_ep %in% c(13,36:39,41,42) ~ 4,
                               disrsn_ep %in% c(64:67,69,70) ~ 11,
                               disrsn_ep %in% c(57:60,62,63) ~ 10,
                               disrsn_ep %in% c(8,14) ~ 87,
                               disrsn_ep %in% c(33,40,47,54,61,68) ~ 5,
                               disrsn_ep == 12 ~ 88,
                               disrsn_ep %in% c(3,29:32,34,35) ~ 86,
                               disrsn_ep %in% c(74,93:97) ~ 83,
                               # Gives large number for inconsistent 
                               # in maxima in next process
                               disrsn_ep %in% c(0,11) ~ 500,
                               TRUE ~ as.double(disrsn_ep))) %>%
  # Sorts discharge dates here to pick the latest discharge reason by 
  # discharge date associated with the triage date (episode start),
  # regardless of the agency code (provider)
  arrange(attrb, dat, agncy, triaged, desc(disd_ep)) %>%
  group_by(attrb, dat, agncy, triaged) %>%
  # If any discharge reason for the episode is not already flagged as
  # inconsistent and has multiple discharge reasons for the same discharge date
  # code these out to 'inconsistent' value, except in cases of:
  # i) As there are several discharge reasons of 'successful completion' and 
      # 'treatment declined', having multiple discharge reasons of these type 
      # are not counted as inconsistent - see ii)
  mutate(disrsn_jy = if_else(n_distinct(disrsn_ep, na.rm = TRUE) > 1 &
                               disrsn_ep < 500 &
                               # i)
                               !(disrsn_ep %in% c(1,2,15:22,71,80:82,88)),
                                  999,
                             disrsn_ep),
         disrsn_ep = dplyr::first(na.omit(disrsn_ep))) %>%
  ungroup()

# Discharge reason of the journey------------------------------------------------  

# If journey discharge date exists and this matches the episode discharge date,
# create journey discharge reason from episode discharge reason
discharges <- discharges %>%
  mutate(disrsn_jy = ifelse(disd_jy != disd_ep | is.na(disd_jy),
                             -Inf,
                            disrsn_jy))

# Finalises journey level discharge reason
discharges <- discharges %>%
  group_by(attrb, dat, n_jy, disd) %>%
  # If more than one discharge reason in the journey, flag as inconsistent
  mutate(disrsn_jy = ifelse(n_distinct(disrsn_jy, na.rm = TRUE) > 1,
                            # ii) If the only discharge reasons in the journey
                            # are for successful completion a), or for 
                            # treatment declined b), these are not flagged
                            # as inconsistent but instead the max/'worst' case
                            # is used; most of the time this will be the latest
                            # for the dataset - for successful completions it is
                            # 82 - 'SC - Occasional user'
                            ifelse(# a)
                                   max(as.numeric(!disrsn_jy %in% c(80:82))) == 0 |
                                   # b)
                                     max(as.numeric(!disrsn_jy %in% c(71,88))) == 0, 
                                   max(disrsn_ep),
                                   999),
                           disrsn_jy)) %>%
  ungroup() %>%
  group_by(attrb, dat, n_jy) %>%
  arrange(attrb, dat, n_jy, disd) %>%
  mutate(disrsn_jy = max(disrsn_jy, na.rm = TRUE),
         disrsn_jy = ifelse(disrsn_jy == -Inf,
                            999,
                            disrsn_jy)) %>%
  ungroup()

# Discharge reason groups-------------------------------------------------------------

# Recodes discharge reasons group variables
discharges <- discharges %>%
  mutate(# Older grouping used in the regular reporting?
         disrsn_jy.grp = case_when(disrsn_jy %in% c(1,2,15:28,80:82) ~ 1,
                                 disrsn_jy %in% c(5,33,40,47,54,
                                                  61,83,84,93:97) ~ 2,
                                 disrsn_jy %in% c(3,4,6:10,12,13,85:89) ~ 3,
                                 disrsn_jy %in% c(0,11,64:67,69:71,500) |
                                   (!is.na(disd_jy) & disrsn_jy < 0) ~ 4,
                                 is.na(disd_jy) ~ 5,
                                 disrsn_jy == 999 ~ 6,
                                 TRUE ~ 6),
         # Grouping used in the Annual Report (Adult and YP)
         disrsn_jy.grp_ar = case_when(disrsn_jy %in% c(2,22:28,82) ~ 1,
                               disrsn_jy %in% c(1,15:21,80,81) ~ 2,
                               disrsn_jy %in% c(9,89) ~ 3,
                               disrsn_jy %in% c(6,43:46,48,49,85) ~ 4,
                               disrsn_jy %in% c(7,50:53,55,56) ~ 5,
                               disrsn_jy %in% c(4,13,36:39,41,42) ~ 6,
                               disrsn_jy %in% c(0,11,64:67,69,70,500) |
                                 (!is.na(disd_jy) & disrsn_jy < 0) ~ 7,
                               disrsn_jy %in% c(10,57:60,62,63) ~ 8,
                               disrsn_jy %in% c(8,14,87) ~ 9,
                               disrsn_jy %in% c(74,83,93:97) ~ 10,
                               disrsn_jy == 84 ~ 11,
                               disrsn_jy %in% c(5,33,40,47,54,61,68) ~ 12,
                               disrsn_jy %in% c(12,71,88) ~ 13,
                               disrsn_jy %in% c(3,29:32,34,35,86) ~ 14,
                               disrsn_jy == 999 ~ 15,
                               TRUE ~ 15))

# Merge back into episode ------------------------------------------------------
# Has to be merged back in at episode level for discharges at provider level to
# be accurate.

# Creates a basic dataset of episode and discharge reason
# deduplicated by unique episode to match into main file
discharges <- discharges %>%
  select(attrb, dat, agncy, triaged, starts_with("disrsn_")) %>%
  distinct(attrb, dat, agncy, triaged, .keep_all = TRUE) 

# Merge back into master file
journey <- merge(journey,
                   discharges,
                   by=c("attrb", "dat", "agncy", "triaged"), # NB not specific agency
                   all.x=TRUE)
rm(discharges) # Remove discharges 
gc() # Frees up RAM after merge
         
#===============================================================================
