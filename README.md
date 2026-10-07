# Introduction

This software repository contains software to analyse the Police, Drugs,
and Diversion (PDD) project

# Directory structure

-   `analysis` This directory contains separate and individual analyses
    -   `Equity` Who is criminalised and who is offered diversion, as a
        function of ethnicity
    -   `Out1Reoffending` Primary analysis: diversion and reoffending
    -   `Out2DrugTreatment` Secondary analysis: diversion and entry into
        drug treatment
-   `common` Scripts and resources that are common to more than one
    analysis task.
-   `input_cleaning` Cleaning of existing analysis inputs
    -   `police_force_data` Everything required to clean police provided
        suspect data.
-   `input_synthesis` Creation of analysis inputs from external data.
    -   `deprivation_baseline` Creation of police-force level
        deprivation data from LA/UTLA stats.
    -   `druguse_baseline` Creation of police-force level drug use data
        from LA/UTLA stats.
    -   `pfa_to_utla` Maps police force areas to upper tier local
        authorities
    -   `postcode_sector_to_pfa` Maps postcode sectors to police force
        areas
    -   `reoffending_baseline` Creation of police-force level
        reoffending data from LA/UTLA stats.
-   `linkage` Linking suspect data to external databases
    -   `ndtms` Link the police force data to the National Drug
        Treatment Monitoring System
-   `master_scripts` Scripts to run EVERYTHING or partially for creation
    of final analysis data.
-   `matching` Creation of matched data for each intervention force from
    analysis datasets
-   `imputation` Bayesian imputation code, as well as legacy code from alternative matching approach

# Accessing final analysis datasets

It is probable that you might only need to access final analysis
datasets.

Once you have `file_paths.R` setup and working, if you import this, you
will expose the abstract mapped names of data on the secure drive. The
most important ones are:

1.  `PFD_analysis_full` This is is the filename of a `feather` file for
    the fully cleaned and consolidated police force data, consolidated
    in a single file, with linked NDTMS fields included, but stripped of
    identifiers. This is what you should use for most analysis

# Process to go from raw police force data to final analysis datasets

There is a script `master_scripts/run_everything.R` which will execute
all the steps outlined below. Do not do this unless you need to
regenerate everything as it will take quite a while.
does:

The script `00_process_all.R` is executed from within `input_cleaning/police_force_data` this will itself call a bunch of scripts to perform the following steps:

    1.  Load in each raw police-force supplied excel file and partially
        clean it: this entails joining together different excel sheets,
        normalising all the column names, and removing erroneous data.
    2.  Join together the normalised police-force data and clean it: 
    
        i) Normalise ethnicity data to coarse ethnicity categories.
        ii) Normalise offence data to valid home office offence codes. 
        iii) Normalise drug offence data to named drugs. 
        iv) Normalise outcome types to valid home office outcome codes.
        v) Reduce multi-crime incidents to a single incident each
        vi) Create censorship windows for each incident.
        vii) Merge in any force-level variables such as per capita spending on policing.

The script `00_process_all.R` is executed from within `linkage/ndtms` this will itself call a bunch of scripts to perform the following steps:

1.  Create journey-level discharge reasons for each episode in NDTMS
    using the parent journey dischanrge reason
2.  Add in fields we will use for linkage
3.  Reduce the data down to just these linkage fields for linking
4.  Since we use a deterministic link, the splink stuff is defunct, an R
    join is now used instead.
5.  The linkage step can produce one-to-many and many-to-many links, so
    we select only those links that are one-to-one to determine those
    that appear in our police force data
6.  Subset NDTMS to only those that appear in our police force data for
    later use
7.  Merge the linked NDTMS data with the police force data to create a
    linked dataset. Note that this step can be re-executed standalone
    without having to re-run the linkage if the police force data
    changes (without ID data changing) such as new metadata added or if
    we want to extract different variables from NDTMS. The subscript
    that does this is called `07_merge_linked_data_with_pfd.R`

    
The script `create_matched_controls.R` is executed from within `matching` to create matched controls for each intervention force / group combination. It checks the control reuse percentage for each `K` and each intervention force / group combination and selects the `K` that maximises the analysis data cardinality without exceeding 30% reuse. Note that the script `rejoing_matched_rows.R` can be executed to rejoin the police force data based on the matches without re-running

We also need imputation etc. At some point the "run everything from one place" approach broke down due to time constraints and changing requirements, but generally each directory has a self-contained series of operations as sequentially numbered scripts.