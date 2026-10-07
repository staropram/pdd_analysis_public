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

# Pipeline overview

There is a script `master_scripts/run_everything.R` which will execute  all the steps outlined below. Do not do this unless you need to regenerate everything as it will take quite a while. Three of the main processes are explained below, but there are others in their respective directories.

### Police Force Data Cleaning (`input_cleaning/police_force_data/00_process_all.R`)

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
        

### 2. NDTMS Linkage (`linkage/ndtms/00_process_all.R`)

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

    
### 3. Matched Control Selection (`matching/create_matched_controls.R`)
- Generates matched controls for each intervention force and group combination.
- Evaluates control reuse percentages across potential ratio values ($K$).
- Selects the optimal $K$ that maximises analytical cardinality while maintaining a maximum control reuse threshold of 30%.
- Matched indices can be rejoined onto the primary dataset using `rejoining_matched_rows.R`.


# Data availability and governance notice

Due to data governance protocols and sensitive system configurations across the Ministry of Justice (MOJ), Department of Health and Social Care (DHSC), and participating police forces, raw administrative data, import and export scripts, and scripts processing PNC data are not included in this public repository. 

This repository is provided for **methodological transparency** and documents the analytical logic downstream of the final linked, de-identified analytical dataset.