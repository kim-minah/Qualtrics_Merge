# Qualtrics Merge – Child Psych Study

## Overview
This repository contains R scripts for merging **Emotional Development Questionnaire** data exported from Qualtrics as part of the Child Psych study.  

The workflow harmonizes questionnaire outputs across multiple age groups and pilot data, standardizes variable names, removes redundant/test entries, and produces a single cleaned dataset for analysis. Previous raw exports and merged files are automatically archived for reproducibility.

---

## Repository Contents
- **`ChildPsych_Qualtrics_Merge_v3.R`**  
  Main script for reading, cleaning, and merging Qualtrics CSV exports. Handles age-specific datasets, applies standardized variable names, archives old files, and outputs a merged CSV.

- **`ChildPsych_QualtricsColNames_v2.R`**  
  Supporting script that defines standardized column names across study waves (main and pilot).  
  - Reads `input/Qualtrics_Questions.csv` to build variable name lists.  
  - Produces `colnames_all`, a list of column name sets used by the merge script.  
  - Accounts for differences between age groups (e.g., CBCL Preschool vs. CBCL, pilot-specific GEM & ICUP questionnaires, reordered preschool items).  

- **`input/`**  
  Directory for raw Qualtrics CSV exports. Required files include:  
  - Pilot datasets (`Emotional+Epigenetic+Development...csv`)  
  - Main study datasets (4–5 yrs, 6 yrs, 7 yrs, 8–12 yrs)  
  - `Qualtrics_Questions.csv` (reference file for variable naming)

- **`archived_qualtrics_exports/`**  
  Created automatically. Stores old Qualtrics exports after each merge.

- **`archived_merged_qualtrics/`**  
  Created automatically. Stores old merged outputs after each merge.

- **Output file**  
  `Childpsych_Merged_Qualtrics_M_DD_YYYY.csv` (date-stamped merged dataset)

---

## Requirements
Developed in **R ≥ 4.0**.  
Install required packages with:

```r
install.packages(c("psych", "stringi", "filesstrings"))

## Workflow
1. Place raw Qualtrics CSV exports in the `input/` directory.  
2. Ensure `Qualtrics_Questions.csv` is present in `input/`.  
3. Run the merge script:
   ```r
   source("ChildPsych_Qualtrics_Merge_v3.R")
   ```
(This will automatically source ChildPsych_QualtricsColNames_v2.R.)
4. Script actions:

Applies harmonized column names from ChildPsych_QualtricsColNames_v2.R
Removes redundant rows (e.g., test data, Qualtrics metadata fields)
Reconciles differences across age groups and pilot datasets
Merges all datasets into a single file
Archives previous exports and merged outputs
Writes a new merged dataset to the repo root

## Output
Childpsych_Merged_Qualtrics_M_DD_YYYY.csv
Cleaned, standardized dataset combining pilot and main study questionnaire data.
QC steps at the end of the script compare ExternalReference and DEMOS_Q1 IDs and report mismatches.

## Notes

Pilot data participants are treated as timepoint 2 in ID fields.

Differences in questionnaire structure between age groups (e.g., CBCL Preschool vs. CBCL, missing ICUP_Q7 in pilot) are resolved automatically by the colnames script.

Large CSV files and history files (*.csv, .Rhistory) should be excluded from version control via .gitignore.
