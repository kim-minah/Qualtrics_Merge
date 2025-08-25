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
