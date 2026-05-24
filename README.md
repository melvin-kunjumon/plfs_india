# Periodic Labour Force Survey (PLFS), 2017-18 to 2025

## Overview

At present, this repository hosts R scripts to:  

1) standardise and recode variables across survey rounds (both agricultural and calendar years), and save the cleaned and merged dataframes as Parquet files
2) generate summary statistics reported in the PLFS publications (to be done)

## Repository Structure

### `00_docs/`

Contains official PLFS documentation obtained from the Ministry of Statistics and Programme Implementation (MoSPI) Microdata website ([https://microdata.gov.in/NADA/index.php/home](https://microdata.gov.in/NADA/index.php/home)).

### `01_data/`

Contains processed dataframes and derived outputs.

**Note:** This repository does not include the raw PLFS data files. The scripts use the original `.txt` files (`.dta` for calendar years 2022 and 2023), which must be downloaded separately by the user from the MoSPI Microdata website.  

### Required Raw Data Structure

After downloading the raw files and their data layouts, place them in:  

`01_data/01_data_input/01_raw_data/`

Organise the files by survey round as follows:

`01_data/01_data_input/01_raw_data/plfsYYYY_YY/` for agricultural years

`01_data/01_data_input/01_raw_data/plfsYYYY/` for calendar years

Within each round-specific folder, the files should follow this naming convention:

`plfsYYYY_YY_perv1.txt` for Person Visit 1  
`plfsYYYY_YY_hhv1.txt` for Household Visit 1  
`plfsYYYY_YY_datalayout.xlsx` for the data layout  

**Note:** Replace `YYYY_YY` and `YYYY` with the relevant survey year (e.g., `2023_24` or `2025`).

### `02_scripts/`

Contains all the R scripts used to process the data files into survey year specific Parquet file.
