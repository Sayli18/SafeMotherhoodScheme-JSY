# Replication Codes: The Seen and Unseen - Impact of Conditional Cash Transfer Program on Prenatal Sex Selection

#### This document provides instructions and an overview of Stata scripts to replicate the analysis in the paper 

*The Seen and Unseen: The Unintended Impact of a Conditional Cash Transfer Program on Prenatal Sex Selection*. 

#### Authors:
- [Sayli Javadekar](https://www.linkedin.com/in/sayli-javadekar-ph-d-4214492a/), ThoughtWorks Gmbh
- [Kritika Saxena](https://www.kritikasaxena.com), University of Groningen 

  
###### The PDF of the paper can be found in the repository.

---

## Prerequisites

### 1. Software and Packages
- **Stata Version**: 14 or above
- **Required Package**: `reghdfe`
  - Install `reghdfe` in Stata:
    ```stata
    ssc install reghdfe, replace
    ```

### 2. Data Requirements
- **NFHS 2015-16, NFHS 2005/06 (National Family Health Survey)**: Obtain from [NFHS India website](https://rchiips.org/nfhs/) or Ministry of Health and Welfare, Government of India.
- **MIS Data (Mother and Infant Information)**: Request from the Ministry of Health and Welfare, India.
- **Rainfall Data**: Download from CHIRPS dataset at [Climate Hazards Group website](https://www.chc.ucsb.edu/data/chirps).

### 3. Directory Setup
Organize your files in the following structure:

```plaintext

/Project_Root/
  ├── data/
  │    ├── nfhs_2015_16.dta
  │    ├── mis_data.dta
  │    ├── chirps_rainfall.dta
  ├── scripts/
  |    |── STEP1_merge_hh_birth
  │    ├── codes_JS21_Jan2025jope.do
  │    ├── falsification_dhs2005-06.do
  │    ├── child_anthopometrics.do
  │    ├── 1_zscores_rain.do
  │    ├── Step6_regression.do
  ├── output/
       ├── tables/
       ├── figures/ 

```
## Scripts Overview

The repository contains the following Stata `.do` files:

1. **`STEP1_merge_hh_birth.do`**  
   - Purpose: Prepares and merges datasets (NFHS birth recode and NFHS household recode).

2. **`codes_JS21_Jan2025jope.do`**  
   - Purpose: Data Cleaning, creating new relevant variables and running the final difference - in - difference (DiD) regression.


3. **`falsification_dhs2005-06.do`**  
   - Purpose: Runs the DiD regression for placebo data NFHS 2005/06.

4. **`1_zscores_rain.do`**  
   - Purpose: Runs the regression for the income effect channel.

5. **`Step6_regression.do`**  
   - Purpose: Runs the regression for the health worker channel.   

### Instructions to Run

1. Ensure Stata is installed and version 14 or higher is available.
2. Place the required datasets in the `data/` folder.
3. Run the `.do` files in the above order.

### 3. Acknowledgements
- **National Family Health Survey Data:** Demographic Health Survey USAID
- **MIS Data:** Ministry of Health and Welfare, India
- **CHIRPS Rainfall Data:** Climate Hazards Group
- **Stata Package:** reghdfe [Correia (2015)](https://scorreia.com/software/reghdfe/) <br>
  Reference: Correia, Sergio. 2017. “Linear Models with High-Dimensional Fixed Effects: An Efficient and Feasible Estimator” Working Paper. http://scorreia.com/research/hdfe.pdf

### 4. Contact

- This Markdown file provides an organized guide for replication. Please do not modeify any contents. Let me know if you find any errors or need further explanation!

