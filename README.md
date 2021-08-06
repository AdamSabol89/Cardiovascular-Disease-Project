# Cardiovascular Disease Death Estimation: Project Overview
- Utilized various linear models to estimate the effects that mean_bmi or %obesity have on cardiovascular disease deaths with best model test RMSE of 22.71.
- Data was sourced from ourworldindata.org, cleaning was done to form one final dataset including cardiovascular disease deaths, mean_bmi, and gdp_per_capita. 
- Exploratory analysis and modeling notebooks were published on RPubs. 
- Utilized fixed and random effects models to estimate cardiovascular deaths. 

## Code and Resources Used
- R version 4.1 
- Libraries: tidyverse, ggplot, MASS, plotly, lme4, caTools, naniar. 
- My [RPubs](https://rpubs.com/AdamSabol89) account. 
### Data Sourcing and Cleaning 
- All data was sourced from ourworldindata.org, raw data files are provided in csv format for all files in this repo.
- Aggregated female and male BMI/%obesity data. 
- Data was filtered to ensure no missing year data, variable and country names were cleaned.
- Data was merged using a combination of inner and outer joins with a resultant clean dataset containing all features. 
### [Exploratory Data Analysis](https://rpubs.com/AdamSabol89/776735)
- For a thorough investigation of the created dataset I highly recommend consulting the Rmd-notebook, hosted on RPubs and linked above, provided below are a few sample images.
<p align="center">
  <img src="https://github.com/AdamSabol89/Cardiovascular-Disease-Project/blob/main/figures/scatter_year.png">
</p>
<p align="center">
  <img src="https://github.com/AdamSabol89/Cardiovascular-Disease-Project/blob/main/figures/missing_values.png">
</p>
<p align="center">
  <img src="https://github.com/AdamSabol89/Cardiovascular-Disease-Project/blob/main/figures/correlations_over_time.png">
</p>
### Model Building 
-
-
-
