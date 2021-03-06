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

### [Model Building](https://rpubs.com/AdamSabol89/796262)
- For a thorough explanation of the models and cross-validation methods used in this project I highly recommend consulting the Rmd-notebook; hosted on RPubs and linked above.
- Three linear models were used in this notebook, two fixed effect, and one mixed effect. 
- All models include fixed effects for year, the first model is a simple fixed effects mode, the second a fixed effect model with a log-tranformed response, and the third is a mixed effects model with random effects for mean_bmi by country. 
- Provided below is the predicted vs. actual plot for the mixed effect model with raw residuals, the final image is a depiction of the test error for all three models. 
<p align="center">
  <img src="https://github.com/AdamSabol89/Cardiovascular-Disease-Project/blob/main/figures/download.png">
</p>

<p align="center">
  <img src="https://github.com/AdamSabol89/Cardiovascular-Disease-Project/blob/main/figures/model_errors.png">
</p>
