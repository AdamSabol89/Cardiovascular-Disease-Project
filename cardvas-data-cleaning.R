#Libraries
library(tidyverse)

#Read in the data
cardiovasc_deaths <- read.csv("cardiovascular-disease-death-rates.csv")
bmi_data <- read.csv("NCD_RisC_Lancet_2017_BMI_age_standardised_country.csv")
gdp_data <- read.csv("gdp-per-capita-worldbank.csv")
gini_data <- read.csv("economic-inequality-gini-index.csv")

#Get only data from shared years
bmi_data <- bmi_data %>% 
  filter(Year > 1989)
gdp_data <- gdp_data %>% 
  filter(Year > 1989)
gini_data <- gini_data %>% 
  filter(Year > 1989 )
cardiovasc_deaths <- cardiovasc_deaths %>% 
  filter(Year <2017)

#Verify above functions
bmi_data$Year %>% 
  unique() %>% 
  length()

cardiovasc_deaths$Year %>% 
  unique() %>% 
  length()


#Create data frame by sex
male_bmi <- bmi_data %>% 
  subset(bmi_data$Sex == 'Men', c("Country.Region.World","Sex","Year","Mean.BMI","Prevalence.of.BMI..30.kg.m²..obesity.","Prevalence.of.BMI..35.kg.m²..severe.obesity.") )
female_bmi <- bmi_data %>% 
  subset(bmi_data$Sex == 'Women', c("Country.Region.World","Sex","Year","Mean.BMI","Prevalence.of.BMI..30.kg.m²..obesity.","Prevalence.of.BMI..35.kg.m²..severe.obesity.") )

#Make dataframe for cumulative obesity and BMI rates 
cum_bmi <- data.frame(Entity = male_bmi$Country.Region.World, Year = male_bmi$Year, mean_bmi = (male_bmi[,4] + female_bmi[,4])/2, percent_obesity = (male_bmi[,5] + female_bmi[,5])/2, percent_severe_obesity = (male_bmi[,6] + female_bmi[,6])/2)

#Change variable names
names(gini_data)[3] <- 'GINI'
names(gdp_data)[4] <- 'GDP_Per_Capita'
names(cardiovasc_deaths)[4] <- 'cardiovasc_deaths'

#Rename US 
cum_bmi$Entity<-cum_bmi$Entity %>% 
  as.character()

cum_bmi[cum_bmi == "United States of America"] <- "United States"

cum_bmi$Entity<-cum_bmi$Entity %>% 
  as.factor()

#Join all of the datasets
final_data <- inner_join(cardiovasc_deaths, cum_bmi, c("Entity", "Year"))
final_data <- left_join(final_data, gdp_data, c("Entity", "Year"))
final_data <- left_join(final_data, gini_data, c("Entity", "Year"))

#Check missing values 
final_data$GINI %>% 
  is.na() %>% 
  mean()

#Interpolation is likely not possible given the prevalence of missing values so we drop 
final_data$GINI <- NULL 

#Save our progress
final_data %>% 
  write.csv("final_data.csv")