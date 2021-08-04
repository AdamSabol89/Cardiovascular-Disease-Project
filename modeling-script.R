library(zoo)
library(MASS)
library(tidyverse)
library(ggplot2)
library(caTools)
library(plotly)
final_data <- read.csv("final_data.csv")

#Instantiate PRESS function
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(pr)
}

#Interpolate the rest of our data and remove na gdp_per_capita entries
model_data <- final_data %>% 
  group_by(Entity) %>% 
  mutate(GDP_Per_Capita = na.approx(GDP_Per_Capita, maxgap = 25, rule = 2))

model_data <- model_data %>% 
  na.omit()

#Start out with a basic model 
formula <- (cardiovasc_deaths ~ mean_bmi +percent_obesity + percent_severe_obesity  + GDP_Per_Capita +factor(Entity) + factor(Year))

model1  <- lm(formula, data=model_data)


#Create dataframe with new residuals and predictions. 
model_eval<- function(model, data){

  stanresid<-model %>% 
    rstandard()
  
  yhat <-model %>% 
    predict()
  
  resid <- model$residuals
  
  pressres <- model %>% 
    PRESS()
  
  stures <-  model %>% 
  studres()
  
  stanres <- model %>% 
    rstandard()
  fulldf<-cbind(data$cardiovasc_deaths, yhat, resid, stanres, pressres, stures)
  fulldf = as.data.frame(fulldf)
  names(fulldf) <- c("Y","Y_Hat", "residuals", "standardized_residuals", "press_residuals", "studentized_residuals")
  return(fulldf)
}

residuals_model1<- model_eval(model1, model_data)

#Plot diagnostic plots
ggplot(residuals_model1, aes( Y, Y_Hat))+
       labs(x='Actual', y ='Predicted')+
       geom_point()+
      geom_abline(intercept =0, slope =  1, color='red', size =2)

ggplot(residuals_model1, aes( Y_Hat, residuals))+
  labs(x='Predicted', y ='Residual')+
  geom_point()+
  geom_abline(intercept =0, slope =  0, color='blue', size =1)

ggplot(residuals_model1, aes( Y_Hat, standardized_residuals))+
  labs(x='Predicted', y ='Standardized Residual')+
  geom_point()+
  geom_abline(intercept =0, slope =  0, color='blue', size =1)

ggplot(residuals_model1, aes( Y_Hat, press_residuals))+
  labs(x='Predicted', y ='PRESS Residual')+
  geom_point()+
  geom_abline(intercept =0, slope =  0, color='blue', size =1)

ggplot(residuals_model1, aes( Y_Hat, studentized_residuals))+
  labs(x='Predicted', y ='Studentized Residual')+
  geom_point()+
  geom_abline(intercept =0, slope =  0, color='blue', size =1)

#Let's Keep track of the errors for our models
errors<- data.frame("Full_Test" = numeric(),"LOOCV" = numeric(),"Segment_Test" = numeric(),"Segment_Train" = numeric())
errors[1,1] <- sqrt(sum((residuals_model1$Y - residuals_model1$Y_Hat)^2)/length(residuals_model1[,1]))
up <-sum(residuals_model1$press_residuals^2)/length(residuals_model1[,1])   
errors[1,2]<-sqrt(up)

#Split data into train and test sets to approximate error for new obs.
set.seed(789)
training_samples <- model_data$cardiovasc_deaths %>% 
    createDataPartition(p= .9, list= FALSE)

training_samples <- training_samples %>% 
  as.vector()

train_data <- model_data[training_samples, ]
test_data <- model_data[-training_samples, ]

train_model  <- lm(formula, data=train_data)
summary(train_model)

predictions <- train_model %>% 
  predict(test_data)

errors[1,3] <- RMSE(predictions, test_data$cardiovasc_deaths)

predictions <- train_model %>% 
  predict(train_data)

errors[1,4] <- RMSE(predictions, train_data$cardiovasc_deaths)

#Lets try another model 
formula <- (log(cardiovasc_deaths) ~ mean_bmi +percent_obesity + percent_severe_obesity  + GDP_Per_Capita +factor(Entity) + factor(Year))

model2  <- lm(formula, data=model_data)


residuals_model2 <- model_eval(model2, model_data)

residuals_model2$Y <- log(residuals_model2$Y)

#Same plots as before
ggplot(residuals_model2, aes( Y, Y_Hat))+
  labs(x='Actual', y ='Predicted')+
  geom_point()+
  geom_abline(intercept =0, slope =  1, color='red', size =2)

ggplot(residuals_model2, aes( Y_Hat, residuals))+
  labs(x='Predicted', y ='Residual')+
  geom_point()+
  geom_abline(intercept =0, slope =  0, color='blue', size =1)

ggplot(residuals_model2, aes( Y_Hat, standardized_residuals))+
  labs(x='Predicted', y ='Standardized Residual')+
  geom_point()+
  geom_abline(intercept =0, slope =  0, color='blue', size =1)

ggplot(residuals_model2, aes( Y_Hat, press_residuals))+
  labs(x='Predicted', y ='PRESS Residual')+
  geom_point()+
  geom_abline(intercept =0, slope =  0, color='blue', size =1)

ggplot(residuals_model2, aes( Y_Hat, studentized_residuals))+
  labs(x='Predicted', y ='Studentized Residual')+
  geom_point()+
  geom_abline(intercept =0, slope =  0, color='blue', size =1)

#Add errors
errors[2,1] <- RMSE(exp(residuals_model2$Y), exp(residuals_model2$Y_Hat))

Yresid <- exp(residuals_model2$Y) - exp(residuals_model2$Y_Hat)
pr <- Yresid/(1-lm.influence(model2)$hat)
errors[2,2] <- sqrt(sum(pr^2)/length(pr))

train_model  <- lm(formula, data=train_data)

predictions <- train_model %>% 
  predict(test_data) %>% 
  exp()

errors[2,3] <- RMSE(predictions, test_data$cardiovasc_deaths)

predictions <- train_model %>% 
  predict(train_data) %>% 
  exp()
errors[2,4] <- RMSE(predictions, train_data$cardiovasc_deaths)

#Now lets try a model with random effects for country 
library(lme4)
formula <- (cardiovasc_deaths ~ percent_obesity + percent_severe_obesity + GDP_Per_Capita + 1+(1 + mean_bmi|Entity) + factor(Year))
model_3 <- lmer(formula, data=model_data, REML=FALSE)
summary(model_3)
jtools::summ(model_3)

#Compute full error
predictions <- model_3 %>% 
  predict(model_data)

errors[3,1]<- RMSE( model_data$cardiovasc_deaths, predictions)

#Compute train and test error
train_model <- lmer(formula, data=train_data, REML=FALSE)
predictions <- train_model %>% 
  predict(test_data)
errors[3,3]<- RMSE( test_data$cardiovasc_deaths, predictions)

predictions <- train_model %>% 
  predict(train_data)
errors[3,4]<- RMSE( train_data$cardiovasc_deaths, predictions)

predictions <- model_3 %>% 
  predict(model_data)

residuals_model3 <- as.data.frame(cbind(predictions, model_data$cardiovasc_deaths))
residuals_model3$residuals <- residuals_model3$V2 - residuals_model3$predictions 
names(residuals_model3)[2] <- "actual"
residuals_model3$Entity <- model_data$Entity

ggplot(residuals_model3, aes( actual, predictions))+
  labs(x='Actual', y ='Predicted')+
  geom_point()+
  geom_abline(intercept =0, slope =  1, color='red', size =2)

ggplot(residuals_model3, aes( predictions, residuals))+
  labs(x='Predicted', y ='Residual')+
  geom_point()+
  geom_abline(intercept =0, slope =  0, color='blue', size =1)

errors[1, 5] <- "model_1"
errors[2, 5] <- "model_2"
errors[3, 5] <- "model_3"
names(errors)[5] <- "Model"

ggplot(errors, aes(x=Model, y=Segment_Test ))+
  labs( x= "Model", y= "Test RMSE") +
  geom_bar(stat="identity", width=.009)