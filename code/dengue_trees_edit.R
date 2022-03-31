library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rsample)
library(modelr)
library(fastDummies)
library(gbm)
library(pdp)

# Question 2
# Each row in the data set corresponds to a single week in a single city. The 
# variables in the data set are as follows:
# total_cases: Total recorded number of dengue fever cases that week. This is 
  # the outcome variable of interest in all regression models. 
# city: City in which the data was recorded (sj = San Juan, Puerto Rico; 
  # iq = Iquitos, Peru) 
# season: Season the data was recorded (spring, summer, fall, winter) 
# specific_humidity:Average specific humidity in grams of water per kilogram of 
  # air for the week. This is a raw measure of humidity based purely on how much 
  # water is in the air.
# tdtr_k: Average Diurnal Temperature Range (DTR) for the week. DTR is the 
#   difference between the maximum and minimum temperature for a single day.
# precipitation_amt: Rainfall for the week in millimeters
dengue <- read_csv("data/dengue.csv")

#Displays how many NAs are in a given column, then remove them
# 214 out of 1456 observations dropped by removing NAs here, not great, but best performance
sapply(dengue, function(x) sum(is.na(x)))
dengue = na.exclude(dengue)

# recoding categorical variables to make PD plots, removing irrelevant columns
dengue$season_num = recode(dengue$season, spring=1, summer=2, fall=3, winter=4)
dengue$city_num = recode(dengue$city, sj=0, iq=1)
dengue$specific_humidity = as.numeric(dengue$specific_humidity)
dengue$precipitation_amt = as.numeric(dengue$precipitation_amt)
dengue = dengue %>% select(-c(city, season))
view(dengue)
dengue = as.data.frame(dengue)

#train-test split
dengue_split = initial_split(dengue, prop = .8)
dengue_train = training(dengue_split)
dengue_test = testing(dengue_split)

#CART (CV build in)
dengue_tree_spec = rpart(total_cases ~ . -ndvi_ne - ndvi_nw - ndvi_se - ndvi_sw,
                    data = dengue_train, control = rpart.control(cp = 0.00001))
dengue_tree = rpart(total_cases ~ .,
                    data = dengue_train, control = rpart.control(cp = 0.00001))
#Random Forest
dengue_forest_all = randomForest(total_cases ~ .,
                             data = dengue_train, importance = TRUE, 
                             na.action = na.exclude)
dengue_forest_spec = randomForest(total_cases ~ . - ndvi_ne - ndvi_nw - ndvi_se - ndvi_sw,
                                 data = dengue_train, importance = TRUE, 
                                 na.action = na.exclude)
######################

#boost
boost1 = gbm(total_cases ~ .,
                         data = dengue_train,
             interaction.depth=5, n.trees=500, shrinkage=.05)

# Look at error curve -- stops decreasing much after ~300
gbm.perf(boost1)


yhat_test_gbm = predict(boost1, dengue_test, n.trees=50)

# RMSEs
modelr::rmse(dengue_tree, dengue_test)
modelr::rmse(dengue_tree_spec, dengue_test)
modelr::rmse(dengue_forest_all, dengue_test) #Best so far
modelr::rmse(dengue_forest_spec, dengue_test)
modelr::rmse(boost1, dengue_test)

# boost predict rmse
(yhat_test_gbm - dengue_test$total_cases)^2 %>% mean %>% sqrt

# PD plots
partialPlot(dengue_forest_all, dengue_test, 'specific_humidity', las=1)
partialPlot(dengue_forest_all, dengue_test, 'precipitation_amt', las=1)
partialPlot(dengue_forest_all, dengue_test, 'city_num', las=1)
