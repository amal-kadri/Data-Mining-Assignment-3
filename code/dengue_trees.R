library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rsample)
library(modelr)
library(fastDummies)

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
dengue <- read_csv(file.path(path, "data/dengue.csv"))

#adding dummies for city and season, removing original columns
dengue = dummy_cols(dengue)
dengue = dengue %>% select(-c(city, season))
#dengue = na.exclude(dengue) ### suss NA solution from the internet

#Displays how many NAs are in a given column
sapply(dengue, function(x) sum(is.na(x)))
#train-test split
dengue_split = initial_split(dengue, prop = .8)
dengue_train = training(dengue_split)
dengue_test = testing(dengue_split)

#CART
dengue_tree = rpart(total_cases ~ season_winter + season_spring 
                    + season_summer + season_fall 
                    + city_iq + city_sj
                    + specific_humidity + tdtr_k + precipitation_amt, 
                    data = dengue_train, control = rpart.control(cp = 0.00001))

#Random Forest
# both models run pretty quick
# only 13 out of 1164 observations dropped by excluding NAs
dengue_forest = randomForest(total_cases ~ season_winter + season_spring 
                             + season_summer + season_fall 
                             + city_iq + city_sj
                             + specific_humidity + tdtr_k + precipitation_amt,
                              data = dengue_train, importance = TRUE,
                              na.action = na.exclude)
######################
# 194 observations dropped by removing NAs here, less great
dengue_forest_all = randomForest(total_cases ~ .,
                             data = dengue_train, importance = TRUE, 
                             na.action = na.exclude)
######################

