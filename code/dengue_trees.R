library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rsample) 

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
