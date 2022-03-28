# green certification

library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rsample)
library(modelr)
library(fastDummies)

onlygreens <- read.csv(file.path(path, 'data/greenbuildings.csv'))

view(greenbuildings)

sapply(greenbuildings, function(x) sum(is.na(x)))
greenbuildings = na.exclude(greenbuildings)