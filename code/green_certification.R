# green certification

library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rsample)
library(modelr)
library(fastDummies)
library(here)
library(glmnet)
library(gbm)
path <-  here()

onlygreens <- read.csv(file.path(path, 'data/greenbuildings.csv'))
view(onlygreens)

# generate relevant variables
onlygreens = onlygreens %>% mutate(
  rent_psf = Rent * leasing_rate
)

#remove na
sapply(onlygreens, function(x) sum(is.na(x)))
onlygreens = na.exclude(onlygreens)

#test split
green_split = initial_split(onlygreens, prop = 0.8)
green_train = training(green_split)
green_test = testing(green_split)

#lasso model w/ comp
greenY <- green_train$rent_psf
greenX <- model.matrix(rent_psf ~ (.-CS_PropertyID - cluster - amenities- LEED - Energystar)^2 -1, data = green_train)

greenLasso_Wcomp <- cv.glmnet(x = greenX,y = greenY ,alpha = 1, nfold = 20, trace.it = 1, standardize = FALSE)

green_coef_Wcomp = coef(greenLasso_Wcomp) %>% 
  as.matrix() %>% 
  as.data.frame()
green_coef_Wcomp = green_coef_Wcomp %>% 
  mutate(mag = abs(s1)) %>% 
  filter(mag > 0)
green_coef_Wcomp <- tibble::rownames_to_column(green_coef_Wcomp, "VALUE")
green_coef_Wcomp = green_coef_Wcomp[2:nrow(green_coef_Wcomp),1]
green_coef_Wcomp

#lasso model w/o Rent and leasing_rate
greenY <- green_train$rent_psf
greenX <- model.matrix(rent_psf ~ (.-CS_PropertyID - cluster - amenities - Rent - leasing_rate - LEED - Energystar)^2 -1, data = green_train)

greenLasso <- cv.glmnet(x = greenX,y = greenY ,alpha = 1, nfold = 20, trace.it = 1, standardize = FALSE)

green_coef = coef(greenLasso) %>% 
  as.matrix() %>% 
  as.data.frame()
green_coef = green_coef %>% 
  mutate(mag = abs(s1)) %>% 
  filter(mag > 0)
green_coef <- tibble::rownames_to_column(green_coef, "VALUE")
green_coef = green_coef[2:nrow(green_coef),1]
green_coef

#random forest
green_forest = randomForest(rent_psf ~ (.- CS_PropertyID - LEED - Energystar - Rent - leasing_rate),
                                 data = green_train, importance = TRUE, 
                                 na.action = na.exclude)
modelr::rmse(green_forest, green_test)
sqrt(greenLasso$cvm[greenLasso$lambda == greenLasso$lambda.min])

yhat_test = predict(green_forest, green_test)
plot(yhat_test, green_test$rent_psf)

# boost
boost1 = gbm(rent_psf ~ (.- CS_PropertyID - LEED - Energystar - Rent - leasing_rate), 
             data = green_train,
             interaction.depth=5, n.trees=500, shrinkage=.05)

# Look at error curve -- stops decreasing much after ~300
gbm.perf(boost1)


yhat_test_gbm = predict(boost1, green_test, n.trees=200)

#rmse boost
rmse(boost1, green_test)

varImpPlot(green_forest)

partialPlot(green_forest, green_test, 'age', las=1)
partialPlot(green_forest, green_test, 'size', las=1)
partialPlot(green_forest, green_test, 'stories', las=1)
partialPlot(green_forest, green_test, 'City_Market_Rent', las=1)
partialPlot(green_forest, green_test, 'green_rating', las=1)
