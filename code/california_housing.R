# california housing

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
library(ggmap)
library(caret)
library(MASS)
path <-  here()
calhousing <- read.csv(file.path(path, 'data/CAhousing.csv'))

### stuff

view(calhousing)

#avg rooms

calhousing = calhousing %>% 
  mutate(avg_rooms = totalRooms/households,
         avg_bedrooms = totalBedrooms/households)


#test split
calhousing_split = initial_split(calhousing, prop = 0.8)
calhousing_train = training(calhousing_split)
calhousing_test = testing(calhousing_split)

#sequoia
calhousing_forest = randomForest(medianHouseValue ~ . - totalRooms - totalBedrooms,
                            data = calhousing_train, importance = TRUE, 
                            na.action = na.exclude)
#RMSE
modelr::rmse(calhousing_forest, calhousing_test)

#predict
yhat_test = predict(calhousing_forest, calhousing_test)
plot(yhat_test, calhousing_test$medianHouseValue)

calhousing_test = calhousing_test %>% 
  mutate(yhat_test = predict(calhousing_forest, calhousing_test),
         abs_diff = abs(yhat_test-medianHouseValue))

view(calhousing_test)

#var import
varImpPlot(calhousing_forest)

#partial deps
partialPlot(calhousing_forest, calhousing_test, 'longitude', las=1)
partialPlot(calhousing_forest, calhousing_test, 'latitude', las=1)

#cal boost
cal_boost = gbm(medianHouseValue ~ .- totalRooms - totalBedrooms, 
             data = calhousing_train,
             interaction.depth=5, n.trees=500, shrinkage=.05)

gbm.perf(cal_boost)


yhat_test_gbm = predict(cal_boost, calhousing_test, n.trees=200)

rmse(cal_boost, calhousing_test)

#gradient boosted
ctrl <- trainControl(
  method = "cv",
  number = 10
)

tuneGrid <- expand.grid(
  n.trees = c(50, 60, 70, 80, 90, 100),
  interaction.depth = c(1, 2, 3, 4, 5),
  shrinkage = 0.1,
  n.minobsinnode = 10
)

model5 <- train(
  medianHouseValue ~ .- totalRooms - totalBedrooms,
  data = calhousing_train,
  method = 'gbm',
  trControl = ctrl,
  tuneGrid = tuneGrid,
  verbose = F
)

plot(model5)

test.features = subset(calhousing_test, select=-c(medianHouseValue))
test.target = subset(calhousing_test, select=medianHouseValue)[,1]

predictions = predict(model5, newdata = test.features)

#gradient boosted RMSE
sqrt(mean((test.target - predictions)^2))

#utpoia model
utopia_forest = randomForest(medianHouseValue ~ . - totalRooms - totalBedrooms - longitude - latitude,
                                 data = calhousing_train, importance = TRUE, 
                                 na.action = na.exclude)

modelr::rmse(utopia_forest, calhousing_test)

yhat_test = predict(utopia_forest, calhousing_test)
plot(yhat_test, calhousing_test$medianHouseValue)

calhousing_test = calhousing_test %>% 
  mutate(yhat_test = predict(utopia_forest, calhousing_test),
         abs_diff = abs(yhat_test-medianHouseValue))

varImpPlot(utopia_forest)

#sqrt(calhousingLasso$cvm[calhousingLasso$lambda == calhousingLasso$lambda.min])

# a plot of the original data, using a color scale to show medianHouseValue (or log medianHouseValue) versus longitude (x) and latitude (y)

cali <- get_map("california")

ggmap(cali)

calhousing %>% 
  ggmap() +
  geom_point(aes(x = latitude, y = longitude, color = medianHouseValue))
