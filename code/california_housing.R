# california housing
calhousing <- read.csv(file.path(path, 'data/CAhousing.csv'))

# mutate given housing features into household and population averages
calhousing = calhousing %>% 
  mutate(avg_rooms = totalRooms/households,
         avg_bedrooms = totalBedrooms/households,
         rooms_per_person = totalRooms/population,
         bedrooms_per_person = totalRooms/population, 
         household_density = households/population)

# initial train test split
calhousing_split = initial_split(calhousing, prop = 0.8)
calhousing_train = training(calhousing_split)
calhousing_test = testing(calhousing_split)

# operation "Sequoia"
# develop the best random forest model
calhousing_forest = randomForest(medianHouseValue ~ . - totalRooms - totalBedrooms,
                            data = calhousing_train, importance = TRUE, 
                            na.action = na.exclude)

# out of sample performance measure: RMSE
modelr::rmse(calhousing_forest, calhousing_test)

# make predictions plot to show journey of thought
# seems like the model does worse at predicting the most expensive houses.
# and better at predicting lower price houses
yhat_test = predict(calhousing_forest, calhousing_test)
plot(yhat_test, calhousing_test$medianHouseValue)

calhousing = calhousing %>% 
  mutate(yhat_forest = predict(calhousing_forest, calhousing),
         abs_diff = abs(yhat_test-medianHouseValue))


#var import
varImpPlot(calhousing_forest)

#partial deps
partialPlot(calhousing_forest, calhousing_test, 'longitude', las=1)
partialPlot(calhousing_forest, calhousing_test, 'latitude', las=1)

# operation "4th Booster"
# dev a gradient boosted tree based model of med house prices
calhousing_boost = gbm(medianHouseValue ~ . - totalRooms - totalBedrooms, 
             data = calhousing_train,
             interaction.depth=5, n.trees=500, shrinkage=.05)

gbm.perf(calhousing_boost)


yhat_test_gbm = predict(cal_boost, calhousing_test, n.trees=200)
yhat_test_gbm = predict(cal_boost, calhousing, n.trees=200)


rmse(calhousing_boost, calhousing_test)

# trying to boost the boosted 
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

# I propose we axe the utopia model as lat long are important, 
# vis a vie the varimpmat(cal_forest)
# utpoia model
utopia_forest = randomForest(medianHouseValue ~ . - totalRooms - totalBedrooms - longitude - latitude,
                                 data = calhousing_train, importance = TRUE, 
                                 na.action = na.exclude)
modelr::rmse(utopia_forest, calhousing_test)
yhat_test = predict(utopia_forest, calhousing_test)
plot(yhat_test, calhousing_test$medianHouseValue)

calhousing_test = calhousing_test %>% 
  mutate(yhat_test = predict(utopia_forest, calhousing_test),
         abs_diff = abs(yhat_test-medianHouseValue))


calhousing = calhousing %>% 
  mutate(yhat = predict(utopia_forest, calhousing),
         abs_diff = abs(yhat_test-medianHouseValue))


varImpPlot(utopia_forest)

# sqrt(calhousingLasso$cvm[calhousingLasso$lambda == calhousingLasso$lambda.min])

# a plot of the original data, using a color scale to show medianHouseValue (or log medianHouseValue) versus longitude (x) and latitude (y)
# color hex codes: https://htmlcolorcodes.com/
cali <- ggmap(get_googlemap(center = c(lon = -119.449444, lat = 37.166111), 
                    zoom = 6, 
                    maptype = 'satellite'))

# map for real median house prices
# compare this color scheme to the predictions map in the next code chunk
# same colors but different order
# I (DS) am partial to this one where white is low and blue is high 
mid = mean(calhousing$medianHouseValue)
cali + geom_point(data = calhousing, 
                  size = 1, 
                  aes(x = longitude, 
                           y = latitude, 
                           color = medianHouseValue)) +
  scale_color_gradientn(colors = c('#FFFFFF', '#00A6FF', '#014468'))

# map for predicted house meds
# as noted above, compare color to real data map
mid = mean(calhousing$yhat_forest)
cali + geom_point(data = calhousing, 
                  size = 1, 
                  aes(x = longitude, 
                      y = latitude, 
                      color = yhat_forest)) +
  scale_color_gradientn(colors = c('#014468', '#00A6FF', '#FFFFFF'))
# map of abs_diff
# color needs work , perhaps a interval shift, pin the low part of the gradient at a smaller number
# since the predications are all relatively the same in accuracy, there's not a whole lotta 
# color contrast here. 
mid = mean(calhousing$yhat)
cali + geom_point(data = calhousing, 
                  size = 1, 
                  aes(x = longitude, 
                      y = latitude, 
                      color = abs_diff)) +
  scale_color_gradientn(colors = c('#17B417', '#F2F52A', '#DF4E14'))
