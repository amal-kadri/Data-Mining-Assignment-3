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
ch_forest <- modelr::rmse(calhousing_forest, calhousing_test)

# make predictions plot to show journey of thought
# seems like the model does worse at predicting the most expensive houses.
# and better at predicting lower price houses
yhat_test = predict(calhousing_forest, calhousing_test)
howwell_chforest <- plot(yhat_test, calhousing_test$medianHouseValue)


#var import
chf_imp <- varImpPlot(calhousing_forest)

#partial deps
pd_lng <- partialPlot(calhousing_forest, calhousing_test, 'longitude', las=1)
pd_lat <- partialPlot(calhousing_forest, calhousing_test, 'latitude', las=1)

# operation "4th Booster"
# dev a gradient boosted tree based model of med house prices
calhousing_boost = gbm(medianHouseValue ~ . - totalRooms - totalBedrooms, 
             data = calhousing_train,
             interaction.depth=5, n.trees=500, shrinkage=.05, cv.folds = 10)

booster_unboosted <- gbm.perf(calhousing_boost)


yhat_test_gbm = predict(calhousing_boost, calhousing_test, n.trees=200)
yhat_test_gbm = predict(calhousing_boost, calhousing, n.trees=200)


booster_unboosted_rmse <- rmse(calhousing_boost, calhousing_test)

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

calhousing_boostest <- train(
  medianHouseValue ~ .- totalRooms - totalBedrooms,
  data = calhousing_train,
  method = 'gbm',
  trControl = ctrl,
  tuneGrid = tuneGrid,
  verbose = F
)

boostest <- plot(calhousing_boostest)

test.features = subset(calhousing_test, select=-c(medianHouseValue))
test.target = subset(calhousing_test, select=medianHouseValue)[,1]
predictions = predict(calhousing_boostest, newdata = test.features)

#gradient boosted RMSE
ch_boostest <- modelr::rmse(calhousing_boostest, calhousing_test)

rmse <- c(round(ch_forest,3), round(ch_boostest, 3))
models <- c("Best Forest", "Best Boost")

perform <- cbind(Models = models, RMSE = rmse) %>% as.data.frame() %>%  kable(caption = "Comparing Model Out-of-Sample Performance")
## MAPS

calhousing = calhousing %>% 
  mutate(yhat_forest = predict(calhousing_forest, calhousing),
         abs_diff = abs(yhat_test-medianHouseValue))

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
medpirce_map <- cali + geom_point(data = calhousing, 
                  size = 1, 
                  aes(x = longitude, 
                           y = latitude, 
                           color = medianHouseValue)) +
  scale_color_gradientn(colors = c('#014468', '#00A6FF', '#FFFFFF'), label = comma) +
  labs(title = "California Houses: Median Prices", 
       x = "longitude",
       y = "latitude",
       color = "Median House Price")

# map for predicted house meds
# as noted above, compare color to real data map
mid = mean(calhousing$yhat_forest)
yhat_map <- cali + geom_point(data = calhousing, 
                  size = 1, 
                  aes(x = longitude, 
                      y = latitude, 
                      color = yhat_forest)) +
  scale_color_gradientn(colors = c('#014468', '#00A6FF', '#FFFFFF'), label = comma) +
  labs(title = "California Houses: Estimated Median Prices", 
       x = "longitude",
       y = "latitude",
       color  = "Estimated Med Price ($)")
# map of abs_diff
# color needs work , perhaps a interval shift, pin the low part of the gradient at a smaller number
# since the predications are all relatively the same in accuracy, there's not a whole lotta 
# color contrast here. 
mid = mean(calhousing$yhat)
absdiff_map <- cali + geom_point(data = calhousing, 
                  size = 1, 
                  aes(x = longitude, 
                      y = latitude, 
                      color = abs_diff)) +
  scale_color_gradientn(colors = c('#17B417', '#F2F52A', '#DF4E14'), label = comma) +
  labs(title = "California Houses: Best Median House Price Model Error", 
       x = "longitude",
       y = "latitude",
       color = "House Price Error")
