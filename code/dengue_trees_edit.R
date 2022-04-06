dengue <- read_csv(file.path(path, "data", "dengue.csv"))

#Displays how many NAs are in a given column, then remove them
# 214 out of 1456 observations dropped by removing NAs here, not great, but best performance
sapply(dengue, function(x) sum(is.na(x)))
dengue = na.exclude(dengue)
#recode(season, spring=1, summer=2, fall=3, winter=4)
# recoding categorical variables to make PD plots, removing irrelevant columns
#season_num = factor(season, levels = c("spring", "summer", "fall", "winter"))
#recode(city, sj=0, iq=1)

dengue <- dengue %>% 
  mutate(season_num = factor(season, levels = c("spring", "summer", "fall", "winter")), 
         city_num = factor(city, levels = c("sj", "iq")), 
         specific_humidity = as.numeric(specific_humidity),
         precipitation_amt = as.numeric(precipitation_amt)) %>%
  dplyr::select(-c(city, season)) %>% 
  as.data.frame()


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

varImpPlot(dengue_forest_spec)
varImpPlot(dengue_forest_all)

######################

#boost
boost1 = gbm(total_cases ~ .,
                         data = dengue_train,
             interaction.depth=5, n.trees=500, shrinkage=.05, cv.folds = 10)

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

dengue_boost <- train(
  total_cases ~ .,
  data = dengue_train,
  method = 'gbm',
  trControl = ctrl,
  tuneGrid = tuneGrid,
  verbose = F
)

# Look at error curve -- stops decreasing much after ~300
gbm.perf(dengue)
yhat_test_gbm = predict(boost1, dengue_test, n.trees=50)
yhat_test_gbm = predict(dengue_boost, dengue_test, n.trees=50)

# RMSEs
tree_all = modelr::rmse(dengue_tree, dengue_test)
tree_engineer = modelr::rmse(dengue_tree_spec, dengue_test)
forest_all = modelr::rmse(dengue_forest_all, dengue_test) #Best so far
forest_engineer = modelr::rmse(dengue_forest_spec, dengue_test)
boost_all = modelr::rmse(boost1, dengue_test) # i think this works
boost_cv = modelr::rmse(dengue_boost, dengue_test)

model = c("tree_all", "tree_engineer", "forest_all", "forest_engineer", "boost_all")
rmse = c(tree_all, tree_engineer, forest_all, forest_engineer, boost_all)

rmse_table = data.frame(model,rmse)
# # boost predict rmse
# (yhat_test_gbm - dengue_test$total_cases)^2 %>% mean %>% sqrt
# 
# # tests rmse
# yhat_test_gbm = predict(boost1, dengue_test, n.trees=50)

# make var importance plot
# use this to decide the PD plots to make
forest_all_imp_plot <- varImpPlot(dengue_forest_all)

# PD plots
pd_spechum <- partialPlot(dengue_forest_all, 
                          dengue_test, 
                          'specific_humidity', 
                          las=1)
pd_city <- partialPlot(dengue_forest_all, 
                       dengue_test, 
                       'precipitation_amt', 
                       las=1)
pd_season <- partialPlot(dengue_forest_all, 
                         dengue_test, 
                         'season_num', 
                         las=1)
pd_min_temp <- partialPlot(dengue_forest_all, 
                       dengue_test, 
                       'min_air_temp_k', 
                       las=1)



