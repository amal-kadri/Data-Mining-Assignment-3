dengue <- read.csv(file.path(path, "data", "dengue.csv"))

#Displays how many NAs are in a given column, then remove them
# 214 out of 1456 observations dropped by removing NAs here, not great, but best performance
sapply(dengue, function(x) sum(is.na(x)))
dengue = na.exclude(dengue)

# perhaps use pipe 
# recoding categorical variables to make PD plots, removing irrelevant columns

dengue <- dengue %>% 
  mutate(specific_humidity = as.numeric(dengue$specific_humidity),
         precipitation_amt = as.numeric(dengue$precipitation_amt), 
         city_num = recode(season, spring=1, summer=2, fall=3, winter=4), 
         season_num = recode(city, sj=0, iq=1)) %>% 
  dplyr::select(everything(), -c(city, season)) %>% 
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
dengue_forest_spec = randomForest(total_cases ~ . 
                                  - ndvi_ne - ndvi_nw - ndvi_se - ndvi_sw, 
                                  data = dengue_train, importance = TRUE, 
                                 na.action = na.exclude)
######################

#boost
boost1 = gbm(total_cases ~ .,
                         data = dengue_train,
             interaction.depth=5, n.trees=500, shrinkage=.05)

# Look at error curve -- stops decreasing much after ~300
gbm.perf(boost1)


# RMSEs
modelr::rmse(dengue_tree, dengue_test)
modelr::rmse(dengue_tree_spec, dengue_test)
modelr::rmse(dengue_forest_all, dengue_test) #Best so far
modelr::rmse(dengue_forest_spec, dengue_test)

# modelr::rmse(boost1, dengue_test)
# tests rmse
yhat_test_gbm = predict(boost1, dengue_test, n.trees=50)

# boost predict rmse
(yhat_test_gbm - dengue_test$total_cases)^2 %>% mean %>% sqrt

# make var importance plot
# use this to decide the PD plots to make
forest_all_imp <- varImpPlot(dengue_forest_all)


# PD plots
pd_spechum <- partialPlot(dengue_forest_all, 
                          dengue_test, 
                          'specific_humidity', 
                          las=1)

pd_precip <- partialPlot(dengue_forest_all, 
                        dengue_test, 
                        'precipitation_amt', 
                        las=1)

pd_city <- partialPlot(dengue_forest_all, 
                       dengue_test, 
                       'city_num', 
                       las=1)


