# we are going to predict stroke using logistic regression and random forest
# using tidy models
# hyper tune the models and compare results.

# loading the libraries
library(tidymodels)
library(tidyverse)
library(vip) # important variables in random forest

# loading the data
stroke = read_csv("introduction/data/healthcare-dataset-stroke-data.csv")

# most of the variables are wrongly code ( data types)
# e.g hypertension should be factor/categorical
# i will convert into character and the categorical
stroke = stroke %>%
  mutate(
    hypertension = as.character(hypertension),
    heart_disease = as.character(heart_disease),
    bmi = as.numeric(bmi),
    stroke = as.character(stroke)
  ) %>%
  mutate_if(
    is.character, as.factor
  )

# missing data
# for the sake of this analysis, we will drop the missing data
colSums(is.na(stroke))

stroke = stroke %>%
  na.omit()

# data splitting
# we will split the data into train (train, validation), test.
# train set will further divided for validation purposes.

# setting the seed for reproducibility
set.seed(1)

splits = initial_split(stroke, strata = stroke)
train = training(splits)
test = testing(splits)

# validation split
# used for fitting the model and tuning
# we use the train data and split ui further
set.seed(2)
val = validation_split(train, strata = stroke, prop = 0.8)

# logistic model
# we will define the model, recipe, tuning grid and fit
# we will tune the penalty
lr_model = logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine('glmnet')

# the recipe
# response variable: stroke, categorical
lr_recipe = 
  recipe(stroke~., data = train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% # dumify factors
  step_zv(all_predictors()) %>% # remove single observations
  step_normalize(all_predictors()) # normalize all numeric observations

# the workflow
lr_wf = 
  workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe)

# defining the grid for tuning penalty
lr_grid_penalty = tibble(penalty = 10^seq(-4,-1,length.out=30))

# model training
lr_training = 
  lr_wf %>%
  tune_grid(
    val,
    grid=lr_grid_penalty,
    control=control_grid(save_pred = TRUE),
    metrics=metric_set(roc_auc)
  )

# as the penalty increases, the performance reduces.
lr_training %>%
  collect_metrics() %>%
  ggplot(aes(x=penalty,y=mean))+
  geom_point()+
  geom_line()

# best model after tuning
lr_best = 
  lr_training %>%
  collect_metrics() %>%
  arrange(penalty) %>%
  slice(12)

# AUC 
lr_auc = 
  lr_training %>%
  collect_predictions(parameters = lr_best) %>%
  roc_curve(stroke, .pred_0) %>%
  mutate(model = 'lr')
autoplot(lr_auc)

# random forest
# we will tune mtry
# model
rf_model = 
  rand_forest(mtry = tune(), trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# recipe
rf_recipe = 
  recipe(stroke~., data = train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% # dumify factors
  step_zv(all_predictors()) %>% # remove single observations
  step_normalize(all_predictors()) # normalize all numeric observations
  
# workflow
rf_wf = 
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe)

# training the rf model
set.seed(2)
rf_training = 
  rf_wf %>%
  tune_grid(
    val,
    grid=25,
    control=control_grid(save_pred = TRUE),
    metrics=metric_set(roc_auc)
  )

# the best model
rf_best = 
  rf_training %>%
  select_best('roc_auc')

autoplot(rf_training)

# AUC
rf_auc = 
  rf_training %>%
  collect_predictions(parameters=rf_best) %>%
  roc_auc(stroke, .pred_0) %>%
  mutate(model = 'rf')

# comparing lr and rf
# the performance is identical
bind_rows(lr_auc, rf_auc) %>%
  ggplot(aes(1-specificity, y=sensitivity,col=model))+
  geom_path(lwd=1.5, alpha=.8)+
  geom_abline(lty=3)+
  coord_equal()

# final model
rf_final_model = 
  rand_forest(mtry = 7, trees = 1000) %>%
  set_engine('ranger', importance="impurity") %>%
  set_mode("classification")

# final workflow
# updating the previous wf
rf_final_wf = 
  rf_wf %>%
  update_model(rf_final_model)

# fit
set.seed(4)

rf_final_fit = 
  rf_final_wf %>%
  last_fit(splits)

rf_final_fit %>%
  collect_metrics()

rf_final_fit %>%
  pluck(".workflow",1) %>%
  extract_fit_parsnip() %>%
  vip(num_features=20)

rf_final_fit %>%
  collect_predictions() %>%
  roc_curve(stroke, .pred_0) %>%
  autoplot()
