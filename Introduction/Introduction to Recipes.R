# loading the libraries
library(tidymodels)
library(tidyverse)

# the data
data("iris")
View(iris)

glimpse(iris)
summary(iris) # no NA

# percentage composition of Species
iris %>%
  count(Species) %>%
  mutate(prop = round((n/sum(n))*100,2))

# data splitting
data_split <- initial_split(iris,8/10) # 80 - 20 split

# train
train <- training(data_split)

train %>%
  count(Species) %>%
  mutate(prop = round((n/sum(n))*100,2))

# test
test <- testing(data_split)

test %>%
  count(Species) %>%
  mutate(prop = round((n/sum(n))*100,2))
# the data is balanced after splitting

########################################################################
# THE RECIPE
# THE RECIPE IS USED TO PREPROCESS DATA BEFORE MODELLING
########################################################################
iris_rec <- 
  recipe(Species~., data = train) %>%
  step_normalize(all_predictors())
summary(iris_rec)

#############
# THE MODEL
#############
lr_model <- 
  logistic_reg(mode="classification",engine = "glm", penalty = 0.01)

#############
# THE WORKFLOW
# WORKFLOW COMBINES THE RECIPE, MODEL AND DATA
############
iris_wf <- 
  workflow() %>%
  add_model(lr_model) %>%
  add_recipe(iris_rec)
iris_wf

###########
# FIT
##########
iris_fit <- 
  iris_wf %>%
  fit(train)

# model summary
iris_fit %>%
  tidy()


########################
# MAKING PREDICTIONS
#######################
iris_fit %>%
  predict(test)

# methods 2
iris_fit_pred <- 
  iris_fit %>%
  augment(test)

# confusion matrix
iris_fit_pred %>%
  conf_mat(truth=Species, .pred_class)

# accuracy
iris_fit_pred %>%
  accuracy(truth=Species, .pred_class)
