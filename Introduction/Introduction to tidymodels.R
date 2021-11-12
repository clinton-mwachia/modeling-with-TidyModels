# loading the libraries
library(tidyverse) # for data manipulations.
library(tidymodels) # tidy modelling
library(dotwhisker) # plotting predictions

# the data
#we will use the trees data for this video but feel free to use any data set.
data("trees")
View(trees)

str(trees)
summary(trees)

# no NA

# split the data into train and test the tidymodel way
data_split <- initial_split(trees,prop = 7/10) # data splits for 70/30.

# train data
train <- training(data_split)

# test data
test <- testing(data_split)

# lets define our model
lm_model <- 
  linear_reg() # for linear regression

# fit the model
lm_fit <- 
  lm_model %>%
  fit(Volume~., data=train)

# model output
lm_fit

# tidy modeloutput
tidy(lm_fit)

# plot the model output using dot and whisker
tidy(lm_fit) %>%
  dwplot(
    dot_args = list(size=2,color='black'),
    whisker_args = list(color='black'),
    vline = geom_vline(xintercept = 0,color='grey50', linetype=2)
  )+
  xlab('coefficient')

# predictions
pred <- predict(lm_fit, new_data = test)

# predictions with  confidenceinterval
pred_conf <- predict(lm_fit,new_data = test, type='conf_int')

# combing predictions with actual
comb <- test %>%
  bind_cols(pred) %>%
  bind_cols(pred_conf)

# plot predictions against actual
ggplot(comb, aes(x=Volume)) +
  geom_point(aes(y=.pred)) +
  geom_errorbar(aes(ymin=.pred_lower,ymax=.pred_upper))

# plot with confidence interval
# combining the metrics
comb %>%
  metrics(truth=Volume, estimate=.pred)

comb %>%
  ggplot(aes(x=.pred,y=Volume)) +
  geom_point(aes(y=.pred)) +
  geom_smooth(method = 'lm', se=TRUE)+
  theme_bw()
