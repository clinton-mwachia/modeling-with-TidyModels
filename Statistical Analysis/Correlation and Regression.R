# libraries
library(tidymodels)
library(tidyverse)

# the data
data(Orange,mtcars)

# visualization
Orange %>%
  ggplot(aes(age, circumference, color=Tree))+
  geom_line() +
  theme_bw()

# correlation analysis
cor(Orange$age, Orange$circumference)

# correlation by tree
Orange %>%
  group_by(Tree) %>%
  summarise(corr = cor(age, circumference))

# correlation test
cor.test(Orange$age, Orange$circumference) %>%
  tidy()

# correlation across tress
nested <- 
  Orange %>% 
  nest(data = c(age, circumference))

nested %>% 
  mutate(test = map(data, ~ cor.test(.x$age, .x$circumference)))

nested %>% 
  mutate(
    test = map(data, ~ cor.test(.x$age, .x$circumference)),
    tidied = map(test, tidy)
  )

Orange %>% 
  nest(data = c(age, circumference)) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$age, .x$circumference)),
    tidied = map(test, tidy)
  ) %>% 
  unnest(cols = tidied) %>% 
  select(-data, -test)

# REGRESSION MODELS
lm_fit <- lm(age ~ circumference, data = Orange)
summary(lm_fit)

# tidy results
lm_fit %>%
  tidy()

# fiting multiple regressions
Orange %>%
  nest(data = c(-Tree)) %>% 
  mutate(
    fit = map(data, ~ lm(age ~ circumference, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  select(-data, -fit)

# mtcars
mtcars

# regression
mtcars %>%
  nest(data = c(-am)) %>% 
  mutate(
    fit = map(data, ~ lm(wt ~ mpg + qsec + gear, data = .x)), 
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  select(-data, -fit)

# augment model output
regressions <- 
  mtcars %>%
  nest(data = c(-am)) %>% 
  mutate(
    fit = map(data, ~ lm(wt ~ mpg + qsec + gear, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

regressions %>% 
  select(tidied) %>% 
  unnest(tidied)

regressions %>% 
  select(glanced) %>% 
  unnest(glanced)

regressions %>% 
  select(augmented) %>% 
  unnest(augmented)
