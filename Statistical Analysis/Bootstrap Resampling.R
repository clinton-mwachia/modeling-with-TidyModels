# the packages
library(tidymodels)

source('F:/R/modeling-with-TidyModels/Statistical Analysis/utils.R')

# relationship between mpg and wt
ggplot(mtcars, aes(mpg, wt)) + 
  geom_point() +
  theme_bw()

# non linear least squares
nlsfit <- nls(mpg ~ k / wt + b, mtcars, start = list(k = 1, b = 0))
summary(nlsfit)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_line(aes(y = predict(nlsfit)))

# BOOTSTRAPPING MODELS
set.seed(100)
boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)
boots


boot_models <-
  boots %>% 
  mutate(model = map(splits, fit_nls_on_bootstrap),
         coef_info = map(model, tidy))

boot_coefs <- 
  boot_models %>% 
  unnest(coef_info)

boot_coefs

# confidence intervals
percentile_intervals <- int_pctl(boot_models, coef_info)
percentile_intervals

ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue") +
  theme_bw()

# model augment
boot_aug <- 
  boot_models %>% 
  sample_n(200) %>% 
  mutate(augmented = map(model, augment)) %>% 
  unnest(augmented)

boot_aug

ggplot(boot_aug, aes(wt, mpg)) +
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = "blue") +
  geom_point() +
  theme_bw()

boot_splines <- 
  boots %>% 
  sample_n(200) %>% 
  mutate(spline = map(splits, fit_spline_on_bootstrap),
         aug_train = map(spline, augment))

splines_aug <- 
  boot_splines %>% 
  unnest(aug_train)

ggplot(splines_aug, aes(x, y)) +
  geom_line(aes(y = .fitted, group = id), alpha = 0.2, col = "blue") +
  geom_point() +
  theme_bw()
