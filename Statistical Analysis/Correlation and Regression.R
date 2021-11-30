# libraries
library(tidymodels)
library(tidyverse)

# the data
data("Orange")

# correlation analysis
cor(Orange$age, Orange$circumference)

# correlation by tree
Orange %>%
  group_by(Tree) %>%
  summarise(corr = cor(age, circumference))

# visualization
Orange %>%
  ggplot(aes(age, circumference, color=Tree))+
  geom_line() +
  theme_bw()
