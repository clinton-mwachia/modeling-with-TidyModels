# libraries
library(tidymodels)
library(tidyverse)

# the data
data("iris")

# kmeans clustering
kclust <- kmeans(iris[,-5], centers = 3)
tidy(kclust)

# augementation
augment(kclust, iris[,-5])


# eda
kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(iris[,-5], .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, iris[,-5])
  )

kclusts

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

p1 <- 
  ggplot(assignments, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()
