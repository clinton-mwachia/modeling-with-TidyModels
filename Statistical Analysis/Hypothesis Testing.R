# the libraries
library(tidymodels)

# the data
data(gss)
View(gss)

summary(gss)

# get required variables
gss %>%
  specify(age~ partyid)

gss %>%
  specify(response = age, explanatory = partyid)

gss %>%
  specify(response = college, success = 'degree')

# declaring hypothesis
gss %>%
  specify(college ~ partyid, success = "degree") %>%
  hypothesize(null = "independence")
