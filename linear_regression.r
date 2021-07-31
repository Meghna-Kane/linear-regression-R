# Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling funinstactions
library(broom)      # helps to tidy up model outputs


advertising <- read_csv("https://raw.githubusercontent.com/justmarkham/scikit-learn-videos/master/data/Advertising.csv") %>%
  select(-"X1")

head(advertising)

set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(advertising), replace = T, prob = c(0.6,0.4))

train <- advertising[sample, ]
test <- advertising[!sample, ]


dim(train)
dim(test)

model1 <- lm(Sales ~ TV, data = train)

ggplot(train, aes(TV, Sales)) +
  geom_point() +
  geom_smooth(method = "lm")

test <- test %>% 
  add_predictions(model1)

head(test)

rsquare(model1, data=test)