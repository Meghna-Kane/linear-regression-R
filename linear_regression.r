# Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling funinstactions
library(broom)      # helps to tidy up model outputs


advertising <- read_csv("https://raw.githubusercontent.com/justmarkham/scikit-learn-videos/master/data/Advertising.csv") %>%
  select(-"X1")#this select fn takes the first column which just contains sr. no.s and names it X1

head(advertising)

set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(advertising), replace = T, prob = c(0.6,0.4))

train <- advertising[sample, ]
test <- advertising[!sample, ]
head(train)
head(test)


dim(train) #just checking the dimensions of the subset
dim(test)

model1 <- lm(Sales ~ TV, data = train)

ggplot(train, aes(TV, Sales)) +
  geom_point() +
  geom_smooth(method = "lm") #you can switch off the confidence interval by adding another argument here as: ,se(FALSE)

test <- test %>% 
  add_predictions(model1)

head(test)

rsquare(model1, data=test)

#trying linear programming with another independent variables- note: lm is only for a bi-variate regressional analysis
model2<-lm(Sales ~ Newspaper,data = train)
