
library(tidyverse)
library(skimr)
library(rsample)
library(rpart)
library(rpart.plot)
library(caret)

credit <- read_csv("data/credit.csv")
credit

set.seed(42)

ind <- initial_split(credit)
train <- training(ind)
test <- testing(ind)

tree1 <- rpart(default~.,
               data=train,
               method="class")

tree1 %>% rpart.plot()
tree1 %>% rpart.rules(extra=4)

pred1 <- predict(tree1, 
                 newdata = test,
                 type = "class")

confusionMatrix(data=factor(pred1),
                factor(test$default))


# 5.1

tree2 <- rpart(default ~ age + 
                 amount +
                 percent_of_income +
                 purpose +
                 employment_duration +
                 housing,
               data = train,
               method = "class",
               minsplit = 50)

tree2 %>% rpart.plot()


tree3 <- rpart(default ~ age + amount + percent_of_income + purpose + employment_duration + housing, 
                     data=train, method="class", cp = .00001)

tree3 %>% rpart.plot()



tree4 <- rpart(default ~ age + amount + percent_of_income + purpose + employment_duration + housing, 
                     data=train, method="class", 
                     cp = .005, minbucket = 5, minsplit = 10, maxdepth = 7)

tree4 %>% rpart.plot()
tree4 %>% 
  prune(cp = .015) %>% 
  rpart.plot()


# 5.2

lossm <- matrix(c(0, 4, 1, 0), ncol=2)
lossm


treel <- rpart(default~.,
               data = train,
               parms=list(loss=lossm),
               method = "class")

treel %>% rpart.plot()

# 5.3

# 5.4

testing_pred <- test %>% 
  mutate(default_pred = predict(treel, 
                                newdata=test, 
                                type="class"))


tab <- testing_pred %>% 
  select(default_pred, default) %>% 
  table()

confusionMatrix(tab)


























