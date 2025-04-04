
library(tidyverse)
library(skimr)
library(rsample)
library(rpart)
library(ipred)
library(e1071)
library(caret)

credit <- read.csv("data/credit.csv",
                   stringsAsFactors = T)
credit

set.seed(42)

bag1 <- 
  bagging(default~.,
          data = credit,
          nbagg = 100)

pred1 <- 
  credit %>% 
  mutate(default_pred = predict(bag1,
                           type = "class"))

tab1 <- 
  pred1 %>%
  select(default_pred,
         default) %>% 
  table()

confusionMatrix(tab1)

bag2 <-
  bagging(default~.,
          data = credit,
          nbagg = 100,
          coob = TRUE)

pred2 <- 
  credit %>% 
  mutate(default_pred = predict(bag2,
                                type = "class"))

tab2 <- 
  pred2 %>% 
  select(default_pred,
         default) %>% 
  table()

confusionMatrix(tab2)

bag3 <-
  bagging(default~.,
          data = credit,
          nbagg = 500,
          coob = TRUE,
          control = rpart.control(maxdepth = 6,
                                  cp = 0.0001))

pred3 <- 
  credit %>% 
  mutate(default_pred = predict(bag3,
                                type = "class"))

tab3 <- 
  pred3 %>%
  select(default_pred, default) %>% 
  table()

confusionMatrix(tab3)

