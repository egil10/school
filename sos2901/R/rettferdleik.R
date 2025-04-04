
library(tidyverse)
library(randomForest)
library(caret)
library(fairness)

compas <- readRDS("data/compas.rds")
compas
compas %>% glimpse()

set.seed(4356)

rf1 <- 
  randomForest(Two_yr_Recidivism~.,
               data = compas)

pred1 <-
  compas %>% 
  mutate(pred_rf = predict(rf1))

confusionMatrix(
  pred1$pred_rf,
  pred1$Two_yr_Recidivism,
  positive = "1"
)

male <- pred1 %>% 
  filter(Sex == "Male")

confusionMatrix(
  male$pred_rf,
  male$Two_yr_Recidivism,
  positive = "1"
)

female <- pred1 %>% 
  filter(Sex == "Female")

confusionMatrix(
  female$pred_rf,
  female$Two_yr_Recidivism,
  positive = "1"
) 

acc1 <- acc_parity(data = pred1,
                   outcome = 'Two_yr_Recidivism',
                   group = 'Sex',
                   preds = 'pred_rf',
                   base = 'Female')

acc1

strat1 <- compas %>% 
  mutate(strat = paste0(Sex, Two_yr_Recidivism)) %>% 
  pull(strat)

table(strat1)

set.seed(45)

rf <- randomForest(Two_yr_Recidivism ~ .,
                   data = compas,
                   strata = strat1, 
                   sampsize = c(215, 290, 1500, 1500), 
                   ntree=800)

compas_p <- compas %>% 
  mutate(pred_rf = predict(rf))  

acc <- acc_parity(data = compas_p, 
                  outcome      = 'Two_yr_Recidivism', 
                  group        = 'Sex',
                  preds        = 'pred_rf', 
                  base         = 'Female')
acc[[2]]

acc <- acc_parity(data = compas_p, 
                  outcome      = 'Two_yr_Recidivism', 
                  group        = 'Ethnicity',
                  preds        = 'pred_rf', 
                  base         = 'Caucasian')
acc[[2]]



strat <- compas %>% 
  mutate(strat = paste0(Sex, Ethnicity, Two_yr_Recidivism)) %>% 
  pull(strat)

table(strat)
strat <- compas %>% 
  mutate(caucasian = ifelse(Ethnicity == "Caucasian", "Caucasian", "Other")) %>% 
  mutate(strat = paste0(Sex, caucasian, Two_yr_Recidivism)) %>% 
  pull(strat)

table(strat)



set.seed(45)
rf <- randomForest(Two_yr_Recidivism ~ .,
                   data = compas,
                   strata = strat, 
                   sampsize = c(120, 120, 
                                200, 200, 
                                400, 400, 
                                900, 900), 
                   ntree=800)

rf

compas_p <- compas %>% 
  mutate(pred_rf = predict(rf))  

acc <- acc_parity(data = compas_p, 
                  outcome      = 'Two_yr_Recidivism', 
                  group        = 'Sex',
                  preds        = 'pred_rf', 
                  base         = 'Female')
acc[[2]]


acc <- acc_parity(data = compas_p, 
                  outcome      = 'Two_yr_Recidivism', 
                  group        = 'Ethnicity',
                  preds        = 'pred_rf', 
                  base         = 'Caucasian')
acc[[2]]

