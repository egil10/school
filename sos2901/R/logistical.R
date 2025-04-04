
library(tidyverse)   # datah??ndtering, grafikk og glimpse()
library(skimr)       # funksjonen skim() for ?? se p?? data
library(rsample)     # for ?? dele data i training og testing
library(pROC)        # Beregne ROC-curve
library(gtsummary)   # Pent formatert regresjonstabell
library(caret)       # Funksjonen confusionMatrix()

attrition <- read_rds("data/Attrition.rds") %>% 
  as_tibble()
attrition %>% skim()

set.seed(426)
attrition_split <- initial_split(attrition)
train <- training(attrition_split)
test <- testing(attrition_split)
train
test
train %>% nrow()
test %>% nrow()

train$Attrition <-
  as.numeric(train$Attrition == "Yes")

mean(train$Attrition)
