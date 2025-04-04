
library(tidyverse)
library(skimr)
library(rpart)
library(fairmodels)
library(randomForest)
library(caret)

# 7.1

compas <- readRDS("data/compas.rds") 

compas

set.seed(4356)

rf1 <- randomForest(Two_yr_Recidivism~.,
                    data = compas)  

rf1

plot(rf1)

pred4 <- compas %>% 
  mutate(predrf = predict(rf1))

tab4 <- table(
  pred4$predrf,
  pred4$Two_yr_Recidivism
)

tab4

confusionMatrix(tab4, positive = "1")

rf2 <- randomForest(Two_yr_Recidivism~.,
                    ntree = 1500,
                    data = compas) 

plot(rf2)
abline(v=500, col="grey")

rf3 <- randomForest(Two_yr_Recidivism~.,
                    mtry = 4,
                    data = compas) 

rf4 <- randomForest(Two_yr_Recidivism~.,
                    importance = TRUE,
                    data = compas) 

rf4
varImpPlot(rf4, type = 1)

partialPlot(rf1,
            pred.data = compas,
            x.var = Number_of_Priors,
            which.class = 1)








