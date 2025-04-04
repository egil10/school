
library(tidyverse)
library(rsample)
library(caret)
library(fairness)

# 4.1

Attrition <- attrition
training  <- train
testing   <- test

est_multlogit <- glm(Attrition ~ ., data = training, family = "binomial")

summary(est_multlogit)

attrition_test <- testing %>% 
  mutate(prob = predict(est_multlogit, newdata = testing, type = "response")) %>% 
  mutate(attrition_class = as.factor(ifelse(prob < .5, "No", "Yes")))

cm <- confusionMatrix(attrition_test$Attrition, attrition_test$attrition_class, positive = "Yes")

cm

labTech <- attrition_test %>% 
  filter(JobRole %in% c("Laboratory Technician"))

others <- attrition_test %>% 
  filter( !(JobRole %in% c("Laboratory Technician") ))

cm1 <- confusionMatrix(labTech$Attrition, labTech$attrition_class, positive = "Yes")

cm2 <- confusionMatrix(others$Attrition, others$attrition_class, positive = "Yes")

cm1 

pred_rate_parity(data = attrition_test,
                 outcome = "Attrition", 
                 group = "JobRole", 
                 preds = "attrition_class", 
                 base = "Laboratory Technician"
)

equal_odds(data = attrition_test,
           outcome = "Attrition", 
           group = "JobRole", 
           preds = "attrition_class", 
           base = "Laboratory Technician"
)[[2]] 

fpr_parity(data = attrition_test,
           outcome = "Attrition", 
           group = "JobRole", 
           preds = "attrition_class", 
           base = "Laboratory Technician"
)[[2]] 


