
library(tidyverse)
library(rsample)
library(gbm)
library(caret)
library(fairness)
library(gtsummary)

compas <- read_rds("data/compas.rds") %>% 
  mutate(Two_yr_Recidivism = 
           ifelse(Two_yr_Recidivism == "1",
                  1, 0)) %>% 
  na.omit()

compas %>% glimpse()

set.seed(42)
int <- initial_split(compas)
training <- training(int)
testing <- testing(int)

# 9.1

set.seed(542)

gbm1 <- gbm(formula = Two_yr_Recidivism~.,
            data = training,
            distribution = "bernoulli",
            n.trees = 4000,
            interaction.depth = 3,
            n.minobsinnode = 1,
            shrinkage = 0.001,
            bag.fraction = 0.5)

gbm1 %>% 
  gbm.perf(oobag.curve = TRUE,
           method = "OOB",
           plot.it = T,
           overlay = T)


# 9.2

sum1 <-
  gbm1 %>% summary(method = permutation.test.gbm,
                   normalize = TRUE,
                   plotit = FALSE)

sum1 %>% 
  ggplot(aes(x = reorder(var, rel.inf),
             y = rel.inf)) +
  geom_col() +
  coord_flip()

plot(gbm1, 'Number_of_Priors', type = "response")
plot(gbm1, 'Age_Below_TwentyFive', type = "response")
plot(gbm1, "Ethnicity", type = "response")


compas %>% colnames()
compas %>% colnames() 
list(compas %>% colnames())

prd <-
  setdiff(colnames(compas),
          "Two_yr_Recidivism")

for (i in prd) {
  plot(gbm1, 
       i, 
       type = "response",
       main = paste("Effect of", i))
}

# 9.3

pred1 <- training %>% 
  mutate(pred = 
           predict(gbm1, type = "response"),
         class =
           ifelse(pred > .5, 1, 0))

tab1 <- table(
  pred1$class,
  training$Two_yr_Recidivism
)

tab1
confusionMatrix(tab1,
                positive = "1")

# 9.4

wts <- training %>% 
  mutate(wts = 
           ifelse(Two_yr_Recidivism == 1, 1, 2)) %>% 
  pull(wts)

set.seed(542)

gbm2 <- gbm(Two_yr_Recidivism~.,
            data = training,
            weights = wts,
            distribution = "bernoulli",
            n.trees = 4000,
            interaction.depth = 3,
            n.minobsinnode = 1,
            shrinkage = 0.001,
            bag.fraction = 0.5)

pred2 <- training %>% 
  mutate(
    pred = 
      predict(gbm2, type = "response"),
    class = 
      ifelse(pred > 0.5, 1, 0)
  )

tab2 <- table(
  pred2$class,
  training$Two_yr_Recidivism
)

tab2

# 9.5

acc1 <- fairness::acc_parity(
  data = pred2,
  outcome = 'Two_yr_Recidivism',
  group = 'Sex',
  preds = 'class',
  base = 'Female'
)

acc1

# 9.6

training %>% 
  tbl_cross(
    row = Sex,
    col = Two_yr_Recidivism
  )

wts <- training %>%  
  mutate(wts1 = case_when(
    Sex == "Male" & Two_yr_Recidivism == 0 ~ 1,
    Sex == "Male" & Two_yr_Recidivism == 1 ~ 2, 
    Sex == "Female" & Two_yr_Recidivism == 0 ~ 2,
    Sex == "Female" & Two_yr_Recidivism == 1 ~ 4)) %>% 
  pull(wts1)

library(gtsummary)
df <- training %>% 
  mutate(wts = factor(wts))

df %>%  
  select(Sex, Two_yr_Recidivism, wts) %>% 
  tbl_strata(
    strata = Sex,
    ~.x %>%
      tbl_summary(
        by = Two_yr_Recidivism, 
        #type = everything()~"categorical", 
        statistic = all_categorical() ~ "{n}"
      )
  )








