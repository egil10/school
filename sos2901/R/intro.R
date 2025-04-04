
invisible(Sys.setlocale(locale = 'no_NB.utf8'))

library(tidyverse)
library(skimr)
library(rsample)
library(readr)
library(caret)
library(gtsummary)

kommune <- read_rds("data/kommunedata.rds") %>% 
  as_tibble()

kommune
kommune %>% glimpse()
kommune %>% skim()

set.seed(42)

split <- initial_split(kommune,
                        prop = .7)

train <- training(split)
test <- testing(split)

train %>% 
  mutate(ratio = (menn_18_25 + menn_26_35) /
      bef_totalt*100) %>% 
  ggplot(aes(x=ratio, y=voldskriminalitet)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE)
