
library(tidyverse)
library(skimr)
library(rsample)
library(dendextend)
library(directlabels)

# 10.1

kommune <- readRDS("data/kommunedata.rds") %>% 
  filter(year == 2020) %>% 
  mutate(kommune = 
           ifelse(kommune == "Oslo municipality",
                  "Oslo", kommune)) %>% 
  select(kommune, menn_18_25:menn_18min,
         inntekt_totalt_median:andre_lovbrudd) %>% 
  select(-inntekt_eskatt_median) %>% 
  column_to_rownames(var = "kommune")

kommune

library(GGally)
ggpairs(kommune[, c(1,3,6,7,8,10,11)])

library(stats)
prk1 <- prcomp(kommune,
               scale. = TRUE,
               center = TRUE)
prk1 %>% summary()

par(mar=c(2,1,2,1))
biplot(prk1,
       cex = .4,
       cex.axis = .6,
       cex.lab = .6)

prk1$rotation[,1:2]

prk1$x %>% head()

pvar <- prk1$sdev^2
pve <- pvar/sum(pvar)

dt <- data.frame(
  components = 1:length(pve),
  prop_var_expl = pve
)

dt %>% 
  ggplot(aes(x = components,
             y = prop_var_expl)) +
  geom_line() +
  geom_point()
