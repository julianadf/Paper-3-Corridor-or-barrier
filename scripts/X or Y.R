# Data analysis paper 3 part 1
# By: Juliana Dániel Ferreira

rm(list=ls())
library(here)
library(tidyverse)
library(vegan)
library(lattice)
library(ggpubr)
library(lme4)
library(DHARMa)
library(interactions)
library(jtools)
library(emmeans)
library(visreg)
library(interactions)
library(jtools)
library(MuMIn)

# Load data and fix data
db.0 <- read.csv2(here("data", "database_models.csv" ))
str(db.0)

db <- db.0 %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Road.verge = as.factor(Road.verge)) %>% 
  rename(traffic = Traffic.int) %>% 
  rename(trials = Total.no..of.flowers) %>% 
  rename(successes = Number.marked) %>% 
  mutate(Same.side = as.factor(Same.side)) %>% 
  mutate(Direction = as.factor(Direction)) %>% 
  rename(distance = Distance.to.marked..m.) %>%
  rename(insects = Insect.abundance) %>% 
  rename(flower.abund = Number.of.flowers) %>% 
  mutate(failures = trials - successes) %>% 
  mutate(obs = c(1:240))
str(db)


direction.mod <- glmer(formula = cbind(successes, failures) ~ Same.side * Direction +
                         scale(distance) + scale(flower.abund) + (1 | Site) + (1 | obs),
                       family = "binomial", data = db)
summary(direction.mod)
mod_dharma <- direction.mod %>% simulateResiduals(n=1000)
plot(mod_dharma)#, quantreg = F) #the quantile regression didn't work so I supressed it
plotResiduals(predict(direction.mod), mod_dharma$scaledResiduals)
unique(predict(direction.mod))
plotResiduals(mod_dharma, direction$Environment1, quantreg = F)
testDispersion(mod_dharma)
hist(mod_dharma)
hist(resid(direction.mod))
mod_dharma %>% testDispersion()
mod_dharma %>% testZeroInflation()

chat <- deviance(direction.mod) / df.residual(direction.mod)
QAIC(direction.mod, chat=chat, k=2)

drop1(direction.mod, test = "Chi")

direction.crossed <- cat_plot(direction.mod, pred =Direction, modx =Same.side,
                              interval = TRUE, plot.points = FALSE,
                              int.type = "confidence",geom ="line",vary.lty =TRUE)
direction.crossed