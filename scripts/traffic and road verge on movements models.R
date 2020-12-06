# Data analysis paper 3 part 2
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
  rename(flower.dens = Number.of.flowers) %>% 
  mutate(failures = trials - successes) %>% 
  mutate(obs = c(1:240))
str(db)

# Total across ----
# select only observations where the marked flower was on the opposite side of the road (Same side=N), exclude group C.

# Check if the flower density on the other side of the road is affecting the frequency that insects cross the road.
across.db <- db %>% 
  filter(., Same.side == "N") %>% 
  filter(., !Group %in% c("1C", "2C")) %>% 
  mutate(obs.across = c(1:80)) %>% 
  mutate(traffic.cat = ifelse(traffic >= 4000, 1,0))
str(across.db)

across.flower.mod <- glmer(formula = cbind(successes, failures) ~ scale(delta.flower) +
                             scale(distance) + scale(traffic) + 
                             log10(insects + 1) + (1 | Site) + (1 | obs.across), 
                         family = "binomial", data = across.db)
summary(across.flower.mod)
mod_dharma <- across.flower.mod %>% simulateResiduals(n=1000)
plot(mod_dharma)
mod_dharma %>% testDispersion()

drop1(across.flower.mod, test = "Chi")

# Now check whether traffic intensity and road verge quality affect the frequency of movements across

across.mod <- glmer(formula = cbind(successes, failures) ~ Road.verge * scale(traffic) +
                      scale(distance) + log10(insects+1) + (1 | Site) + (1 | obs), 
                    family = "binomial", data = across.db)

summary(across.mod)
mod_dharma <- across.mod %>% simulateResiduals(n=1000)
plot(mod_dharma)#, quantreg = F) #the quantile regression didn't work so I supressed it
plotResiduals(predict(across.mod), mod_dharma$scaledResiduals)
unique(predict(across.mod))
plotResiduals(mod_dharma, direction$Environment1, quantreg = F)
testDispersion(mod_dharma)
hist(mod_dharma)
hist(resid(direction.mod))
mod_dharma %>% testDispersion()
mod_dharma %>% testZeroInflation()

across.plot <- interact_plot(across.mod, pred = traffic, modx = Road.verge,
                              interval = TRUE, plot.points = FALSE,
                              int.type = "confidence",geom ="line",vary.lty =TRUE)

across.plot

# Total along ----
# select only observations where the marked flower was on the same side of the road (Same side=Y), exclude group C
along.db <- db %>% 
  filter(., Same.side == "Y") %>% 
  filter(., !Group %in% c("1C", "2C")) %>% 
  mutate(obs.along = c(1:80))
str(along.db)

along.mod <- glmer(formula = cbind(successes, failures) ~ Road.verge * scale(traffic) +
                     scale(distance) + log10(insects+1) + (1 | Site), 
                   family = "binomial", data = along.db)

summary(along.mod)
mod_dharma <- along.mod %>% simulateResiduals(n=1000)
plot(mod_dharma)#, quantreg = F) #the quantile regression didn't work so I supressed it
plotResiduals(predict(along.mod), mod_dharma$scaledResiduals)
unique(predict(along.mod))
plotResiduals(mod_dharma, direction$Environment1, quantreg = F)
testDispersion(mod_dharma)
hist(mod_dharma)
hist(resid(direction.mod))
mod_dharma %>% testDispersion()
mod_dharma %>% testZeroInflation()

drop1(along.mod, test = "Chi")
along.plot <- interact_plot(along.mod, pred = traffic, modx = Road.verge,
                             interval = TRUE, plot.points = FALSE,
                             int.type = "confidence",geom ="line",vary.lty =TRUE)

along.plot

# Into the neighbouring habitat ----
# Select only observations where the marked flower was on the other side of the road (Same side=N)
neighb.across <- db %>% 
  filter(., Same.side == "N") %>% 
  filter(., Group %in% c("1C", "2C")) 
str(neighb.across)

neighbacross.mod <- glm(formula = cbind(successes, failures) ~ Road.verge * scale(traffic)  +
                            scale(distance) + log10(insects+1) + (1 | Site), 
                          family = "binomial", data = neighb.across)
summary(neighbacross.mod)
mod_dharma <- neighbacross.mod %>% simulateResiduals(n=1000)
plot(mod_dharma)

neighacross.plot <- interact_plot(neighbacross.mod, pred = traffic, modx = Road.verge,
                                  interval = TRUE, plot.points = FALSE,
                                  int.type = "confidence",geom ="line",vary.lty =TRUE)

neighacross.plot

# Select only observations where the marked flower was on the same side of the road (Same side=Y)
neighb.along <- db %>% 
  filter(., Same.side == "Y") %>% 
  filter(., Group %in% c("1C", "2C")) 
str(neighb.along)

neighbalong.mod <- glmer(formula = cbind(successes, failures) ~ Road.verge * scale(traffic)  +
                           scale(distance) + log10(insects+1) + (1 | Site), 
                         family = "binomial", data = neighb.along)
summary(neighbalong.mod)
mod_dharma <- neighbalong.mod %>% simulateResiduals(n=1000)
plot(mod_dharma)

neighbalong.plot <- interact_plot(neighbalong.mod, pred = traffic, modx = Road.verge,
                            interval = TRUE, plot.points = FALSE,
                            int.type = "confidence",geom ="line",vary.lty =TRUE)

neighbalong.plot


