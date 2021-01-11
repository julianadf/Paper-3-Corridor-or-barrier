# Data analysis paper 3 part 3
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
library(sjPlot)
library(MuMIn)
library(multcomp)
library(multcompView)

# Load data and fix data
square.0 <- read.csv2(here("data", "release square.csv" ))
str(square.0)
square.1 <- square.0[,c(4,5,7:14)]


release.square <- square.1 %>% 
  drop_na() %>% 
  mutate(Sex = as.factor(Sex)) %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Traffic = as.factor(Traffic)) %>% 
  mutate(Road.verge = as.factor(Road.verge)) %>% 
  mutate(Species = as.factor(Species)) %>% 
  mutate(Crossed.road = as.factor(Crossed.road)) %>% #for the barrier effect
  mutate(Behaviour = as.factor(Behaviour)) %>% 
  mutate(Stayed.roadverge = ifelse(Behaviour == 2, 1, 0)) %>% # for the corridor effect
  mutate(Stayed.roadverge = as.factor(Stayed.roadverge)) %>% 
  mutate(prob.crossing = ifelse(Behaviour == 1, 0.25, 0.75)) %>% 
  mutate(offset = qlogis(0.25))
str(release.square)

both.sp <-table(release.square$Behaviour, release.square$Site)
barplot(both.sp, beside=T, col=c("#ef8a62", "#f7f7f7", "#67a9cf"), 
        names.arg=c("High traff, High RVQ", "High traff, Low RvQ", "Low traff, High RVQ", "Low traff, Low RVQ"), main= "Both species")

legend("topright", legend=c("crossed", "stayed", "habitat"),fill=c("#ef8a62", "#f7f7f7", "#67a9cf"))

# Pärlgräsfjäril - Coenonympha arcania ----
pärlis <- release.square %>% 
  filter(Species %in% "C. arcania")
str(pärlis)

table(pärlis$Behaviour, pärlis$Site, pärlis$Sex)
behaviour.site.pärlis <-table(pärlis$Behaviour, pärlis$Site)

barplot(behaviour.site.pärlis, beside=T, col=c("#ef8a62", "#f7f7f7", "#67a9cf"), 
        names.arg=c("High traff, High RVQ", "High traff, Low RvQ", "Low traff, High RVQ", "Low traff, Low RVQ"), 
        main= "Coenonympha arcania")

legend("topright", legend=c("crossed", "stayed", "habitat"),fill=c("#ef8a62", "#f7f7f7", "#67a9cf"))

chisq.test(pärlis$Traffic, pärlis$Crossed.road) # tests whether the traffic treatment had any effect on the bfs crossing the road
chisq.test(pärlis$Road.verge, pärlis$Crossed.road) # tests whether the road verge treatment had any effect on the bfs crossing the road
chisq.test(pärlis$Crossed.road, pärlis$prob.crossing) # tests if the obs prob of crossing the road is significantly different from the expected (0.25)

# Is the road acting as a behavioural barrier to movement?
pärlis.mod1 <- glm(Crossed.road ~ Traffic + Road.verge + Sex , family = "binomial", data =pärlis)
summary(pärlis.mod1) #Estimates are odds ratios
mod_dharma1 <- pärlis.mod1 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
emmeans(pärlis.mod1, ~ Traffic + Road.verge , type="response")
plot(emmeans(pärlis.mod1, ~ Traffic * Road.verge + Sex, type="response"))
test(emmeans(pärlis.mod1, ~ Traffic + Road.verge, type="response"), null=qlogis(0.25))
AICc(pärlis.mod1)

test(emmeans(pärlis.mod1, pairwise ~ Traffic + Road.verge, type = "response"), null=qlogis(0.25))

plot.mod2 <- emmeans(direction.mod.2, "Same.side", type="response")
pairs(plot.mod2)
plot(plot.mod2, comparisons=TRUE)

# Is the road verge acting as a corridor?
pärlis.mod2 <- glm(Stayed.roadverge ~ Traffic + Road.verge + Sex, family = "binomial", data =pärlis)
summary(pärlis.mod2)
mod_dharma2 <- pärlis.mod2 %>% simulateResiduals(n=1000)
plot(mod_dharma2)
AICc(pärlis.mod2)
emmeans(pärlis.mod2, ~ Traffic + Road.verge + Sex, type="response")
plot(emmeans(pärlis.mod2, ~ Traffic * Road.verge, type="response"))


plot.mod2 <- emmeans(pärlis.mod2 , ~ Traffic * Road.verge, type="response")
pairs(plot.mod2)
plot(plot.mod2, comparisons=TRUE)


pärlis.corridor <- cat_plot(pärlis.mod2, pred = Traffic, modx=Road.verge, interval = TRUE, plot.points = FALSE,
                             int.type = "confidence",geom ="line",vary.lty =TRUE)
pärlis.corridor

pärlis.corr.sex <- effect_plot(pärlis.mod2, pred = Sex, interval = TRUE, plot.points = FALSE,
                               int.type = "confidence",geom ="line",vary.lty =TRUE)
pärlis.corr.sex

# Luktgräsfjäril - Aphantopus hyperantus ----
luktis <- release.square %>% 
  filter(Species %in% "A. hyperantus")
luktis

table(luktis$Behaviour, luktis$Site, luktis$Sex)
behaviour.site.luktis<-table(luktis$Behaviour, luktis$Site)

barplot(behaviour.site.luktis, beside=T, col=c("#ef8a62", "#f7f7f7", "#67a9cf"), 
        names.arg=c("High traff, High RVQ", "High traff, Low RvQ", "Low traff, High RVQ", "Low traff, Low RVQ"), 
        main= "Aphantopus hyperantus")

legend("bottomright", legend=c("crossed", "stayed", "habitat"),fill=c("#ef8a62", "#f7f7f7", "#67a9cf"))

chisq.test(luktis$Traffic, luktis$Crossed.road)
chisq.test(luktis$Road.verge, luktis$Crossed.road)

# Is the road acting as a behavioural barrier to movement?
luktis.mod1 <- glm(Crossed.road ~ Traffic + Road.verge + Sex, family = "binomial", data =luktis)
summary(luktis.mod1) #Estimates are odds ratios
mod_dharma1 <- luktis.mod1 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
AICc(luktis.mod1)
tab_model(luktis.mod1)
em <- emmeans(luktis.mod1, ~ Traffic + Road.verge, type="response")
plot(emmeans(luktis.mod1, ~ Traffic + Road.verge, type="response"))
plot(test(emmeans(luktis.mod1, ~ Traffic + Road.verge + Sex, type="response"), null=qlogis(0.25)))
plot(test(emmeans(luktis.mod1, ~ Traffic + Road.verge, type="response"), null=qlogis(0.25)))

em <- emmeans(luktis.mod1, pairwise ~ Traffic + Road.verge, type = "response")
em <- multcomp::cld(em, Letters = letters)
em <- as.data.frame(em)
ggplot(em, aes(prob, Traffic:Road.verge)) +
  geom_point() +
  geom_errorbar(aes(xmin = asymp.LCL, xmax = asymp.UCL), width = 0.1) +
  geom_text(aes(x = 1.1, label = .group)) +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  xlab("Estimated probability of crossing road") +
  coord_cartesian(xlim = c(0,1.2)) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())




# Is the road verge acting as a corridor?
luktis.mod2 <- glm(Stayed.roadverge ~ Traffic + Road.verge + Sex, family = "binomial", data =luktis)
summary(luktis.mod2)
mod_dharma2 <- luktis.mod2 %>% simulateResiduals(n=1000)
plot(mod_dharma2)
emmeans(luktis.mod2, ~ Traffic * Road.verge , type="response")
plot(emmeans(luktis.mod2, ~ Traffic * Road.verge, type="response"))

plot.mod2 <- emmeans(luktis.mod2, "Traffic","Road.verge", type="response")
pairs(plot.mod2)
plot(plot.mod2, comparisons=TRUE)


pärlis.corridor <- cat_plot(pärlis.mod2, pred = Traffic, modx=Road.verge, interval = TRUE, plot.points = FALSE,
                            int.type = "confidence",geom ="line",vary.lty =TRUE)
pärlis.corridor

pärlis.corr.sex <- effect_plot(pärlis.mod2, pred = Sex, interval = TRUE, plot.points = FALSE,
                               int.type = "confidence",geom ="line",vary.lty =TRUE)
pärlis.corr.sex
