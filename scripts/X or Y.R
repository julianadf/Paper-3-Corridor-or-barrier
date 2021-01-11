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
library(ggplot2)
library(sjPlot)
library(effects)
library(png)
library(car)

# Load and fix data
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

# Model with interaction ----
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
QAIC(direction.mod, chat=chat, k=2) # 1161.345
printvc(direction.mod)
drop1(direction.mod, test = "Chi")

direction.crossed <- cat_plot(direction.mod, pred =Direction, modx =Same.side,
                              interval = TRUE, plot.points = FALSE,
                              int.type = "confidence",geom ="line",vary.lty =TRUE)
direction.crossed

# Take away the interaction ----
direction.mod.2 <- glmer(formula = cbind(successes, failures) ~ Same.side + Direction + scale(distance) + scale(flower.abund) +
                           (1 | Site) + (1 | obs),
                         family = "binomial", data = db)
summary(direction.mod.2)
# be careful with this:
options(contrasts = c("contr.sum","contr.poly")) # need to return it to normal 
car::Anova(direction.mod.2, type="3")
options(contrasts = c("contr.treatment","contr.poly"))

tab_model(direction.mod.2)
vif(direction.mod.2)
test <- allEffects(direction.mod.2)
plot(allEffects(direction.mod.2))
anova(direction.mod.2, type=3, ddf="Kenward-Roger")
AIC(direction.mod.2) # no flower ab: 1158.23
mod_dharma <- direction.mod.2 %>% simulateResiduals(n=1000)
plot(mod_dharma)#, quantreg = F) #the quantile regression didn't work so I supressed it
plotResiduals(predict(direction.mod.2), mod_dharma$scaledResiduals)
unique(predict(direction.mod.2))
plotResiduals(mod_dharma, db$Environment1, quantreg = F)
testDispersion(mod_dharma)
hist(mod_dharma)
hist(resid(direction.mod.2))
mod_dharma %>% testDispersion()
mod_dharma %>% testZeroInflation()

QAIC(direction.mod.2, chat=chat, k=2) # 1160.227
drop1(direction.mod.2, test = "Chi")
write.csv2(test, "visreg_xorymod.csv")


# Reporting
# The glmer package predicts the log-odds. If type="response" the probability is calculated instead. To predict numbers, I need to multiply by N.

plot.mod2 <- emmeans(direction.mod.2, "Same.side", type="response")
pairs(plot.mod2)
plot(plot.mod2, comparisons=TRUE)


plot.data <- emmip(direction.mod.2, ~  "Same.side", type="response",  CIs = TRUE, plotit = FALSE) 

gg.model <- ggplot(plot.data, aes(x=Same.side, y=yvar), colour=Same.side) + 
  geom_pointrange(size = 1, aes(x= Same.side, ymin=LCL, ymax=UCL), colour="black") +
  labs(x="Movement on the same side", y = NULL) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text.x.bottom = element_text(size=14, family="serif"), axis.text.y.left = element_text(size=14, family="serif"),
        axis.title.y = element_text(size=18, family="serif"), axis.title.x = element_text(size=20, family="serif"))
gg.model + scale_x_discrete(labels=c("N" = "No", "Y" = "Yes"))
# add figure label
b <- readPNG("//storage.slu.se/Home$/jada0002/My Documents/My Pictures/Illustrations/b.png")
ggdraw(gg.model) + draw_image(b, x = 0.199, y = 1.02, hjust = 1, vjust = 1, width = 0.13, height = 0.13) 

plot.data.2 <- emmip(direction.mod.2, ~  "Direction", type="response",  CIs = TRUE, plotit = FALSE)

gg.model <- ggplot(plot.data.2, aes(x=Direction, y=yvar), colour=Direction) + 
  geom_pointrange(size = 1, aes(x= Direction, ymin=LCL, ymax=UCL), colour="black") +
  labs(x="Direction", y = "P(marking a flower)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text.x.bottom = element_text(size=14, family="serif"), axis.text.y.left = element_text(size=14, family="serif"),
        axis.title.y = element_text(size=20, family="serif"), axis.title.x = element_text(size=20, family="serif"))
gg.model

# add figure label
a <- readPNG("//storage.slu.se/Home$/jada0002/My Documents/My Pictures/Illustrations/a.png")
ggdraw(gg.model) + draw_image(a, x = 0.27, y = 1.02, hjust = 1, vjust = 1, width = 0.13, height = 0.13) 

plot.data.2 <- emmip(direction.mod.2, ~  "Direction", type="response",  CIs = TRUE, plotit = FALSE)
