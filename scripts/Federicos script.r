##### Riva, Acorn and Nielsen 2018 - Narrow anthropogenic corridors direct the movement of a generalist boreal butterfly
##### coded with R version 3.3.3, using Rstudio 1.0.136

##### 0) Install required packages 
if(!require(readr)){install.packages("readr")}
if(!require(lme4)){install.packages("lme4")}
if(!require(sjPlot)){install.packages("sjPlot")}
if(!require(gplots)){install.packages("gplots")}
if(!require(glmmTMB)){install.packages("glmmTMB")}

##### 1) Import the data
experimental_releases <- read_csv("C:/Riva_et_al_Narrow_anthropogenic_corridors.csv")

##### 2) Prepare the data
# convert data to dataframe
data.frame_release <- data.frame(experimental_releases)

# encode as factors some of the columns in the dataset
experimental_releases$atreat <- as.factor(experimental_releases$atreat)
experimental_releases$persistent_movement <- as.numeric(experimental_releases$persistent_movement)
experimental_releases$immediate_movement_ew <- as.numeric(experimental_releases$immediate_movement_ew)
experimental_releases$immediate_movement_ns <- as.numeric(experimental_releases$immediate_movement_ns)
experimental_releases$immediate_movement_corridor <- as.numeric(experimental_releases$immediate_movement_corridor)

# subset data: corridors, wellpads, controls
subset_corridor <- subset.data.frame(data.frame_release, type == 'corridor')
subset_wellpad <- subset.data.frame(data.frame_release, type == 'wellpad')
subset_control <- subset.data.frame(data.frame_release, type == 'control')

subset_corridor8 <- subset(subset_corridor, treat == 'corridor_8')
subset_corridor4 <- subset(subset_corridor, treat == 'corridor_4')
subset_control4 <- subset(subset_control, treat =='control_4')
subset_control8 <- subset(subset_control, treat =='control_8')
subset_wellpad4 <- subset(subset_wellpad, treat =='wellpad_4')
subset_wellpad8 <- subset(subset_wellpad, treat =='wellpad_8')

# subset 4-m and 8-m wide arenas
subset_4 <- rbind(subset_corridor4, subset_control4, subset_wellpad4)
subset_8 <- rbind(subset_corridor8, subset_control8, subset_wellpad8)

##### 3) Modeling; see supplementary materials for information on analyses
###### Legend: atreat= arenas categories (controls: forest and clearing vs. corridors, at 4-m and 8-m arena size)
##### before each GLMM model (random effect on release arena), a simple GLM withouth the random effect is provided for comparison. The estimates of the two models are always very similar (variance and st.dev of random effect = 0; run for comparison)

##### 4-m scale
# a) probability of immediate movement in east-west direction
# modelEW4 <- glm(immediate_movement_ew ~ atreat + corridor_direction , family = binomial("logit"), subset_4)
modelEW4 <- glmer(immediate_movement_ew ~ atreat + corridor_direction + (1|arena_id) , family = binomial("logit"), subset_4)
summary(modelEW4)
sjp.glmer(modelEW4, type = "fe")
sjp.glmer(modelEW4, y.offset = .4)


# number of parameter
attributes(logLik(modelEW4))
# confidence interval (95%)
confint(modelEW4)
# predict model, transform back to real values (e.g. log link-> exp()) and return the standard error
predict(modelEW4, type="response", se.fit = TRUE) 

# b) probability of immediate movement in north-south direction
# modelNS4 <- glm(immediate_movement_ns ~ atreat + corridor_direction , family = binomial("logit"), subset_4)
modelNS4 <- glmer(immediate_movement_ns ~ atreat + corridor_direction + (1|arena_id) , family = binomial("logit"), subset_4)
summary(modelNS4)
sjp.glmer(modelNS4, type = "fe")
attributes(logLik(modelNS4))
confint(modelNS4)
predict(modelNS4, type="response", se.fit = TRUE) 

# c) probability of persistence in directional movement after 12 m
# model3F4 <- glm(persistent_movement ~ atreat, family = binomial, subset_4)
model3F4 <- glmer(persistent_movement ~ atreat + (1|arena_id), family = binomial, subset_4)
summary(model3F4)
sjp.glmer(model3F4, type = "fe")
attributes(logLik(model3F4))
confint(model3F4)
predict(model3F4, type="response", se.fit = TRUE) 

##### 8-m scale
# a) probability of immediate movement in east-west direction
# modelEW8 <- glm(immediate_movement_ew ~ atreat + corridor_direction , family = binomial, subset_8)
modelEW8 <- glmer(immediate_movement_ew ~ atreat + corridor_direction + (1|arena_id), family = binomial, subset_8)
summary(modelEW8)
sjp.glmer(modelEW8, type = "fe")
attributes(logLik(modelEW8))
confint(modelEW8)
predict(modelEW8, type="response", se.fit = TRUE) 

# b) probability of immediate movement in north-south direction
# modelNS8 <- glm(immediate_movement_ns ~ atreat + corridor_direction, family = binomial, subset_8)
modelNS8 <- glmer(immediate_movement_ns ~ atreat + corridor_direction + (1|arena_id), family = binomial, subset_8)
summary(modelNS8)
sjp.glmer(modelNS8, type = "fe")
attributes(logLik(modelNS8))
confint(modelNS8)
predict(modelNS8, type="response", se.fit = TRUE) 

# c) probability of persistence in directional movement after 12 m
# model3F8 <- glm(persistent_movement ~ atreat, family = binomial, subset_8)
model3F8 <- glmer(persistent_movement ~ atreat + (1|arena_id), family = binomial, subset_8)
summary(model3F8)
sjp.glmer(model3F8, type = "fe")
attributes(logLik(model3F8))
confint(model3F8)
predict(model3F8, type="response", se.fit = TRUE)

### plotting expected vs observed probability of (1) immediate movement in east-west direction, (2) immediate movement in north-south direction, and (3) persistent movement in initial direction, in forest, clearing and corridor arenas.
### because intervals of confidence in forest and clearing always overlap with the expected probability under the null hypothesis of random movement, we then focused only on corridors.
plotmeans( subset_4$immediate_movement_ew ~ subset_4$treat_and_direction2, main= ("Immediate east-west movement (4-m scale)"),  ylab = "% of occurrence", ylim = c(0.1, 0.9), ci.label=F, digits = 2, barwidth = 2, connect= F, use.t=F, pch=1, barcol = "darkgreen")+ abline(h = 0.5, untf = FALSE, lty=4)
plotmeans( subset_8$immediate_movement_ew ~ subset_8$treat_and_direction2, main= ("Immediate east-west movement (8-m scale)"),  ylab = "% of occurrence", ylim = c(0.1, 0.9), ci.label=F, digits = 2, barwidth = 2, connect= F, use.t=F, pch=1, barcol = "darkgreen")+ abline(h = 0.5, untf = FALSE, lty=4)
plotmeans( subset_4$immediate_movement_ns ~ subset_4$treat_and_direction2, main= ("Immediate north-south movement (4-m scale)"),  ylab = "% of occurrence", ylim = c(0.15, 0.90), ci.label=F, digits = 2, barwidth = 2, connect= F, use.t=F, pch=1, barcol = "darkgreen" )+ abline(h = 0.5, untf = FALSE, lty=4)
plotmeans( subset_8$immediate_movement_ns ~ subset_8$treat_and_direction2, main= ("Immediate north-south movement (8-m scale)"),  ylab = "% of occurrence", ylim = c(0.15, 0.90), ci.label=F, digits = 2, barwidth = 2, connect= F, use.t=F, pch=1, barcol = "darkgreen"  )+ abline(h = 0.5, untf = FALSE, lty=4)
plotmeans( subset_4$persistent_movement ~ subset_4$atreat, main= ("Persistence in directional movement (4-m scale)"), ylab = "% of occurrence", ylim = c(0, 0.4), ci.label=F, digits = 2, barwidth = 2, connect= F, use.t=F, pch=1, barcol = "darkgreen"   )+ abline(h = 0.09, untf = FALSE, lty=4)
plotmeans( subset_8$persistent_movement ~ subset_8$atreat, main= ("Persistence in directional movement (8-m scale)"), ylab = "% of occurrence", ylim = c(0, 0.4), ci.label=F, digits = 2, barwidth = 2, connect= F, use.t=F, pch=1, barcol = "darkgreen"   )+ abline(h = 0.155, untf = FALSE, lty=4)

##### assessing the effects of butterfly sex, corridor characteristics (width and direction), forest height (used as a proxy of forest density), and the interaction of forest height and corridor width
# subset corridors
# testing the effect of date (date_ord), time of release (time.n), temperature (temp), and position of the releaser (sq_corner_code) on immediate and persistent arctic fritillary movements

summary(glmer(immediate_movement_corridor ~  (1|arena_id) + date_ord, family = binomial, subset_corridor))
summary(glmer(immediate_movement_corridor ~  (1|arena_id) + time.n, family = binomial, subset_corridor))
summary(glmer(immediate_movement_corridor ~  (1|arena_id) + temp, family = binomial, subset_corridor))
summary(glmer(immediate_movement_corridor ~  (1|arena_id) + sq_corner_code, family = binomial, subset_corridor))

summary(glmer(persistent_movement ~  (1|arena_id) + date_ord, family = binomial, subset_corridor))
summary(glmer(persistent_movement ~  (1|arena_id) + time.n, family = binomial, subset_corridor))
summary(glmer(persistent_movement ~  (1|arena_id) + temp, family = binomial, subset_corridor))
summary(glmer(persistent_movement ~  (1|arena_id) + sq_corner_code, family = binomial, subset_corridor))

# probability of immediate movement in corridor
# modelimmediate <- glm(immediate_movement_corridor ~  atreat , family = binomial, subset_corridor)
modelimmediate <- glmer(immediate_movement_corridor ~  atreat + (1|arena_id), family = binomial, subset_corridor)
summary(modelimmediate)
attributes(logLik(modelimmediate))
confint(modelimmediate)
predict(modelimmediate, type="response", se.fit = TRUE) 
## plot used to create Fig. 2 in the paper (random effect variance is 0)
plotmeans( subset_corridor$immediate_movement_corridor ~  subset_corridor$atreat, main= ("Immediate movement in corridor direction"),  ylab = "Probability of event", ylim = c(0.4, 0.8), ci.label=F, digits = 2, barwidth = 2, connect= F, use.t=F, pch=1, barcol = "darkgreen")+ abline(h = 0.5, untf = FALSE, lty=4)

##### testing for the effect of the other covariates, with no support for effect of 
# model1 <- glm(immediate_movement_corridor ~  atreat + canopy_height, family = binomial, subset_corridor)
# model2 <- glm(immediate_movement_corridor ~  atreat * canopy_height, family = binomial, subset_corridor)
# model3 <- glm(immediate_movement_corridor ~  atreat + sex , family = binomial, subset_corridor)
# model4 <- glm(immediate_movement_corridor ~  atreat + corridor_direction, family = binomial, subset_corridor)
# model5 <- glm(immediate_movement_corridor ~  atreat * canopy_height + corridor_direction + sex, family = binomial, subset_corridor)
model1 <- glmer(immediate_movement_corridor ~  atreat + canopy_height+ (1|arena_id), family = binomial, subset_corridor)
model2 <- glmer(immediate_movement_corridor ~  atreat * canopy_height+ (1|arena_id), family = binomial, subset_corridor)
model3 <- glmer(immediate_movement_corridor ~  atreat + sex + (1|arena_id), family = binomial, subset_corridor)
model4 <- glmer(immediate_movement_corridor ~  atreat + corridor_direction + (1|arena_id), family = binomial, subset_corridor)
model5 <- glmer(immediate_movement_corridor ~  atreat * canopy_height + corridor_direction + sex + (1|arena_id), family = binomial, subset_corridor)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
confint(model5)

# probability of persistent movement in corridor
# modelpersist <- glm(persistent_movement ~ atreat, family = binomial, subset_corridor)
modelpersist <- glmer(persistent_movement ~ atreat + (1|arena_id), family = binomial, subset_corridor)
summary(modelpersist)
attributes(logLik(modelpersist))
confint(modelpersist)
predict(modelpersist, type="response", se.fit = TRUE) 
## plot used to create Fig. 2 in the paper
plotmeans( subset_corridor$persistent_movement ~  subset_corridor$atreat, main= ("Persistent movement in corridor direction"),  ylab = "Probability of event", ylim = c(0, 0.4), ci.label=F, digits = 2, barwidth = 2, connect= F, use.t=F, pch=1, barcol = "darkgreen")+ abline(h = 0.09, untf = FALSE, lty=4)+ abline(h = 0.155, untf = FALSE, lty=4)

##### testing for the effect of the other covariates, with no support for effect of 
# model11 <- glm(persistent_movement ~  atreat + canopy_height, family = binomial, subset_corridor)
# model12 <- glm(persistent_movement ~  atreat * canopy_height, family = binomial, subset_corridor)
# model13 <- glm(persistent_movement ~  atreat + sex, family = binomial, subset_corridor)
# model14 <- glm(persistent_movement ~  atreat + corridor_direction, family = binomial, subset_corridor)
# model15 <- glm(persistent_movement ~  atreat * canopy_height + corridor_direction + sex, family = binomial, subset_corridor)
model11 <- glmer(persistent_movement ~  atreat + canopy_height + (1|arena_id), family = binomial, subset_corridor)
model12 <- glmer(persistent_movement ~  atreat * canopy_height + (1|arena_id), family = binomial, subset_corridor)
model13 <- glmer(persistent_movement ~  atreat + sex + (1|arena_id), family = binomial, subset_corridor)
model14 <- glmer(persistent_movement ~  atreat + corridor_direction + (1|arena_id), family = binomial, subset_corridor)
model15 <- glmer(persistent_movement ~  atreat * canopy_height + corridor_direction + sex+ (1|arena_id), family = binomial, subset_corridor)
summary(model11)
summary(model12)
summary(model13)
summary(model14)
summary(model15)
confint(model15)


##### 4) Chi-square tests between expected and observed distributions; see supplementary material for further information on how the expected probabilities were calculated
## Legend: narrow= 4-m arenas; large= 8-m arenas; F= forest; CL= clearing; CO= corridor; COEW: corridor oriented on east-west; CONS: corridor oriented on north-south
## number of butterfly selecting the immediate east-west direction vs. not (results are equal for north-south)
narrowF <- c(49, 57)
chisq.test(narrowF, p = c(1/2, 1/2))

narrowCL <- c(49, 50)
chisq.test(narrowCL, p = c(1/2, 1/2))

narrowCOEW <- c(55, 22)
chisq.test(narrowCOEW, p = c(1/2, 1/2))

narrowCONS <- c(32, 56)
chisq.test(narrowCONS, p = c(1/2, 1/2))

largeF <- c(49, 57)
chisq.test(largeF, p = c(1/2, 1/2))

largeCL <- c(50, 49)
chisq.test(largeCL, p = c(1/2, 1/2))

largeCOEW <- c(54, 26)
chisq.test(largeCOEW, p = c(1/2, 1/2))

largeCONS <- c(29, 60)
chisq.test(largeCONS, p = c(1/2, 1/2))

## number of butterflies selecting the immediate corridor direction at the release vs. not (independent of corridor orientation) 
corridorsallsmall <- c(111, 54)
chisq.test(corridorsallsmall, p = c(0.5, 0.5))

corridorsalllarge <- c (113, 56)
chisq.test(corridorsalllarge, p = c(0.5, 0.5))

## number of butterflies passing the 12-m mark vs. not

narrowF12 <- c(8, 98)
chisq.test(narrowF12, p = c(0.09, 0.91))

narrowCL12 <- c(10, 89)
chisq.test(narrowCL12, p = c(0.09, 0.91))

narrowCO12 <- c(27, 138)
chisq.test(narrowCO12, p = c(0.09, 0.91))

largeF12 <- c(12, 94)
chisq.test(largeF12, p = c(0.155, 0.845))

largeCL12 <- c(14, 85)
chisq.test(largeCL12, p = c(0.155, 0.845))

largeCO12 <- c(44, 125)
chisq.test(largeCO12, p = c(0.155, 0.845))

