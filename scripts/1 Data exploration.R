rm(list=ls())
library(here)
library(tidyverse)
library(vegan)
library(lattice)
library(ggpubr)
library(car)
library(psych)
library(lme4)
library(glmulti)
library(visreg)
library(MASS)
library(corrplot)
library(DHARMa)

# Load and check data ----
movement.data <- read.csv2(here("data", "database_models.csv" ))
rutor <- read.csv2(here("data", "Provrutor_plants.csv"))
roadverge <- read.csv2(here("data", "Roadside_colour.csv"))
width.data <- read.csv2(here("data", "Road_verge_width.csv"))

str(movement.data)
db <- movement.data %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Road.verge = as.factor(Road.verge)) %>% 
  rename(traffic = Traffic.int) %>% 
  rename(trials = Total.no..of.flowers) %>% 
  rename(successes = Number.marked) %>% 
  mutate(Same.side = as.factor(Same.side)) %>% 
  mutate(Direction = as.factor(Direction)) %>% 
  rename(insects = Insect.abundance) %>% 
  rename(flower.dens = Number.of.flowers)
str(db)

str(rutor)  
plants <- rutor %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Side = as.factor(Side)) %>% 
  mutate(Visit = as.factor(Visit)) %>% 
  mutate(Cut = as.factor(Cut))  
str(plants) 

str(roadverge)
side.metadata <- roadverge %>%
  mutate(Site = as.factor(Site)) %>% 
  mutate(Side = as.factor(Side)) %>% 
  mutate(Colour = as.factor(Colour))
str(side.metadata)

str(width.data)
rv.width <- width.data %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Side = as.factor(Side)) %>% 
  group_by(Site, Side) %>% 
  summarise(mean.width.side = sum(Width)/n()) # takes the mean width from 5 measures
rv.width

# Check for outliers
fig <- par(mfrow= c(2,2), mar = c(3,3,3,1))
dotchart(db$successes, main = "number of marked", group = db$Road.verge)
dotchart(db$trials, main = "number of trials", group = db$Road.verge)
dotchart(db$insects, main = "insect abundance/road verge", group = db$Road.verge)
dotchart(db$flower.dens, main = "number flowers/road verge", group = db$Road.verge)
par(fig)

# Make histograms of number of successes
ggplot(db, aes(x=successes)) + geom_histogram(color="black", fill="white") +
  facet_grid(~Road.verge + Same.side)

# Create data frame with number of flowers in each side of the road per site
# For visit 1:
flowers.0 <- plants %>% 
  filter(Visit %in% "1") %>% 
  group_by(Site, Side, Quadrant.no.) %>% 
  summarise(Flower.abund = sum(Number.of.flower.heads)/n())
flowers.0

flower.roadside <- flowers.0 %>% 
  group_by(Site, Side) %>% 
  summarise(Flower.abund = sum(Flower.abund))
flower.roadside


# Explore flower data ----

# Differences in flower density between sides in the same site
gg.flowerside <- ggplot(flowers.0, aes(x = Site, y = Flower.abund)) + 
  geom_boxplot(aes(fill=Side)) + labs(x="Site", y="Number of flowers") 
gg.flowerside

# Are species rich sites more diverse? Only visit 1 
p.rich <- plants %>% 
  filter(Visit %in% "1") %>% 
  group_by(Site, Species) %>% 
  summarise(na.rm = TRUE) %>% 
  summarise(p.rich=n())
p.rich

db.plantsp <- left_join(db, p.rich, by="Site")

sp.site <- db.plantsp %>% 
  group_by(Site, Road.verge, p.rich) %>% 
  summarise()
sp.site

mod.sp.sitecat <- lm(p.rich~Road.verge, data=sp.site)
summary(mod.sp.sitecat)
plot(mod.sp.sitecat)
visreg(mod.sp.sitecat, ylab="Number of plant species", xlab="Road verge") #yes

# Is the flower density different between site categories?
site.flower <- db %>% 
  group_by(Site, Side, Road.verge) %>% 
  summarise(Flower.abund.0 = sum(flower.dens)/n()) %>% 
  group_by(Site, Road.verge) %>% 
  summarise(flowers.tot = sum(Flower.abund.0))
site.flower

ggplot(site.flower, aes(x=log10(flowers.tot + 1))) + geom_histogram(color="black", fill="white")

mod.flowerdens <- lm(log10(flowers.tot + 1) ~ Road.verge, data = site.flower)
summary(mod.flowerdens) # yes
plot(mod.flowerdens) #looks ok
visreg(mod.flowerdens)

# Is the flower abundance changing with traffic?
flower.traffic.0 <- db %>% 
  group_by(Site, traffic, flower.dens) %>% 
  summarise()
flower.traffic.0

flower.traffic.1 <- flower.traffic.0 %>% 
  group_by(Site, traffic) %>% 
  summarise(flower.site = sum(flower.dens))
flower.traffic.1

ggplot(flower.traffic.1, aes(x=log10(flower.site+1))) + 
  geom_histogram(color="black", fill="white") 

flower.traffic.mod <- lm(log10(flower.site + 1) ~ traffic, 
                         data = flower.traffic.1)
summary(flower.traffic.mod)
plot(flower.traffic.mod)
visreg(flower.traffic.mod)

# Explore insect abundance -----
insect.traffic.0 <- db %>% 
  group_by(Site, traffic, insects) %>% 
  summarise()
insect.traffic.0

insect.traffic <- insect.traffic.0 %>% 
  group_by(Site, traffic) %>% 
  summarise(insect.site = sum(insects))
insect.traffic

# Does traffic intensity affect insect abundance?
insect.traffic.mod <- lm(log10(insect.site + 1) ~ traffic, data = insect.traffic)
summary(insect.traffic.mod)
plot(insect.traffic.mod) #ok
visreg(insect.traffic.mod)

# Is the insect abundance different between site categories?
site.insect <- db %>% 
  group_by(Site, Side, Road.verge) %>% 
  summarise(insect.0 = sum(insects)/n()) %>% 
  group_by(Site, Road.verge) %>% 
  summarise(insects.tot = sum(insect.0))
site.insect

ggplot(site.insect, aes(x=log10(insects.tot + 1))) + geom_histogram(color="black", fill="white")

mod.insectcat <- lm(log10(insects.tot + 1) ~ Road.verge, data = site.insect)
summary(mod.insectcat) # yes
plot(mod.insectcat) #QQ looks a bit weird
visreg(mod.insectcat)

# Explore road verge width ----

# Extract road verge width (per site) and join with the database
hist(rv.width$mean.width.side) # looks ok
rv.width.site <- rv.width %>% 
  group_by(Site) %>% 
  summarise(site.width = sum(mean.width.side)/n())
rv.width.site  

width.site.type <- left_join(db, rv.width.site, by="Site")

# Is the road verge width different between sp poor and sp rich?
width.flower <- width.site.type %>% 
  group_by(Site, Road.verge, site.width) %>% 
  summarise()
width.flower
hist(width.flower$site.width)
hist(log10(width.flower$site.width +1))

rv.sitecat <- lm(site.width ~ Road.verge, data = width.flower)
summary(rv.sitecat) # no
plot(rv.sitecat)
visreg(rv.sitecat)


# Is the road verge width changing with traffic intensity?
width.traffic <- width.site.type %>% 
  group_by(Site, traffic, site.width) %>% 
  summarise()
width.traffic

rv.traffic <- lm(site.width ~ traffic, data = width.traffic)
summary(rv.traffic) # yes
plot(rv.traffic)
visreg(rv.traffic)

# Finally, are the number of insects correlated to the number of flowers?
insect.flower <- left_join(insect.traffic, site.flower, by="Site")
cor.test(insect.flower$insect.site, insect.flower$flowers.tot, method = "pearson")
