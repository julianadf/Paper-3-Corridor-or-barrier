movement.data <- read.csv2(here("data", "Fluorescent dye data 2019.csv" ))
rutor <- read.csv2(here("data", "Provrutor_plants.csv"))
roadverge <- read.csv2(here("data", "Roadside_colour.csv"))
width.data <- read.csv2(here("data", "Road_verge_width.csv"))

str(movement.data)
hist(movement.data$Insect.abundance) # need to transform in the model

mov.freq <- movement.data %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Road.verge = as.factor(Road.verge)) %>% 
  mutate(Side = as.factor(Side)) %>% 
  mutate(Side.colour = as.factor(Side.colour)) %>% 
  rename(trials= Total.no..of.flowers) %>% 
  rename(successes.blue= Number.of.blue.marked) %>% 
  rename(successes.red= Number.of.red.marked) %>% 
  mutate(failures.red = trials - successes.red) %>% 
  rename(dist.to.blue = Distance.to.1A..m.) %>% 
  rename(dist.to.red = Distance.to..1B..m.) %>% 
  mutate(direction.red = ifelse(Side.colour == "B", "x", "y")) %>% 
  mutate(direction.red = as.factor(direction.red)) %>% 
  mutate(blue.side = ifelse(Side.colour == "A", 1, 0)) %>% 
  mutate(blue.side = as.factor(blue.side)) %>% 
  
  mov.freq <- movement.data %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Road.verge = as.factor(Road.verge)) %>% 
  mutate(Side = as.factor(Side)) %>% 
  mutate(Side.colour = as.factor(Side.colour)) %>%
  rename(dist.to.blue = Distance.to.1A..m.) %>% 
  mutate(prop.blue = Number.of.blue.marked/Total.no..of.flowers) %>% 
  mutate(same.side = ifelse(Side.colour=="A", 1, 0)) %>% # 1: same side (didn't cross)
  mutate(direction = ifelse(Side.colour == "A", "x", "y")) %>% 
  mutate(direction = as.factor(direction))
str(mov.freq)

```{r, iclude=FALSE}  # organzises by traffic intensity. didnt made much sense
fig <- par(mfrow= c(2,2), mar = c(3,3,3,1))
dotchart(direction$successes, main = "number of marked", group = direction$traffic)
dotchart(direction$trials, main = "number of trials", group = direction$traffic)
dotchart(direction$insects, main = "insect abundance/Traffic intensity", group = direction$traffic)
dotchart(direction$flower.abund, main = "number flowers/Traffic intensity", group = direction$traffic)
par(fig)
```
# doesnt make much sense:
```{r, echo=FALSE}
width.data <- read.csv2(here("data", "Road_verge_width.csv"))

str(width.data)
rv.width <- width.data %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Side = as.factor(Side)) %>% 
  group_by(Site, Side) %>% 
  summarise(mean.width.side = sum(Width)/n())
rv.width

width.db <- left_join(direction, rv.width, by= c("Site", "Side"))
str(width.db)

# Extract unique observations (20) and test:
width.unique <- width.db %>% 
  group_by(Site, traffic, mean.width.side, flower.abund) %>% 
  summarise()
width.unique

gg.width <-ggplot(width.unique, aes(x=flower.abund, y=traffic)) + geom_point()
gg.width

mod.width <- lm(mean.width.side ~ traffic, data= width.unique)
summary(mod.width)
plot(mod.width)
visreg(mod.width)
ggscatter(width.unique, x="traffic", y="flower.abund", add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean road verge width/site", ylab = "Traffic intensity")

#Check if number of flowers in the road verge is correlated to traffic intensity
mod.width <- glmer(flower.abund ~ scale(traffic) + (1 | Site), data= width.unique, family = "poisson")
summary(mod.width)

