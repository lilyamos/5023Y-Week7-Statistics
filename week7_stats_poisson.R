library(tidyverse)

cuckoo <- read_csv("data/cuckoo.csv")

### How does nestling mass affect begging rates between the different species?

head(cuckoo)

ggplot(cuckoo, aes(x=Mass, 
                   y=Beg, 
                   colour=Species)) + 
  geom_point()

fit <- lm(Beg ~ Mass+Species+Mass:Species, data=cuckoo) 

par(mfrow=c(2,2))
plot(fit, col="darkgrey")
### normality: not terrible but  signs of overdispersion.
### equal variance of residuals: Definitely not! The residual plot depicts a 
### clear “funnel”. The model assumption is badly violated, and we can see that 
### as the mean increases so does the variance.
### Independence of the residuals: Mostly ok, but difficult to say for sure 
### because of the strong heteroscedasticity. A subtle slope pattern is visible 
### in the bottom left. This is cause by the bounding of the data at zero - but 
### the model predicts negative values.
### Influential outliers: at least one influential outlier.

logλ=β0+β1∗Mass+β2∗Species
### slope shows evidence of an interaction effect the relationship between Mass 
### and calling appears to change by species.
### is reasonable that species might change the relationship between mass and 
### begging calls? Yes, hypothesis that the type of bird species might 
### change the relationship between mass and begging calls. 
### logλ=β0+β1∗Mass+β2∗Species+β3∗(Mass∗Species)

### count_model <- glm(Beg Mass+Species+Mass:Species,family=poisson(link=‘log’),
###                data=cuckoo)
count_model <- glm(Beg ~ Mass+Species+Mass:Species, data=cuckoo, family=poisson(link="log"))

summary(count_model) 
### Intercept = β0: intercept for the reference so here the log of the mean 
### number of begging calls for cuckoos when mass = 0
###Mass = β1 (slope: the change in the log mean count of begging calls for every
### gram of bodyweight for cuckoos)
### SpeciesWarbler = β2 (the increase/decrease in the intercept for warblers 
### relative to cuckoos)
### Mass:SpeciesWarbler =β3 (the increase/decrease in the slope for warblers 
### relative to cuckoos)]

par(mfrow=c(2,2))
plot(count_model, col="darkgrey")
### Data appears to fit a poisson distribution better than a linear distribution

### check for overdispersion: if residual deviance is bigger than the residual 
### degrees of freedom then there is more variance than we expect from the 
### prediction of the mean by our model.
### from summary() it is 10.4 and >1 is overdispersion
### fit a quasi-likelihood model which accounts for this.
count_model <- glm(Beg ~ Mass+Species+Mass:Species, data=cuckoo, 
                   family=quasipoisson(link="log"))

main_effect_count_model <- glm(Beg ~ Mass+Species, data=cuckoo, 
                               family=quasipoisson(link="log")) 
### model without the interaction term but otherwise specified in an identical way
anova(count_model, main_effect_count_model, test="Chi") 
### fits type 1 sums of squares to compare the two models
### cannot drop the interaction term without significantly affecting fit of model.

car::Anova(count_model, type="III") 
### exponentiate estimates to get them on same scale as the response (y) variable
exp(coef(count_model)[1]) ### Intercept - Incidence rate at x=0
exp(coef(count_model)[2]) ### Change in the incidence rate with Mass
exp(coef(count_model)[3]) ### Change in the incidence rate intercept by Species
exp(coef(count_model)[4])

### tidy model with broom: specify you want to put model predictions on the 
### response variable scale by specfiying exponentiate=T which removes log 
### transformation
broom::tidy(count_model, 
            exponentiate=T, 
            conf.int=T)
broom::glance(count_model)

### plot regression lines for each species. specify we want our fitted values to
### be on the “response” scale otherwise it will default to the “log-link”.
broom::augment(count_model, cuckoo, type.predict="response") %>% 
  ggplot(aes(x=Mass,
             y=Beg,
             colour=Species))+
  geom_point(aes(x=Mass,
                 y=Beg,
                 colour=Species))+
  geom_line(aes(x=Mass, y=.fitted))+
  theme_minimal()

### when you report Incidence Rates and Confidence Intervals you capture effect
### sizes which explains the biology which underpins our analyses:

### Mass 1.06 (95%CI: 1.05-1.06): For every gram of bodyweight gained by a cuckoo 
### chick there was an average increase of 1.06 in the rate of begging calls per 
### six seconds. or On average begging rates go up as chicks get heavier

### SpeciesWarbler 0.59 (95%CI: 0.43-0.8): Warblers had an average call rate that
### was at least 0.67 higher than that of a Cuckoo of an equivalent weight

### Mass:SpeciesWarbler 1.02 (95%CI: 1-1.023): Warbler’s increased their call
### rate relative to body weight faster than cuckoo’s. for every gram of weight 
### gained, Warbler’s add one extra begging call (compared to the increase in 
### cuckoos) to the six-second observation period. This accounts for the steeper 
### curve in the warbler begging rate - the warblers clearly leave the nest at a 
### much lower body mass, while the cuckoos continue to beg and grow much larger! 
### Perhaps the steeper rate in begging reflects the faster maturity of the warbler chicks?

