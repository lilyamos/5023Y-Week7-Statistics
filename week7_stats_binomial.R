library(tidyverse)

challenger <- read_csv("data/Challenger.csv")

head(Challenger)

### only look at the flights where a failure had occurred: concluded that 
### temperature did not appear to affect o-ring risk of failure
challenger %>% 
  filter(oring_dt > 0) %>% 
  ggplot(aes(y=oring_dt, x=temp))+geom_point()+
  ggtitle("Temperature on flight launches where an O-ring incident occurred")

### include the flights without incident and those with incident, we can see that
### there is a very clear relationship between temperature and the risk of an 
### o-ring failure.
challenger %>% 
  ggplot(aes(y=oring_dt, 
             x=temp))+
  geom_point()+
  geom_smooth(method="lm")+
  ggtitle("All launch data")

### use dplyr to generate binary column of no incident(0) & fail(1) for anything >0.
challenger <- challenger %>% 
  mutate(oring_binary = ifelse(oring_dt =='0', 0, 1))
### p represents the probability of an o-ring failure and 1-p represents the 
### probability of no incident

binary_model <- glm(oring_binary~temp, family=binomial, data=challenger)
broom::tidy(binary_model, conf.int=T) %>% 
  kableExtra::kbl() %>% 
  kableExtra::kable_minimal()
### Intercept = β0 = 23.77: When temperature is 0°F the log-odds are 23.77 for a
### failure incident in the O-rings
### Temp = β1 = -0.37: For every rise in the temp by 1°F, the log-odds of a 
### critical incident fall by 0.37.

### can see that the effect of temperature appears to be highly significant and 
### we can present the likelihood ratio summary with ANOVA tables
kableExtra::kbl(car::Anova(binary_model, type="II")) %>% 
  kableExtra::kable_minimal()

### determine base-level risk of O-ring failure for the mean temp on launch day(69.6°F)
emmeans::emmeans(binary_model, specs=~temp, type="response")
### or 
estimate_at_69.6 <- exp(coef(binary_model)[1]+coef(binary_model)[2]*69.6)
estimate_at_69.6/(1+estimate_at_69.6)
### or 
estimate_at_70.6<- exp(coef(binary_model)[1]+coef(binary_model)[2]*70.6)
estimate_at_69.6-estimate_at_70.6
### or
### divide-by-four rule: can be described as max difference in probability for a
### unit change in X, which is β/4. In our example the max difference in 
### probability from a one degree change in Temp is −0.37/4=−0.09. So the max
### difference in probability of failure corresponding to a one degree change is 9%
### or
augmented_model <- broom::augment(binary_model,Challenger, 
                                  type.predict="response",
                                  se_fit=T)
augmented_model %>% 
  head() %>% 
  kableExtra::kbl() %>% 
  kableExtra::kable_minimal()

### add CI by extracting regressions through family & link functions
fam <- family(binary_model)
ilink <- fam$linkinv
### augment data with the model but dont specify response. manually use the link
### function to convert estimate back into the response scale, BUT importantly 
### also transform the S.E. properly.
broom::augment(binary_model,Challenger,se_fit=T)%>%
  mutate(.lower = ilink(.fitted - 1.96*.se.fit),
         .upper = ilink(.fitted + 1.96*.se.fit), exp.fitted=ilink(.fitted)) %>% 
  ggplot(aes(x=temp, y=oring_binary))+geom_line(aes(x=temp, y=exp.fitted))+
  geom_ribbon(aes(ymin=.lower, ymax=.upper), alpha=0.2)

### make new dataset with the temp on the day of the Challenger Launch 36°F
new_data <- tibble(temp=36, oring_binary=1)
new_data
### use augment function to fit our model - fitted using our original data - and
### map it to this new data.
augmented_new_data <- broom::augment(binary_model, newdata = new_data, type.predict = "response", se_fit=T)
augmented_new_data 
### can see there was a greater than 99.99% probability of o-ring failure
### represent it visually:
augmented_new_data <- broom::augment(binary_model, newdata = new_data, se_fit=T) 
### new data on link scale
augmented_old_data <- broom::augment(binary_model,Challenger, se_fit=T)
## old data on link scale
augmented_full_data <- augmented_new_data %>% full_join(augmented_old_data)
### full join combines the datasets
augmented_full_data %>% 
  mutate(.lower = ilink(.fitted - 1.96*.se.fit),
         .upper = ilink(.fitted + 1.96*.se.fit), exp.fitted=ilink(.fitted)) %>%  
  ggplot(aes(x=temp, y=exp.fitted))+
  geom_point(aes(x=temp, y=oring_binary), colour="red")+
  geom_ribbon(aes(ymin=.lower, ymax=.upper), alpha=0.2)+   
  ### Fits one shape to display confidence interval
  geom_line(linetype="dashed", width=1.5)+
  theme_minimal()+
  gghighlight::gghighlight(temp<40, 
                           unhighlighted_colour = alpha("darkgrey",0.9))+   
  ### emphasise particular data
  geom_label(label="Challenger",    
             ### add custom labels
             nudge_x=3.5, 
             nudge_y=-0.05)+
  labs(x=expression(paste('External Air Temperature (',~degree,'F)', sep='')), 
       y="Risk of O-ring failure")+
  ggtitle("Model predictions for the risk of O-ring failure \nwith changing external air temperature")

### check model
par(mfrow=c(2,2))
plot(binary_model, col="darkgrey")

plot(binary_model, col="darkgrey", which=1)

arm::binnedplot(augmented_model$.fitted,augmented_model$.resid)
### can see there is a slight slope in residuals (we expect two groups but 
### ideally points would be horizontal), this might indicate another predictor 
### variable at work, but we have nothing else to work with in our dataset
### The binned residual plot tests for over-dispersion (we cannot use the ratio 
### of residual deviance to residual DF because of the constrained nature of the
### data). This approach works better with more data so is not much use here. 
### Ideally we want residual points within the grey lines (2*Standard errors of 
### the binned residuals).