# tidymodels example of survival

library(survival)
library(survminer)
# read data from Time_to_event_Torgeir5.R
obs <- readRDS("obs_surv_prepared.rds")
#some final touches
unique(obs$validated_species)
obs<-obs[obs$t.diff>=0,]
obs$flashed<-as.factor(obs$flashed)
obs$flash<-as.factor(obs$flash)
sp="rev"

# mod0 from Neri
mod0<-coxph(Surv(t.diff, event, type="right")~flashed, data=obs[obs$validated_species%in%sp & !obs$period%in%"Control",])
summary(mod0)
# Keep in mind that positive values means higher risk of "dying" or being detected again, and negative values means lower risk of "dying" or being detected again
# In this case the positive sign for flashed means that when the animal was flashed the time to new detection is shorter than when the animal was not flashed. 
# However, the effect is not significant. 


?survreg
# mod example from tidymodels
lung_mod <- survreg(Surv(time, status) ~ ph.ecog + age + strata(sex), data = lung)
summary(lung_mod)

# explore for yourself
sp_mod <- coxph(Surv(t.diff, event, type="right") ~ flashed + strata(validated_species),
                data = obs[!obs$period%in%"Control",]) # remove control data
sp_mod

fit <- survfit(Surv(t.diff, event, type="right") ~ flashed + strata(validated_species), data = obs[!obs$period%in%"Control",]) 
ggsurvplot(fit, data = obs[!obs$period%in%"Control",])
# crazy amounts of species! Filter out the ones I wanted to focus on

# sp = c("rev", "raadyr", "gaupe", "hjort", "grevling", "elg")
sp = c("rev", "raadyr")
fit<-survfit(Surv(t.diff, event, type="right")~flashed + strata(validated_species), data=obs[obs$validated_species%in%sp & !obs$period%in%"Control",])
ggsurvplot(fit, data = obs[obs$validated_species%in%sp & !obs$period%in%"Control",])
