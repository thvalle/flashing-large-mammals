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


fjern <- c("nothing","hund", "menneske", "kjoeretoey", "motorsykkel", "sykkel", "ukjent", 
           "sau", "ku", "fugl", "skogshons", "smagnagere", "andre_maardyr", "andre_pattedyr") # uninteresting or too general groups
passes <- obs %>% group_by(validated_species) %>% 
  summarise(count = n(),   # flashed = mean(flash, na.rm = T), # don't know if i can find a relevant use of this
            period = period, flash = flash) %>% 
  filter(!is.na(validated_species), !(validated_species %in% fjern))
ggplot(passes) +
  geom_bar(aes(reorder(validated_species, count, FUN = mean)), position = "dodge") +  # reorders by mean count
  geom_hline(yintercept = 50) + coord_flip() # flip the axes
# removing small mammals
small <- c("maar", "ekorn", "hare")
p_sp_focus <- passes %>% 
  filter(count > 50, !validated_species  %in% small) %>%  
  ggplot(aes(reorder(validated_species, count, FUN = mean))) + coord_flip()
p_sp_focus + geom_bar(aes(fill = flash),position = "dodge") + geom_hline(yintercept = 50)



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


library(ViewPipeSteps)

# Model formula displayer in RMarkdown!
library(equatiomatic)

# fit a basic multiple linear regression model
model <- lm(price ~ carat + depth,
            data = diamonds)

extract_eq(model,
           use_coefs = TRUE)

#If the equation is long, you can display it on multiple lines by adding the argument wrap = TRUE:
  
  model <- lm(price ~ carat + x + y + z + depth,
              data = diamonds)

extract_eq(model,
           use_coefs = TRUE,
           wrap = TRUE)



# Fit (complexe) survival curves

fit3 <- survfit( Surv(time, status) ~ sex + rx + adhere,
                 data = colon )

# Visualize
#++++++++++++++++++++++++++++++++++++
ggsurv <- ggsurvplot(fit3, data = colon,
                     fun = "cumhaz", conf.int = TRUE,
                     risk.table = TRUE, risk.table.col="strata",
                     ggtheme = theme_bw())

# Faceting survival curves
curv_facet <- ggsurv$plot + facet_grid(rx ~ adhere)
curv_facet

# Faceting risk tables:
# Generate risk table for each facet plot item
ggsurv$table + facet_grid(rx ~ adhere, scales = "free")+
  theme(legend.position = "none")

# Generate risk table for each facet columns
tbl_facet <- ggsurv$table + facet_grid(.~ adhere, scales = "free")
tbl_facet + theme(legend.position = "none")

# Arrange faceted survival curves and risk tables
g2 <- ggplotGrob(curv_facet)
g3 <- ggplotGrob(tbl_facet)
min_ncol <- min(ncol(g2), ncol(g3))
g <- rbind(g2[, 1:min_ncol], g3[, 1:min_ncol], size="last")
g$widths <- grid::unit.pmax(g2$widths, g3$widths)
grid::grid.newpage()
grid::grid.draw(g)


## End(Not run)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Example 3: CUSTOMIZED PVALUE
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Customized p-value
fit1<- survfit(Surv(time, status) ~ sex, data = lung)

ggsurvplot(fit1, data = lung, pval = TRUE, title = "Roe deer", ylab= "Capture probability")
ggsurvplot(fit1, data = lung, pval = 0.03)
ggsurvplot(fit1, data = lung, pval = "The hot p-value is: 0.031",surv.median.line = "hv")

