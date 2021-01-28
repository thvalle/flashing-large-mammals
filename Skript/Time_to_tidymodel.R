# tidymodels example of survival
library(tidyverse)
library(tidymodels)
library(survival)
library(survminer)
# read data from Time_to_event_Torgeir5.R
sp = c("rev", "raadyr", "gaupe", "hjort", "grevling", "elg")
covs<-readRDS("CTloc_covs.rds") %>% as.data.frame() 
library(corrplot)
covs %>% select(!c(1,3,4)) %>% cor() %>% 
  corrplot(type = "upper", method = "number")
names(covs)
obs <- readRDS("obs_tte5.rds") %>% filter(validated_species %in% sp) %>% 
  merge(covs, by.x="loc", by.y="LokalitetID", all.x=TRUE, all.y=FALSE) %>% 
  # removing various IDs, lat lon which correlates with slope, build dens,
  # field_d2, as well as geometry which I don't understand what is
  select(-c(distance, num_animals, 4:7, 14, 19:21) ) %>%  
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)



obs %>% 
  skimr::skim(validated_species, flashed) 


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

sp = c("rev", "raadyr", "gaupe", "hjort", "grevling", "elg")
# sp = c("rev", "raadyr")
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

library(Rtool)

# remotes::install_github("easystats/report") # finally worked after several days
#  had to accept all updates (ie. type 1 at every prompt)
library(report)
library(tidyverse)
dat <- mtcars %>%
  select(-vs, -am)
test <- cor.test(dat$drat, dat$qsec)
test
report(test)

remotes::install_github("easystats/easystats")


