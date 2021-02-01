# tidymodels example of survival
library(tidyverse)
library(tidymodels)
library(survival)
library(survminer)
# read data from Time_to_event_Torgeir5.R
covs<-readRDS("CTloc_covs.rds") %>% as.data.frame() 
library(corrplot)
covs %>% select(!c(1,3,4)) %>% cor() %>% 
  corrplot(type = "upper", method = "number")
names(covs)

obs <- readRDS("obs_tte5.rds") %>% rename(species = validated_species) %>% 
  merge(covs, by.x="loc", by.y="LokalitetID", all.x=TRUE, all.y=FALSE) %>% 
  # removing various IDs, lat lon which correlates with slope, build dens,
  # field_d2, as well as geometry which I don't understand what is
  select(-c(4:9, 11, 19:21) ) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)
names(obs)


obs %>% 
  skimr::skim(species, flashed) 
#sp = "raadyr"
sp = c("rev", "raadyr", "gaupe", "hjort", "grevling", "elg")
obs_sp <- obs %>%  # making obs_sp for specific species, removing control group
  filter(species %in% sp, !period == "Control")

sp_mod0<-coxph(Surv(t.diff, event, type="right")~flashed 
            + strata(species), data=obs_sp)
summary(sp_mod0)




surv_diff <- survdiff(Surv(t.diff, event) ~ flashed + strata(loc), 
                      data = obs_sp)
surv_diff
surv_diff_sp <- survdiff(Surv(t.diff, event) ~ species +
                         flashed + strata(loc), data=obs_sp)
surv_diff_sp


fit<-survfit(Surv(t.diff, event, type="right") ~ flashed +  species,
             data=obs_sp)
ggsurvplot(fit, data = obs_sp,
           ggtheme = theme_minimal(),
           censor.shape = "|", censor.size = 3)



# Visualizations ------------------------------------------------
ggsurv <- ggsurvplot(fit, data = obs_sp,
                     #fun = "cumhaz", conf.int = TRUE,
                    # risk.table = T, risk.table.height = "none",
                     risk.table.y.text.col = T,# colour risk table text annotations.
                     risk.table.y.text = FALSE,# show bars instead of names in text annotations
                     ggtheme = theme_bw(),  
                     xlim = c(0,120)
                     )

# Faceting survival curves
curv_facet <- ggsurv$plot + facet_grid( ~ species) + theme(legend.position = "none")
curv_facet

# Faceting risk tables:
# Generate risk table for each facet plot item
ggsurv$table + facet_grid( ~ species, scales = "free") +
  theme(legend.position = "none")

# Generate risk table for each facet columns
tbl_facet <- ggsurv$table + facet_grid(.~ species, scales = "free") 
#+ theme(legend.position = element_blank())
tbl_facet + theme(legend.position = "none")

# Arrange faceted survival curves and risk tables
g2 <- ggplotGrob(curv_facet)
g3 <- ggplotGrob(tbl_facet)
min_ncol <- min(ncol(g2), ncol(g3))
g <- rbind(g2[, 1:min_ncol], g3[, 1:min_ncol], size="last")
g$widths <- grid::unit.pmax(g2$widths, g3$widths)
grid::grid.newpage()
grid::grid.draw(g) 



# report --------------------------
library(report) #not available for coxph >:^(
library(tidyverse)
dat <- mtcars %>%
  select(-vs, -am)
test <- cor.test(dat$drat, dat$qsec)
test
report(test)

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


