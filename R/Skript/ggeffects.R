time.dep <- readRDS("time.dep")
library(ggeffects)
library(lme4)
# ggeffects ---------------------------------------------------------------------
# Because ggeffects is similar to broom, it is not something that works
# together with broom
# They are different means at achieving the same goal

sp <- c("raadyr", "rev", "hjort", "grevling", "elg", "gaupe")
time.dep2 <- time.dep %>% 
  filter(validated_species %in% sp) %>% 
  mutate(flash = ifelse(period == "Control", "Control", flash)) %>%
  filter(!period == "Control")

# only roe deer
m  <- glmer(n.obs ~ time.deploy + as.factor(flash) + (1 | loc) + (1 | month), 
            data=filter(time.dep2, validated_species %in% "raadyr"), family = poisson)

check_model(m)
summary(m)
report::report(m) # veldig ryddig og fin output!



p <- ggpredict(m, terms = c("time.deploy", "flash"))
plot(p)
plot(p, add.data = TRUE)
plot(p, residuals = TRUE)







# make a fit of the whole dataset-------------------------------------------
# val_sp as a faceting factor
fit <- glmer(
  n.obs ~ time.deploy + as.factor(flash) + validated_species + # fixed effects
    (1 | loc) + (1 | month),  # random effects
  data=time.dep2, family = poisson) # data and distribution

mydf <- ggpredict(fit, terms = c("time.deploy", "flash", "validated_species"))
mydf

ggplot(mydf, aes(x, predicted, colour = group)) + 
  geom_line() + 
  facet_wrap(~facet)

plot(mydf, facet = TRUE)
plot(mydf, rawdata = TRUE)
plot(mydf, residuals = TRUE)
plot(mydf, residuals = TRUE, residuals.line = TRUE)




# ggstatsplot ------------------------------------------------------------------
time.dep <- time.dep %>% 
  mutate(species = validated_species)

library(tidyverse)
obs <- readRDS("Observations_prepared1.rds")
sp <- c("raadyr", "rev", "grevling", "elg" ,"hjort" , "gaupe")

set.seed(123) #for reproducibility

# n.days and n.obs per period per camera
obs_sp <- obs %>% rename(species= validated_species) %>%
  filter(species %in% sp) %>%
  group_by(loc, period) %>% 
  mutate(n.days = # Count days per period
           list(length(seq(min(date), max(date), by="1 day")))) %>%
  unnest(cols = n.days) %>% # unnest and repeat for species
  group_by(loc, period, species) %>% 
  mutate(n.obs = #this is every time, unable to filter out close events
           length(unique(datetime))) %>% 
  ungroup()
# Now, collapsing the tibble, and removing all other data
obs_sp <- obs_sp %>% 
  group_by(loc,period,n.days,species,n.obs) %>% nest() %>% 
  select(-data) %>% ungroup() %>% 
  mutate(freq = n.obs/n.days) %>% #frequency column
  arrange(loc) #arrange by location
obs_sp2 <- obs_sp %>%  # collapsing different flash-periods into each other
  mutate(flash_period = fct_collapse(period,
                                     Control = c("Control"),
                                     white_LED = c("1_1", "1_2"),
                                     IR = c("0_1", "0_2")) )

## Comparing between groups
ggstatsplot::grouped_ggbetweenstats(
  data =  obs_sp2,
  x = flash_period,
  y = freq,
  grouping.var = species, # grouping variable
  outlier.tagging = TRUE, # whether outliers need to be tagged
  outlier.label = period, # variable to be used for tagging outliers
  #using period, as x is a collapsed form of period
  outlier.coef = 2,
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "holm", # method for adjusting p-values for multiple comparisons
  # adding new components to `ggstatsplot` default
  ggplot.component = list(ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis())),
  title.prefix = "Species",
  caption = substitute(paste(italic("Source"), ": NINA")),
  palette = "default_jama",
  package = "ggsci",
  plotgrid.args = list(nrow = 2),
  annotation.args = list(title = "Differences in detection rate by treatment periods for different species")
)


## Within groups!

# for reproducibility and data
set.seed(123)
ggstatsplot::ggwithinstats(
  data = WineTasting,
  x = Wine,
  y = Taste,
  title = "Wine tasting",
  caption = "Data source: `WRS2` R package",
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE
)

# plot
ggstatsplot::ggwithinstats(
  data = filter(obs_sp2, species  %in% "rev"),
  x = flash_period,
  y = freq,
  title = "Fox",
  caption = "Data source: `WRS2` R package",
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE
)
obs_sp2 %>% 
  filter(species  %in% "rev") %>% 
  na.omit %>% 
  ggstatsplot::ggwithinstats(
    x = period,
    y = freq,
    title = "Fox",
    caption = "Differences between frequencies",
    ggtheme = ggthemes::theme_fivethirtyeight(),
    ggstatsplot.layer = FALSE
  )





##













