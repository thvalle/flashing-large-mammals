library(tidyverse)
library(lme4)
library(ggeffects)   # Estimated Marginal Means and Marginal Effects from Regression Models
                          # (calls effects::effect)

# Data drom Data_exploration2.R
time.dep <- readRDS("timedep.rds")

ctrl <- c("Control_1", "Control_2", "Control_3","Control_4")
sp <- c("raadyr", "rev", "hjort", "grevling", "elg", "gaupe", "ekorn", "hare", "maar")
time.dep2 <- time.dep %>% 
  rename(species = validated_species) %>%  #shortening name
  #  filter(species %in% sp) %>% #filtering out species
  # including Control as part of the flash-column, since it differs from flash=0
  mutate(flash = factor(
    ifelse(period %in% ctrl, "Control", flash)),
    week = lubridate::isoweek(date),
    period = factor(period))
time.dep2 <- time.dep2 %>% 
  mutate(flash = fct_relevel(flash, "Control","0","1")) # relevel to make Control the model intercept
levels(time.dep2$flash) <- c("Control", "IR", "LED")    # relabel
levels(time.dep2$period) <- c("IR_1", "IR_2", "LED_1", "LED_2", # renaming the periods
                              "Control_1", "Control_2", "Control_3", "Control_4")

# Trimming the data -------------------------------------------
# find median period length
time.period <- time.dep2 %>% group_by(loc, period, flash) %>% 
  summarise(period_length = max(time.deploy))

# checking shortest periods
time.period %>% arrange(period_length) # 1 period (LED) is 0 days

# then merge lengths and filter out period of 0 days
time.dep3 <- time.dep2 %>% left_join(time.period) %>% 
  filter(period_length > 0)

# find median length 
time.period %>% filter(flash == "LED") %>%  
  summary() # median period length 85 days, mean: 84
time.period %>% filter(flash == "IR") %>%  
  summary() # median period length 78 days, mean: 89
time.period %>% filter(flash == "Control") %>%  
  summary() # median period length 89 days, mean: 93

# extract lengths of each unique period
h <- time.dep3 %>% group_by(loc, period, period_length, flash)%>% nest() %>% 
  select(!data) 
#extracting median and multiplying by 10, to use in the correctly scaled plot
hh <-       h$period_length[h$flash == "LED"]      %>%  median()       #  white LED
hh <- c(hh, h$period_length[h$flash == "IR" ]      %>%  median())      # +  IR
hh <- c(hh, h$period_length[h$flash == "Control" ] %>%  median()) * 10 # +  Control
# smallest median 
h <- min(hh)
h # 84 shortest median (IR)

# filtering out periods longer than (shortest) median length.
time.dep4 <- time.dep3 %>% filter(time.deploy < h/10) # h is normal scale, must be rescaled by /10
time.dep5 <- time.dep4 %>% mutate(time.deploy = time.deploy/8.4) # fully scaled

# prepare species subset
sp ="ekorn"
time_sp <- filter(time.dep4, species %in% sp) #.dep4 = trimmed data
# time_sp <- filter(time.dep5, species %in% sp) # still doesn't converge
# Model
m_sp  <- lme4::glmer(n.obs ~ time.deploy * flash + # fixed effects
                       (1 | loc) + (1 | week), # random effects
                     data   = time_sp,
                     family = poisson) # poisson family of distributions
# Warning messages:
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     Model failed to converge with max|grad| = 0.113957 (tol = 0.002, component 1)
#   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     Model is nearly unidentifiable: very large eigenvalue
#                       - Rescale variables?


# plotting marginal effects
p_sp    <- ggeffects::ggeffect(m_sp, terms = c("time.deploy [all]", "flash"))
plot(p_sp) + labs(title = paste0("Predicted counts of ", sp))
