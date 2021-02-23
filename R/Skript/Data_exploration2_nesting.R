# This script explores the data and make some graphical
# presentations


library(plyr)
library(dplyr)
library(tidyverse)

# Import data
effort <- readRDS("Effort_prepared.rds")
obs <- readRDS("Observations_prepared1.rds")


# Now, we aggregate the data by camera trap and period for
# the effort to see how many days the camera was active
effort.stat <- ddply(effort, .(loc, period), summarise, n.days = length(unique(date)))

colnames(obs)
# Then, we aggregate the observations of the different
# species based on the obs object
obs.stat <- ddply(obs, .(loc, period, validated_species), summarise, 
  n.obs = length(unique(datetime)))  # Change to datetime here since we might have several 
# observation during a single day


torg <- obs %>% group_by(loc,period,validated_species) %>%
  summarise( diff = round(difftime(datetime, lag(datetime), units="mins"), 0) )
torg$diff[is.na(torg$diff)] <- 60 # all "first"-observations gets an arbitrary t.diff of 1 hour

torg2 <- torg %>% group_by(loc,period,validated_species) %>%
  summarise( n.obs2 = sum( diff > 10) ) # count all observations that were
                              # more than 10 mins later than the previous event

diff.stat <- obs.stat %>% left_join(torg2) %>% 
  mutate( eq = n.obs != n.obs2)
sum(diff.stat$eq > 0) # 585 instances of 
# diff.stat %>%  filter(eq > 0)

diff.stat %>% group_by(validated_species) %>% 
  summarise(n = sum(eq > 0)) %>% filter(n > 0) %>% 
  ggplot(aes(n,reorder(validated_species, n, FUN = mean))) +
  geom_col() +
  labs(y = element_blank(), x = element_blank(),
       title = "number of times obs.stat differs from torg2",
       subtitle = "filtered timediff > 10min" )

# Not all possible combinations of camera traps, camera traps
# and species will be in the obs.stat, since not all species
# are caputred on a single camera.  Extending the effort.stat
# frame to contain all possible detection on every camera
# trap location
temp <- effort.stat
for (i in 2:length(unique(obs$validated_species))) {
  effort.stat <- rbind(effort.stat, temp)
}

effort.stat$validated_species <- rep(as.character(unique(obs$validated_species)), 
  each = nrow(temp))  # assigning the validated_species 
# View(effort.stat) # Looks good

freq <- merge(obs.stat, effort.stat, by = c("loc", "period", 
  "validated_species"), all = TRUE)
# Merging issue here should disapper after you have had a
# look at the gaps.

# All the rows with NA in n.obs did not exist in the
# obs.stat, which means that they are 0 in this.
freq[is.na(freq$n.obs), "n.obs"] <- 0
freq$freq <- freq$n.obs/freq$n.days

# Creating new column with flash vs not flash
freq$flash <- ifelse(freq$period %in% c("1_1", "1_2"), TRUE, FALSE)

# Looking at the number of days the cameras have for each
# period
hist(freq$n.days, breaks = 100)  # Some have very few, those could be removed
range(freq$n.days)
write_rds(freq, "freq.rds")
# Setting 20 days as an arbitrary limit
freq <- freq[freq$n.days > 19, ]

# ---------------- Visualizing the data
sp = "rev"
plot(freq ~ as.factor(flash), freq[freq$validated_species %in% 
  sp, ])
plot(freq ~ as.factor(period), freq[freq$validated_species %in% 
  sp, ])

# ---------------- Showing example with ggplot
sp = c("rev", "gaupe", "raadyr")

ggplot(freq[freq$validated_species %in% sp, ]) + facet_wrap(~validated_species) + 
  geom_boxplot(aes(x = period, y = freq))

sp = c("rev", "gaupe", "raadyr", "elg", "hare", "grevling")
ggplot(freq[freq$validated_species %in% sp, ]) + facet_wrap(~validated_species) + 
  geom_boxplot(aes(x = flash, y = freq))


# I suggest that you try to find a nice way to visualize
# these patterns. From first inspection it does seem to be a
# large effect of the blits.

# Simple regression to see if having a flash during the given
# period influence the frequency of the focal species.
sp = "raadyr"

my.lm <- lm(freq ~ flash, freq[freq$validated_species %in% sp, 
  ])
summary(my.lm)

# Looking at activity patterns for cameras with and without flash ##################################################

library(overlap)

# The densityPlot functions requires the input to be in radians
obs$hour <- as.numeric(format(obs$datetime, "%H"))
obs$mins <- as.numeric(format(obs$datetime, "%M"))

obs$rad <- ((obs$hour * 60 + obs$mins)/(24 * 60)) * 2 * pi  # Converting hours to minutes and dividing the number of minutes by the total number of minutes during the day 
# and multiplyting with 2 pi
range(obs$rad)  # Good

# Activity pattern for all year and all cameras
sp = "rev"
densityPlot(obs[obs$validated_species %in% sp, ]$rad)

# Activity pattern for only flash
sp = "rev"
densityPlot(obs[obs$validated_species %in% sp & obs$flash == 
  TRUE, ]$rad)
densityPlot(obs[obs$validated_species %in% sp & obs$flash == 
  FALSE, ]$rad, add = TRUE, col = "red", lty = 2)
legend("bottomleft", legend = c("Flash", "No flash"), col = c("black", 
  "red"), lty = c(1, 2))



# Time since deployment ##################################################
effort$time.deploy <- NA

effort <- effort[order(effort$date), ]
rownames(effort) <- 1:nrow(effort)

for (i in unique(obs$loc)) {
  temp <- effort[effort$loc %in% i, ]
  my.rle <- rle(temp$period)
  if (length(my.rle$lengths) == 1) {
    temp$time.deploy <- c(0:(my.rle$lengths[1] - 1))
    effort[effort$loc %in% i, ]$time.deploy <- temp$time.deploy
  }
  if (length(my.rle$lengths) == 3) {
    temp$time.deploy <- c(0:(my.rle$lengths[1] - 1), 0:(my.rle$lengths[2] - 
      1), 0:(my.rle$lengths[3] - 1))
    effort[effort$loc %in% i, ]$time.deploy <- temp$time.deploy
  }
  if (length(my.rle$lengths) == 4) {
    temp$time.deploy <- c(0:(my.rle$lengths[1] - 1), 0:(my.rle$lengths[2] - 
      1), 0:(my.rle$lengths[3] - 1), 0:(my.rle$lengths[4] - 
      1))
    effort[effort$loc %in% i, ]$time.deploy <- temp$time.deploy
  }
}


torg <- obs %>% group_by(loc,date,validated_species) %>%
  summarise( diff = round(difftime(datetime, lag(datetime), units="mins"), 0) )
torg$diff[is.na(torg$diff)] <- 60
# obs.agg <- ddply(obs, .(loc, date, validated_species), summarise, 
#   n.obs = length(validated_species))

obs.agg <- torg %>% group_by(loc,date,validated_species) %>%
  summarise( n.obs = sum( diff > 15) ) # count all observations that were
# more than 15 mins later than the previous event



time.dep <- effort
temp <- time.dep
time.dep$validated_species <- unique(obs$validated_species)[1]

for (i in 2:length(unique(obs$validated_species))) {
  temp$validated_species <- unique(obs$validated_species)[i]
  time.dep <- rbind(time.dep, temp)
}

time.dep <- merge(time.dep, obs.agg, by = c("loc", "date", "validated_species"),
  all.x = TRUE)

time.dep[is.na(time.dep$n.obs), "n.obs"] <- 0

# Plotting average number of events against time since deploy
sp = "rev"
sp.freq <- ddply(time.dep[time.dep$validated_species %in% sp & 
  !time.dep$period %in% "Control", ], .(flash, time.deploy), 
  summarise, mean.p = mean(n.obs))

plot(mean.p ~ time.deploy, sp.freq[sp.freq$flash %in% 0, ], type = "l")
lines(mean.p ~ time.deploy, sp.freq[sp.freq$flash %in% 1, ], 
  col = "red")

# Generalized linear model to test if there is an effect of time since deployment
# (i.e. are the visitation rate declining with flash)
time.dep$loc <- as.factor(time.dep$loc)
my.glm <- glm(n.obs ~ time.deploy + as.factor(flash), time.dep[time.dep$validated_species %in% 
  sp & !time.dep$period %in% "Control", ], family = poisson)
summary(my.glm)  # Ignores the fact that there might be seasonal effects or potential effect of camera site. 
plot(n.obs ~ time.deploy, ylim=c(0,15), data=filter(time.dep, validated_species!="nothing"))
                                        
new.dat <- expand.grid(flash = c(0, 1), time.deploy = 0:100)
new.dat$fit <- predict(my.glm, newdata = new.dat, type = "response")

plot(fit ~ time.deploy, new.dat[new.dat$flash %in% 0, ], type="l", # flash = F
 ylim = c(0, 0.2), title(main = "Predicted mean observations by glm-fit"))
lines(fit ~ time.deploy, new.dat[new.dat$flash %in% 1, ], lty=2) # flash = TRUE
legend("bottomleft",legend=c("white LED","IR"),
       lty = c(2,1))

library(lme4)
# Testing with generalized linear mixed models
time.dep$month <- as.factor(format(time.dep$date, "%m")) # month-variable
time.dep$loc <- as.factor(time.dep$loc) # loc as factor
time.dep$time.deploy <- time.dep$time.deploy/10 # shortening / zooming out on deploy

                                 saveRDS(time.dep, "timedep.rds") #for å ta med meg datasett til glmm_in_process

                                 
# After saving --------------------------------------------------------------------

# Model with random effect of loc and month on the intercept
sp = "rev"
my.glmer <- glmer(n.obs ~ time.deploy + as.factor(flash) + (1 | loc) + (1 | month), 
  time.dep[time.dep$validated_species %in% sp & 
          !time.dep$period %in% "Control"
          & time.dep$time.deploy < 6.1 # = all tim.d less than 61 days
          ,], family = poisson) 
summary(my.glmer)
report::report(my.glmer) # veldig ryddig og fin output!

# Plotting the fixed effects of the model. I need to look up a way to do this correctly...
new.dat <- expand.grid(flash = c(0, 1), time.deploy = 0:6)
new.dat$fit <- exp(-3.490394 + 0.006966 * new.dat$time.deploy + 
  0.089652 * new.dat$flash)
new.dat$time.deploy <- new.dat$time.deploy * 10  # Rescaling back to the original scale

plot(fit ~ time.deploy, new.dat[new.dat$flash %in% 0, ], ylim = c(0, 
  0.2), type = "l")
lines(fit ~ time.deploy, new.dat[new.dat$flash %in% 1, ], ylim = c(0, 
  0.2), lty = 2)
legend("topleft",legend=c("white LED","IR"),
       lty = c(2,1))


# Nesting data by species -------------------------------------
by_sp <- time.dep %>% 
  mutate(species = validated_species) %>% 
  filter(species %in% c("raadyr", "rev", "hjort", "grevling", "elg", "gaupe")) %>% 
  group_by(species) %>% nest() #nest
# making a model function with glmer-model used on fox in Neris example above
sp_model <- function(df) {
  glmer(n.obs ~ time.deploy + as.factor(flash) + (1 | loc) + (1 | month), 
        data=df, family = poisson) # random effect arguments for loc and month
}
# applying model function on species data
      #models <- map(by_sp$data, sp_model) #map is an interation function

# same thing, but adding each model into my tibble
by_sp <- by_sp %>% 
  mutate(model = map(data, sp_model))
by_sp

# by_sp %>%    #attempt to retrieve report from the nested models | failed
#   mutate(glance = map(model, report::report)) %>% 
#   unnest(glance)

glance_sp <- by_sp %>%
  mutate(glance = map(model, broom.mixed::glance)) %>% 
  unnest(glance) %>%   select(-data, -model) # as .drop in unnest is deprecated
glance_sp %>% 
  arrange(logLik)
tidy_sp <- by_sp %>% 
  mutate(tidy = map(model, broom.mixed::tidy)) %>% 
  unnest(tidy) %>%   select(-data, -model) # as .drop in unnest is deprecated






# Vignettes in package ‘lme4’:
#   
# Theory                                Computational Methods (source, pdf)
# lmer                                  Fitting Linear Mixed-Effects Models using lme4 (source, pdf)
# lmerperf                              lmer Performance Tips (source, html)
# PLSvGLS                               PLS vs GLS for LMMs (source, pdf)
vignette("Theory")
# *****        ********       *********        ********        **************
# broom vignettes
# adding-tidiers                        Adding tidiers to broom (source, html)
# available-methods                     Available methods (source, html)
# broom_and_dplyr                       broom and dplyr (source, html)
# broom                                 Introduction to broom (source, html)
# kmeans                                kmeans with dplyr and broom (source, html)
# bootstrapping                         Tidy bootstrapping (source, html)
vignette("broom")



vignette("introduction_partial_residuals",package = "ggeffects")

# chisq test with tidymodels ----------------------------
#r chisq
library(tidymodels)
Control <- filter(obs, period == "Control")
obs %>% 
  #  filter(!species %in% c("null", "nothing","ukjent")) %>%
  filter(species %in% c("raadyr","rev","elg", "grevling", "hjort","gaupe")) %>% 
  na.omit() %>% 
  chisq_test(species ~ period)

obs_chisq <- obs %>%   filter(species %in% c("raadyr","rev","elg", "grevling", "hjort","gaupe")) %>% 
  select(species, flash) 
# calculate observed statistic
observed_indep_statistic <- obs_chisq %>%
  specify(species ~ flash) %>%
  calculate(stat = "Chisq")
# generate the null distribution using randomization
null_distribution_simulated <- obs_chisq %>%
  specify(species ~ flash) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>%
  calculate(stat = "Chisq")
# generate the null distribution by theoretical approximation
null_distribution_theoretical <- obs_chisq %>%
  specify(species ~ flash) %>%
  hypothesize(null = "independence") %>%
  # note that we skip the generation step here!
  calculate(stat = "Chisq")
# visualize the null distribution and test statistic!
null_distribution_simulated %>%
  visualize() + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")
# visualize the theoretical null distribution and test statistic!
obs_chisq %>%
  specify(species ~ flash) %>%
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical") + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")
# visualize both null distributions and the test statistic!
null_distribution_simulated %>%
  visualize(method = "both") + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")
# calculate the p value from the observed statistic and null distribution
p_value_independence <- null_distribution_simulated %>%
  get_p_value(obs_stat = observed_indep_statistic,
              direction = "greater")

p_value_independence

#Please be cautious in reporting a p-value of 0. This result is an approximation based on the number of `reps` chosen in the `generate()` step. See `?get_p_value()` for more information.


