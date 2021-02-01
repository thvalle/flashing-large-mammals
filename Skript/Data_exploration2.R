# This script explores the data and make some graphical presentations


library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
setwd()
# Import data
effort<-readRDS("Effort_prepared.rds")
obs<-readRDS("Observations_prepared1.rds")

unique(effort$loc)
unique(obs$loc)
# Now, we aggregate the data by camera trap and period for the effort to see how many days the camera was active
effort.stat <- ddply(effort, .(loc, period), summarise,
                   n.days=length(unique(date)))

colnames(obs)
# Then, we aggregate the observations of the different species based on the obs object
obs.stat <- ddply(obs, .(loc, period, validated_species), summarise,
                n.obs=length(unique(datetime))) # Change to to datetime here since we might have several observation during a single day

# Not all possible combinations of camera traps, camera traps and species will be in the obs.stat, since not all species are caputred on a single camera.
# Extending the effort.stat frame to contain all possible detection on every camera trap location
temp<-effort.stat
for(i in 2:length(unique(obs$validated_species))){
  effort.stat<-rbind(effort.stat, temp)
}

effort.stat$validated_species<-rep(as.character(unique(obs$validated_species)), each=nrow(temp)) # assigning the validated_species 
# View(effort.stat) # Looks good

freq<-merge(obs.stat, effort.stat, by=c("loc", "period", "validated_species"), all=TRUE)
# Merging issue here should disapper after you have had a look at the gaps.

# All the rows with NA in n.obs did not exist in the obs.stat, which means that they are 0 in this. 
freq[is.na(freq$n.obs),"n.obs"]<-0
#freq[freq$validated_species == "nothing","n.obs"]<-0
freq[freq$validated_species == "nothing",]
freq$freq<-freq$n.obs/freq$n.days



# Creating new column with flash vs not flash
freq$flash<-ifelse(freq$period%in%c("1_1","1_2"), TRUE, FALSE)

unique(freq$loc)
                                                                    # na.omit(freq[is.na(freq$loc),])
# Looking at the number of days the cameras have for each period
hist(freq$n.days, breaks=100) # Some have very few, those could be removed
range(freq$n.days) #NA values disrupts 

# Setting 20 days as an arbitrary limit
freq<-freq[freq$n.days>19,]

# ---------------- Visualizing the data
sp="rev"
plot(freq~as.factor(period), freq[freq$validated_species%in%sp,])
plot(freq~as.factor(flash), freq[freq$validated_species%in%sp,])

# ---------------- Showing example with ggplot
library(ggplot2)
sp=c("rev", "gaupe", "raadyr")

ggplot(freq[freq$validated_species%in%sp,]) + 
  facet_wrap(~validated_species) +
  geom_boxplot(aes(x=period, y=freq))

sp=c("rev", "gaupe", "raadyr", "elg", "hare", "grevling")
ggplot(freq[freq$validated_species%in%sp,]) + 
  facet_wrap(~validated_species) +
  geom_boxplot(aes(x=flash, y=freq))


# I suggest that you try to find a nice way to visualize these patterns. From first inspection it does seem to be a large effect of the blits. 

sp="raadyr"

my.lm<-lm(freq~flash, freq[freq$validated_species%in%sp,])
summary(my.lm)

# Looking at activity patterns for cameras with and without flash #################################################################
library(overlap)

# The densityPlot functions requires the input to be in radians
obs$hour<-as.numeric(format(obs$datetime, "%H"))
obs$mins<-as.numeric(format(obs$datetime, "%M"))

obs$rad<-((obs$hour*60+obs$mins)/(24*60))*2*pi # Converting hours to minutes and dividing the number of minutes by the total number of minutes during the day 
# and multiplyting with 2 pi
range(obs$rad) # Good

# Activity pattern for all year and all cameras
sp="rev"
densityPlot(obs[obs$validated_species%in%sp,]$rad)

# Activity pattern for only flash
sp="rev"
densityPlot(obs[obs$validated_species%in%sp & obs$flash==TRUE,]$rad)
densityPlot(obs[obs$validated_species%in%sp & obs$flash==FALSE,]$rad, add=TRUE, col="red", lty=2)
legend("bottomleft", legend=c("Flash", "No flash"), col=c("black", "red"), lty=c(1,2))



# Time since deployment ##############################################################################
effort$time.deploy<-NA

effort<-effort[order(effort$date),]
rownames(effort)<-1:nrow(effort)

for(i in unique(obs$loc)){
  temp<-effort[effort$loc%in%i,]
  my.rle<-rle(temp$period)
  if(length(my.rle$lengths)==1){
    temp$time.deploy<-c(0:(my.rle$lengths[1]-1))
    effort[effort$loc%in%i,]$time.deploy<-temp$time.deploy
  }
  if(length(my.rle$lengths)==3){
    temp$time.deploy<-c(0:(my.rle$lengths[1]-1), 0:(my.rle$lengths[2]-1), 0:(my.rle$lengths[3]-1))
    effort[effort$loc%in%i,]$time.deploy<-temp$time.deploy
  }
  if(length(my.rle$lengths)==4){
    temp$time.deploy<-c(0:(my.rle$lengths[1]-1), 0:(my.rle$lengths[2]-1), 0:(my.rle$lengths[3]-1), 0:(my.rle$lengths[4]-1))
    effort[effort$loc%in%i,]$time.deploy<-temp$time.deploy
  }
}

obs.agg<-ddply(obs, .(loc,date,validated_species), summarise,
               n.obs=length(validated_species))

time.dep<-effort
temp<-time.dep
time.dep$validated_species<-unique(obs$validated_species)[1]

for(i in 2:length(unique(obs$validated_species))){
  temp$validated_species<-unique(obs$validated_species)[i]
  time.dep<-rbind(time.dep, temp)
}

time.dep<-merge(time.dep, obs.agg, by=c("loc","date","validated_species"), all.x=TRUE)

time.dep[is.na(time.dep$n.obs),"n.obs"]<-0

# Plotting average number of events against time since deploy -------------------
sp="raadyr"
sp.freq<-ddply(time.dep[time.dep$validated_species%in%sp & !time.dep$period%in%"Control",], 
               .(flash, time.deploy), summarise,
                mean.p=mean(n.obs))

plot(mean.p ~ time.deploy, sp.freq[sp.freq$flash %in% 0,], type="l")
lines(mean.p ~ time.deploy, sp.freq[sp.freq$flash %in% 1,], col="red")


# Generalized linear model to test if there is an effect of time since deployment 
# (i.e. are the visitation rate declining with flash)
time.dep$loc<-as.factor(time.dep$loc)
my.glm <- glm(n.obs ~ time.deploy + as.factor(flash), 
            time.dep[time.dep$validated_species %in% sp & !time.dep$period %in% "Control",], family = poisson)
summary(my.glm) # Ignores the fact that there might be seasonal effects or potential effect of camera site. 

new.dat <- expand.grid(flash = c(0,1), time.deploy = 0:100)
new.dat$fit <- predict(my.glm, newdata = new.dat, type = "response")

plot(fit ~ time.deploy, new.dat[new.dat$flash %in% 0,], ylim=c(0,0.2), type = "l",
     ylab = "Events per day", xlab = "Days after camera operation")
lines(fit ~ time.deploy, new.dat[new.dat$flash %in% 1,], col="red")
legend("bottomleft", legend = c("white LED flash", "IR flash"), col = c("red","black"), lty = c(1, 1))


library(lme4)
# Testing with generalized linear mixed models
time.dep$month <- as.factor(format(time.dep$date, "%m")) # retrieve months of the date column
time.dep$loc <- as.factor(time.dep$loc)                 # make loc into factor
time.dep$time.deploy <- time.dep$time.deploy / 10       # make time.deploy more concentrated (1/10 in size)

# Model with random effect of loc and month on the intercept. 
my.glmer<-glmer(n.obs~time.deploy+as.factor(flash)+(1|loc)+(1|month), 
                time.dep[time.dep$validated_species%in%sp & !time.dep$period%in%"Control"& time.dep$time.deploy<6.1,],
                family = poisson)
summary(my.glmer)

# Plotting the fixed effects of the model. I need to look up a way to do this correctly... 
new.dat<-expand.grid(flash=c(0,1), time.deploy=0:10)
new.dat$fit<-exp(-3.44897+0.05823*new.dat$time.deploy+0.14379*new.dat$flash)
new.dat$time.deploy<-new.dat$time.deploy*10 # Rescaling back to the original scale

plot(fit~time.deploy, new.dat[new.dat$flash%in%0,], ylim=c(0,0.2), type="l",
     ylab = "Events per day", xlab = "Days after camera operation")
lines(fit~time.deploy, new.dat[new.dat$flash%in%1,], ylim=c(0,0.2), col="red")
legend("bottomleft", legend = c("white LED flash", "IR flash"), col = c("red","black"), lty = c(1, 1))


# tidymodels attempt - Torgeir -------------------------------------

covs <- readRDS("CTloc_covs.rds") %>% 
        #remove geometry, cause of object-type,
        # lon, build_dens and field_d2 cause of correlations
  select(!c(3,4,8,12))%>%
  as.data.frame() %>% # Chaning class to data.fram and not a sf data.frame
  mutate( LokalitetID = factor(LokalitetID))
class(covs); names(covs)

set.seed(123)
flash_data <- 
  time.dep %>% 
  # Convert the loc and flash to factor, make a numeric date-col
  # convert n.obs into factor with many breaks,
  # because outcome=factor is required for fitting
  mutate( loc = factor(loc),
          flash = factor(flash), 
          numeric_date = as.numeric(date),
          month = as.factor(format(time.dep$date, "%m"))
          #n.obs = factor(n.obs, levels = 0:15, ordered = T)
          ) %>%
  # Include the covs data
  inner_join(covs, by = c("loc" = "LokalitetID")) %>% 
  rename(species = validated_species) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)
names(flash_data)

fjern <- flash_data %>% #making a vector to remove
  group_by(species) %>% #all "species" with very few sightings
  summarise(sum = sum(n.obs)) %>% 
  filter(sum<40) %>% distinct(species)
flash_data <- flash_data %>% # removing them, and "unknown",
  filter(!species %in% fjern$species, # null, and nothing
         !species %in% c("ukjent", "null", "nothing"))
flash_data %>% 
  distinct(species) # 17 "species" left
unique(flash_data$n.obs)

flash_data %>% 
  skimr::skim(n.obs, species)
library(tidyverse);library(tidymodels)
library(modelr)
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
{set.seed(555)
# Put 3/4 of the data into the training set 
data_split <- initial_split(flash_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

test_data %>% 
  distinct(species) %>% 
  anti_join(train_data) 
#no species that are only in the test set.
}

by_sp <- flash_data %>% 
  group_by(species) %>%
# although I don't understand why it is important to remove control
  filter(!period == "Control") %>% 
  nest()
by_sp
# 
# # don't understand (1|loc)+(1|month), and it doesn't work
# sp_model <- function(df) {
#   glm(n.obs ~ time.deploy + flash + loc + month,
#       data = df)
# }
# # Model with random effect of loc and month on the intercept. 
# my.glmer<-glmer(n.obs~time.deploy+as.factor(flash)+(1|loc)+(1|month), 
#                 time.dep[time.dep$validated_species%in%sp & !time.dep$period%in%"Control"& time.dep$time.deploy<6.1,],
#                 family = poisson)
# summary(my.glmer)
# 
# 
sp_model <- function(df) {
  lm(n.obs ~ time.deploy + flash, #+ loc + month,
      data = df)
}

models <- map(by_sp$data, sp_model)

by_sp <- by_sp %>% 
  mutate(model = map(data, sp_model))
by_sp %>% 
  arrange(species)
# unnesting
#   Obs!   There are no residuals in GLM !!
by_sp <-  by_sp %>%
  mutate(
    resids = map2(data, model, add_residuals) #modelr needed!
    )
by_sp

              # resids <- unnest(by_sp, resids)
              # resids                            # Computer freezes!

#resids %>% 
  ggplot(aes(n.obs, resid)) +
  geom_line(aes(group = species), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)


  
# Model with random effect of loc and month on the intercept. 
my.glmer<-glmer(n.obs~time.deploy+as.factor(flash)+(1|loc)+(1|month), 
                time.dep[time.dep$validated_species%in%sp & !time.dep$period%in%"Control"& time.dep$time.deploy<6.1,],
                family = poisson)
summary(my.glmer)

# Plotting the fixed effects of the model. I need to look up a way to do this correctly... 
new.dat <- expand.grid(flash = c(0,1), time.deploy = 0:10)
new.dat$fit <- exp( - 3.44897 + 0.05823 * new.dat$time.deploy +
                      0.14379 * new.dat$flash)
# Rescaling back to the original scale
new.dat$time.deploy <- new.dat$time.deploy * 10

plot(fit~time.deploy, new.dat[new.dat$flash%in%0,],
     ylim=c(0,0.2), type="l")
lines(fit~time.deploy, new.dat[new.dat$flash%in%1,], 
      ylim=c(0,0.2), col="red")


# Infer --------------------------------------------------------
library(tidymodels) # Includes the infer package
sp=c("rev", "gaupe", "raadyr", "elg", "gaupe", "grevling")
set.seed(313)
sp_pval <- tibble(.rows = 2)
  for (i in sp) {
  flash_data_sp <- filter(flash_data, species %in% sp, !period %in% "Control")
  d_hat <- flash_data_sp %>% 
    specify(n.obs ~ flash) %>%
    calculate(stat = "diff in means", order = c(1, 0))
  # Then, generating the null distribution,
  null_distn <- flash_data_sp %>%
    specify(n.obs ~ flash) %>%
    hypothesize(null = "independence") %>% 
    generate(reps = 1000, type = "permute") %>% 
    calculate(stat = "diff in means", order = c(1, 0))
  # Visualizing the observed statistic alongside the null distribution,
  visualise(null_distn) + labs(tag = sp) +
    shade_p_value(obs_stat = d_hat, direction = "two-sided")
  sp_pval[[i]] <- null_distn %>%
    get_p_value(obs_stat = d_hat, direction = "two-sided")
}
sp_pval

flash_data_sp <- filter(flash_data, species %in% sp, !period %in% "Control")
d_hat <- flash_data_sp %>% 
  specify(n.obs ~ flash) %>%
  calculate(stat = "diff in means", order = c(1, 0))
# Then, generating the null distribution,
null_distn <- flash_data_sp %>%
  specify(n.obs ~ flash) %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c(1, 0))
# Visualizing the observed statistic alongside the null distribution,
visualise(null_distn) + labs(tag = sp) +
  shade_p_value(obs_stat = d_hat, direction = "two-sided")
null_distn %>%
  get_p_value(obs_stat = d_hat, direction = "two-sided")
# elg p-value   : 0.54
# raadyr --||-- : 0.388
# grevling -||- : 0.142
# rev   --||--  : 0.05
# hjort --||--  : 0.014
# gaupe --||--  : 0 #please be cautious in reporting a p-value of 0

help(package = "infer")













# didn't manage :( -------------------------------
sp <- "raadyr"
train_data_sp <- filter(train_data, species %in% sp)
# Make recipe
flash_rec <- 
  recipe(n.obs ~ time.deploy + flash,
         data = train_data_sp) %>% 
  #extracting month and day of week as predictors
  # step_date(date, features = c("dow", "month")) %>% 
  # update_role(date, new_role = "ID") %>% # keeping date as an ID
  step_normalize(all_numeric()) %>% 
  # step_dummy(all_nominal(), -all_outcomes()) %>%  #making dummy variables
  step_zv(all_predictors()) #removing any predictors containing only one value

summary(flash_rec)
# Make model
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")
# Make workflow
flash_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flash_rec)
flash_wflow

flash_fit <- 
  flash_wflow %>% 
  fit(data = train_data_sp)

flash_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

test_data_sp <- filter(test_data, species %in% sp)
predict(flash_fit, test_data_sp)

flash_pred <- 
  predict(flash_fit, test_data_sp, type = "prob") %>% 
  bind_cols(test_data_sp %>% select(n.obs, time.deploy, flash, loc)) 

# The data look like: 
flash_pred
# ROC curve
flash_pred %>%
  roc_curve(truth= n.obs, .pred_0) %>% autoplot()






#step_geodist() 
#step_
# PLS part
set.seed(57343)
folds <- vfold_cv(flash_data, repeats = 10)

folds <- 
  folds %>%
  mutate(recipes = map(splits, prepper, recipe = norm_rec))



# Make model
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")
# Make workflow
flash_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flash_rec)
flash_wflow

flash_fit <- 
  flash_wflow %>% 
  fit(data = train_data)

flash_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

