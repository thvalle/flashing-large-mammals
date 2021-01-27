# This script prepare camera trap data and cox proportional
# hazard models (time to event analysis).  I will only focus
# on fixed effects models, but there is possible to run mixed
# effect cox proportional hazard models, but these have often
# convergvence issues.  Here we also assume that all the
# cameras have been active for all the year

# Some online resources for cox ph models:
# https://en.wikipedia.org/wiki/Proportional_hazards_model
# https://www.statsdirect.com/help/survival_analysis/cox_regression.htm
# http://www.sthda.com/english/wiki/cox-proportional-hazards-model
# http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Survival/BS704_Survival6.html

library(survival)
library(survminer)

# link to the manual for the survival package
# https://cran.r-project.org/web/packages/survival/survival.pdf


# Import data and data manipulation
# ###############################################################################

obs <- readRDS("Observations_prepared1.rds")

# Including the flashes
flash <- readRDS("exifdatablits.RDS")

# Giving them a loc ID
flash$loc <- as.numeric(gsub("([0-9]+).*$", "\\1", flash$Directory))
flash <- flash[, c("loc", "DateTimeOriginal", "InfraredIlluminator", 
  "Flash", "Model", "TriggerMode")]

# Fixing datetime
flash$datetime <- as.POSIXct(flash$DateTimeOriginal, "%Y:%m:%d %H:%M:%S", 
  tz = "UTC")
flash$date <- as.Date(flash$datetime, format = "%Y-%m%-%d")

# Fixing the different naming of the blitz locations
flash <- merge(flash, unique(obs[, c("loc", "Kam.nr. til blitskamera")]), 
  by = c("loc"), all.x = TRUE, all.y = FALSE)
flash <- flash[!is.na(flash$`Kam.nr. til blitskamera`), ]

# The response in a cox ph model is the time the 'individual'
# was monitored and whether or not the 'individual'
# experienced the event.  Need to set up the data in such a
# format

flash <- flash[flash$InfraredIlluminator == 1, ]  # Keeping only observations that actually flashed the animal

# Using the flash object to mark whether or not the animal
# was flashed
for (i in 1:nrow(flash)) {
  loc <- flash[i, "loc"]
  datetime <- flash[i, "datetime"]
  obs[obs$loc %in% loc & obs$datetime > (datetime - 10 * 60) & 
    obs$datetime < (datetime + 10 * 60), "flashed"] <- TRUE
}

obs[is.na(obs$flashed), "flashed"] <- FALSE
table(obs$flashed)  # This is weird, we lost a lot of the blitses, did not the camera standing there detect the animal at all? Needs to be checked out. 

# Recording the time until next observations after first
# detection
obs$ID <- paste0(obs$loc, "_", obs$validated_species, "_", obs$period)

obs <- obs[order(obs$datetime), ]
obs <- obs[order(obs$validated_species), ]
obs <- obs[order(obs$loc), ]
rownames(obs) <- 1:nrow(obs)

for (i in unique(obs$ID)) {
  row.n <- row.names(obs[obs$ID %in% i, ])
  if (length(row.n) > 0) {
    obs[row.n, "t.diff"] <- c(as.numeric(difftime(obs[row.n[-1], 
      "datetime"], obs[row.n[-length(row.n)], "datetime"], 
      units = "days")), NA)
  }
}


# Setting a correct end diff time for those without any
# second observation.
row.n <- row.names(obs[is.na(obs$t.diff), ])
effort <- readRDS("Effort_prepared.rds")

for (i in row.n) {
  period <- obs[i, "period"]
  loc <- obs[i, "loc"]
  max.time <- max(effort[effort$loc %in% loc & effort$period %in% 
    period, "date"])  # Setting the latest datetime as the end of the period, this account for when the camera was active
  obs[i, ]$t.diff <- difftime(max.time, obs[i, "datetime"], 
    units = "days")
  obs[i, "event"] <- FALSE  # These were right censored
}

obs[is.na(obs$event), "event"] <- TRUE

table(obs[obs$validated_species %in% "rev", ]$event)



# Inspecting the data
# ##################################################################################################

# Inspecting the number of observations during the different
# treatments
sp = "rev"
table(obs[obs$validated_species %in% sp, ]$flash)

# And periods
table(obs[obs$validated_species %in% sp, ]$period)

# Histogram of time to new detection in a period with flash
hist(obs[obs$flash == TRUE & obs$validated_species %in% sp & 
  !obs$period %in% "Control" & obs$event == TRUE, ]$t.diff)
# Histogram of time to new detection in a period without
# flash
hist(obs[obs$flash == FALSE & obs$validated_species %in% sp & 
  !obs$period %in% "Control" & obs$event == TRUE, ]$t.diff)

# Histogram of time to new detection when animal was flashed
hist(obs[obs$flashed == TRUE & obs$validated_species %in% sp & 
  !obs$period %in% "Control" & obs$event == TRUE, ]$t.diff)
# Histogram of time to new detection when animal was not
# flashed
hist(obs[obs$flashed == FALSE & obs$validated_species %in% sp & 
  !obs$period %in% "Control" & obs$event == TRUE, ]$t.diff)

saveRDS(obs, "obs_surv_prepared.rds")
# Analysis - Survival
# ##########################################################################################
obs <- readRDS("obs_surv_prepared.rds")

unique(obs$validated_species)
obs <- obs[obs$t.diff >= 0, ]
obs$flashed <- as.factor(obs$flashed)
obs$flash <- as.factor(obs$flash)
sp = "rev"

mod0 <- coxph(Surv(t.diff, event, type = "right") ~ flashed, 
  data = obs[obs$validated_species %in% sp & !obs$period %in% 
    "Control", ])
summary(mod0)
# Keep in mind that positive values means higher risk of
# 'dying' or being detected again, and negative values means
# lower risk of 'dying' or being detected again In this case
# the positive sign for flashed means that when the animal
# was flashed the time to new detection is shorter than when
# the animal was not flashed.  However, the effect is not
# significant.

# Survival probabilty against time.
fit <- survfit(Surv(t.diff, event, type = "right") ~ flashed, 
  data = obs[obs$validated_species %in% sp & !obs$period %in% 
    "Control", ])
ggsurvplot(fit, data = obs[obs$validated_species %in% sp & !obs$period %in% 
  "Control", ])
# You see the same pattern here. The blue (flashed) line is
# generally lower than the red (not flashed)

# A nicer way to visualize the coffecient estimate and the
# confidence interval. Since the interval overlap 1 (hazard
# ratio of 1) the coeffecient estimate is not significant.
ggforest(mod0, data = obs[obs$validated_species %in% sp & !obs$period %in% 
  "Control", ])

# Diagnostics - Are we violating the proportional hazard
# assumption?

# Test the proportional hazards assumption
d.mod0 <- cox.zph(mod0)  # Non-significant --> we can assume proportional hazards.
d.mod0

# Can also look at Schoenfeld residuals, there should be no
# pattern with time
ggcoxzph(d.mod0)

# Analysis - Survival. Including spatial covariates
# #####################################################################################
covs <- readRDS("CTloc_covs.rds")
class(covs)

covs <- as.data.frame(covs)  # Chaning class to data.fram and not a sf data.frame

obs <- merge(obs, covs, by.x = "loc", by.y = "LokalitetID", all.x = TRUE, 
  all.y = FALSE)

# Checking if there is any NAs in the covariates
lapply(1:ncol(obs), function(x) {
  any(is.na(obs[, x]))
})
colnames(obs)[8:10]  # Not any in the covariates, but in some species... 

# Fitting model with spatial covariates
sp = "rev"

# Example with distance to forestroads and houses
mod1 <- coxph(Surv(t.diff, event, type = "right") ~ flashed + 
  house_d2 + forestroad_d2, data = obs[obs$validated_species %in% 
  sp & !obs$period %in% "Control", ])
summary(mod1)  # Be careful when interpreting distance to features, a negative sign in this case means that 

ggforest(mod1, data = obs[obs$validated_species %in% sp & !obs$period %in% 
  "Control", ])

# Trying with a log transformation on the covariates, this
# makes the effect deviate with the distance from the
# feature.  Try to plot the log of distance to feature agains
# the distance and you see why. e.g. plot(1:1000,log(1:1000))

any(is.infinite(log(obs$forestroad_d2)))  # This means that the log transformation will create some infinite values
log(seq(0, 1, 0.1))  # Here you see why, log(0)=-Inf

# This transformation is the same as saying that the camera
# trap is 1 m of the road instead of on it.
obs$house_d2_ln <- ifelse(obs$house_d2 > 0, log(obs$house_d2), 
  0)
obs$forestroad_d2_ln <- ifelse(obs$forestroad_d2 > 0, log(obs$forestroad_d2), 
  0)

mod2 <- coxph(Surv(t.diff, event, type = "right") ~ flashed + 
  house_d2_ln + forestroad_d2_ln, data = obs[obs$validated_species %in% 
  sp & !obs$period %in% "Control", ])
summary(mod2)

ggforest(mod2, data = obs[obs$validated_species %in% sp & !obs$period %in% 
  "Control", ])


# Test the proportional hazards assumption
d.mod2 <- cox.zph(mod2)  # Non-significant --> we can assume proportional hazards.
d.mod2

# Can also look at Schoenfeld residuals, there should be no
# pattern with time
ggcoxzph(d.mod2)

# Analysis - Generalized linear models (if it is hard to wrap
# your head around the survival analysis)
# ##################################

sp = "rev"

# Witout any random effects
my.glm <- glm(round(t.diff) ~ flashed + house_d2_ln + forestroad_d2_ln, 
  data = obs[obs$validated_species %in% sp & !obs$period %in% 
    "Control", ], family = "poisson")
summary(my.glm)





