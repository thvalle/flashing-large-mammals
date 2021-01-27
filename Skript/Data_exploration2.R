# This script explores the data and make some graphical presentations


library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)

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
View(effort.stat) # Looks good

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

# Plotting average number of events against time since deploy
sp="rev"
sp.freq<-ddply(time.dep[time.dep$validated_species%in%sp & !time.dep$period%in%"Control",], .(flash, time.deploy), summarise,
      mean.p=mean(n.obs))

plot(mean.p ~ time.deploy, sp.freq[sp.freq$flash %in% 0,], type="l")
lines(mean.p ~ time.deploy, sp.freq[sp.freq$flash %in% 1,], col="red")

#                                      sp.freq <- sp.freq[!sp.freq$mean.p>.4,] # remove impossible frequency values
#                                      sp.freq[sp.freq$mean.p>.4,] # observations removed
# TODO                            In addition there are some NA's in the $loc that I do not understand  

# Generalized linear model to test if there is an effect of time since deployment (i.e. are the visitation rate declining with flash)
time.dep$loc<-as.factor(time.dep$loc)
my.glm<-glm(n.obs~time.deploy+as.factor(flash), time.dep[time.dep$validated_species%in%sp & !time.dep$period%in%"Control",], family = poisson)
summary(my.glm) # Ignores the fact that there might be seasonal effects or potential effect of camera site. 

new.dat<-expand.grid(flash=c(0,1), time.deploy=0:100)
new.dat$fit<-predict(my.glm, newdata=new.dat, type="response")

plot(fit~time.deploy, new.dat[new.dat$flash%in%0,], ylim=c(0,0.2))
lines(fit~time.deploy, new.dat[new.dat$flash%in%1,])

library(lme4)
# Testing with generalized linear mixed models
time.dep$month<-as.factor(format(time.dep$date, "%m"))
time.dep$loc<-as.factor(time.dep$loc)
time.dep$time.deploy<-time.dep$time.deploy/10

# Model with random effect of loc and month on the intercept. 
my.glmer<-glmer(n.obs~time.deploy+as.factor(flash)+(1|loc)+(1|month), time.dep[time.dep$validated_species%in%sp & !time.dep$period%in%"Control"& time.dep$time.deploy<6.1,], family = poisson)
summary(my.glmer)

# Plotting the fixed effects of the model. I need to look up a way to do this correctly... 
new.dat<-expand.grid(flash=c(0,1), time.deploy=0:10)
new.dat$fit<-exp(-3.44897+0.05823*new.dat$time.deploy+0.14379*new.dat$flash)
new.dat$time.deploy<-new.dat$time.deploy*10 # Rescaling back to the original scale

plot(fit~time.deploy, new.dat[new.dat$flash%in%0,], ylim=c(0,0.2), type="l")
lines(fit~time.deploy, new.dat[new.dat$flash%in%1,], ylim=c(0,0.2), col="red")

