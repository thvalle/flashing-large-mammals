# This script prepare camera trap data and cox proportional hazard models (time to event analysis).
# I will only focus on fixed effects models, but there is possible to run mixed effect cox proportional hazard models,
# but these have often convergvence issues. 
# Here we also assume that all the cameras have been active for all the year

# Some online resources for cox ph models:
# https://en.wikipedia.org/wiki/Proportional_hazards_model 
# https://www.statsdirect.com/help/survival_analysis/cox_regression.htm
# http://www.sthda.com/english/wiki/cox-proportional-hazards-model
# http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Survival/BS704_Survival6.html
# 

rm(list=ls()) # Clear workspace

library(survival)
library(survminer)

# link to the manual for the survival package
# https://cran.r-project.org/web/packages/survival/survival.pdf

getwd()

#########################################################################################################################
# Import data and data manipulation
#########################################################################################################################
# Import data
obs<-readRDS("Data_20200818.rds")
colnames(obs)[1]<-"loc"
colnames(obs)
# Including the flashes
flash<-readRDS("exifdatablits.RDS")

# Giving them a loc ID
flash$loc<-as.numeric(gsub("([0-9]+).*$", "\\1", flash$Directory))
flash<-flash[,c("loc", "DateTimeOriginal", "InfraredIlluminator", "Flash", "Model", "TriggerMode")]

# Fixing datetime
flash$datetime<-as.POSIXct(flash$DateTimeOriginal, "%Y:%m:%d %H:%M:%S", tz="UTC")
flash$date<-as.Date(flash$datetime, format="%Y-%m%-%d")

# Fixing the different naming of the blitz locations
flash<-merge(flash, unique(obs[,c("loc","Kam.nr. til blitskamera")]), by=c("loc"), all.x=TRUE, all.y=FALSE)
flash<-flash[!is.na(flash$`Kam.nr. til blitskamera`),] 

# Including the sorted species of the blitz locations
flash<-merge(flash, obs, by.x=c("Kam.nr. til blitskamera", "datetime"),by.y=c("loc", "datetime"),all.x=TRUE, all.y=FALSE)
flash<-flash[!duplicated(flash$image_id),]

# Expanding a grid with all possible combinations of dates and location ID for the flash camera for the whole study period, this is used to seperate into periods
loc_dates<-expand.grid(date=seq(min(flash$date), max(flash$date), "days"), loc=unique(flash$loc))
uni.date<-unique(flash[flash$TriggerMode%in%"T",c("loc","date")])

loc_dates$flash<-0

# Marking whether the camera had a blits there or not. 
for(i in 1:nrow(uni.date)){
  loc_dates[loc_dates$date%in%uni.date[i,"date"] & loc_dates$loc%in%uni.date[i,"loc"],"flash"]<-1
}

# Marking the different periods
flash$period<-NA

for(i in unique(loc_dates$loc)){
  my.rle<-rle(loc_dates[loc_dates$loc%in%i,"flash"])
  row.nr<-rownames(loc_dates[loc_dates$loc%in%i,])
  loc_dates[row.nr[1:my.rle$lengths[1]],"period"]<-paste0(my.rle$values[1], "_", 1)
  loc_dates[row.nr[(my.rle$lengths[1]+1):sum(my.rle$lengths[1:2])],"period"]<-paste0(my.rle$values[2], "_", 1)
  loc_dates[row.nr[(sum(my.rle$lengths[1:2])+1):sum(my.rle$lengths[1:3])],"period"]<-paste0(my.rle$values[3], "_", 2)
  if(length(my.rle$lengths)>3){
    loc_dates[row.nr[(sum(my.rle$lengths[1:3])+1):sum(my.rle$lengths[1:4])],"period"]<-paste0(my.rle$values[4], "_", 2)
  }
  if(length(my.rle$lengths)>4){
    loc_dates[row.nr[(sum(my.rle$lengths[1:4])+1):sum(my.rle$lengths[1:5])],"period"]<-paste0(my.rle$values[5], "_", 3)
  }
}

# 0_1 indicates first period without blits, 0_2 the second and so on
# 1_1 indicates the first period with blits, ..... 

# Check out 257, seems to be missing photos.
table(loc_dates$period)

# Now, filtering out the observations inside the study period based on loc_dates

obs$date<-as.Date(obs$datetime, "%Y-%m%-%d", tz="UTC")
obs<-obs[obs$date%in%loc_dates$date,]
obs<-obs[obs$loc<1500,] # Removing the blits cameras, they are stored in the object flash
obs<-obs[!duplicated(obs$timeserie_id),]

# Transferring the periods to the observations
obs<-merge(obs, loc_dates, by=c("loc", "date"), all.x=TRUE, all.y=FALSE)
obs[is.na(obs$flash),"flash"]<-0 # Setting the NAs to 0, all of these are the control
obs[is.na(obs$period),"period"]<-"Control"

# Inspecting the number of cameras in the different periods/treatments
unique(obs[obs$period%in%"Control","loc"])


# Inspecting the number of observations during the different treatments
sp="rev" #observations of fox
table(obs[obs$validated_species%in%sp,]$flash)

# And periods
table(obs[obs$validated_species%in%sp,]$period)

# The response in a cox ph model is the time the "individual" was monitored and whether or not the "individual" experienced the event. 
# Need to set up the data in such a format

flash<-flash[flash$InfraredIlluminator==1,] # Keeping only observations that actually flashed the animal
flash<-flash[!flash$validated_species%in%"nothing",] 
flash<-flash[!duplicated(flash$timeserie_id),] # This is all the flashes of the blits cameras. 

# Using the flash object to mark whether or not the animal was flashed
for(i in 1:nrow(flash)){
  loc<-flash[i,"loc"]
  datetime<-flash[i,"datetime"]
  obs[obs$loc%in%loc & obs$datetime>(datetime-10*60) & obs$datetime<(datetime+10*60), "flashed"]<-TRUE
}

obs[is.na(obs$flashed),"flashed"]<-FALSE
table(obs$flashed) # This is weird, we lost a lot of the blitses, did not the camera standing there detect the animal at all? Needs to be checked out. 

# Recording the time until next observations after first detection
obs$ID<-paste0(obs$loc,"_",obs$validated_species,"_",obs$period)

obs<-obs[order(obs$datetime),]
obs<-obs[order(obs$validated_species),]
obs<-obs[order(obs$loc),]
rownames(obs)<-1:nrow(obs)

for(i in unique(obs$ID)){
  row.n<-row.names(obs[obs$ID%in%i,])
  if(length(row.n)>0){
    obs[row.n,"t.diff"]<-c(as.numeric(difftime(obs[row.n[-1],"datetime"],obs[row.n[-length(row.n)],"datetime"], units="days")),NA) 
  }
}

# Setting a correct end diff time for those without any second observation. 
row.n<-row.names(obs[is.na(obs$t.diff),])

for(i in row.n){
  period<-obs[i,"period"]
  loc<-obs[i,"loc"]
  max.time<-max(obs[obs$loc%in%loc & obs$period%in%period,"datetime"])
  obs[i,]$t.diff<-difftime(max.time,obs[i,"datetime"], units="days")
  obs[i,"event"]<-FALSE # These were right censored
}

obs[is.na(obs$event),"event"]<-TRUE

#########################################################################################################################
# Analysis 
#########################################################################################################################
colnames(obs)

obs$flashed<-as.factor(obs$flashed)
obs$flash<-as.factor(obs$flash)
sp="rev"

mod1<-coxph(Surv(obs[obs$validated_species%in%sp,]$t.diff, obs[obs$validated_species%in%sp,]$event, type="right")~flash, data=obs[obs$validated_species%in%sp,])
summary(mod1)



# Survival probabilty against time. 
survfit(mod1, data = obs)
fit$call$formula <- formula   #fikser problem med "Error: object of type 'symbol' is not subsettable"
ggsurvplot(survfit(mod1), data = obs)

?ggsurvplot

# Diagnostics - Are we violating the proportional hazard assumption?

# Test the proportional hazards assumption
d.mod1<-cox.zph(mod1) # Non-significant --> we can assume proportional hazards.
d.mod1

# Can also look at Schoenfeld residuals, there should be no pattern with time
ggcoxzph(d.mod1)


#Torgeirs forsøk
sp <- "gaupe"

fit <- survfit(Surv(t.diff, event, type = "right")~flash, data=obs[obs$validated_species%in%sp,])  #trenger berre å lage subsett i data,
                                                                                            #ikkje for kvar variabel
ggsurvplot(fit, data = obs)
#må fjerne kontroll-kamera/ vertfall lengde av datasett


mod2<-coxph(Surv(t.diff, event, type = "right")~flash, data=obs[obs$validated_species%in%sp,])
d.mod2<-cox.zph(mod2)
d.mod2

modTot<- coxph(Surv(t.diff, event, type = "right")~flash, data=obs)
d.modTot<- cox.zph(modTot)

table(obs$validated_species)
names(table(obs$validated_species))
LMammals <- names(table(obs$validated_species)[-c(1,2,3,4,6,11,14,15,17,18,19,22,23,24,25,26,27,28,29)])
LMammals

sp<-LMammals
fit <- survfit(Surv(t.diff, event, type = "right")~flash, data=obs[obs$validated_species%in%sp,])
ggsurvplot(fit, data = obs)
par(mfrow=c(3,4))


for(i in 1:10){
  sp<-LMammals[i,1]
  mod.sp<-coxph(Surv(t.diff, event, type = "right")~flash, data=obs[obs$validated_species%in%sp,])
  d.mod.sp <- cox.zph(mod.sp)
  print(d.mod.sp)
  }

#mod.sp<-coxph(Surv(t.diff, event, type = "right")~flash, data=obs[obs$validated_species%in%sp,])

#cox.zph(fit

###################
#Neri tips
###################
#Sjå korleis arter fordeler seg på dei ulike periodene med og utan blits

# ->  n rev utan, med, utan2, med2, utan3
# -> frekvens i perioder med blits == frekvens utan blits?
#   -> enkeltkamera mot blits -> kva for ein art blei blitsa? blei han blitsa mykje? 3månaders perioder. har det nokon effekt?


#FUNKSJONER Å SJEKKE UT
#dapply, lapply, piper,   

#Neri skal sjekke ut $flashed, noko var feil der