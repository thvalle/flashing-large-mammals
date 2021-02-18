# This script divide the study period into different treatments period, based on whether it was a blits there or not
# 

library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Import data and divide into treatments #################################################################

# Import data
obs<-readRDS("Data_20200818.rds")

colnames(obs)[1]<-"loc"

            # Removing some cameras not meeting criteria
            obs <- obs[obs$loc != "972",] # Remove 972 as this turned out to be a PC850 white LED camera
            obs <- obs[obs$loc != "823",] # remove 823, as this camera lost power too often, and has too few active days
            unique(obs$loc)  # 972, 823 removed!

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

      # Torgeir: Removing 127 false blitz
      delete <- data.frame(loc = 127, 
                           date = seq(as.POSIXct("2019-01-10", tz="UTC", "%Y-%m-%d"), 
                                      as.POSIXct("2019-05-15", tz="UTC", "%Y-%m-%d"), 
                                      by=24*60*60))
      delete$delete <- TRUE # mark the ones to be deleted
      flash <-merge(flash, delete, by=c("loc", "date"), all=TRUE)
      flash <- flash[!flash$delete%in%TRUE,]
      flash$delete<-NULL

# Including the sorted species of the blitz locations
flash<-merge(flash, obs, by.x=c("Kam.nr. til blitskamera", "datetime"),by.y=c("loc", "datetime"),all.x=TRUE, all.y=FALSE)
flash<-flash[!duplicated(flash$image_id),]

# Expanding a grid with all possible combinations of dates and location ID for the flash camera for the whole study period, this is used to seperate into periods
treatment<-expand.grid(date=seq(min(flash$date), max(flash$date), "days"), loc=unique(flash$loc))
uni.date<-unique(flash[flash$TriggerMode%in%"T",c("loc","date")])

treatment$flash<-0

# Marking whether the camera had a blits there or not. 
for(i in 1:nrow(uni.date)){
  treatment[treatment$date%in%uni.date[i,"date"] & treatment$loc%in%uni.date[i,"loc"],"flash"]<-1
}

# Camera 925 had probably an issue with the flash on the 2020-02-05, setting that one to 1, as that makes most sense
treatment[treatment$loc%in%925 & treatment$date%in%as.Date("2020-02-05"),]$flash<-1 

# Marking the different periods
flash$period<-NA

for(i in unique(treatment$loc)){
  my.rle<-rle(treatment[treatment$loc%in%i,"flash"])
  row.nr<-rownames(treatment[treatment$loc%in%i,])
  treatment[row.nr[1:my.rle$lengths[1]],"period"]<-paste0(my.rle$values[1], "_", 1)
  treatment[row.nr[(my.rle$lengths[1]+1):sum(my.rle$lengths[1:2])],"period"]<-paste0(my.rle$values[2], "_", 1)
  treatment[row.nr[(sum(my.rle$lengths[1:2])+1):sum(my.rle$lengths[1:3])],"period"]<-paste0(my.rle$values[3], "_", 2)
  if(length(my.rle$lengths)>3){
    treatment[row.nr[(sum(my.rle$lengths[1:3])+1):sum(my.rle$lengths[1:4])],"period"]<-paste0(my.rle$values[4], "_", 2)
  }
  if(length(my.rle$lengths)>4){
    treatment[row.nr[(sum(my.rle$lengths[1:4])+1):sum(my.rle$lengths[1:5])],"period"]<-paste0(my.rle$values[5], "_", 3)
  }
}

ggplot(data=treatment[!is.na(treatment$period),])+
  facet_wrap(~period, ncol=3) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))

# From this plot we see that there is many cameras in the period 0_1 that have a low number of days.
# We the start period for these cameras a bit later to fix this issue. 

treatment.agg<-ddply(treatment, .(period, loc), summarise,
                     n=length(date))

loc.remove.first.period<-treatment.agg[treatment.agg$n<40 & treatment.agg$period%in%"0_1",]$loc
loc.remove.last.period<-treatment.agg[treatment.agg$n<40 & treatment.agg$period%in%"0_3",]$loc

treatment<-treatment[!(treatment$loc%in%loc.remove.first.period & treatment$period%in%"0_1"),] # Removing first period with few obs
treatment<-treatment[!(treatment$loc%in%loc.remove.last.period & treatment$period%in%"0_3"),] # Removing last period with few obs

# Giving them names all over again
treatment$period<-NA

for(i in unique(treatment$loc)){
  my.rle<-rle(treatment[treatment$loc%in%i,"flash"])
  row.nr<-rownames(treatment[treatment$loc%in%i,])
  treatment[row.nr[1:my.rle$lengths[1]],"period"]<-paste0(my.rle$values[1], "_", 1)
  treatment[row.nr[(my.rle$lengths[1]+1):sum(my.rle$lengths[1:2])],"period"]<-paste0(my.rle$values[2], "_", 1)
  treatment[row.nr[(sum(my.rle$lengths[1:2])+1):sum(my.rle$lengths[1:3])],"period"]<-paste0(my.rle$values[3], "_", 2)
  if(length(my.rle$lengths)>3){
    treatment[row.nr[(sum(my.rle$lengths[1:3])+1):sum(my.rle$lengths[1:4])],"period"]<-paste0(my.rle$values[4], "_", 2)
  }
  if(length(my.rle$lengths)>4){
    treatment[row.nr[(sum(my.rle$lengths[1:4])+1):sum(my.rle$lengths[1:5])],"period"]<-paste0(my.rle$values[5], "_", 3)
  }
}

ggplot(data=treatment[!is.na(treatment$period),])+
  facet_wrap(~period, ncol=3) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))

ggplot(data=treatment[!is.na(treatment$period),])+
 # facet_wrap(~period, ncol=3) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))

# We remove period 0_3 and period 1_3 as these are not a part of the study design. 
treatment<-treatment[!treatment$period%in%c("0_3", "1_3"),]
treatment<-treatment[!treatment$loc%in%257,] # We also remove this one as it appears to have a malfunctioned blits

ggplot(data=treatment[!is.na(treatment$period),])+
  facet_wrap(~period, ncol=2) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))  # 127 flash starts 15.5.19, 0_1 period before that. #REMOVED in line 36-44

ggplot(data=treatment[!is.na(treatment$period),])+
  # facet_wrap(~period, ncol=3) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))


# 0_1 indicates first period without blits, 0_2 the second and so on
# 1_1 indicates the first period with blits, ..... 

# Check out 257, seems to be missing photos.
table(treatment$period)

# Now, filtering out the observations inside the study period based on treatment
# I set the study period based on each camera trap.
obs<-obs[obs$loc<1500,] # Removing the blits cameras, they are stored in the object flash
obs<-obs[!duplicated(obs$timeserie_id),]
obs$date<-as.Date(obs$datetime, "%Y-%m%-%d", tz="UTC")

max.date.range<-range(treatment$date) # The max range for the study period for all cameras
# For loop to remove observations outside study period
for(i in unique(obs$loc)){
  if(i %in% unique(treatment$loc)){
    date.range<-range(treatment[treatment$loc%in%i,]$date)
    obs<-obs[!(obs$loc%in%i & obs$date<date.range[1]),] # Removing dates prior to this date for this camera
    obs<-obs[!(obs$loc%in%i & obs$date>date.range[2]),] # Removing dates after to this date for this camera
  } else{
    obs<-obs[!(obs$loc%in%i & obs$date<max.date.range[1]),] # Removing dates prior to this date for this camera
    obs<-obs[!(obs$loc%in%i & obs$date>max.date.range[2]),] # Removing dates after to this date for this camera
  }
}




# Including the control into the treatment
unique(obs$loc)[which(!unique(obs$loc)%in%treatment$loc)] # This should be the control

control<-expand.grid(loc=unique(obs$loc)[which(!unique(obs$loc)%in%treatment$loc)],
                     date=seq(max.date.range[1], max.date.range[2], by="days"), flash=0, period="Control")

treatment<-rbind(treatment, control)

ggplot(data=treatment[!is.na(treatment$period),])+
  facet_wrap(~period, ncol=3) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))

ggplot(data=treatment[!is.na(treatment$period),])+
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))
  
saveRDS(treatment, "Treatment_overview.rds")


# Creating the effort file and observation file #######################################################################################

effort<-unique(obs[,c("loc", "date")]) # The effort object contains all the dates for each camera trap location and the treatment period they belong to

# Here the issue with the browning cameras is quite clear
ggplot(data=effort)+
  geom_point(aes(y=as.factor(loc), x=date))

# We fix this by assuming that all browning cameras were active the whole time and remove the known inactive gaps. 

visits <- read_csv("visits.csv")
visits_browning <- visits %>% filter(cam_mod == "Browning")
visits_browning %>% 
  tbl_df %>% print(n = Inf)

b.obs <- obs[obs$loc %in% visits_browning$loc,]

temp <- lapply(unique(b.obs$loc), function(i){
  data.frame(loc = i,
             date = seq(min(b.obs[b.obs$loc %in% i,]$date),
                        max(b.obs[b.obs$loc %in% i,]$date),
                        by = "days"))
})

# b.effort contains all the assumed date the camera was active. 
b.effort <- do.call(rbind,temp)

gaps<-read.csv("Neri/Inaktive_gap_Browning.csv", sep="\t")
gaps$first_date<-as.POSIXct(gaps$first_date, "%d.%m.%Y", tz="UTC")
gaps$last_date<-as.POSIXct(gaps$last_date, "%d.%m.%Y", tz="UTC")

# Removing the gaps from b.effort
for(i in 1:nrow(gaps)){
  b.effort<-b.effort[!(b.effort$loc%in%gaps[i,]$loc & 
                         b.effort$date>=gaps[i,]$first_date & b.effort$date<=gaps[i,]$last_date),]
}

effort<-effort[!effort$loc%in%b.effort$loc,] # Removing browning cameras from the effort file
effort<-rbind(effort, b.effort)

# Issue with the effort from the browning cameras are fixed. 
ggplot(data=effort)+
  geom_point(aes(y=as.factor(loc), x=date))


# Now making sure that the effort and observations are all inside the defined study period
for(i in unique(effort$loc)){
  effort<-effort[!(effort$loc%in%i & effort$date<min(treatment[treatment$loc%in%i,]$date)),] # removing dates prior to
  effort<-effort[!(effort$loc%in%i & effort$date>max(treatment[treatment$loc%in%i,]$date)),] # removing dates after
}

for(i in unique(obs$loc)){
  obs<-obs[!(obs$loc%in%i & obs$date<min(treatment[treatment$loc%in%i,]$date)),] # removing dates prior to
  obs<-obs[!(obs$loc%in%i & obs$date>max(treatment[treatment$loc%in%i,]$date)),] # removing dates after
}


# Quick check
plot(loc~date, treatment, pch=19)
points(loc~date, effort, pch=19, cex=0.2, col="blue") # None of the blue should be outside the black

plot(loc~date, effort, pch=19, col="blue")
points(loc~date, obs[!obs$validated_species%in%"nothing",], pch=19, cex=0.2, col="red") # None of the red should be outside the blue
# Good

# Including the treatments to obs and effort
obs<-merge(obs, treatment, by=c("loc", "date"), all.x=TRUE, all.y=FALSE)
effort<-merge(effort, treatment, by=c("loc", "date"), all.x=TRUE, all.y=FALSE)

# Saving observations and effort
saveRDS(effort, "Effort_prepared.rds")
saveRDS(obs, "Observations_prepared1.rds")


