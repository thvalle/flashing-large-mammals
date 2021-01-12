# This script divide the study period into different treatments period, based on whether it was a blits there or not, and explore the data. 
# 

#library(tidyverse)
library(plyr)
library(dplyr)
library(here)

### Import data and divide into treatments --------------------------------------------------------------------------------------

# Import data
obs<-readRDS("Data_20200818.rds")

colnames(obs)[1]<-"loc"
obs <- obs[obs$loc != "971",]
unique(obs$loc)  #971 (a PC850 reconyx -whiteLED) removed!

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
treatment<-expand.grid(date=seq(min(flash$date), max(flash$date), "days"), loc=unique(flash$loc))
uni.date<-unique(flash[flash$TriggerMode%in%"T",c("loc","date")])

treatment$flash<-0

# Marking whether the camera had a blits there or not. 
for(i in 1:nrow(uni.date)){
  treatment[treatment$date%in%uni.date[i,"date"] & treatment$loc%in%uni.date[i,"loc"],"flash"]<-1
}

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

# 0_1 indicates first period without blits, 0_2 the second and so on
# 1_1 indicates the first period with blits, ..... 

# Check out 257, seems to be missing photos. ----------------------------------
table(treatment$period)
unique(treatment$period[treatment$loc == "257"]) #NA står oppført som ein periode. Noko i for-løkka markerer den galt på siste periode.
unique(treatment$period[treatment$loc == "925"]) #  - -  ||  - -                  Er det fordi den blir tolka som ein ekstra periode ETTER 0_3?
unique(treatment$period[treatment$loc == "15"]) #Kun blitsperioder
unique(treatment$period[treatment$loc == "662"]) # character(0) - på alt som ikkje er kamera i gruppe B / C!
unique(treatment$period[treatment$loc == "1552364902345"]) # character(0)



# Now, filtering out the observations inside the study period based on treatment
obs$date<-as.Date(obs$datetime, "%Y-%m%-%d", tz="UTC")
obs<-obs[obs$date%in%treatment$date,]
obs<-obs[obs$loc<1500,] # Removing the blits cameras, they are stored in the object flash
obs<-obs[!duplicated(obs$timeserie_id),]

# Tranferring the periods to the observations
obs<-merge(obs, treatment, by=c("loc", "date"), all.x=TRUE, all.y=FALSE)
obs[is.na(obs$flash),"flash"]<-0 # Setting the NAs to 0, all of these are the control
obs[is.na(obs$period),"period"]<-"Control"

unique(obs$period[obs$loc == "257"]) #NA er blitt til Control. Noko i for-løkka markerer ein periode galt og skubber den omsider ut i kontroll
unique(obs$period[obs$loc == "925"]) #  - -  ||  - -       Mangler også 1_1-perioden, og har ei kort veke som Control i slutten av studien (ref. tidperwrap.plot)
unique(obs$period[obs$loc == "15"]) #Kun blitsperioder
unique(obs$period[obs$loc == "662"]) # Control
unique(obs$period[obs$loc == "1552364902345"]) # character(0)

# Identical parts of this script and time_to_event4
saveRDS(obs, here("obs_common.rds")) 
saveRDS(flash, here("flash_common.rds")) 

# Now you can develop scripts to do downstream work that reload the precious object via 
obs <- readRDS(here("obs_common.rds"))
flash <- readRDS(here("flash_common.rds"))


### Exploring the frequencies of detection inside the different periods ----------------------------------------------------------------------------

# First we need to create an effort file to know how many dates the different cameras was active. 
obs$date<-as.POSIXct(as.character(obs$datetime),"%Y-%m-%d", tz="UTC") # create a new column with date

effort<-unique(obs[,c("loc", "date", "period")]) # The effort object contains all the dates for each camera trap location and the treatment period they belong to

# Now, we aggregate the data by camera trap and period for the effort to see how many days the camera was active
effort.stat<-ddply(effort, .(loc, period), summarise,
                   n.days=length(unique(date)))
colnames(obs)
# Then, we aggregate the observations of the different species based on the obs object
obs.stat<-ddply(obs, .(loc, period, validated_species), summarise,
                   n.obs=length(unique(datetime))) # Change to datetime here since we might have several observation during a single day

# Not all possible combinations of camera traps, camera traps and species will be in the obs.stat, since not all species are captured on a single camera.
# Extending the effort.stat frame to contain all possible detection on every camera trap location
temp<-effort.stat
for(i in 2:length(unique(obs$validated_species))){
  effort.stat<-rbind(effort.stat, temp)
}

effort.stat$validated_species<-rep(as.character(unique(obs$validated_species)), each=nrow(temp)) # assigning the validated_species 
View(effort.stat) # Looks good

freq<-merge(obs.stat, effort.stat, by=c("loc", "period", "validated_species"), all=TRUE)

# All the rows with NA in n.obs did not exist in the obs.stat, which means that they are 0 in this. 
freq[is.na(freq$n.obs),"n.obs"]<-0
freq$freq<-freq$n.obs/freq$n.days

# Creating new column with flash vs not flash
freq$flash<-ifelse(freq$period%in%c("1_1","1_2"), TRUE, FALSE)

# Looking at the number of days the cameras have for each period
hist(freq$n.days, breaks=100,col="grey") # Some have very few, those could be removed
range(freq$n.days)
abline(h=c(20,40,60,80))
# Setting 20 days as an arbitrary limit -----------------------------------------------
freq<-freq[freq$n.days>19,]

# ---------------- Visualizing the data
sp="rev"
plot(freq~as.factor(period), freq[freq$validated_species%in%sp,])
plot(freq~as.factor(flash), freq[freq$validated_species%in%sp,])

#Burde dele opp control i enkelt-perioder for å ha kontroll opp mot kvar enkelt periode
#Nå er 0_1,0_2,1_1,1_2 2sesonger a 40kam kvar, 0_3 er 1sesong a 20kamera, og Control er 5sesonger a 20kam


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
 
plot(table(obs$validated_species))
plot(table(obs$validated_species[obs$validated_species != "nothing" & obs$validated_species != "villsvin" & obs$validated_species != "ulv" &obs$validated_species != "motorsykkel"]))
sp_all <- c("hare","elg","rev","grevling","maar","gaupe","ekorn","skogshøns","raadyr","hjort")
plot(table(obs$validated_species[obs$validated_species %in% sp_all]))

# I suggest that you try to find a nice way to visualize these patterns.
# From first inspection it does seem to be a large effect of the blits. 

# Looking at activity patterns for cameras with and without flash ----------------------------------------------------------------------
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

#Reven "skyvest" på ein måte vekk frå moglege skumringstimer når flash MANGLER! 
#Med andre ord: kan flash auke deteksjonsraten på rev i skumringen?

# Datalagring ----------------------------------------------------------------------------------
write.csv(obs,'obs_datexpl.csv', row.names = F)  #lagrer dataframen min som .csv (kan lagre .txt også)
write.csv(freq,'freq_datexpl.csv', row.names = F)
write.csv(effort, 'effort_datexpl.csv', row.names = F)
#rm(dat) #fjerner objektet fra environment
obs <- read_csv('obs_datexpl.csv')
freq <- read_csv('freq_datexpl.csv')
effort <- read_csv('effort_datexpl.csv')
stations <- read_csv('stations.csv')


# adding abc and cam_mod to effort
{
  effort$abc <- "A"
  B <- stations$loc[stations$abc == "B"]
  C <- stations$loc[stations$abc == "C"]
  effort$abc[effort$loc %in% B] <- "B"
  effort$abc[effort$loc %in% C] <- "C"
  effort$cam_mod <- "Reconyx"
  effort$cam_mod[effort$loc %in% stations$loc[stations$cam_mod == "Browning"]] <- "Browning"
  effort$cam_mod <- as.factor(effort$cam_mod)
}


