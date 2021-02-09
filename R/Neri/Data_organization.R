# This script divide the study period into different treatments period, based on whether it was a blits there or not
# 

library(tidyverse)
library(plyr)
library(dplyr)

# Import data and divide into treatments ########################################################################################

# Import data
obs<-readRDS("Data_20210125.rds")
colnames(obs)[2]<-"loc"

    obs <- obs[!obs$loc %in% c(943,328,823, 972),] # Remove 972 as this turned out to be a PC850 white LED camera
    any(obs$loc  %in%  c(943,328,823, 972))

# Including the flashes
flash1<-readRDS("exifdatablits.RDS")
flash2<-readRDS("exifdatablits_additional.RDS")

# Giving them a loc ID
flash1$loc<-as.numeric(gsub("([0-9]+).*$", "\\1", flash1$Directory))
flash1<-flash1[,c("loc", "DateTimeOriginal", "InfraredIlluminator", "Flash", "Model", "TriggerMode")]

flash2$loc<-as.numeric(lapply(gsub("([0-9]+).*$", "\\1", flash2$Directory), 
                              function(x){str_split(x, "/")[[1]][6]}))
flash2<-flash2[,c("loc", "DateTimeOriginal", "InfraredIlluminator", "Flash", "Model", "TriggerMode")]
flash2[flash2$loc%in%3188,"loc"]<-822
flash2[flash2$loc%in%3178,"loc"]<-829
flash2[flash2$loc%in%3193,"loc"]<-831
flash2[flash2$loc%in%3198,"loc"]<-845
flash2[flash2$loc%in%3200,"loc"]<-953
a<-unique(obs[,c("loc", "Kam.nr. til blitskamera")])
flash<-rbind(flash1, flash2)

# Fixing datetime
flash$datetime<-as.POSIXct(flash$DateTimeOriginal, "%Y:%m:%d %H:%M:%S", tz="UTC")
flash$date<-as.Date(flash$datetime, format="%Y-%m%-%d")

# Fixing the different naming of the blitz locations
flash<-merge(flash, unique(obs[,c("loc","Kam.nr. til blitskamera")]), by=c("loc"), all.x=TRUE, all.y=FALSE)
flash<-flash[!is.na(flash$`Kam.nr. til blitskamera`),] 

# Including the sorted species of the blitz locations
flash<-merge(flash, obs, by.x=c("Kam.nr. til blitskamera", "datetime"),by.y=c("loc", "datetime"),all.x=TRUE, all.y=FALSE)
flash<-flash[!duplicated(flash$image_id),]

# Removing flash prior to 2019.05.15 for camera 127 (email from Torgeir 2021.01.2010:22)
flash<-flash[!(flash$loc%in%127 & flash$date<as.Date("2019-05-15")),]

saveRDS(flash, "Flash_prepared.RDS")

# Expanding a grid with all possible combinations of dates and location ID for the flash camera for the whole study period, this is used to seperate into periods
treatment<-expand.grid(date=seq(min(flash$date), max(flash$date), "days"), loc=unique(flash$loc))
uni.date<-unique(flash[,c("loc","date")])

treatment$flash<-0

# Marking whether the camera had a blits there or not. 
for(i in 1:nrow(uni.date)){
  treatment[treatment$date%in%uni.date[i,"date"] & treatment$loc%in%uni.date[i,"loc"],"flash"]<-1
}

# Camera 925 had probably an issue with the flash on the 2020-02-05, setting that one to 1, as that makes most sense
treatment[treatment$loc%in%925 & treatment$date%in%as.Date("2020-02-05"),]$flash<-1 
treatment[treatment$loc%in%257 & treatment$date%in%as.Date("2019-08-16"),]$flash<-1 

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
  facet_wrap(~period, ncol=2) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))

ggplot(data=treatment[!is.na(treatment$period),])+
 # facet_wrap(~period, ncol=3) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))

# We remove period 0_3 and period 1_3 as these are not a part of the study design. 
treatment<-treatment[!treatment$period%in%c("0_3", "1_3"),]

# Removing the following camera based on mail from Torgeir 2021.01.21  16:06 and script browning gap fille(2), sent 2021.01.14 19:50
treatment<-treatment[!treatment$loc%in%c(943,328,823),] #

ggplot(data=treatment[!is.na(treatment$period),])+
  facet_wrap(~period, ncol=2) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))

ggplot(data=treatment[!is.na(treatment$period),])+
  # facet_wrap(~period, ncol=3) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))


# 0_1 indicates first period without blits, 0_2 the second and so on
# 1_1 indicates the first period with blits, ..... 

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
  



# Creating the effort file and observation file ####################################################################

effort<-unique(obs[,c("loc", "date")]) # The effort object contains all the dates for each camera trap location and the treatment period they belong to

# Here the issue with the browning cameras is quite clear
ggplot(data=effort)+
  geom_point(aes(y=as.factor(loc), x=date))

# We fix this by assuming that all browning cameras were active the whole time and remove the known inactive gaps. 

visits <- read_csv("visits.csv")
visits_browning <- visits %>% filter(cam_mod == "Browning")
visits_browning %>% 
 print(n = Inf)

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


# Issue with the effort from the browning cameras is fixed. 
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
ggplot(treatment)+
  geom_point(aes(as.factor(loc), date)) +
  geom_point(aes(as.factor(loc), date), shape = ".", colour = "red", data = effort) +
  geom_point(aes(as.factor(loc), date), shape = "'", colour = "white",
             data = filter(obs, !validated_species  %in% "nothing")) + coord_flip()
# You see that for some cameras the treatment extends too far outside the effort, however, I do not think that is an issue

# None of the:  white should be outside the red
#               red   should be outside the black 
# white < red < black
# Good

# Including the treatments to obs and effort
obs<-merge(obs, treatment, by=c("loc", "date"), all.x=TRUE, all.y=FALSE)
effort<-merge(effort, treatment, by=c("loc", "date"), all.x=TRUE, all.y=FALSE)

# Plotting to visualize the effort based on periods. Spend some time to double check that this looks fine. 
ggplot(data=effort)+
  facet_wrap(~period, ncol=2) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))

ggplot(data=effort)+
  # facet_wrap(~period, ncol=3) +
  geom_point(aes(y=as.factor(loc), x=date, col=factor(flash)))

# 535 rart at halve mangler, ingen feilmeldinger i excel-arket mitt, siste visitt 25.02.2020 (SD sendt inn frå Ivar Knai)
# 460 hadde tekniske feil underveis
# 501 hadde teknisk trøbbel i starten
# 827 hadde teikniske feil i slutten
# 829 har gaps eg har visst om lenge frå plottene, men har aldri skrevet ned nokon gaps, og det står ingenting i bildelogg
# 842 rart at halve mangler, ingen feilmeldinger i excel-arket mitt, siste visitt 12.02.2020
# 848 mangler siste del,                                             siste visitt 14.02.2019

# dei fem frå sunniva-mail i effort
# 822 - mangler siste del
# 829 - har siste del 
# 831 - mangler siste del
# 845 - mangler siste del
# 953 - mangler siste del


# Saving treatment, observations and effort
saveRDS(treatment, "Treatment_overview.rds")
saveRDS(effort, "Effort_prepared.rds")
saveRDS(obs, "Observations_prepared1.rds")

b<-a[!duplicated(a$timeserie_id),]
table(b$validated_species)

write.csv(unique(flash$loc), "LokaliteterMedBlits.csv")

#Mikrohabitat frå John ---------------------------------------------------------

habitat <- readxl::read_excel("camerahabitat20210110.xlsx") %>% 
  select(LOKID, TYPE) %>% 
  rename(loc = LOKID, habitat = TYPE) %>% 
  add_row(loc = c(840, 841, 850, 925, 1254, 1255),
          habitat = c("cliff","foot_path","foot_path",
                      "forest_road","wildlife_trail","canyon")) %>% 
  arrange(loc)
# number of loc - matching locs in habitat
length(unique(obs$loc)) - length(habitat$loc[habitat$loc %in% obs$loc]) # 6
unique(obs$loc[!obs$loc %in% habitat$loc]) # 6 cam not in habitat file:
# 840 - cliff         Toppen av ein klippe, kameraet med best utsikt av alle loc
# 841 - foot_path     bak skihoppbakken, gjengrodd traktorvei ved bekken, 7 menneske i obs
# 850 - foot_path     Tyristrand, ved vannet der Olav sov på isen, ørten menneske
# 925 - forest_road   Stengt skogsvei med bauta og bom, ved Tyrifjorden. 10 kjøretøy, 21 menneske
# 1254- wildlife_trail  Nær fotsti ved toglinja mot Bergen, men peiker mot lite brukte dyretråkk
# 1255- canyon        Før flå, mellom felte trær i ein skarp dalformasjon
stations <- read_csv("stations.csv") %>% select(loc, cam_mod)
habitat <- left_join(habitat, stations, by = "loc") # include cam_mod in the habitat-object
saveRDS(habitat, "habitat.rds")

# Checking species passed by each loc
obs %>% 
  filter(loc %in% 1255) %>% # enter location
  group_by(species) %>% summarise(n = n()) # sums up observations per species


library(lubridate)
# # Kjapp feilsøking etter tidspunkter -----------------------------------------
#  på 831 (blits var riktig) og 833 (blit var feil) 
# 
# # 831
# # Gaupe passerer 29.5.19 22:22:02 på blits
# x <- obs$datetime[obs$loc == 831 & obs$validated_species == "gaupe"] ; x 
# feil <- ymd_hms(x[1]) ; feil
# riktig <- ymd_hms("2019-05-29 22:22:02") ; riktig
# # t.difference
# difftime(riktig, feil, units = "secs")
# # Time difference of -43262 secs

# # 833
# # elg passerer 8:55:55 og 9:08:40 den 7.3.2019
# feil <- ymd_hms("2019-03-07 08:55:55");feil
# 
# 
# x <- obs$datetime[obs$loc == 833 & obs$validated_species == "elg"] ; x
# riktig <- x[1]; riktig
# difftime(riktig, feil, units = "secs")
# # Time difference of 86213 secs
# 
# obs <- readRDS("Observations_prepared1.rds")
# 
# # 513
# # rådyr passerer 09:03:03 og 20:18:37 den 2019-08-13
# feil <- ymd_hms("2019-08-13 09:03:03");feil
# 
# 
# x <- obs$datetime[obs$loc == 513 & obs$validated_species == "raadyr"] ; x
# #[48] "2019-08-13 07:52:03 UTC" [49] "2019-08-13 19:07:27 UTC"
# riktig <- x[48]; riktig
# difftime(riktig, feil, units = "secs")
# # Time difference of -4260 secs






#328  954  494  662  664  943  830  
#535 1166  864  823  821  818  638  
#863  841  861  460  860  258  855 1255

# k <- c(328,  954,  494,  662,  664,  943,  830,  535, 1166,  864,  823,  
#        821,  818,  638,  863,  841,  861,  460,  860,  258,  855, 1255)
# stations <- read_csv("stations.csv")
# feil_kontroll <- k[!k  %in% stations$loc[stations$abc == "A"]]
# feil_kontroll