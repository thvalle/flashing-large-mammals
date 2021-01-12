#cameras in periods - checking if every camera from my excel-sheet is included

unique(obs$period)
unique(obs[obs$period%in%"Control","loc"]) #Control-group
unique(obs[obs$period%in%"0_1","loc"]) #no flash 1
unique(obs[obs$period%in%"1_1","loc"]) #with flash 1
unique(obs[obs$period%in%"0_2","loc"]) #no flash 2
unique(obs[obs$period%in%"1_2","loc"]) #with flash 2
unique(obs[obs$period%in%"0_3","loc"]) #no flash 3

all_cam_obs <- unique(c(obs[obs$period%in%"Control","loc"], obs[obs$period%in%"0_1","loc"],obs[obs$period%in%"1_1","loc"],
                            obs[obs$period%in%"0_2","loc"], obs[obs$period%in%"1_2","loc"] ,obs[obs$period%in%"0_3","loc"]))
all_cam_obs <- sort(all_cam_obs,decreasing = T) 
#all cameras in my excel-sheet, under "vitjingar". 972 is excluded from the experiment, but included to match the "obs" data.
vitjingar <- c(1255, 1254,1253,1166,972,971,954,953,944,943,942,925,864,863,861,860,855,850,849,
848,845,843,842,841,840,839,835,834,833,831,830,829,828,827,825,824,823,822,821,818,
664,662,656,651,638,535,513,501,494,488,460,456,455,328,258,257,231,193,127,15)
all_cam_obs == vitjingar #need to exclude 972 from the obs data frame
obs_cut1 <- obs[!obs$loc=="972",] #remove 972, as it turned out to be a LED flash CT
unique(obs_cut1$loc) #972 is removed

## Attempt to make objects containing active camera days of each defined period ##
#Ndays_cam15 <-
(unique(obs_cut1[obs_cut1$loc%in%"15", "date"]))
#Ndays_flash1 <- 
#Ndays_flash0 <-
#Ndays_0_1 <- 
table(obs[obs$period%in%"0_1","date"]) #number of days first period w/out flash
#Ndays_0_2 <-
#Ndays_0_3 <-
#Ndays_1_1 <-
#Ndays_1_2 <-
#Ndays_control

# N(sp) med og utan blits  (perioder 1_1 0_1) #

# Inspecting the number of observations during the different treatments
table(obs_cut1$validated_species)/
names(table(obs_cut1$validated_species))
sp <- names(table(obs_cut1$validated_species)[c(5,7,8,9,10,16,20,21)]) #Wild, large mammals with more than 50 obs
table(obs_cut1[obs_cut1$validated_species%in%sp[1],]$flash)
table(obs_cut1[obs_cut1$validated_species%in%sp,]$period)
{
elg     <-table(obs[obs$validated_species%in%sp[1],]$flash)
gaupe   <-table(obs[obs$validated_species%in%sp[2],]$flash)
grevling<-table(obs[obs$validated_species%in%sp[3],]$flash)
hare    <-table(obs[obs$validated_species%in%sp[4],]$flash)
hjort   <-table(obs[obs$validated_species%in%sp[5],]$flash)
maar  <-table(obs[obs$validated_species%in%sp[6],]$flash)
raadyr  <-table(obs[obs$validated_species%in%sp[7],]$flash)
rev  <-table(obs[obs$validated_species%in%sp[8],]$flash)

elg.p     <-table(obs[obs$validated_species%in%sp[1],]$period)
gaupe.p   <-table(obs[obs$validated_species%in%sp[2],]$period)
grevling.p<-table(obs[obs$validated_species%in%sp[3],]$period)
hare.p    <-table(obs[obs$validated_species%in%sp[4],]$period)
hjort.p   <-table(obs[obs$validated_species%in%sp[5],]$period)
maar.p  <-table(obs[obs$validated_species%in%sp[6],]$period)
raadyr.p  <-table(obs[obs$validated_species%in%sp[7],]$period)
rev.p  <-table(obs[obs$validated_species%in%sp[8],]$period)
} #objects containing N obs ~ period or flash 0/1
#period.table <- c(elg.p, gaupe.p, grevling.p,hare.p,hjort.p,katt.p,maar.p,raadyr.p,rev.p)

obs_sp <- obs_cut1[obs_cut1$validated_species%in%sp,] # subsets obs of all species in "sp"
table(obs_sp$validated_species) #
names(obs_sp) 


#Histogram of N obs ~ flash 0/1 per species
par(mfrow=c(2,4)) #2x4 plot of each species side by side
for(i in 1:8){
  plot(table(obs[obs$validated_species%in%sp[i],]$period)/nrow(obs), ylab="N obs", xlab = sp[i])
} #N validated species. Need to divide on active camera days to get comparable numbers between the groups
for(i in 1:8){
  plot(table(obs[obs$validated_species%in%sp[i],]$flash), ylab="N obs", xlab = sp[i])
}
par(mfrow=c(2,4)) #2x4 plot of each species side by side





for(i in 1:9){
hist(table(obs_sp[obs_sp$validated_species%in%sp[i],]$period), main=sp[i], xlab=" ")
} #non-sense

library(ggplot2)
qplot(x=obs$period, y=obs$validated_species%in%"rev") #non-sense




# Frekvenser 











# Blei dyrene flasha?



#Synkronisere utløser-data frå LED-FLASH med IR-FLASH
#Kan eg lage ein funksjon som henter data frå IR (x=) og LED (y=)
#   slik at time(y)==time(x).
#Altså for å passe på at klokkene er nokonlunde stilte etter kvarandre.
#Deretter kan eg enklere teste synkronisert utløysing ved å sjå kor ofte
#dei utløyser likt innanfor +/- 1 min eller liknande.

