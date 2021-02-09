# alle browning
# hent ut start og slutt for alle browningkamera i vissittdato
library(tidyverse)
library(lubridate)
library(plyr)
library(dplyr)

# visits   ------------------------------------
# visits <- readxl::read_excel("~/Downloads/Kamera-notat.xlsx", sheet = "Vitjingar")
# visits <- visits[-2]
# visits <- rename(visits, replace= c("Kamera" = "loc"))
# visits$loc <- as.integer(visits$loc)
# visits <- visits[visits$loc != "971",]
# visits <- left_join(visits, stations, by = "loc")
# visits <- visits %>%
#   select(1, starts_with("ab"), starts_with("ca"), starts_with("Treat dat"))
# names(visits) <- c("loc", "abc", "cam_mod", "v_date1","v_date2","v_date3","v_date4","v_date5")
# visits$v_date1 <- dmy(visits$v_date1)
# visits$v_date2 <- dmy(visits$v_date2)
# visits$v_date3 <- dmy(visits$v_date3)
# visits$v_date4 <- dmy(visits$v_date4)
# visits$v_date5 <- dmy(visits$v_date5)
# write.csv(visits, "visits.csv", row.names = F)
visits <- read_csv("visits.csv")
visits_browning <- visits %>% filter(cam_mod == "Browning")

# Oversikt over
  {
# 1255  # A OK
# 1254 v4  gap v5 # 0% batt (16.02.2020)
# 1253  # A OK
# 1166 v2 gap v4:v5 # Full card (23/10/19)
# 943  v2 gap v3:v5 # Full card (13/8/19)
# 942  v2 gap v3:v5 # Full card (28/7/19)
# 925  v2 gap v3    # Full card (12/08/19)
# 863  frst - last  # checked by elisabeth + katie
# 861  frst - last  #      - - | | - - 
# 860  v3 gap v5    # frosset bOKs, 0% batt
# 855  v1 gap v4 gap v5 # fullt SD + tomt batteri | kamera truffet av is - batteri-kapsel løst ut
# 850  v2 gap v3    # full card (12/08/19) | stopper stundom å ta bilder?
# 849  # A OK
# 848  # A OK
# 843  # A OK
# 842  # A OK
# 841  # A OK
# 840  v1 gap v2 gap v3 # 0% batt | full card
# 835  # A OK
# 834  # A OK
# 833  # A OK
# 831  # A OK
# 830  # A OK
# 829  # A OK
# 828  # A OK
# 827  # A OK
# 825  # A OK
# 824  # A OK
# 823 v2 er start, gap gap gap # ekskluder frå studie?
# 822  # A OK
# 821  # A OK
# 818  # A OK
# 455  # A OK
# 
}

A_OK <- c("1255", "1253", "849" ,"848", "843", "842", "841", "835", "834", 
      "833", "831","830","829","828","827","825","824","822","821","818","455")

# Now, we aggregate the data by camera trap and period for the effort to see how many days the camera was active
#effort.stat<-ddply(effort, .(loc, period), summarise,
#                   n.days=length(unique(date)))
#effort.stat

# fixing for all browning without gaps
# firts on a separate browning object
#effort.browningAOK <- filter(effort, loc %in% A_OK)
#effort.stat.browningAOK <- ddply(effort, .(loc, period), summarise,
#                   n.days= length(seq(min(date), max(date), by = "day")))  # worked!

#replace(effort.stat, A_OK, effort.stat.browningAOK) #think this worked


# problemet starter eigentleg i effort. forsøke å fikse det her?

### Exploring the frequencies of detection inside the different periods ----------------------------------------------------------------------------
# Now you can develop scripts to do downstream work that reload the precious object via 
obs <- readRDS("obs_common.rds")     # tilsvarer ~linje 90
flash <- readRDS("flash_common.rds") #    - - | | - - 


# First we need to create an effort file to know how many dates the different cameras was active. 
obs$date<-as.POSIXct(as.character(obs$datetime),"%Y-%m-%d", tz="UTC") # create a new column with date
effort<-unique(obs[,c("loc", "date", "period")]) # The effort object contains all the dates for each camera trap location and the treatment period they belong to

# New code Neri ------------------------------
# Let us assume that all browning cameras was active from first to last picture
# That assumption should give the following effort for the browning cameras

b.obs <- obs[obs$loc %in% visits_browning$loc,]

temp <- lapply(unique(b.obs$loc), function(i){
  data.frame(loc = i,
             date = seq(min(b.obs[b.obs$loc %in% i,]$date),
                        max(b.obs[b.obs$loc %in% i,]$date),
                        by = 24 * 60 * 60 ))
})

# b.effort contains all the assumed date the camera was active. 
b.effort <- do.call(rbind,temp)
head(b.effort) 
summary(temp)
head(b.obs)
plot(loc ~ date, b.effort, ylim = c(900,956))
points(loc ~ date, b.obs, col="red", pch="l") 
# It generally looks nice, but there might be som issue with the cameras right below 1000 

# deletion setup ---------------------------------------------
# What I would do next is to create another data.frame and use that one to remove the gaps. 
# This is not the most elegant way, but it could be easier when you only have few gaps. 
# For example

delete <- data.frame(loc = 925, 
                     date = seq(as.POSIXct("2019-06-01", tz="UTC", "%Y-%m-%d"), 
                                as.POSIXct("2019-08-12", tz="UTC", "%Y-%m-%d"), 
                                by=24*60*60))
points(loc ~ date, delete, col="blue", pch="l")  # updated temp to delete

delete$delete <- TRUE # mark the ones to be deleted
b.effort<-merge(b.effort, delete, by=c("loc", "date"), all=TRUE)
b.effort<-b.effort[!b.effort$delete%in%TRUE,]
b.effort$delete<-NULL
# 925  v2 gap v3    # Full card (12/08/19)          # DONE

# functions ------------- --------------
gap_delete_select <- function(x,d1,d2) {
  delete <- data.frame(loc = x, 
                       date = seq(as.POSIXct(d1, tz="UTC", "%Y-%m-%d"), 
                                  as.POSIXct(d2, tz="UTC", "%Y-%m-%d"), 
                                  by=24*60*60)
  )
  points(loc ~ date, delete, col="blue", pch="l")  # updated temp to delete
  assign('delete', delete, 1)
}
gap_delete_confirm <- function(x = delete){
  delete$delete <- TRUE # mark the ones to be deleted
  b.effort<-merge(b.effort, delete, by=c("loc", "date"), all=TRUE)
  b.effort<-b.effort[!b.effort$delete%in%TRUE,]
  b.effort$delete<-NULL
  assign('b.effort', b.effort,1)
}

# gap_deletion --------------------------

# 1166 v2 gap v4:v5 # Full card (23/10/19)          # DONE
plot(loc ~ date, b.effort, ylim = c(1165,1256))
points(loc ~ date, b.obs, col="red", pch="l") 
gap_delete_select(1166,"2019-09-20","2019-10-20") # 1166a inaccurate dates
  gap_delete_confirm(delete)
gap_delete_select(1166,"2019-12-10","2019-12-25") # 1166b inaccurate dates
  gap_delete_confirm(delete)

# 1254 v4  gap v5 # 0% batt (16.02.2020)            # DONE (but not really necessary)
plot(loc ~ date, b.effort, ylim = c(1250,1256))
points(loc ~ date, b.obs, col="red", pch="l") 
gap_delete_select(1254,"2019-12-18","2020-02-17") # 1254 accurate dates
  gap_delete_confirm(delete)

# 943  v2 gap v3:v5 # Full card (13/8/19)           # DONE
# 942  v2 gap v3:v5 # Full card (28/7/19)           # DONE
plot(loc ~ date, b.effort, ylim = c(941,944))
points(loc ~ date, b.obs, col="red", pch="l") 
gap_delete_select(943,"2019-01-23","2019-05-29") # 943a accurate dates (+ "3 shots normal" instead of "rapidfire")
  gap_delete_confirm(delete)
gap_delete_select(943,"2019-07-02","2019-08-13") # 943b accurate dates (remains from slaughtered cow near camera)
  gap_delete_confirm(delete)
gap_delete_select(942,"2019-06-28","2019-07-28") # 942 (accurate dates)
  gap_delete_confirm(delete)


# 863  frst - last  # checked by elisabeth + katie  # A OK
# 861  frst - last  #      - - | | - -              # A OK
# 860  v3 gap v5    # frosset boks, 0% batt         # DONE
# 855  v1 gap v4 gap v5 # fullt SD + tomt batteri | # DONE  | kamera truffet av is - batteri-kapsel løst ut
# 850  v2 gap v3    # full card (12/08/19) | sometimes stops taking photos
plot(loc ~ date, b.effort, ylim = c(850,864))
points(loc ~ date, b.obs, col="red", pch="l") 
gap_delete_select(860,"2019-12-24","2020-03-13") # 860 (accurate dates)
  gap_delete_confirm(delete)
gap_delete_select(855,"2019-06-21","2019-10-01") # 855a (accurate dates)
  gap_delete_confirm(delete)
gap_delete_select(855,"2019-12-11","2020-01-10") # 855b (accurate dates)
  gap_delete_confirm(delete)
gap_delete_select(850,"2019-05-26","2019-08-12") # 850a (accurate dates)
  gap_delete_confirm(delete)
gap_delete_select(850,"2019-08-23","2019-10-17") # 850b (accurate dates) |sometimes stops taking photos
  gap_delete_confirm(delete)

# 840  v1 gap v2 gap v3 # 0% batt | full card
plot(loc ~ date, b.effort, ylim = c(823,840))
points(loc ~ date, b.obs, col="red", pch="l") # Oisann! 828 + 824 tvilsomme
gap_delete_select(840,"2019-03-30","2019-05-25") # 840a (accurate dates)
  gap_delete_confirm(delete)
gap_delete_select(840,"2019-06-22","2019-07-29") # 840b (accurate dates)
  gap_delete_confirm(delete)
# 823 v2 er start, gap gap gap # ekskluder frå studie?
gap_delete_select(823,"2019-04-03","2019-05-27") # 823a (accurate dates) summer time + 1min photo delay + none rapidfire
# gap_delete_confirm(delete)
gap_delete_select(823,"2019-06-06","2019-07-28") # 823b (accurate dates)
  gap_delete_confirm(delete)
gap_delete_select(823,"2019-08-30","2019-10-21") # 823c (accurate dates)
  gap_delete_confirm(delete)

# 829 - gaps that are not listed in bildelogg!
# 828 - appearant gaps that I did not know of in advance
# 824 -             -   -   |  |   -   -
gap_delete_select(829,"2019-05-31","2019-09-05") # 829a (inaccurate last date)
  gap_delete_confirm(delete)
gap_delete_select(829,"2019-09-18","2019-10-22") # 829b (inaccurate first date) checked camera 23/10/19
  gap_delete_confirm(delete)
gap_delete_select(828,"2019-04-07","2019-04-19") # 828 (accurate dates)
  gap_delete_confirm(delete)
gap_delete_select(824,"2019-07-03","2019-08-12") # 824 (accurate dates)
  gap_delete_confirm(delete)

plot(loc ~ date, b.effort, ylim=c(800,1260))
points(loc ~ date, b.obs, col="red", pch="l") 
  

# Now you need to assign the correct period and rbind b.effort to effort and run unique(effort)

#effort <- rbind(effort, b.effort)

effort <- union(effort, b.effort) # lacks the period col