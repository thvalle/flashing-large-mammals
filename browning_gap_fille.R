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
visits_browning %>% 
  tbl_df %>% print(n = Inf)

# Oversikt over
{
1255  # A OK
1254 v4  gap v5 # 0% batt (16.02.2020)
1253  # A OK
1166 v2 gap v4:v5 # Full card (23/10/19)
943  v2 gap v3:v5 # Full card (13/8/19)
942  v2 gap v3:v5 # Full card (28/7/19)
925  v2 gap v3    # Full card (12/08/19)
863  frst - last  # checked by elisabeth + katie
861  frst - last  #      - - | | - - 
860  v3 gap v5    # frosset bOKs, 0% batt
855  v1 gap v4 gap v5 # fullt SD + tomt batteri | kamera truffet av is - batteri-kapsel løst ut
850  v2 gap v3    # full card (12/08/19) | stopper stundom å ta bilder?
849  # A OK
848  # A OK
843  # A OK
842  # A OK
841  # A OK
840  v1 gap v2 gap v3 # 0% batt | full card
835  # A OK
834  # A OK
833  # A OK
831  # A OK
830  # A OK
829  # A OK
828  # A OK
827  # A OK
825  # A OK
824  # A OK
823 v2 er start, gap gap gap # ekskluder frå studie?
822  # A OK
821  # A OK
818  # A OK
455  # A OK
}

A_OK <- c("1255", "1253", "849" ,"848", "843", "842", "841", "835", "834", 
      "833", "831","830","829","828","827","825","824","822","821","818","455")

# Now, we aggregate the data by camera trap and period for the effort to see how many days the camera was active
effort.stat<-ddply(effort, .(loc, period), summarise,
                   n.days=length(unique(date)))
effort.stat

# fixing for all browning without gaps
# firts on a separate browning object
effort.browningAOK <- filter(effort, loc %in% A_OK)
effort.stat.browningAOK <- ddply(effort, .(loc, period), summarise,
                   n.days= length(seq(min(date), max(date), by = "day")))  # worked!

replace(effort.stat, A_OK, effort.stat.browningAOK) #think this worked


# problemet starter eigentleg i effort. forsøke å fikse det her?

### Exploring the frequencies of detection inside the different periods ----------------------------------------------------------------------------
# Now you can develop scripts to do downstream work that reload the precious object via 
obs <- readRDS("obs_common.rds")     # tilsvarer ~linje 90
flash <- readRDS("flash_common.rds") #    - - | | - - 


# First we need to create an effort file to know how many dates the different cameras was active. 
obs$date<-as.POSIXct(as.character(obs$datetime),"%Y-%m-%d", tz="UTC") # create a new column with date

effort<-unique(obs[,c("loc", "date", "period")]) # The effort object contains all the dates for each camera trap location and the treatment period they belong to

# Feilen burde helst rettast opp i her, men eg veit ikkje kva for fiks fakseri som skal til.
# Ein slags expand grid, men som ikkje overskrider kvar enkelt periode.




# Forsøk på å legge til direkte i obs heilt i starten

obs$date[obs$loc %in% A_OK,] 
test  <- expand.grid(loc=as.numeric(A_OK),  
                   image_id = NA, timeserie_id = NA,  dataset = NA ,               
                   captured_at_exif = NA, predicted_species = obs$predicted_species[2], validated_species = "nothing",    
                   distance = NA, num_animals = NA,  datetime = seq(min(obs$datetime), max(obs$datetime), "days") ,                
                   `Kam.nr. til blitskamera` = NA)
test <- arrange(test, loc, !desc(datetime))
obs <- bind_rows(obs, test)
obs <- arrange(obs, loc, !desc(datetime))