
# True setup -------------------------------
        # stations <- read.csv("kam_geo/kameraliste.csv", header = T)
        # names(stations)
        # stations <- stations[c(3,9,10,2,7)] #subset variables I want
        # names(stations) <- c("loc","x", "y", "abc","cam_mod")
        # names(stations) #worked
        # stations <- stations[1:60,] #cut away info below observations
        # nrow(stations) #it worked
        # 
        # #Rename camera types to include only Browning and Reconyx
        # names(stations) <- c("loc","x", "y", "abc","cam_mod")
        # names(stations) #worked
        # stations <- stations[1:60,] #cut away info below observations
        # nrow(stations) #it worked
        # 
        # #Rename camera types to include only Browning and Reconyx
        # unique(stations$cam_mod)
        # stations[stations$cam_mod=="Browning (+reconyx)",5] <- "Browning" # 5 marks the cam_mod colon
        # stations[stations$cam_mod=="RECONYXINFRA",5] <- "Reconyx"
        # stations[stations$loc == 15,5] <- "Reconyx"
        # stations$cam_mod <- factor(stations$cam_mod, levels= c("Reconyx","Browning"))
        # levels(stations$cam_mod)

        # loc_points <- data.frame(x=stations$x,y=stations$y)
        # loc_points <- SpatialPoints(loc_points,proj4string=CRS("+proj=utm +zone=32 +datum=WGS84"))
        # loc_UTM33 <- spTransform(loc_points,CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
        # stations$x <- loc_UTM33$x
        # stations$y <- loc_UTM33$y
         stations$loc <- as.integer(stations$loc)
#write_rds(stations, "stations.rds") 

library(tidyverse)
library(here)
library(rgdal)  # spatial data tools (geospatial data abstraction library)
library(raster) # raster tools
library(sp)     # vector tools
library(fields) # colour ramps (e.g. tim.colors)
library(rgbif)  # global biodiversity data (GBIF) tools

# plotting ---------------------------------------------------------
stations <- readRDS('stations.rds')

plot(stations$x, stations$y, type="n", xlab="cam_mod",ylab="")
points(stations[stations$cam_mod=="Reconyx",2],
       stations[stations$cam_mod=="Reconyx",3],pch=19,col="black")
points(stations[stations$cam_mod=="Browning",2],
       stations[stations$cam_mod=="Browning",3],pch=19,col="brown")
points(stations[stations$abc=="B",2],
       stations[stations$abc=="B",3],pch=16,col="white", cex=0.5)
points(stations[stations$abc=="C",2],
       stations[stations$abc=="C",3], pch = 16 ,col = "white", cex = 0.5)
legend("bottomleft",legend=c("Reconyx","Browning"),
       pch = 19, col = c("black", "brown"))


# Kommune-kart ------------------------------------------------------
Viken <- readOGR("Basisdata_30_Viken_25833_Kommuner_GML.gml")
Oslo <- readOGR("Basisdata_03_Oslo_25833_Kommuner_GML.gml")
ogrListLayers("Basisdata_30_Viken_25833_Kommuner_GML.gml")

# Connect Oslo & Viken
# OsloViken<-rbind(Viken,Oslo)
# stopifnot(proj4string(OsloViken) == proj4string(loc_UTM33))  # Check in same projection before combining!
# plot(Viken)
# plot(Oslo, col = "blue", add = T) #Lagt til Oslo
# points(stations[stations$cam_mod=="Reconyx",2],
#        stations[stations$cam_mod=="Reconyx",3],pch=19,col="black")
# points(stations[stations$cam_mod=="Browning",2],
#        stations[stations$cam_mod=="Browning",3],pch=19,col="brown")
# points(stations[stations$abc=="B",2],
#        stations[stations$abc=="B",3],pch="--",col="white", cex=0.8)
# points(stations[stations$abc=="C",2],
#        stations[stations$abc=="C",3],pch="|",col="white", cex=0.8)
# legend("bottomleft",legend=c("Reconyx","Browning"),
#        pch = 19, col = c("black", "brown"))

# coordinates(stations) <- ~ x + y
# raster::spplot(Viken, "navn", scales = list(draw = TRUE), xlab = "easting", ylab = "northing", #col.regions = rainbow(99, start=.1),
#                sp.layout=list("sp.points", stations[,2:3], cex = 2, pch = 20, col="green"), 
#                main = "Oslo & Viken", sub = "Kommunegrenser", col = "grey", maxpixels = 1000) #sp.points må nok vere i eit glm-format
# 
# 
# Studieområde - base R------------------------------------
plot(Viken,xlim=c(190000,290000),ylim=c(6580000,6720000))
invisible(text(coordinates(OsloViken),
               labels = as.character(OsloViken$navn), cex=0.5)) #Kommunenamn

points(stations[stations$cam_mod=="Reconyx",2],
       stations[stations$cam_mod=="Reconyx",3],pch=19,col="black")
points(stations[stations$cam_mod=="Browning",2],
       stations[stations$cam_mod=="Browning",3],pch=19,col="brown")
points(stations[stations$abc!="A",2], stations[stations$abc!="A",3], pch=16 ,col="black", cex=0.5)
legend("bottomleft",legend=c("Browning","Reconyx"),
       pch = 19, col = c("brown", "black"))



#_____________________________________________________

covs<-readRDS("CTloc_covs.rds") %>% mutate(loc = LokalitetID)
class(covs)
str(covs)
statcov <- stations %>%
        left_join(covs, by = "loc")
class(statcov)

# covs<-as.data.frame(covs) # Chaning class to data.fram and not a sf data.frame
#_____________________________________________________


## ------------------------------------------------------------------------
library(sf)
library(tidyverse)

ju_sfg <- st_point(c(-134.4333, 58.3059)) #Juneau
an_sfg <- st_point(c(-149.8631, 61.2174)) #Anchorage
fa_sfg <- st_point(c(-147.7767, 64.8354)) #Fairbanks
nm_sfg <- st_point(c(-165.4064, 64.5011)) #Nome





## ------------------------------------------------------------------------
## Create MULTIPOINT object
# loc_points <- data.frame(x=stations$x,y=stations$y)
# loc_points <- SpatialPoints(loc_points,proj4string=CRS("+proj=utm +zone=32 +datum=WGS84"))
# loc_UTM33 <- spTransform(loc_points,CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# stations$x <- loc_UTM33$x
# stations$y <- loc_UTM33$y

loc_sfg <- st_multipoint(cbind(stations$x,stations$y))
plot(loc_sfg)

loc_sfc <- st_sfc(loc_sfg, crs = 23032) #23032 for utm zone 32 (23033 for utm33)
st_crs(loc_sfc)
plot(loc_sfc)

# Create data.frame with attributes
loc_df <- data.frame(loc = stations$loc,
                        cam_mod = stations$cam_mod,
                        abc = stations$abc)

# Combine data.frame and spatial data
loc_sf <- st_sf(loc_df, geometry = loc_sfc)

loc_sf

class(loc_sf)

str(loc_sf)


## ------------------------------------------------------------------------
Viken <- rgdal::readOGR("Basisdata_30_Viken_25833_Kommuner_GML.gml")
Oslo <- rgdal::readOGR("Basisdata_03_Oslo_25833_Kommuner_GML.gml")
Viken_<-rbind(Viken,Oslo)

class(Viken_)
### Covert from SpatialPolygonsDataframe to sf
Viken_sf <- st_as_sf(Viken_)
### Set CRS to utm zone 33 (23033 for utm33)
Viken_sf <- st_transform(Viken_sf, crs = 23032)

### View object
Viken_sf



## For å hente ut koordinater til å plotte tekst, til dømes
loc_sf <- mutate(loc_sf, 
                   x = purrr::map_dbl(geometry, 1), 
                   y = purrr::map_dbl(geometry, 2))


ggplot(data = Viken_sf) +
        geom_sf() +     # Viken
        geom_sf_label(aes(label = navn)) + #denne kommandoen kan også hente ut namnelapper
        geom_sf(data = loc_sf, color = "red", size = 3,   # Cities
        show.legend = "point") +
        theme_minimal() +
        theme(axis.title = element_blank()) 

## ------------------------------------------------------------------------

# plotting all norway-maps
library(maps)
x <- map('world', 'Norway', names = TRUE, plot = FALSE)
for (i in x) { map("world",i) }



#tmap has interactive leaflet maps:
library(tmap)
qtm(nc)
tmap_mode("view")
## tmap mode set to interactive viewing
tm_shape(nc) + tm_fill("BIR74", palette = sf.colors(5))