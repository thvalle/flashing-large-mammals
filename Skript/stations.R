library(tidyverse)
library(here)
library(sp)     # vector tools
library(raster) # raster tools
library(rgdal)  # spatial data tools (geospatial data abstraction library)
library(fields) # colour ramps (e.g. tim.colors)
library(rgbif)  # global biodiversity data (GBIF) tools

# True setup -------------------------------
stations <- read.csv("kam_geo/kameraliste.csv", header = T)
names(stations)
stations <- stations[c(3,9,10,2,7)] #subset variables I want
names(stations) <- c("loc","x", "y", "abc","cam_mod")
names(stations) #worked
stations <- stations[1:60,] #cut away info below observations
nrow(stations) #it worked

#Rename camera types to include only Browning and Reconyx
names(stations) <- c("loc","x", "y", "abc","cam_mod")
names(stations) #worked
stations <- stations[1:60,] #cut away info below observations
nrow(stations) #it worked

#Rename camera types to include only Browning and Reconyx
unique(stations$cam_mod)
stations[stations$cam_mod=="Browning (+reconyx)",5] <- "Browning" # 5 marks the cam_mod colon
stations[stations$cam_mod=="RECONYXINFRA",5] <- "Reconyx"
stations[stations$loc == 15,5] <- "Reconyx" 
stations$cam_mod <- factor(stations$cam_mod, levels= c("Reconyx","Browning"))
levels(stations$cam_mod)

# write.csv(stations,'stations.csv', row.names = FALSE)
stations <- read.csv('stations.csv')



loc_points <- data.frame(x=stations$x,y=stations$y)
loc_points <- SpatialPoints(loc_points,proj4string=CRS("+proj=utm +zone=32 +datum=WGS84"))
loc_UTM33 <- spTransform(loc_points,CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
stations$x <- loc_UTM33$x
stations$y <- loc_UTM33$y


# plotting
plot(stations$x, stations$y, type="n", xlab="cam_mod",ylab="")
points(stations[stations$cam_mod=="Reconyx",2],
       stations[stations$cam_mod=="Reconyx",3],pch=19,col="black")
points(stations[stations$cam_mod=="Browning",2],
       stations[stations$cam_mod=="Browning",3],pch=19,col="brown")
points(stations[stations$abc=="B",2],
       stations[stations$abc=="B",3],pch="B",col="black", cex=0.8)
points(stations[stations$abc=="C",2],
       stations[stations$abc=="C",3],pch="C",col="black", cex=0.8)
legend("bottomleft",legend=c("Reconyx","Browning"),
       pch = 19, col = c("black", "brown"))


# Kommune-kart ------------------------------------------------------
Viken <- readOGR(here("kam_geo","Nedlastingspakke","Basisdata_30_Viken_25833_Kommuner_GML.gml"))
Oslo <- readOGR(here("kam_geo","Nedlastingspakke","Basisdata_03_Oslo_25833_Kommuner_GML.gml"))
ogrListLayers(here("kam_geo","Nedlastingspakke","Basisdata_30_Viken_25833_Kommuner_GML.gml"))

# Connect Oslo & Viken
OsloViken<-rbind(Viken,Oslo)
stopifnot(proj4string(OsloViken) == proj4string(loc_UTM33))  # Check in same projection before combining!
plot(Viken)
plot(Oslo, col = "blue", add = T) #Lagt til Oslo
points(stations[stations$cam_mod=="Reconyx",2],
       stations[stations$cam_mod=="Reconyx",3],pch=19,col="black")
points(stations[stations$cam_mod=="Browning",2],
       stations[stations$cam_mod=="Browning",3],pch=19,col="brown")
points(stations[stations$abc=="B",2],
       stations[stations$abc=="B",3],pch="--",col="white", cex=0.8)
points(stations[stations$abc=="C",2],
       stations[stations$abc=="C",3],pch="|",col="white", cex=0.8)
legend("bottomleft",legend=c("Reconyx","Browning"),
       pch = 19, col = c("black", "brown"))

coordinates(stations) <- ~ x + y
raster::spplot(Viken, "navn", scales = list(draw = TRUE), xlab = "easting", ylab = "northing", #col.regions = rainbow(99, start=.1),
               sp.layout=list("sp.points", stations[,2:3], cex = 2, pch = 20, col="green"), 
               main = "Oslo & Viken", sub = "Kommunegrenser", col = "grey", maxpixels = 1000) #sp.points må nok vere i eit glm-format


#Studieområde - base R------------------------------------
#plot differing factors in differing colours
plot(Viken,xlim=c(190000,290000),ylim=c(6580000,6720000))
invisible(text(coordinates(OsloViken), labels=as.character(OsloViken$navn), cex=0.5)) #Kommunenamn

points(stations[stations$cam_mod=="Reconyx",2],
       stations[stations$cam_mod=="Reconyx",3],pch=19,col="black")
points(stations[stations$cam_mod=="Browning",2],
       stations[stations$cam_mod=="Browning",3],pch=19,col="brown")
# points(stations[stations$abc=="B",2], stations[stations$abc=="B",3],pch="B",col="black", cex=0.8)
# points(stations[stations$abc=="C",2], stations[stations$abc=="C",3],pch="C",col="black", cex=0.8)
legend("bottomleft",legend=c("Browning","Reconyx"),
       pch = 19, col = c("brown", "black"))


# R-pakke: sp

library(sp)

data(meuse)
coordinates(meuse) <- c("x", "y")

plot(meuse)
plot(meuse, pch = 19, cex = sqrt(meuse$zinc)/20, 
     col = gray.colors(20)[cut(meuse$zinc, 20)])

# R-pakke: rgdal

library(rgdal)

UTM33.WGS84 <- CRS("+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
GEO.WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


k <- which(complete.cases(stations[,c(2,3)]))
# N1K.sp <- spTransform(stations[,2:3], GEO.WGS84)

library(maps) # Crude world map, needs to installed first

par(mar = c(0,0,0,0)) # Maximize plot area
map("world","norway", xlim = c(5,31), ylim = c(58,72))
points(N1K.sp, pch = 19, col = topo.colors(20))

library(mapdata) # Higher resolution maps, needs maps package
par(mar = c(0,0,0,0)) # Maximize plot area
map("worldHires","norway", xlim = c(5,31), ylim = c(58,72))




# ESRI shape-filformat

library(rgdal)
shore <- readOGR("Riskedalsvatnet.shp","Riskedalsvatnet")
plot(shore, col = "lightblue", lwd = 3)


# R-pakke: raster

library(raster)
alt <- getData('alt', country='NOR', mask=TRUE)
plot(alt)
??Worldclim

# worldclim, gir globale klimatiske data, scalert med 10 (alt ganget med 10)
library(raster)
w = ?getData('worldclim', var='tmin', res=0.5, lon=5, lat=45)
plot(w)

b = getData('worldclim', var='bio', res=0.5, lon=5, lat=45)
plot(b)

getData('ISO3') #169! 'NOR' Norway
c = getData('GADM',country='NOR',level=2)
plot(c)

r <- getData("worldclim",var="bio",res=10)
r1 <- r[[c(1,12)]]
names(r1) <- c("Temp","Prec")
points <- spsample(as(r@extent, 'SpatialPolygons'),n=100, type="random")    
values <- extract(r,points)
df <- cbind.data.frame(coordinates(points),values)
head(df)
plot(r[[1]])
plot(points,add=T)


# spatial data operations ---------------------------

library(sf)
library(raster)
library(dplyr)
library(spData)

plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")


# TIFF_GRAIN from DistLab -------------------------------------

# 1. All of Norway
r.list <- list.files("TIFF_GRAIN/NORWAY/10km", pattern="10km.tif$", full.names=TRUE)
predictors_Norway <- stack(r.list)

# 1. All of Norway
r.list <- list.files("TIFF_GRAIN/NORWAY/10km", pattern="10km.tif$", full.names=TRUE)
predictors_Oslo <- stack(r.list)









