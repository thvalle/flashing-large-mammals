# Kartografiske verktøy i R, fra forelesning til Tom 25. oktober

# R-pakke: maps

library(maps)
library(mapdata)
map("worldHires", xlim= c(5, 15), ylim=c(58, 64)) # lager kart over sørnorge




# R-pakke: sp

library(sp)

data(meuse)
coordinates(meuse) <- c("x", "y")

plot(meuse)
plot(meuse, pch = 19, cex = sqrt(meuse$zinc)/20, 
     col = gray.colors(20)[cut(meuse$zinc, 20)])


# Tom forklarer oss funksjonen cut
x <- rnorm(2)
x
x <- rnorm(20)
cut(x, 5)


# R-pakke: rgdal

library(rgdal)

UTM33.WGS84 <- CRS("+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
GEO.WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


N1K <- read.table("NIVA.N1K.txt", header = TRUE)
k <- which(complete.cases(N1K[,c(20,21)]))
N1K.sp <- SpatialPointsDataFrame(N1K[k,c(20,21)],
                                 N1K[k,c(13,1,2,5,6,7,28)], proj4string = UTM33.WGS84)
N1K.sp <- spTransform(N1K.sp, GEO.WGS84)
writeOGR(N1K.sp, "NIVA.N1K.kml", "NIVA.N1K", driver = "KML")


library(maps) # Crude world map, needs to installed first

par(mar = c(0,0,0,0)) # Maximize plot area
map("world","norway", xlim = c(5,31), ylim = c(58,72))
points(N1K.sp, pch = 19, col = topo.colors(20)[cut(N1K.sp$TOC,20)])

library(mapdata) # Higher resolution maps, needs maps package
par(mar = c(0,0,0,0)) # Maximize plot area
map("worldHires","norway", xlim = c(5,31), ylim = c(58,72))
points(N1K.sp, pch = 19, col = topo.colors(20)[cut(N1K.sp$TOC,20)])
legend("bottomright", levels(cut(N1K.sp$TOC,20)), pch=19, col = topo.colors(20))




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


