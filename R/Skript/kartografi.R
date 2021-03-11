library(sf)
library(tidyverse);library(tidyr)
v <- st_read("Basisdata_30_Viken_25833_Kommuner_GML.gml", drivers = "GML", layer = "Kommune")
o <- st_read("Basisdata_03_Oslo_25833_Kommuner_GML.gml",  drivers = "GML", layer = "Kommune")
viken<-rbind(v,o) #legg Oslo inn i Viken
Viken <- viken[8]
### Transform CRS to utm zone 32 (23033 for utm33)
Viken <- st_transform(Viken, crs = 23032)
## automatically selected the first layer in a data source containing more than one.Reading layer `Kommune' from data source `C:\Users\Nora.Familie-PC\Documents\TorgeiR\flm\R\Basisdata_30_Viken_25833_Kommuner_GML.gml' using driver `GML'
## GDAL Message 1: 64 bit integer overflow when converting 172793172790172913172915173031
# Setting up loc-points
stations <- readRDS("stations.rds")
loc_sfg <- st_multipoint(cbind(stations$x,stations$y)) 
loc_sfc <- st_sfc(loc_sfg, crs = 23032) #23032 for utm zone 32 (23033 for utm33)
# Create data.frame with attributes
loc_df <- data.frame(loc = stations$loc,
                     cam_mod = stations$cam_mod,
                     abc = stations$abc)
# Combine data.frame and spatial data
loc_sf <- st_sf(loc_df, geometry = loc_sfc)
loc_sf
class(loc_sf)
str(loc_sf)
# loc_sf <- mutate(loc_sf, 
#                    x = purrr::map_dbl(geometry, 1), 
#                    y = purrr::map_dbl(geometry, 2))

saveRDS(Viken, "viken.rds")


plot(Viken[8], reset = FALSE)# reset = FALSE: we want to add to a plot with a legend
plot(Viken[8,1], col = 'black', add = TRUE)
plot(o[8], col = 'black', add = TRUE)

plot(Viken, graticule = TRUE, key.pos = NULL, axes = TRUE)
library(maps)
Norge = st_as_sf(map("world", 'Norway', plot = FALSE, fill = TRUE))
Norge <- st_transform(Norge, crs = 23032)



ggplot() + geom_sf(data = Norge) 

demo(nc, ask = FALSE, echo = FALSE)
ggplot() + 
  geom_sf(data = nc, aes(fill = BIR74)) + 
  scale_y_continuous(breaks = 34:36)



library(tmap)


tmap_mode("view")
qtm(Viken) 

tm_shape(Viken) 
+ tm_squares(size = 1, col = NA, shape = 22, scale = 4/3, ...)
ttm()
tmap_last()




# tmap vignettes ----------------------------------------------------------
vignette("tmap-getstarted", package = "tmap")
library(tmap)


leaflet::providers # some of the maps I want to look at:

# "NASAGIBS.ViirsEarthAtNight2012" ALAN illustrasjon!
# "Thunderforest_SpinalMap for Lava-variant av verden
# "Thunderforest_Landscape" |simpelt kart
# "Stamen.Terrain"          |--||--
# "OpenTopoMap"             |--||--
# "OpenStreetMap.Mapnik"    |--||--

tmap_mode("plot")
tm_basemap() + 
  tm_shape(loc_sf) + tm_bubbles(col = "cam_mod", size = 0.5)

loc_sf$cam_mod

ttm()
tmap_last()
# permanent options
opts <- tmap_options(basemaps = c(Canvas = "Esri.WorldGrayCanvas", Imagery = "Esri.WorldImagery"),
                     overlays = c(Labels = paste0("http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
                                                    "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}")))
tmap_mode("plot")
tm_basemap() + 
  tm_shape(Viken) +
  tm_shape(loc_sf) + tm_bubbles(col = "red")
  #tm_polygons(Viken) + #will only draw polygon, not additional layers

tm_shape(World) +
  tm_polygons("HPI") +
  tm_layout(bg.color = "skyblue", inner.margins = c(0, .02, .02, .02))
# map options
tmap_options(bg.color = "black", legend.text.color = "white")
tm_shape(World) +
  tm_polygons("HPI", legend.title = "Happy Planet Index")

tmap_style("classic")
tmap_style("watercolor")
tmap_style("beaver")
tmap_style("cobalt")
tmap_style("natural")
tmap_style("albatross")

tm_shape(World) +
  tm_polygons("HPI", legend.title = "Happy Planet Index")
# see what options have been changed
tmap_options_diff()
# reset the options to the default values
tmap_options_reset()

# restore options
tmap_options(opts)
# restore current mode
tmap_mode(current.mode)
  
tm_shape(World) +
  tm_polygons("HPI") +
  tm_layout(bg.color = "skyblue", inner.margins = c(0, .02, .02, .02))

# Exporting maps
tm <- tm_shape(World) +
  tm_polygons("HPI", legend.title = "Happy Planet Index")
# 
# ## save an image ("plot" mode)
# tmap_save(tm, filename = "world_map.png")
# 
# ## save as stand-alone HTML file ("view" mode)
# tmap_save(tm, filename = "world_map.html")
# 
# tmap_tip()


# Third tmap Vignette -----------------------------------------------------
Viken <- readRDS("viken.rds")
vignette("tmap-changes", package = "tmap")



data(World, metro)
tmap_mode("view")

tm_basemap(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB basemap") +
  tm_shape(World) +
  tm_polygons("HPI", group = "Countries") +
  tm_tiles(leaflet::providers$CartoDB.PositronOnlyLabels, group = "CartoDB labels") +
  tm_shape(metro) +
  tm_dots(col = "red", group = "Metropolitan areas")

tmap_format()
panorama <- tmap_format("World")
panorama$asp <- 6
tmap_format_add(panorama, name = "panorama")
tmap_format()
tm_shape(World) + tm_polygons("HPI") + tm_format("panorama")
tmap_mode("plot")
tm_shape(World) + tm_polygons(Viken) + tm_format("panorama")

qtm(World, "HPI", fill.palette = "-plasma")
