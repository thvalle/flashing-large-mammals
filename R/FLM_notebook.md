Flashing Large Mammals - exploring the data
================
Torgeir Holmgard Valle
05 mars, 2021

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.6     v dplyr   1.0.4
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(overlap)
# All the tibbles from Data_explor_setup.R, and package + function from Density.R
obs      <- readRDS("Observations_prepared1.rds") %>%
  mutate(species = validated_species)
stations <- readRDS("stations.rds")
# Tweaking obs to resemble time.dep in glmm_sp.Rmd
ctrl <- c("Control_1", "Control_2", "Control_3","Control_4")
effort   <- readRDS("Effort_prepared.rds") %>%
    mutate(flash = factor(
            ifelse(period %in% ctrl, "Control", flash)), # Including Control in flash
          week = lubridate::isoweek(date), # extracting week-column
          period = factor(period, labels = c("IR_1", "IR_2", "LED_1", "LED_2", # turning period into a factor
                          "Control_1", "Control_2", "Control_3", "Control_4"))) # and relabeling periods
effort <- effort %>% 
   mutate(flash = factor(flash, levels = c("Control","0","1"),
                ordered = T, labels = c("Control","IR","LED"))) # relevel to make Control the model intercept
```

# Presenting the data

``` r
min(obs$date) # [1] "2019-01-15"
```

    ## [1] "2019-01-15"

``` r
max(obs$date) # [1] "2020-02-17"  #but last field date was 2020-02-26 at camera 850 & 822!!
```

    ## [1] "2020-02-26"

``` r
sp_focus <- c("ekorn", "elg", "grevling", "hare", "raadyr", "rev")
obstation <- obs %>% left_join(stations, by = "loc") %>% filter(species %in% sp_focus) %>% unite(abc, abc, flash) # unite lager ny faktor med alle kombinasjoner

#obstation %>% with(table(species, abc)) %>% 
 # knitr::kable(caption = "Table with kable")

# Total nr of image (rapidfire)series can be presented by:
length(unique(obs$timeserie_id)) # identical numbers for timeserie_id and image_id
```

    ## [1] 25710

``` r
length(unique(obs$image_id))     # is it because only one picture is selected per timeseries?
```

    ## [1] 25710

``` r
obs_by_loc <- obs %>% group_by(loc) %>% summarise(n = length(timeserie_id)) %>% arrange(desc(n))
# plot(obs_by_loc)
```

## Timeseries plot of periods from the troubleshooting and organisation with Neri

``` r
p_eff <- effort %>%
  mutate(trt_gr = ifelse(period %in% ctrl, "Control", "Treatment")) %>% 
ggplot() +
  facet_grid(rows = "trt_gr", scales = "free_y", space = "free_y") +
  geom_point(aes(date, as.factor(loc), col=period), size = 0.9) +
  theme_classic() + labs(y = "Location") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "top") +
  scale_color_manual(values = c(rep(c("#abd9e9","#4575b4"),each=2), # trt-colr
                                rep(c("#f46d43","#fdae61"),2) ) ) + #ctrl-colr
  guides(colour = guide_legend(nrow = 2, override.aes = list(size = 2)), #adjust rows of legend
               x = guide_axis(n.dodge = 2)) # dodge axis text into n rows
               # x = guide_axis(angle = -20)) + #adjust axis-text angle
p_eff +
  scale_x_date(NULL, breaks = f_work, date_labels = "%d-%b",
               sec.axis = sec_axis(~., breaks = c(c$c,f_work[10]), labels = scales::date_format("%d-%b %y") ) ) +
  geom_vline(xintercept = f_work, linetype = "dashed",  alpha =.5) +
  #geom_rect(aes(xmin = f_work[c(1,3,5,7,9)], xmax = f_work[c(2,4,6,8,10)],
  #              ymin = rep(-Inf,5), ymax = rep(-Inf,5), fill = "grey"), alpha = 0.4)
  #geom_rect(aes(x=f_work[1] ,y=Inf) )
  #geom_rect(aes(xmin=df$xmin,xmax=df$xmax,ymin=df$ymin,ymax=df$ymax), col="grey") 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf),
            alpha = 0.2 )
```

![](FLM_notebook_files/figure-gfm/effort-facet-1.png)<!-- -->

## Which species did we get?

``` r
sp_all <- c("hare", "elg", "rev", "grevling", "maar", "gaupe", "ekorn", "raadyr", "hjort") # interesting species with enough datapoints
by_sp <- obs %>%
  left_join(stations, by = "loc") %>%
  group_by(species)
passes <- summarise(by_sp,
  count = n(),   # flashed = mean(flash, na.rm = T), # don't know if i can find a relevant use of this
  abc = abc, period = period, flash = flash)
```

    ## `summarise()` has grouped output by 'species'. You can override using the `.groups` argument.

``` r
passes %>% 
  filter(!is.na(species), !(species %in% "nothing")) %>% 
  ggplot() +
  geom_bar(aes(reorder(species, count, FUN = mean)), position = "dodge") +  # shows actual counts of each species
  geom_hline(yintercept = 200) + coord_flip() # flip the axes
```

![](FLM_notebook_files/figure-gfm/sp_focus-1.png)<!-- -->

``` r
# most datapoints on ekorn, elg, grevling, hare, raadyr, rev, 
fjern <- c("nothing","hund", "menneske", "kjoeretoey", "motorsykkel", "ukjent", "sau", "ku", "fugl") # uninteresting or too general groups
p_sp_focus <- passes %>% 
  filter(count > 200, !(species %in% fjern), !is.na(species)) %>%  # removing low counts and sp in 'fjern'
  ggplot() + scale_fill_brewer() + geom_hline(yintercept = 200) + coord_flip()
p_sp_focus + geom_bar(aes(reorder(species, count, FUN = mean)), position = "dodge") + geom_hline(yintercept = 200) # same as first plot, more filtered
```

![](FLM_notebook_files/figure-gfm/sp_focus-2.png)<!-- -->

``` r
p_sp_focus + geom_bar(aes(abc, fill = species), position = "dodge")  # same as first plot, more filtered
```

![](FLM_notebook_files/figure-gfm/sp_focus-3.png)<!-- -->

``` r
p_sp_focus + geom_bar(aes(period, fill = species), position = "dodge")
```

![](FLM_notebook_files/figure-gfm/sp_focus-4.png)<!-- -->

``` r
p_sp_focus + geom_bar(aes(flash, fill = species), position = "dodge")
```

![](FLM_notebook_files/figure-gfm/sp_focus-5.png)<!-- -->

Removing sightings of “nothing”, things related to humans, as well as
NAs and birds (too general group), I am left with quite a few species
still. Filtering for counts lower than 200 renders me the selection
shown in the dark themed plots.

Roe deer and red fox are the species with decidedly most data. The flash
true/false-plot reveals similar proportions of detection abundances
between the eight most common species.

## Density plots showing activity patterns with and without flash

``` r
overlap_flash("rev", "red", rug = TRUE) 
```

![](FLM_notebook_files/figure-gfm/overlap-flash-1.png)<!-- -->

``` r
overlap_flash_2sp("rev", "raadyr", "red", "blue", rug=TRUE)
```

![](FLM_notebook_files/figure-gfm/overlap-flash-2.png)<!-- -->

Sites without white flash produces a more bumpy curve than does the
sites with a flash. Proportion of foxes at sites are markedly lower
before sunrise, and then higher afterwards. Could this simply be because
of a lower detection rate when lacking the additional white flash? There
is a resembling phenomenon happening in the evening twilight as well,
right before the peak activity time of the fox, which happens before
midnight.

The Dhat4 calculation reveals a larger difference in activity for foxes,
than for roe deer, but seems to mainly stem from the twilight hours.
Thus, it could be because of the size of the animals, rather than a
reaction to the flash.

### One plot for all species

``` r
sp <- c("rev", "raadyr", "grevling", "elg", "hjort", "gaupe")
par(mfrow = c(2, 3))
 par(cex = 0.6)
 par(mar = c(0, 0, 0, 0), oma = c(4, 4, 0.5, 0.5))
 par(tcl = -0.25)
 par(mgp = c(2, 0.6, 0))
for (i in sp[1:3]) {
 overlap_flash(i, axes = FALSE,  ann=F, rug=T, xaxt="n")
  legend("top", legend = str_c("\n ",i), bty = "n") # art
}
overlap_flash(sp[4], main=NULL, rug=T) 
   legend("top", legend = str_c("\n ",sp[4]), bty = "n") # art 
   legend(x=-4.2, y=0.018, legend = c("IR", "white LED"), #x+y sets legend position
         col = c("black","blue"), lty = c(1, 2), lwd = 2, bty = "o")
for (i in sp[5:6]){
 overlap_flash(i, axes = FALSE,  ann=F, rug=T) 
   legend("top", legend = str_c("\n ",i), bty = "n") # art
}
 mtext("Time", side = 1, outer = TRUE, cex = 0.7, line = 2.2,
 col = "grey20")
 mtext("Density", side = 2, outer = TRUE, cex = 0.7, line = 2.2,
 col = "grey20")
```

![](FLM_notebook_files/figure-gfm/overlap-flash-sp-1.png)<!-- -->

### Controlling for day-length

``` r
# Dates <- as.POSIXct(obs$date, tz="CET")
# coords <- matrix(c(60.1, 10.6), nrow=1) # Rett nord for Oslo i longlat
# Coords <- sp::SpatialPoints(coords, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
# st <- sunTime(obs$rad, Dates, Coords)
# 
# par(mfrow=2:1)
# densityPlot(st, col='red', lwd=2, xaxt='n', main="Sun time")
# axis(1, at=c(0, 6, 12, 18, 24),
#   labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
# densityPlot(simCalls$time, lwd = 2, main = "Clock time")
# par(mfrow=c(1,1))

# TODO # Sjå density.r for oppdatert skript 
```

## Box plots

### of periods

``` r
freq     <- readRDS("freq.rds") %>%  #from line 79 in Data_exploration2_nesting.R
  mutate(species = validated_species)
# Setting 20 days as an arbitrary limit
freq <- freq[freq$n.days > 19, ]
# frequency on periods, single or plural species
sp <- c("elg", "grevling","hjort", "maar", "raadyr", "rev") # Species with the most datapoints
p_freq_per <- freq %>% # plot of frequency grouped by period
  filter(species %in% sp) %>%
  left_join(stations, by = "loc") %>%
  ggplot(aes(period, freq)) +
  facet_wrap(~species, scales = "free", nrow = 2) +
  labs(title = "Frequencies of species in periods")

# coloured with abc-grouping
p_freq_per + geom_boxplot(outlier.shape = NA) + # Remove outliers when overlaying boxplot with original data points
  geom_jitter(aes(col = abc), width = 0.1) + coord_flip() # add points for each camera
```

![](FLM_notebook_files/figure-gfm/box_plots_periods-1.png)<!-- -->

### of flash on/of

``` r
freq %>% # plot of frequency grouped by abc and flash T/F
  filter(species %in% sp) %>%
  left_join(stations, by = "loc") %>%
  ggplot(aes(abc, freq)) +
  facet_wrap(~species, scales = "free", nrow = 2) + # scales = "free"
  labs(title = "Frequencies of species w/ | w/o flash") +
  geom_boxplot(aes(fill = flash))
```

![](FLM_notebook_files/figure-gfm/box_abc_flash-1.png)<!-- -->

------------------------------------------------------------------------

# Some final plots I want to add in my thesis

## Maps

    ## options:        GML 
    ## Reading layer `Kommune' from data source `C:\Users\Nora.Familie-PC\Documents\TorgeiR\flm\R\Basisdata_30_Viken_25833_Kommuner_GML.gml' using driver `GML'
    ## Integer64 values larger than 9.0072e+15 lost significance after conversion to double;
    ## use argument int64_as_string = TRUE to import them lossless, as character
    ## Simple feature collection with 52 features and 10 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 82868.22 ymin: 6521821 xmax: 328157.6 ymax: 6791455
    ## projected CRS:  ETRS89 / UTM zone 33N

    ## options:        GML 
    ## Reading layer `Kommune' from data source `C:\Users\Nora.Familie-PC\Documents\TorgeiR\flm\R\Basisdata_03_Oslo_25833_Kommuner_GML.gml' using driver `GML'
    ## Simple feature collection with 1 feature and 10 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 248657.3 ymin: 6637442 xmax: 273925.9 ymax: 6674529
    ## projected CRS:  ETRS89 / UTM zone 33N

    ## Simple feature collection with 60 features and 3 fields
    ## geometry type:  MULTIPOINT
    ## dimension:      XY
    ## bbox:           xmin: 516948 ymin: 6581339 xmax: 619655 ymax: 6701802
    ## projected CRS:  ED50 / UTM zone 32N
    ## First 10 features:
    ##     loc  cam_mod abc                       geometry
    ## 1  1254 Browning   B MULTIPOINT ((516948 6701802...
    ## 2   855 Browning   A MULTIPOINT ((516948 6701802...
    ## 3   824 Browning   C MULTIPOINT ((516948 6701802...
    ## 4   828 Browning   C MULTIPOINT ((516948 6701802...
    ## 5  1255 Browning   A MULTIPOINT ((516948 6701802...
    ## 6   834 Browning   B MULTIPOINT ((516948 6701802...
    ## 7   825 Browning   B MULTIPOINT ((516948 6701802...
    ## 8   829 Browning   C MULTIPOINT ((516948 6701802...
    ## 9   863 Browning   A MULTIPOINT ((516948 6701802...
    ## 10  861 Browning   A MULTIPOINT ((516948 6701802...

    ## [1] "sf"         "data.frame"

    ## Classes 'sf' and 'data.frame':   60 obs. of  4 variables:
    ##  $ loc     : int  1254 855 824 828 1255 834 825 829 863 861 ...
    ##  $ cam_mod : Factor w/ 2 levels "Reconyx","Browning": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ abc     : chr  "B" "A" "C" "C" ...
    ##  $ geometry:sfc_MULTIPOINT of length 60; first list element:  'XY' int [1:60, 1:2] 516948 530220 529884 529481 528206 534627 535199 536947 540277 540095 ...
    ##  - attr(*, "sf_column")= chr "geometry"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA
    ##   ..- attr(*, "names")= chr [1:3] "loc" "cam_mod" "abc"

``` r
library(maps)
```

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
library(tmap)
Norge <- st_as_sf(map("world",'Norway', plot = FALSE, fill = TRUE))
ggplot() + geom_sf(data = Norge)
```

![](FLM_notebook_files/figure-gfm/maps-scandi-1.png)<!-- -->

``` r
Scandinavia <- st_as_sf(map("world",c("Norway","Sweden", "Finland","Denmark","UK", "Germany"), plot = F, fill = T))

ggplot() +
  geom_sf(data =Scandinavia) +
  geom_sf(data = Viken, fill = "blue", alpha = .2) +  # Viken polygon
  annotate("rect", xmin = 7.3, xmax = 13,
           ymin = 58.5, ymax = 61.2,
           alpha = .4) +
  coord_sf(xlim = c(0,25), ylim = c(57,71)) +
  theme_bw()
```

![](FLM_notebook_files/figure-gfm/maps-scandi-2.png)<!-- -->

``` r
# ggplot(Viken) +
#   stat_sf_coordinates()
```

``` r
loc_sf <- mutate(loc_sf, 
                   x = purrr::map_dbl(geometry, 1), 
                   y = purrr::map_dbl(geometry, 2))
ggplot(data = Viken) +
        geom_sf() +     # Viken
        geom_sf_label(aes(label = navn)) + #denne kommandoen kan også hente ut namnelapper
        geom_sf(data = loc_sf, color = "red", size = 3,   # CT sites
        show.legend = "point") +
        theme_minimal() +
        theme(axis.title = element_blank()) +
        coord_sf(xlim = range(loc_sf$x), ylim = range(loc_sf$y))
```

![](FLM_notebook_files/figure-gfm/maps-1.png)<!-- -->

``` r
#  xlim(c(190000,290000)) + ylim(c(6580000,6720000))
```

``` r
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
ggplot(nc) +
  geom_sf(aes(fill = AREA))

# If not supplied, coord_sf() will take the CRS from the first layer
# and automatically transform all other layers to use that CRS. This
# ensures that all data will correctly line up
nc_3857 <- sf::st_transform(nc, 3857)
ggplot() +
  geom_sf(data = nc) +
  geom_sf(data = nc_3857, colour = "red", fill = NA)

# Unfortunately if you plot other types of feature you'll need to use
# show.legend to tell ggplot2 what type of legend to use
nc_3857$mid <- sf::st_centroid(nc_3857$geometry)
ggplot(nc_3857) +
  geom_sf(colour = "white") +
  geom_sf(aes(geometry = mid, size = AREA), show.legend = "point")

# You can also use layers with x and y aesthetics: these are
# assumed to already be in the common CRS.
ggplot(nc) +
  geom_sf() +
  annotate("point", x = -80, y = 35, colour = "red", size = 4)

# Thanks to the power of sf, a geom_sf nicely handles varying projections
# setting the aspect ratio correctly.
library(maps)
world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
ggplot() + geom_sf(data = world1)
map()
world2 <- sf::st_transform(
  world1,
  "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"
)
ggplot() + geom_sf(data = world2)

# To add labels, use geom_sf_label().
ggplot(nc_3857[1:3, ]) +
   geom_sf(aes(fill = AREA)) +
   geom_sf_label(aes(label = NAME))
```

``` r
map()   # low resolution map of the world
map(wrap = c(0,360), fill = TRUE, col = 2) # pacific-centered map of the world
map(wrap = c(0, 360, NA), fill = TRUE, col = 2) # idem, without Antarctica
map('usa')  # national boundaries
map('county', 'new jersey') # county map of New Jersey
map('state', region = c('new york', 'new jersey', 'penn'))  # map of three states
map("state", ".*dakota", myborder = 0)  # map of the dakotas
map.axes()              # show the effect of myborder = 0
if(require(mapproj))
  map('state', proj = 'bonne', param = 45)  # Bonne equal-area projection of states

# names of the San Juan islands in Washington state
map('county', 'washington,san', names = TRUE, plot = FALSE)

# national boundaries in one linetype, states in another
# (figure 5 in the reference)
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)

# plot the ozone data on a base map
# (figure 4 in the reference)
data(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()
```

### Blank images

``` r
# Filtering out timelapse-pictures in Reconyx (455 is the only Browning-camera that has significant number of pictures here)
timelapse <- obs %>% left_join(stations, by = "loc") %>%
  filter(hour %in% c(8, 13) & mins == 0  &  secs == 0  &  species == "nothing") # == "nothing" to single out tinelapse-photos
summary(timelapse)
table(timelapse$hour[timelapse$cam_mod =="Reconyx"])

table(timelapse$cam_mod)


# table(timelapse$loc[timelapse$cam_mod == "Browning"])  #455 maybe has two cameras?
# table(timelapse$species)  # when val_sp == "nothing" not specified: 10 other species present
# table(timelapse$species[timelapse$hour == 13]) # elg:1, maar:2, menneske:3, raadyr:1
# table(timelapse$species[timelapse$hour == 8]) # ekorn:1, fugl:9, gaupe:1, katt:1, menneske:1, raadyr:4, rev:2, ukjent:4

# Differentiate "nothing"-sequences and "nothing"-one-shots
# species == "nothing" #by loc,
# and making an "independent event" criteria to distinguish repeated vegetation-triggering and other single-triggered events. 
# TODO
library(data.table)
setDT(obs)[, event_id := 1L + cumsum(c(0L, diff(datetime) > 720)), by=.(loc, species)]

# TODO obs %>% group_by(species, event_id) %>% filter(species == "nothing") %>% table(obs$loc)


names(obs)
```

# Session Info

``` r
sessionInfo()
```

    ## R version 4.0.3 (2020-10-10)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19041)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Norwegian Bokmål_Norway.1252 
    ## [2] LC_CTYPE=Norwegian Bokmål_Norway.1252   
    ## [3] LC_MONETARY=Norwegian Bokmål_Norway.1252
    ## [4] LC_NUMERIC=C                            
    ## [5] LC_TIME=Norwegian Bokmål_Norway.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] tmap_3.3          maps_3.3.0        sf_0.9-7          overlap_0.3.3    
    ##  [5] lubridate_1.7.9.2 forcats_0.5.0     stringr_1.4.0     dplyr_1.0.4      
    ##  [9] purrr_0.3.4       readr_1.4.0       tidyr_1.1.2       tibble_3.0.6     
    ## [13] ggplot2_3.3.3     tidyverse_1.3.0  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] httr_1.4.2         viridisLite_0.3.0  jsonlite_1.7.2     modelr_0.1.8      
    ##  [5] assertthat_0.2.1   sp_1.4-5           highr_0.8          cellranger_1.1.0  
    ##  [9] yaml_2.2.1         lattice_0.20-41    pillar_1.4.7       backports_1.2.1   
    ## [13] glue_1.4.2         digest_0.6.27      RColorBrewer_1.1-2 rvest_0.3.6       
    ## [17] colorspace_2.0-0   htmltools_0.5.1.1  XML_3.99-0.5       pkgconfig_2.0.3   
    ## [21] raster_3.4-5       broom_0.7.4        haven_2.3.1        stars_0.5-1       
    ## [25] scales_1.1.1       generics_0.1.0     farver_2.0.3       ellipsis_0.3.1    
    ## [29] withr_2.4.1        leafsync_0.1.0     cli_2.3.0          magrittr_2.0.1    
    ## [33] crayon_1.4.1       readxl_1.3.1       evaluate_0.14      fs_1.5.0          
    ## [37] xml2_1.3.2         lwgeom_0.2-5       class_7.3-17       tools_4.0.3       
    ## [41] hms_1.0.0          lifecycle_0.2.0    munsell_0.5.0      reprex_0.3.0      
    ## [45] compiler_4.0.3     e1071_1.7-4        rlang_0.4.10       classInt_0.4-3    
    ## [49] units_0.6-7        grid_4.0.3         tmaptools_3.1-1    dichromat_2.0-0   
    ## [53] rstudioapi_0.13    htmlwidgets_1.5.3  crosstalk_1.1.1    base64enc_0.1-3   
    ## [57] leafem_0.1.3       labeling_0.4.2     rmarkdown_2.6      gtable_0.3.0      
    ## [61] codetools_0.2-16   abind_1.4-5        DBI_1.1.1          R6_2.5.0          
    ## [65] knitr_1.31         KernSmooth_2.23-17 stringi_1.5.3      parallel_4.0.3    
    ## [69] Rcpp_1.0.6         png_0.1-7          vctrs_0.3.6        leaflet_2.0.4.1   
    ## [73] dbplyr_2.0.0       tidyselect_1.1.0   xfun_0.20

``` r
# packrat
# checkpoint
```

If you want your code to be reproducible in the long-run (i.e. so you
can come back to run it next month or next year), you’ll need to track
the versions of the packages that your code uses. A rigorous approach is
to use *packrat*, [link](http://rstudio.github.io/packrat/), which
stores packages in your project directory, or *checkpoint*,
[link](https://github.com/RevolutionAnalytics/checkpoint), which will
reinstall packages available on a specified date. A quick and dirty hack
is to include a chunk that runs sessionInfo() — that won’t let you
easily recreate your packages as they are today, but at least you’ll
know what they were.
