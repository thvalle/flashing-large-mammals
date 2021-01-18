library(tidyverse)
library(overlap)

obs <- read_csv("obs_datexpl.csv")

# Density Plot ----------------------------------------------------
sp <- "rev"
densityPlot(obs[obs$validated_species %in% sp & obs$flash == TRUE, ]$rad)
densityPlot(obs[obs$validated_species %in% sp & obs$flash == FALSE, ]$rad, add = TRUE, col = "red", lty = 2)
legend("bottomleft", legend = c("Flash", "No flash"), col = c("black", "red"), lty = c(1, 2))

# Reven "skyvest" på ein måte vekk frå moglege skumringstimer når flash MANGLER!
# Med andre ord: kan flash auke deteksjonsraten på rev i skumringen?

# overlap_flash ---------------------------------------------
# Originally tried to do the calculate the overlap area under the curve for species w/ w/o FLASH using dplyr::group_by with no success
# decided to make a function calculating the Dhat4 for overlapPlots instead, inspired by the camtrapR package
overlap_flash <- function(x, colour = c("black", "blue"), rug = FALSE) {
  x0 <- obs[obs$validated_species %in% x & obs$flash == FALSE, ]$rad
  x1 <- obs[obs$validated_species %in% x & obs$flash == TRUE, ]$rad
  overlapPlot(x1, x0, main = str_to_title(x), linecol = rep(colour, 2), rug = rug, linewidth = c(2, 2), olapcol = "darkgrey", extend = "lightgrey")
  x.est <- round(overlapEst(x1, x0, type = "Dhat4"), 3)
  n0 <- length(x0)
  n1 <- length(x1)
  legend("top", legend = str_c("Dhat4", x.est, sep = " = "), bty = "n")
  legend("bottomleft", legend = c(str_c("LED n", n1, sep = " = "), str_c("    IR n", n0, sep = " = ")),
         col = colour, lty = c(1, 2), lwd = 2, bty = "7")
}

overlap_flash("rev", "blue") #
overlap_flash("kjoeretoey")
overlap_flash("menneske")
overlap_flash("nothing")

# then making plots for all species that could be included in this study
sp_all <- c("hare", "elg", "rev", "grevling", "maar", "gaupe", "ilder", "ekorn", "skogshons", "raadyr", "hjort")
for (i in sp_all) {
  overlap_flash(i, rug = TRUE)
}


# overlap_flash_2sp -overlapPlot within and between 2 species w/ w/o FLASH
overlap_flash_2sp <- function(x, y, col_x, col_y) {
  x1 <- obs[obs$validated_species %in% x & obs$flash == TRUE, ]$rad
  x0 <- obs[obs$validated_species %in% x & obs$flash == FALSE, ]$rad
  y1 <- obs[obs$validated_species %in% y & obs$flash == TRUE, ]$rad
  y0 <- obs[obs$validated_species %in% y & obs$flash == FALSE, ]$rad
  par(mfrow = c(2, 2))
  overlap_flash(x, col_x)
  overlap_flash(y, col_y)
  overlapPlot(x0, y0,
    main = str_to_title("without flash"), linecol = c(col_x, col_y),
    linetype = c(1, 1), linewidth = c(2, 2), olapcol = "darkgrey", extend = "lightgrey"
  )
  xy0.est <- round(overlapEst(x0, y0, type = "Dhat4"), 3)
  xy1.est <- round(overlapEst(x1, y1, type = "Dhat4"), 3)
  legend("top", legend = str_c("Dhat4", xy0.est, sep = " = "), bty = "n")
  legend("bottomleft", legend = c(x, y), col = c(col_x, col_y), lty = c(1, 1), lwd = 2, bty = "n")
  overlapPlot(x1, y1,
    main = str_to_title("with flash"), linecol = c(col_x, col_y),
    linetype = c(2, 2), linewidth = c(2, 2), olapcol = "darkgrey", extend = "lightgrey"
  )
  legend("top", legend = str_c("Dhat4", xy1.est, sep = " = "), bty = "n")
  legend("bottomleft", legend = c(x, y), col = c(col_x, col_y), lty = c(2, 2), lwd = 2, bty = "n")
  par(mfrow = c(1, 1))
}

overlap_flash_2sp("rev", "raadyr", "red", "blue")
save("overlap_rev_raa.png")


overlap_flash_2sp("elg", "hjort", "red", "blue")


# overlapp-plot av fleire artar------------------------------------------------------------------
sp <- c("rev", "elg", "raadyr", "hare", "grevling")

sp <- c("rev", "raadyr", "grevling")
sp_obs <- obs %>%
  filter(validated_species %in% sp[1])
densityPlot(sp_obs$rad, ylim = c(0, .12), rug = T, main = sp)
sp_obs <- obs %>%
  filter(validated_species %in% sp[2])
densityPlot(sp_obs$rad, add = T, col = "brown")
sp_obs <- obs %>%
  filter(validated_species %in% sp[3])
densityPlot(sp_obs$rad, add = T, col = "black", lty = 2)
legend("topleft", legend = sp, col = c("black", "brown", "black"), lty = c(1, 1, 2), bty = "n")


# calculate overlap coeff - bayestestR -----------------------------------------------
rev_obs <- subset(obs, validated_species == "rev")
raad_obs <- subset(obs, validated_species == "raadyr")
bayestestR::overlap(rev_obs$hour, raad_obs$hour)
# $rad og $hour gir same svar: 0.75

# sunTime -------------------------------------------------------------------------------
?sunTime 
# Mitigates pooled data due to shifting of sun-rise/set
# sunTime(clockTime, Dates, Coords) #require "maptools"
coords <- matrix(c(60.1, 10.6), nrow=1) # Rett nord for Oslo i longlat
coords <- sp::SpatialPoints(coords, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
obs_suntime <- obs %>%  mutate(date = as.POSIXct(date, tz = "CET")) 

obs_sun_rev <- obs_suntime %>% filter(validated_species %in% "rev")

st <- sunTime(obs_sun_rev$rad, obs_sun_rev$date, coords)
ct <- obs %>% filter(validated_species == "rev")

par(mfrow=2:1)
densityPlot(st, col='red', lwd=2, xaxt='n', main="Sun time")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
densityPlot(ct$rad, lwd = 2, main = "Clock time")
par(mfrow=c(1,1))


overlap_flash_sun <- function(x, colour = c("black", "blue"), rug = FALSE) {
  obs_sp <- filter(obs, validated_species %in% x) 
  obs_sp <- mutate(obs_sp, date = as.POSIXct(date, tz = "CET"))
#  x0_st <- sunTime(obs_sp[obs$validated_species %in% x & obs$flash == FALSE, ]$rad, obs_sp[obs$validated_species %in% x & obs$flash == FALSE, ]$date, coords)
#  x1_st <- sunTime(obs_sp[obs$validated_species %in% x & obs$flash == TRUE, ]$rad, obs_sp[obs$validated_species %in% x & obs$flash == TRUE, ]$date, coords)

  #Må finne på noko smartere med midlertidig date-objekt og rad-objekt
  
  overlapPlot(x1, x0, main = str_to_title(x), linecol = rep(colour, 2), rug = rug, linewidth = c(2, 2), olapcol = "darkgrey", extend = "lightgrey")
  x.est <- round(overlapEst(x1, x0, type = "Dhat4"), 3)
  n0 <- length(x0)
  n1 <- length(x1)
  legend("top", legend = str_c("Dhat4", x.est, sep = " = "), bty = "n")
  legend("bottomleft", legend = c(str_c("LED n", n1, sep = " = "), str_c("    IR n", n0, sep = " = ")),
         col = colour, lty = c(1, 2), lwd = 2, bty = "7")
}

overlap_flash_sun("rev")





# ggpubr::ggdensity --------------------------------------------------------------------
obs %>%
  filter(validated_species %in% sp) %>%
  ggpubr::ggdensity("hour", fill = "validated_species") + xlab("Time (24 hrs)") +
  ylab("Density")


