library(tidyverse)
library(overlap)

obs <- read_csv("obs_datexpl.csv", col_types = cols(
  loc = col_factor(),
  date = col_date(format = ""),
  image_id = col_double(),
  timeserie_id = col_double(),
  dataset = col_double(),
  captured_at_exif = col_datetime(format = ""),
  predicted_species = col_character(),
  validated_species = col_character(),
  distance = col_double(),
  num_animals = col_double(),
  datetime = col_datetime(format = ""),
  `Kam.nr. til blitskamera` = col_double(),
  flash = col_logical(),
  period = col_factor(),
  hour = col_double(),
  mins = col_double(),
  rad = col_double()
))

?sunTime # TODO
# Mitigates pooled data due to shifting of sun-rise/set
# sunTime(clockTime, Dates, Coords) #require "maptools"


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
overlap_flash <- function(x, colour = c("black", "blue")) {
  x1 <- obs[obs$validated_species %in% x & obs$flash == TRUE, ]$rad
  x0 <- obs[obs$validated_species %in% x & obs$flash == FALSE, ]$rad
  overlapPlot(x1, x0, main = str_to_title(x), linecol = rep(colour, 2), linewidth = c(2, 2), olapcol = "darkgrey", extend = "lightgrey")
  x.est <- round(overlapEst(x1, x0, type = "Dhat4"), 3)
  legend("top", legend = str_c("Dhat4", x.est, sep = " = "), bty = "n")
  legend("bottomleft", legend = c("Flash", "No flash"), col = colour, lty = c(1, 2), lwd = 2, bty = "n")
}

overlap_flash("rev", "blue") #
overlap_flash("kjoeretoey")

# then making plots for all species that could be included in this study
sp_all <- c("hare", "elg", "rev", "grevling", "maar", "gaupe", "ilder", "ekorn", "skogshøns", "raadyr", "hjort")
for (i in sp_all) {
  overlap_flash(i)
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
}

overlap_flash_2sp("rev", "raadyr", "red", "blue")


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


# ggpubr::ggdensity --------------------------------------------------------------------
obs %>%
  filter(validated_species %in% sp) %>%
  ggpubr::ggdensity("hour", fill = "validated_species") + xlab("Time (24 hrs)") +
  ylab("Density")



# setup hour, mins, rad ------------------------------------------------------------------
# The densityPlot functions requires the input to be in radians
obs$hour <- as.numeric(format(obs$datetime, "%H"))
obs$mins <- as.numeric(format(obs$datetime, "%M"))

obs$rad <- ((obs$hour * 60 + obs$mins) / (24 * 60)) * 2 * pi # Converting hours to minutes and dividing the number of minutes by the total number of minutes during the day
# and multiplyting with 2 pi
