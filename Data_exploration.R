library(tidyverse)
library(lubridate)

# readr tibbles
{
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
    period = col_factor(levels = c("0_1", "1_1", "0_2", "1_2", "0_3", "Control")),
    hour = col_double(),
    mins = col_double(),
    rad = col_double()
  ))
  freq <- read_csv("freq_datexpl.csv", col_types = cols(
    loc = col_factor(),
    period = col_factor(levels = c("0_1", "1_1", "0_2", "1_2", "0_3", "Control")),
    validated_species = col_character(),
    n.obs = col_double(),
    n.days = col_double(),
    freq = col_double(),
    flash = col_logical()
  ))
  effort <- read_csv("effort_datexpl.csv", col_types = cols(
    loc = col_factor(),
    date = col_date(format = ""),
    period = col_factor(levels = c("0_1", "1_1", "0_2", "1_2", "0_3", "Control")),
    abc = col_factor(levels = c("A", "B", "C")),
    cam_mod = col_factor()
  ))
  stations <- read_csv("stations.csv", col_types = cols(
    loc = col_factor(),
    x = col_double(),
    y = col_double(),
    abc = col_factor(levels = c("A", "B", "C")),
    cam_mod = col_factor()
  ))
}

# For å bytte fargepalett------------------------------------
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = length(sp_all), name = "Set3") # Max n cuts is 12 in the "Paired" palette

# number of photos taken, according to weekdays--------------------------------
obs %>%
  mutate(wday = wday(captured_at_exif, label = T)) %>%
  ggplot(aes(wday)) +
  geom_bar(aes())
sp_all <- c("hare", "elg", "rev", "grevling", "maar", "gaupe", "ekorn", "raadyr", "hjort") # interesting species with enough datapoints
obs %>%
  mutate(wday = wday(captured_at_exif, label = T)) %>%
  filter(validated_species %in% sp_all) %>% # number of photos taken of species in sp_all
  ggplot(aes(wday)) +
  geom_bar()

# according to month
obs %>%
  mutate(month = month(captured_at_exif, label = T)) %>%
  filter(validated_species %in% sp_all) %>% # filtered by sp_all
  ggplot(aes(month)) +
  geom_bar(aes(fill = validated_species)) +
  scale_fill_brewer(direction = -1) +
  theme_dark()

# Prop of SP ~ flash || ~ period-----------------------------------------------------------------------------------------------
# Proportions and counts of animal species on different groupings. Which animals are well represented in the data?
p.sp <- obs %>%
  left_join(stations) %>%
  filter(validated_species %in% sp_all) %>%
  ggplot() #+ scale_fill_brewer() + theme_dark()

p.sp + geom_bar(aes(period, fill = validated_species), position = "dodge") + geom_hline(yintercept = 100) # shows actual counts of each species
p.sp + geom_bar(aes(period, fill = validated_species), position = "fill") # shows proportions of each species
p.sp + geom_bar(aes(flash, fill = validated_species), position = "dodge") + geom_hline(yintercept = 100)
p.sp + geom_bar(aes(flash, fill = validated_species), position = "fill") # prop
p.sp + geom_bar(aes(abc, fill = validated_species), position = "dodge") + geom_hline(yintercept = 100)

# most datapoints on ekorn, elg, grevling, hare, raadyr, rev. They all have >100 passings in each abc-grouping
sp_most <- c("ekorn", "elg", "grevling", "hare", "raadyr", "rev")

# Time Series plot with differing colours for diff treatments ----------------------------------------------------------------
qplot(date, period, data = effort, col = abc, shape = cam_mod)
# position= "jitter" | "nudge" blir ikkje godkjent. Om eg halverer dataene, går det då?
p_timeseries <- ggplot(data = effort) +
  geom_jitter(mapping = aes(date, period, col = abc), alpha = 1 / 5, width = 0, height = 0.1)
p_timeseries
ggsave("timeseries.png")
p_timeseries_facet <- ggplot(effort, aes(date, as.factor(loc))) +
  theme(axis.text.x = element_text(angle = 20)) +
  facet_wrap(~period, nrow = 2) +
  scale_y_discrete(guide = guide_axis(check.overlap = T))
p_timeseries_facet + geom_point(aes(col = abc), size = .8)
ggsave("timeseries_facet.png")

# Only Control:  - 257 and 925 jumps over to the Control period, although they are B and C respectively
effort %>%
  filter(period == "Control") %>%
  ggplot(aes(date, loc)) +
  theme(axis.text.x = element_text(angle = 20)) +
  scale_y_discrete(guide = guide_axis(check.overlap = T)) +
  geom_point(aes(col = abc), size = .8)


p.tid.per.wrap + geom_point(aes(col = cam_mod), size = .8)
# Only Browning has gaps in the time series, this due to lack of time lapse photos

# Boxplot --------------------------------------------------------------------------------------------


# Flash T/F
sp <- "rev"
plot(freq ~ as.factor(flash), freq[freq$validated_species %in% sp, ])

# frequencies with abc-grouping
freq %>%
  filter(validated_species %in% sp) %>%
  left_join(stations, by = "loc") %>%
  # filter(period != "Control") %>%  # removes Control-period. In this case only group A
  ggplot(aes(abc, freq)) +
  geom_boxplot()

# all periods
plot(freq ~ as.factor(period), freq[freq$validated_species %in% sp, ])

# frequency on periods, single or plural species
sp <- c("gaupe", "hjort", "grevling", "elg")
p_freq_per <- freq %>%
  filter(validated_species %in% sp) %>%
  left_join(stations, by = "loc") %>%
  ggplot(aes(period, freq)) +
  facet_wrap(~validated_species, nrow = 2) +
  labs(title = "Frequencies of species in periods")

# frequency on flash
p_freq_fla <- freq %>%
  filter(validated_species %in% sp) %>%
  left_join(stations, by = "loc") %>%
  ggplot(aes(flash, freq)) +
  facet_wrap(~validated_species, nrow = 2) +
  labs(title = "Frequencies of species w/ | w/o flash")

# coloured with abc-grouping
p_freq_per + geom_boxplot(outlier.shape = NA) + # Remove outliers when overlaying boxplot with original data points
  geom_jitter(aes(col = abc), width = 0.1) # add points for each camera

p_freq_per + geom_boxplot(aes(fill = abc)) # split into differing abc-groups. 257(group B) mess up the control-box

p_freq_per + geom_violin(aes(fill = abc), draw_quantiles = .5) # violin plot, scale = "count" -> Scale maximum width proportional to sample size
# outliers shown in the rewidening of the violins
# experimenting:
# p_freq_per + geom_violin(alpha = 0.1, adjust = .7, aes(fill=abc))    # violin plot
# p_freq_per + geom_violin(aes(group = cam_mod, scale = "width"))

p_freq_fla + geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(col = abc), width = 0.1) # add points for each camera

p_freq_fla + geom_boxplot(aes(fill = abc)) # split into differing abc-groups. 257(group B) mess up the control-box

p_freq_fla + geom_violin(aes(fill = abc), draw_quantiles = .5) # violin plot, scale = "count" -> Scale maximum width proportional to sample size
p_freq_fla + geom_violin(draw_quantiles = .5, scale = "count") # violin plot, scale = "count" -> Scale maximum width proportional to sample size
# ggsave("p_freq_flash.png") ??



# Burde dele opp control i enkelt-perioder for å ha kontroll opp mot kvar enkelt periode?
# Nå er 0_1,0_2,1_1,1_2 2sesonger a 40kam kvar, 0_3 er 1sesong a 20kamera, og Control er 5sesonger a 20kam

## Neri-bit ##
sp <- c("rev", "gaupe", "raadyr")
ggplot(freq[freq$validated_species %in% sp, ]) +
  facet_wrap(~validated_species) +
  geom_boxplot(aes(x = period, y = freq))
?facet_wrap

sp <- c("rev", "gaupe", "raadyr", "elg", "hare", "grevling")
ggplot(freq[freq$validated_species %in% sp, ]) +
  facet_wrap(~validated_species) +
  geom_boxplot(aes(x = flash, y = freq))


# I suggest that you try to find a nice way to visualize these patterns.
# From first inspection it does seem to be a large effect of the blits.



# Filter() & Group_by() --------------------------------------------------------------------------------------------
# Group by!

by_sp <- obs %>%
  left_join(stations, by = "loc") %>%
  group_by(validated_species)
passes <- summarise(by_sp,
  count = n(),
  flashed = mean(flash, na.rm = T),
  abc = abc
)
passes %>%
  filter(between(count, 70, 1000), validated_species != "menneske", !is.na(validated_species)) %>%
  ggplot() +
  geom_bar(aes(abc, fill = validated_species), position = "dodge") +
  geom_hline(yintercept = 100)

unique(obs$validated_species)
fjern <- c("hund", "kjoeretoey", "NA", "ukjent", "sau", "ku", "motorsykkel", "nothing")
passes %>%
  filter(count > 100, count < 10000, !(validated_species == fjern)) %>%
  ggplot(passes, mapping = aes(count, flashed)) +
  geom_point(aes(size = count, col = validated_species), alpha = 1 / 3) +
  geom_smooth(se = F)




# distance from camera
sp_distance <- obs %>%
  group_by(validated_species) %>%
  summarise(
    individuals = mean(num_animals, na.rm = T),
    distance_0 = mean(distance[flash == 0], na.rm = T),
    distance = mean(distance, na.rm = T),
    distance_1 = mean(distance[flash == 1], na.rm = T),
    flashed = mean(flash, na.rm = T),
    n = n()
  )
sp_distance %>%
  filter(n > 20, validated_species != "nothing") %>%
  ggplot(aes(distance, fct_reorder(validated_species, distance))) +
  geom_point(aes(size = n))

# Filter
sp <- c("rev", "elg", "raadyr", "hare", "grevling")

obs %>%
  filter(validated_species %in% sp) %>%
  ggplot() +
  geom_bar(aes(validated_species, y = stat(prop), group = 1))

ggplot(filter(obs, validated_species %in% sp)) +
  geom_bar(mapping = aes(x = validated_species, y = stat(prop), group = 1))

obs %>%
  filter(!(validated_species %in% fjern)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = validated_species, y = stat(prop), group = 1)) +
  theme(axis.text.x = element_text(angle = 20))




# iNEXT  Interpolation and EXTrapolation --------------------------------------------------------------------------------------------
## install iNEXT package from CRAN
# install.packages("iNEXT")

## import packages
# library(iNEXT)


# ser videre op dette om eg får lyst/tid. Mykje å sette seg inn i utan å vite kva i all verden eg kan bruke det til
