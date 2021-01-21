install.packages("camtrapR")
library("camtrapR")
?camtrapR
#vignette 4
#load data
data("camtraps")
data("recordTableSample")
#implementere mine data i camtrapR
detHist <- detectionHistory(recordTable = obs_cut1,
  camOp                = camop_no_problem,
  stationCol           = "loc",
  speciesCol           = "validated_species",
  recordDateTimeCol    = "captured_at_exif",
  species              = "rev",
  occasionLength       = 7, #days per occasion
  day1                 = "date",
  includeEffort        = TRUE,
  scaleEffort          = FALSE,
  writecsv             = TRUE,
  outDir               = "/home/torgeir/Documents/Master/Flashing Large Mammals/Analyse/prosjekt"
)
?detectionHistory
#Density plot
##for fox
activityDensity(obs_cut1,species="rev",
                allSpecies = FALSE,speciesCol = "validated_species",
                recordDateTimeCol = "captured_at_exif",
                recordDateTimeFormat = "%Y:%m:%d %H:%M:%S",
                plotR = TRUE,writePNG = FALSE)
##for all species in obs_sp
activityDensity(obs_sp,
                allSpecies = TRUE,speciesCol = "validated_species",
                recordDateTimeCol = "captured_at_exif",
                recordDateTimeFormat = "%Y:%m:%d %H:%M:%S",
                plotR = TRUE,
                writePNG = FALSE,
                add.rug = T)
