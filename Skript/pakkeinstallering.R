#INSTALLERINGSSKRIPTET SOM ANNA FANT

.packages = c("maps", "mapdata", "Hmisc", "leaflet", "htmlwidgets", "rgdal", "tmap", "tigris", "acs", "stringr", "stringi", "sp", "dplyr", "mapview", "knitr", "rmarkdown")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
suppressMessages(invisible(lapply(.packages, require, character.only=TRUE)))

install.packages('remotes')
library(remotes)
install_version('rgdal', '1.2-20')

# install_version('tidyverse', '1.2.0')

##___________________________________________________________________________________________________________________-
install.packages('lattice')
install.packages('sp')
install.packages('raster')
library(raster)
library(fields)
library(rgdal)
library(sp)
library(rgbif)
library(corrplot)
library(MIAmaxent)
library(dismo)
library(sdm)
library(RMark) #there is no package called 'RMark' 
library(Hmsc) #there is no package called 'hmsc'
install.packages('RMark')
install.packages('Hmsc')
#######################
library(remotes)
install_version('Hmsc', "3.0-2")
install_version('MCMCpack', "1.2-4")
install_version('phytools', "0.4-31")
install_version('animation', "2.2")
#'animation’ had non-zero exit status
#ERROR: dependencies ‘animation’, ‘plotrix’ are not available for package ‘phytools’
#* removing ‘/home/torgeir/R/x86_64-pc-linux-gnu-library/3.4/phytools’
#
# ‘phytools’ 
##############
install_version("mvtnorm", "1.0-6") #-8 var kun tilgjengelig for >3.5.0
install_version("msm", "1.6.6") #fungerte
install.packages('snow') 
install.packages('snowfall') 
install.packages('RMark') #Warning: Software mark not found in path.
#If you have mark executable, you will need to set MarkPath object to its location (e.g. MarkPath="C:/Users/Jeff Laake/Desktop"
#############################################
#Alternative Occupancy package
install.packages('RPresence')
install.packages('RPresence', lib='/home/torgeir/Downloads/',repos = NULL)
?install.packages
untar('RPresence.tar.gz', exdir='/home/torgeir/Downloads/')
install.packages(pkgs = 'RPresence.tar.gz', repos = NULL, lib = '/home/torgeir/Downloads/')
#############################################
install.packages('remotes') 
library(remotes)
install_github("packageauthor/hmsc")
install_bitbucket("packageauthor/hmsc")
install_gitorious("packageauthor/hmsc")

# easystats-installasjon på windows 10 frå data_exploration2.R, line 251
