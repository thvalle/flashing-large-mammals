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

# easystats-installasjon på windows 10 frå data_exploration2.R, line 251 ------------------


# easystats  - attempt to install the damn thing. 

# remotes::install_github("easystats/easystats")
# Im here

# easystats::install_easystats_latest()
# remotes::install_github("easystats/report")  # You only need to do that once

# install.packages("easystats")
# install.packages("report")
# library(report)     # for reporting test-statistics             not available for this version of R

# Warning in install.packages :
#   package ‘easystats’ is not available for this version of R
# 
# A version of this package for your version of R might be available elsewhere,
# see the ideas at
# https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages


#possible solution

# Need to authenticate myself to github in order to retry installations.

# usethis::create_github_token() #sends me to a github-page. this is the third time I've recreated the token,
#  need to update on the linux
# usethis::edit_r_environ() # Så må eg altså skrive GITHUB_PAT="my-pat"

# install.packages("gitcreds")
# gitcreds::gitcreds_set()  #prompt to insert the token
# gitcreds::gitcreds_get()  #successfull

#This process was successfull, but in the end I needed something different.
# Enter Global options -> Git/SVN -> create RSA key -> write som lines in a git Bash
# 
#From Happy git with R:
# 11.4.3.2 Windows
# In a Git Bash shell, make sure ssh-agent is running:

#  $ eval $(ssh-agent -s)
# > Agent pid 59566

#Add your key.
#$ ssh-add ~/.ssh/id_rsa

# worth checking out R version
# #install.packages("installr")
# library(installr)
# updateR()
# FALSE  - already newest version