# Citing R packages
options(citation.bibtex.max = 999) # forcing the printing of bibtex-entry by citation-function

#already included tidyverse, lubridate, overlap, survival-package, survminer, RStudioTeam and RCoreTeam
# 2021.02.09----------------------
citation("base")

# 2021.03.
# !!!
knitr::write_bib(c("base", "lme4","tidyverse","lubridate","ggplot2", "tmap", "maps", "ggmap", "cowplot", "magick", "xtable",
                   "easystats", "performance", "parameters", "sjPlot","see", "bayestestR", "report","ggeffects","effects",
                   "overlap", "sf", "knitr"),
                 file = "../Thesis/tex/R-pkgs.bib") # function that could help in generating the bib.file



citation("easystats")
citation()
citation()
citation()
citation()
citation()

knitr:::.tweak.bib



# Traitdata

# PanTheria is a data set with trait data on loads of mammal species. I'm struggling to access it, but lo and behold,
# there is a package for getting it
remotes::install_github("EcologicalTraitData/traitdataform")
# Error: (converted from warning) package 'ape' was built under R version 4.0.5 !!! version 4.0.5 ?!! det gidder eg ikkje å oppdatera til
# Execution halted
# ERROR: lazy loading failed for package 'phangorn'
# * removing 'C:/Users/Nora.Familie-PC/Documents/R/R-4.0.4/library/phangorn'
# Error: Failed to install 'traitdataform' from GitHub:
#   (converted from warning) installation of package ‘phangorn’ had non-zero exit status

traitdataform::pulldata('pantheria')
