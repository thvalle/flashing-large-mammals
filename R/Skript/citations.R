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
