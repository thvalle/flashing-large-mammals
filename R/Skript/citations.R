# Citing R packages
options(citation.bibtex.max = 999) # forcing the printing of bibtex-entry by citation-function

#already included tidyverse, lubridate, overlap, survival-package, survminer, RStudioTeam and RCoreTeam
# 2021.02.09----------------------
citation("base")
citation("survival")
citation("coxme")
citation("lme4")
citation("ggplot2") # could deserve a proper mention along the way.

# 2021.02.
# !!!
knitr::write_bib() # function that could help in generating the bib.file?


citation("easystats")