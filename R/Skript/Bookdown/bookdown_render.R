# Skript for å strikke bookdown-bøker
# Fleire av dei ligger fritt ute på github, og eg kunne gjerne tenkt meg å få dei inn
# i kindlen min.
# Nokon er også KUN tilgjengeleg om du bygger dei sjølv, som til dømes ggplot2-boka til Hadley W.

# Dette er koden som trengs for å bygge ei bok, men eg skjønner ikkje heilt kva som skal
# gjerast på førehand...
# bookdown::render_book("index.Rmd")

# ggplot2: Elegant graphics for data analysis -------------------------
# https://github.com/thvalle/ggplot2-book # git-versjon
# https://ggplot2-book.org/index.html     # online-versjon
devtools::install_deps()
bookdown::render_book("index.Rmd") 
# bookdown::calibre('ggplot2.epub', 'mobi') # or else I'll convert afterwards
getwd()
# Geocomputation with R ------------------------------
# https://github.com/thvalle/geocompr     # git-versjon
# https://geocompr.robinlovelace.net/     # online-versjon
## 
remotes::install_github("geocompr/geocompkg")
remotes::install_github("r-spatial/sf")
bookdown::render_book("index.Rmd")

# R Graphics Cookbook, 2nd edition        
# https://r-graphics.org/                 # online-versjon
# 
# 



bookdown::calibre('foo.epub', 'mobi') # generates 'foo.mobi'!

# calibre(input, output, options = "")
# Arguments
# input	
# The input filename.
# 
# output	
# The output filename or extension (if only an extension is provided,
# the output filename will be the input filename with its extension replaced
# by output; for example, calibre('foo.epub', 'mobi') generates ‘foo.mobi’).
# 
# options	
# A character vector of additional options to be passed to ebook-convert.