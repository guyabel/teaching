pkgname <- "ADRItools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "ADRItools-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ADRItools')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("every_other")
### * every_other

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: every_other
### Title: Select Every Other (nth) Element
### Aliases: every_other

### ** Examples

every_other(x = letters)
every_other(LETTERS, n = 3, start = 6)
every_other(x = letters, fill = "")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("every_other", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tile_plot")
### * tile_plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tile_plot
### Title: Tile Plot over Multiple Columns
### Aliases: tile_plot

### ** Examples

library(gapminder)
d <- gapminder %>%
  as_tibble() %>%
  mutate(col = as.numeric(continent),
         col = ifelse(test = continent == "Europe", yes = 2, no = col),
         col = ifelse(test = continent == "Oceania", yes = 3, no = col),
         # messing with label lengths to check code is working
         continent = fct_recode(continent, "North and South America" = "Americas"),
         country = as.character(country),
         country = ifelse(continent == "Africa", yes = abbreviate(country), country),
         country = fct_inorder(country),
         country = fct_rev(country))

tile_plot(d = d, x = "year", y = "country", fill = "lifeExp", facets = "continent")
tile_plot(d = d, x = "year", y = "country", fill = "lifeExp", facets = "continent",
          labeller = label_wrap_gen(width = 15))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tile_plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
