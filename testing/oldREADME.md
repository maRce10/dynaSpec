# dynaSpec

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dynaSpec)](https://cran.r-project.org/package=dynaSpec)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/dynaSpec)](http://www.r-pkg.org/pkg/dynaSpec)
[![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/dynaSpec?color=blue)](https://r-pkg.org/pkg/dynaSpec)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

A set of tools to generate dynamic spectrogram visualizations in video format.

Install/load the package from CRAN as follows (**NOT YET IN CRAN**):

```r

# From CRAN would be
#install.packages("dynaSpec")

#load package
library(dynaSpec)

```

To install the latest developmental version from [github](http://github.com/) you will need the R package [devtools](https://cran.r-project.org/package=devtools):

```r

# From github
devtools::install_github("maRce10/dynaSpec")

#load package
library(dynaSpec)

```

## Examples


Black backgound ('colbg' argument):
```r

scrolling_spectro(wave = canyon_wren, wl = 300, t.display = 1.7, pal = viridis, parallel = 3, grid = FALSE, flim = c(1, 9), width = 1000, height = 500, res = 120, file.name = "black.mp4", colbg = "black")

```


<center><iframe  allowtransparency="true" style="background: #FFFFFF;" style="border:0px solid lightgrey;"  width="800" height="400"
src="https://www.youtube.com/embed/ta0OGxE8dBo" 
frameborder="0" 
allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" 
allowfullscreen></iframe></center>



Please cite [dynaSpec](https://marce10.github.io/dynaSpec/) as follows:

Araya-Salas M & M. Wilkins. (2020), *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.
