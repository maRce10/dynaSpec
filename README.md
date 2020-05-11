dynaSpec
================
Marcelo Araya-SAlas
2020-05-10

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dynaSpec)](https://cran.r-project.org/package=dynaSpec) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/dynaSpec)](http://www.r-pkg.org/pkg/dynaSpec) [![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/dynaSpec?color=blue)](https://r-pkg.org/pkg/dynaSpec) [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

A set of tools to generate dynamic spectrogram visualizations in video format.

Install/load the package from CRAN as follows (**NOT YET IN CRAN**):

``` r

# From CRAN would be
#install.packages("dynaSpec")

#load package
library(dynaSpec)
```

To install the latest developmental version from [github](http://github.com/) you will need the R package [devtools](https://cran.r-project.org/package=devtools):

``` r

# From github
devtools::install_github("maRce10/dynaSpec")

#load package
library(dynaSpec)
```

Examples
--------

Default spectrogram:

``` r

library(viridis)

scrolling_spectro(wave = canyon_wren, wl = 300, 
              t.display = 1.7, pal = viridis, 
              grid = FALSE, flim = c(1, 9), 
              width = 1000, height = 500, res = 120, 
              file.name = "default.mp4")
```

<center>
<iframe allowtransparency="true" style="background: #FFFFFF;" style="border:0px solid lightgrey;" width="800" height="400" src="https://www.youtube.com/embed/GucDwVV7GS8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>
</iframe>
</center>
 

A spectrogram of a canyon wren with black background ('colbg' argument):

``` r

scrolling_spectro(wave = canyon_wren, wl = 300, 
              t.display = 1.7, pal = viridis, 
              grid = FALSE, flim = c(1, 9), 
              width = 1000, height = 500, res = 120, 
              file.name = "black.mp4", colbg = "black")
```

<center>
<iframe allowtransparency="true" style="background: #FFFFFF;" style="border:0px solid lightgrey;" width="800" height="400" src="https://www.youtube.com/embed/ta0OGxE8dBo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>
</iframe>
</center>
 

... or yellow:

``` r

scrolling_spectro(wave = canyon_wren, wl = 300, 
              t.display = 1.7, pal = viridis, 
              grid = FALSE, flim = c(1, 9), 
              width = 1000, height = 500, res = 120, 
              file.name = "yellow.mp4", colbg = "yellow")
```

<center>
<iframe allowtransparency="true" style="background: #FFFFFF;" style="border:0px solid lightgrey;" width="800" height="400" src="https://www.youtube.com/embed/cT99V1xVtgM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>
</iframe>
</center>
 

Slow down to 1/2 speed ('speed' argument):

``` r

scrolling_spectro(wave = canyon_wren, wl = 300, 
              t.display = 1.7, pal = viridis, 
              grid = FALSE, flim = c(1, 9), 
              width = 1000, height = 500, res = 120, 
              file.name = "slow.mp4", colbg = "black",
              speed = 0.5)
```

<center>
<iframe allowtransparency="true" style="background: #FFFFFF;" style="border:0px solid lightgrey;" width="800" height="400" src="https://www.youtube.com/embed/jgxOgD5IYxI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>
</iframe>
</center>
 

Or speed up (a little tinamou song):

``` r

library(NatureSounds)
data("Cryp.soui")

scrolling_spectro(wave = Cryp.soui, wl = 300, 
              t.display = 1.7, pal = viridis, 
              grid = FALSE, flim = c(1.4, 2.1), 
              width = 1000, height = 500, res = 120, 
              file.name = "slow.mp4", colbg = "black",
              speed = 2)
```

<center>
<iframe allowtransparency="true" style="background: #FFFFFF;" style="border:0px solid lightgrey;" width="800" height="400" src="https://www.youtube.com/embed/CS2UPDpsIig" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>
</iframe>
</center>
 

Or remove axes (long-billed hermit song):

``` r

data("Phae.long4")

scrolling_spectro(wave = Phae.long4, wl = 300, 
    t.display = 1.7, ovlp = 70, pal = magma, 
    grid = FALSE, flim = c(1, 10), width = 1000, 
    height = 500, res = 120, collevels = seq(-50, 0, 5), 
    file.name = "no_axis.mp4", colbg = "black", 
    axis.type = "none")
```

<center>
<iframe allowtransparency="true" style="background: #FFFFFF;" style="border:0px solid lightgrey;" width="800" height="400" src="https://www.youtube.com/embed/V9WcGA0m_kI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>
</iframe>
</center>
 

------------------------------------------------------------------------

Please cite [dynaSpec](https://marce10.github.io/dynaSpec/) as follows:

Araya-Salas M & M. Wilkins. (2020), *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.
