# Themes for dynamic spectrograms

 

This article showcases the themes of dynamic spectrograms: the settings
of
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
the preset themes and how to set a theme for all the spectrograms of a
session. Themes set the look of the figure (axes, text, backgrounds and
colors) and of the player controls of web pages, in the same way for all
styles. The article [Customizing dynamic
spectrograms](https://marce10.github.io/dynaSpec/articles/customizing.md)
shows the other customization options and annotations. As in the other
articles, the examples create interactive web pages: press play to watch
the spectrogram along with the sound.

``` r

library(dynaSpec)
library(viridis)

data("scythebill")
data("antshrike")

# a short song for static spectrograms
ds_scythebill <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1))

# a longer recording for scrolling spectrograms
ds_antshrike <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))
```

## Theme settings

[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
sets the look of all styles: axis titles (`xlab`, `ylab`), the size of
titles and labels (`cex.lab`, `cex.axis`), the font of titles
(`font.lab`), the color of axes and text (`col.lab`) and of the axis
labels alone (`col.axis`), the type of axes (`axis.type`) and the
background around the panels (`bg`):

``` r

ds_scythebill +
  ds_oscillogram() +
  ds_theme(
    xlab = "Time (s)",
    ylab = "Frequency (kHz)",
    cex.lab = 1.4,
    font.lab = 2,
    col.lab = "gray30"
  )
```

![Spectrogram and oscillogram of a 3.05 s sound](NA)

It also sets the font family of all text (`family`), the orientation of
the axis labels of both axes (`las`, as in base R: 0 parallel to the
axis, 1 horizontal, 2 perpendicular, 3 vertical), the length of ticks
(`tick.length`), boxes around panels (`box`, `col.box`), grid lines
(`grid`, `col.grid`), the background of oscillogram, envelope and
spectrum panels (`col.panel`) and the color and size of titles
(`col.title`, `cex.title`):

``` r

ds_scythebill +
  ds_oscillogram() +
  ds_spectrum() +
  ds_theme(
    family = "serif",
    las = 1,
    tick.length = 0.3,
    box = FALSE,
    grid = TRUE,
    col.panel = "gray95"
  )
```

![Spectrogram, oscillogram and power spectrum of a 3.05 s sound](NA)

Themes can also set the colors of the plot: the spectrogram palette and
background (`pal`, `col.bg`), the color of oscillograms, envelopes and
spectra (`col.wave`), the color of annotations (`col.annotation`), the
play line (`col.line`) and the highlight box of paged spectrograms
(`col.highlight`). These are used when they are not supplied in the
other layers (e.g. `ds_spectrogram(pal = )` takes precedence). `col.bar`
and `bg.bar` set the colors of the player controls of web pages. A dark
theme:

``` r

ds_scythebill +
  ds_oscillogram() +
  ds_theme(
    bg = "black",
    col.lab = "white",
    pal = viridis,
    col.bg = "black",
    col.wave = viridis(10)[7],
    col.line = "white",
    col.bar = "white",
    bg.bar = "black"
  )
```

![Spectrogram and oscillogram of a 3.05 s sound](NA)

### Backgrounds

The backgrounds of a figure are set independently: `col.bg` in
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)
(or
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md))
for the spectrogram, `col.panel` in
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
for oscillograms, envelopes and power spectra, and `bg` in
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
for the whole figure around the panels:

``` r

ds_scythebill +
  ds_spectrogram(pal = viridis) +
  ds_oscillogram(col = "white") +
  ds_theme(col.panel = "gray20", bg = "gray90")
```

![Spectrogram and oscillogram of a 3.05 s sound](NA)

## Preset themes

Preset themes set several settings at once. Their settings can be
changed in the call (e.g. `ds_theme_dark(cex.lab = 1.3)`), or by adding
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
afterwards. Adding a preset replaces previous themes.

### Dark

``` r

ds_scythebill +
  ds_envelope() +
  ds_theme_dark(cex.lab = 1.3) +
  ds_player(buttons = c("time", "loop", "volume"))
```

![Spectrogram and amplitude envelope of a 3.05 s sound](NA)

### Mako

Light theme with the ‘mako’ palette for the spectrogram, and ‘mako’
colors for panels, annotations, play line and player controls:

``` r

ds_scythebill +
  ds_oscillogram() +
  ds_theme_mako()
```

![Spectrogram and oscillogram of a 3.05 s sound](NA)

### Presentation

Large bold text, for slides and projectors:

``` r

ds_antshrike +
  ds_scrolling(t.display = 2) +
  ds_theme_presentation()
```

![Scrolling spectrogram](NA)![](NA)

### Paper

Black and white with serif fonts, for printed figures:

``` r

ds_scythebill +
  ds_oscillogram() +
  ds_theme_paper()
```

![Spectrogram and oscillogram of a 3.05 s sound](NA)

### Minimal

No boxes, light gray axes and short ticks:

``` r

ds_scythebill +
  ds_oscillogram() +
  ds_spectrum() +
  ds_theme_minimal()
```

![Spectrogram, oscillogram and power spectrum of a 3.05 s sound](NA)

### Transparent

Transparent background, to blend in slides or web pages of any color:

``` r

ds_scythebill +
  ds_oscillogram() +
  ds_theme_transparent()
```

![Spectrogram and oscillogram of a 3.05 s sound](NA)

### Void

No axes, titles or boxes (only the sound):

``` r

ds_scythebill +
  ds_theme_void()
```

![Spectrogram of a 3.05 s sound](NA)

### Xeno-canto

[`ds_theme_xenocanto()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
uses a gray spectrogram, labels inside the figure, a solid play line and
rounded player controls with a seek bar. It looks best with scrolling
spectrograms whose time axis moves along (`fix.time = FALSE`) and a
short oscillogram:

``` r

ds_antshrike +
  ds_oscillogram(height = 0.2) +
  ds_theme_xenocanto() +
  ds_scrolling(t.display = 3, fix.time = FALSE)
```

![Scrolling spectrogram](NA)![](NA)![](NA)

As other presets, its settings can be changed. For instance, the color
version of the Xeno-canto player:

``` r

ds_antshrike +
  ds_oscillogram(height = 0.2) +
  ds_theme_xenocanto(
    pal = viridis::inferno,
    col.bg = "black",
    bg = "black",
    col.panel = "black",
    col.wave = "white",
    col.lab = "white",
    col.line = "white"
  ) +
  ds_scrolling(t.display = 3, fix.time = FALSE)
```

![Scrolling spectrogram](NA)![](NA)![](NA)

### Macaulay Library

[`ds_theme_macaulay()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
uses a gray spectrogram, compact axes (frequency labels in a narrow
column on the left and the time in minutes and seconds above the figure)
and a black player bar with a square play button. It looks best with
scrolling spectrograms with an overview of the whole sound below (the
part shown is highlighted):

``` r

ds_antshrike +
  ds_overview() +
  ds_theme_macaulay() +
  ds_scrolling(t.display = 3, fix.time = FALSE)
```

![Scrolling spectrogram](NA)![](NA)

## Building blocks of the themes

The look of these themes comes from settings that can be used in any
theme. The Xeno-canto theme uses `axis.type = "inside"` (labels with
units inside the figure), `lty.line` and `lwd.line` (type and width of
the play line of scrolling spectrograms) and `bar.style = "rounded"`
(round player controls with a seek bar, on the `bg.bar` background). The
Macaulay theme uses `axis.type = "compact"`, `bar.style = "square"`
(flat bar with a square play button in the color of the play line) and
`col.highlight` (color dimming the overview outside the part shown).

`axis.type` can be “standard”, “inside”, “compact”, “none” or, in
scrolling spectrograms, “minimal” (1 s and 1 kHz scale bars):

``` r

ds_antshrike +
  ds_scrolling(t.display = 2) +
  ds_theme(axis.type = "minimal")
```

![Scrolling spectrogram](NA)![](NA)

## Setting themes globally

[`ds_set_theme()`](https://marce10.github.io/dynaSpec/reference/ds_set_theme.md)
sets a theme for all the spectrograms shown afterwards in the R session
(e.g. in all the code chunks of a document), as
[`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html).
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
added to a spectrogram changes some of its settings, and preset themes
replace it.
[`ds_set_theme()`](https://marce10.github.io/dynaSpec/reference/ds_set_theme.md)
returns the previous theme, so it can be restored (`ds_set_theme(NULL)`
removes it):

``` r

old <- ds_set_theme(ds_theme_paper(cex.lab = 1.3))

# paper theme
ds_scythebill + ds_oscillogram()
```

![Spectrogram and oscillogram of a 3.05 s sound](NA)

``` r


# paper theme with another axis title
ds_scythebill + ds_envelope() + ds_theme(ylab = "Freq. (kHz)")
```

![Spectrogram and amplitude envelope of a 3.05 s sound](NA)

``` r


# restore the previous theme
ds_set_theme(old)
```

#### Session information

Click to see

    #> R version 4.6.2 (2026-10-29)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 26.04.1 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.32.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    #>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    #>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    #> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    #> 
    #> time zone: UTC
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] viridis_0.6.5       viridisLite_0.4.3   dynaSpec_2.0.0.9000
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] gtable_0.3.6       jsonlite_2.0.0     compiler_4.6.2     Rcpp_1.1.2        
    #>  [5] stringr_1.6.0      bmp_0.3.1          signal_1.8-1       gridExtra_2.3.1   
    #>  [9] jquerylib_0.1.4    png_0.1-9          systemfonts_1.3.2  scales_1.4.0      
    #> [13] textshaping_1.0.5  yaml_2.3.12        fastmap_1.2.0      ggplot2_4.0.3     
    #> [17] R6_2.6.1           igraph_2.3.4       knitr_1.52         MASS_7.3-66       
    #> [21] backports_1.5.1    checkmate_2.3.4    imager_1.0.8       desc_1.4.3        
    #> [25] readbitmap_0.1.5   tiff_0.1-12        bslib_0.12.0       RColorBrewer_1.1-3
    #> [29] rlang_1.3.0        stringi_1.8.9      cachem_1.1.0       xfun_0.62         
    #> [33] fs_2.1.0           sass_0.4.10        S7_0.2.2           otel_0.3.0        
    #> [37] seewave_2.2.4      cli_3.6.6          magrittr_2.0.5     pkgdown_2.2.1     
    #> [41] digest_0.6.39      grid_4.6.2         lifecycle_1.0.5    vctrs_0.7.3       
    #> [45] evaluate_1.0.5     glue_1.8.1         farver_2.1.2       tuneR_1.4.7       
    #> [49] ragg_1.5.2         purrr_1.2.2        rmarkdown_2.32     jpeg_0.1-11       
    #> [53] pkgconfig_2.0.3    tools_4.6.2        htmltools_0.5.9
