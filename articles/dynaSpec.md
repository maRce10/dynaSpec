# package overview

 

The [dynaSpec](https://marce10.github.io/dynaSpec/) package creates
dynamic spectrograms: spectrograms that move along with the sound. They
can be shown as interactive web pages or saved as videos. This vignette
shows the basic usage of all features. The article [Customizing dynamic
spectrograms](https://marce10.github.io/dynaSpec/articles/customizing.html)
on the package website shows how to customize them. For detailed
information on each function, please refer to the [function
reference](https://marce10.github.io/dynaSpec/reference/index.html) or
use the help files within R (e.g.,
[`?dynaspec`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)).

## Installation

Install the latest development version from GitHub:

``` r

remotes::install_github("maRce10/dynaSpec")

#load package
library(dynaSpec)
```

Web pages do not need any external software. To create video files
dynaSpec uses [ffmpeg](https://ffmpeg.org/download.html), which must be
installed and available in the system path (see the installation
instructions in the
[README](https://github.com/maRce10/dynaSpec#ffmpeg)).

## Layers

Dynamic spectrograms are built in layers, as ‘ggplot2’ graphs:
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
takes the sound, and layers are added with `+`:

``` r

dynaspec(wave, flim = c(1, 10)) + # the sound (and frequency and time ranges)
  ds_spectrogram(wl = 300) + # panels: spectrogram, oscillogram, envelope, spectrum
  ds_oscillogram() +
  ds_annotations(st) + # highlight parts of the sound
  ds_captions("song", start = 0, end = 2) + # text shown during a time range
  ds_theme(xlab = "Time (s)") + # customize axis titles, text and colors
  ds_player(speeds = c(1, 0.5)) + # customize playback buttons of web pages
  ds_scrolling(t.display = 1.7) # spectro style: ds_static() (default), ds_scrolling() or ds_paged()
```

| Layer | What it does |
|----|----|
| [`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md) | Adds a spectrogram (window length, palette, amplitude range, …) |
| [`ds_oscillogram()`](https://marce10.github.io/dynaSpec/reference/ds_oscillogram.md), [`ds_envelope()`](https://marce10.github.io/dynaSpec/reference/ds_envelope.md) | Add an oscillogram or an amplitude envelope (above or below the spectrogram) |
| [`ds_spectrum()`](https://marce10.github.io/dynaSpec/reference/ds_spectrum.md) | Adds a power spectrum next to the spectrogram |
| [`ds_overview()`](https://marce10.github.io/dynaSpec/reference/ds_overview.md) | Adds a lower panel with the whole sound to scrolling spectrograms |
| [`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md) | Highlights parts of the sound with translucent boxes, borders and labels (e.g. from a selection table) |
| [`ds_captions()`](https://marce10.github.io/dynaSpec/reference/ds_captions.md) | Text at a fixed position shown during a time range, like subtitles |
| [`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md), [`ds_theme_dark()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md), … | Axis titles, fonts, colors of the figure and plot, and preset themes |
| [`ds_player()`](https://marce10.github.io/dynaSpec/reference/ds_player.md) | Playback controls of web pages (speed slider, loop, volume, download, …) |
| [`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md), [`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md), [`ds_paged()`](https://marce10.github.io/dynaSpec/reference/styles.md) | Style of the dynamic spectrogram |

Printing a `dynaspec` object shows an interactive web page: press play
to watch the spectrogram along with the sound, click on it to play from
that point, and use the speed slider to slow the sound down. In the R
console web pages are shown in the RStudio viewer (or the web browser),
and in R Markdown and Quarto documents (like this vignette) they are
embedded in the document.
[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md)
saves them as web pages, videos or images (see
[Saving](#saving-web-pages-videos-and-images)).

``` r

library(dynaSpec)
library(seewave)
```

The examples use the songs of a brown-billed scythebill and a great
antshrike (scrolling spectrograms) included in the package. Sounds can
be ‘Wave’ objects or ‘.wav’ and ‘.mp3’ files.

## Panels

Only the panels added as layers are shown. A spectrogram:

``` r

data("scythebill")

ds_scythebill <- dynaspec(scythebill, flim = c(0.2, 4)) +
  ds_spectrogram(wl = 512, collevels = seq(-50, 0, 1), ovlp = 95)

ds_scythebill
```

![Spectrogram of a 3.05 s sound](NA)

The spectrogram background is the color of the lowest amplitude of the
palette (white for the default gray palette), so quiet parts of the
sound and the background look the same. Use `ds_spectrogram(colbg = )`
to set another background.

Layers can be stored in an object (as `ds_scythebill` above) and added
later. A spectrogram with an oscillogram below it:

``` r

ds_scythebill + ds_oscillogram()
```

![Spectrogram and oscillogram of a 3.05 s sound](NA)

Oscillograms and amplitude envelopes can go above or below the
spectrogram, and a power spectrum can be added on its left or right
side. An envelope above the spectrogram and a power spectrum on its
right side:

``` r

ds_scythebill +
  ds_envelope(position = "above") +
  ds_spectrum(side = "right")
```

![Spectrogram, amplitude envelope and power spectrum of a 3.05 s
sound](NA)

Without
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)
only the other panels are shown (e.g. only the oscillogram), and
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
alone shows an empty panel with a time axis and the play line:

``` r

dynaspec(scythebill) + ds_oscillogram()
```

![Oscillogram of a 3.05 s sound](NA)

`flim` sets the frequency range (in kHz) and `tlim` the time range of
the sound used (in s):

``` r

dynaspec(scythebill, flim = c(1.5, 5.5), tlim = c(1.1, 1.76)) +
  ds_spectrogram(wl = 200, collevels = seq(-50, 0, 1))
```

![Spectrogram of a 0.66 s sound](NA)

## Styles

### Static spectrograms

The default style: a line moves over the whole figure as the sound
plays, so
[`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)
is not needed to create static spectrograms (all examples above are
static). It is used to change the settings of static spectrograms, or to
switch back to them from another style (the last style added is used).
Long sounds can be split in several rows (`rows`; `height` sets the
height of each row), and `play.line = FALSE` hides the moving line:

``` r

data("antshrike")

ds_antshrike <- dynaspec(antshrike, flim = c(0.5, 4)) +
   ds_spectrogram(wl = 256, collevels = seq(-50, 0, 1))

ds_antshrike  + 
  ds_static(rows = 2, height = 350)
```

![Spectrogram of a 16.78 s sound](NA)

### Scrolling spectrograms

The spectrogram scrolls along a fixed play line, as in the Macaulay
Library. `t.display` sets the time window (in s) shown at once, and
[`ds_overview()`](https://marce10.github.io/dynaSpec/reference/ds_overview.md)
adds a lower panel with the whole sound, in which the part being shown
is highlighted:

``` r


ds_antshrike <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl = 256, collevels = seq(-50, 0, 1))

ds_antshrike +
  ds_overview() +
  ds_scrolling(t.display = 2)
```

![Scrolling spectrogram](NA)![](NA)

Scrolling spectrograms show a single lower panel: an overview, an
oscillogram or an envelope.

### Paged spectrograms

The spectrogram is split into pages (`t.display` sets the duration of
each page), and a translucent box reveals the part of each page being
played, as in Adobe Audition’s spectral view. Pages turn as the sound
plays:

``` r

ds_scythebill + ds_paged(t.display = 1)
```

![Spectrogram, page 1 of 4](NA)![Spectrogram, page 2 of
4](NA)![Spectrogram, page 3 of 4](NA)![Spectrogram, page 4 of 4](NA)

Settings not supplied take the defaults of each style. For instance,
paged spectrograms use the ‘inferno’ palette and no axes by default.

## Playback

The speed slider slows the sound down. Slowing down also lowers the
pitch (as when slowing down a tape), which makes fast features such as
pulses easier to hear. The *keep pitch* button switches to slowing down
while keeping the original pitch.
[`ds_player()`](https://marce10.github.io/dynaSpec/reference/ds_player.md)
sets the speeds offered (the first one is the initial speed) and whether
the pitch is kept by default:

``` r

ds_scythebill + ds_player(speeds = c(0.5, 1, 0.25), keep.pitch = TRUE)
```

![Spectrogram of a 3.05 s sound](NA)

`buttons` adds more controls: a time readout (shown by default), a seek
bar, a zoom slider (scrolling spectrograms), a loop button, a volume
control, a button to download the sound and a full screen button:

``` r

ds_scythebill +
  ds_player(
    buttons = c("progress", "time", "loop", "volume", "download", "fullscreen")
  )
```

![Spectrogram of a 3.05 s sound](NA)

Several players can be included in the same document or web page; only
one sound plays at a time.

## Highlighting and labeling sounds

[`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md)
highlights parts of the sound with translucent boxes, borders and labels
(e.g. from a selection table of
[warbleR](https://cran.r-project.org/package=warbleR) or
[ohun](https://cran.r-project.org/package=ohun)), and
[`ds_captions()`](https://marce10.github.io/dynaSpec/reference/ds_captions.md)
adds text at a fixed position of the figure shown during a time range,
like subtitles. Both work in the three styles, and are shown in the
article [Customizing dynamic
spectrograms](https://marce10.github.io/dynaSpec/articles/customizing.html#annotations).

## Themes

[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
sets the look of the figure in the same way for all styles: axis titles,
text sizes and fonts, colors of axes and text, boxes, grid lines,
backgrounds, the colors of the plot (e.g. the spectrogram palette, used
when not supplied in
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md))
and the colors of the player controls:

``` r

dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl = 200, pal = viridis::mako, collevels = seq(-100, 0, 1)) +
  ds_theme(
    xlab = "Time (s)",
    ylab = "Frequency (kHz)",
    cex.lab = 1.3,
    font.lab = 2
  )
```

![Spectrogram of a 3.05 s sound](NA)

Preset themes set several settings at once:
[`ds_theme_dark()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
[`ds_theme_mako()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
[`ds_theme_presentation()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
[`ds_theme_paper()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
[`ds_theme_minimal()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
[`ds_theme_transparent()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
[`ds_theme_void()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
[`ds_theme_xenocanto()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
and
[`ds_theme_macaulay()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md).
Their settings can be changed in the call:

``` r

ds_scythebill + ds_oscillogram() + ds_theme_dark(cex.lab = 1.3)
```

![Spectrogram and oscillogram of a 3.05 s sound](NA)

[`ds_set_theme()`](https://marce10.github.io/dynaSpec/reference/ds_set_theme.md)
sets a theme for all the spectrograms shown afterwards (e.g. in all the
code chunks of a document), as
[`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html).
Themes added to a spectrogram change it
([`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md))
or replace it (preset themes):

``` r

ds_set_theme(ds_theme_dark())
```

## Saving web pages, videos and images

[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md)
saves `dynaspec` objects. The extension of the file sets the output:

- `.html`: interactive web page, which can be shared as a single file or
  pasted into any web page
- `.mp4`: video, with the same settings. Videos can be used where web
  pages cannot, such as presentation slides (e.g. PowerPoint) or social
  media. [ffmpeg](https://ffmpeg.org/download.html) must be installed to
  create videos (see the installation instructions in the
  [README](https://github.com/maRce10/dynaSpec#installation))
- `.png`: still image of static spectrograms, or one image per page of
  paged spectrograms

``` r

save_dynaspec(ds_scythebill, "scythebill.html")
save_dynaspec(ds_scythebill, "scythebill.png")
save_dynaspec(ds_scythebill, "scythebill_static.mp4")
save_dynaspec(ds_antshrike + ds_scrolling(t.display = 2), "antshrike_scrolling.mp4")
```

Videos of static and paged spectrograms are created in a few seconds.
Videos of scrolling spectrograms take longer, as every frame is rendered
(a progress bar shows the progress).

Note that embedding sounds makes web pages larger (about 1.4 times the
size of the sound file), so cut (e.g. with `tlim`) or downsample long
recordings before creating web pages.

### Embedding players in other web pages

Web pages created by
[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md)
are self-contained: the images, the sound and the code of the player are
embedded in the file, so they work without R or a server (even offline,
by opening the file). They can be added to other web pages (e.g. a lab
website, a course page or GitHub Pages) in three ways:

- **As a page of its own**, uploaded and linked, or shown within another
  page with an `iframe`. This is the safest option, as the styles of the
  page and the player do not affect each other, and it also works in
  site builders that remove scripts from pasted code:

``` html
<iframe src="scythebill.html" width="100%" height="560" style="border:0"></iframe>
```

- **Pasting the code of the player** into the HTML of a page. Without a
  file name,
  [`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md)
  returns only the code of the player (no page header), which can be
  saved and pasted. Several players can be included in the same page;
  only one sound plays at a time:

``` r

player_code <- save_dynaspec(ds_scythebill, play = FALSE)
writeLines(player_code, "scythebill_player.html")
```

- **As a video** (`.mp4`), with the HTML `video` element
  (`<video controls src="scythebill.mp4"></video>`), in social media or
  in presentation slides. Videos have no speed controls or
  click-to-play, but work wherever videos do.

Pages that block scripts written within the page (with a strict
‘Content-Security-Policy’) show the spectrogram but not the player; use
an `iframe` in that case.

------------------------------------------------------------------------

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
    #> [1] seewave_2.2.4       dynaSpec_2.0.0.9000
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] gtable_0.3.6       jsonlite_2.0.0     compiler_4.6.2     Rcpp_1.1.2        
    #>  [5] stringr_1.6.0      bmp_0.3.1          signal_1.8-1       gridExtra_2.3.1   
    #>  [9] jquerylib_0.1.4    png_0.1-9          systemfonts_1.3.2  scales_1.4.0      
    #> [13] textshaping_1.0.5  yaml_2.3.12        fastmap_1.2.0      ggplot2_4.0.3     
    #> [17] R6_2.6.1           igraph_2.3.4       knitr_1.52         MASS_7.3-66       
    #> [21] viridis_0.6.5      backports_1.5.1    checkmate_2.3.4    imager_1.0.8      
    #> [25] desc_1.4.3         readbitmap_0.1.5   tiff_0.1-12        bslib_0.12.0      
    #> [29] RColorBrewer_1.1-3 rlang_1.3.0        stringi_1.8.9      cachem_1.1.0      
    #> [33] xfun_0.62          fs_2.1.0           sass_0.4.10        S7_0.2.2          
    #> [37] otel_0.2.0         viridisLite_0.4.3  cli_3.6.6          magrittr_2.0.5    
    #> [41] pkgdown_2.2.1      digest_0.6.39      grid_4.6.2         lifecycle_1.0.5   
    #> [45] vctrs_0.7.3        evaluate_1.0.5     glue_1.8.1         farver_2.1.2      
    #> [49] tuneR_1.4.7        ragg_1.5.2         purrr_1.2.2        rmarkdown_2.32    
    #> [53] jpeg_0.1-11        pkgconfig_2.0.3    tools_4.6.2        htmltools_0.5.9
