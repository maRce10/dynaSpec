# Customizing dynamic spectrograms

 

This article shows how to customize dynamic spectrograms, layer by
layer, how to highlight and label parts of the sound with annotations
(including frequency contours) and captions, and how to compare sounds.
Check the [main
vignette](https://marce10.github.io/dynaSpec/articles/dynaSpec.md) for
details on the basic functions of the package and the usage of each
layer, and the article [Themes for dynamic
spectrograms](https://marce10.github.io/dynaSpec/articles/themes.md) for
details on preset themes.

``` r

library(dynaSpec)
library(viridis)
library(seewave)

data("scythebill")
data("antshrike")
```

Some examples use recordings from the package
[NatureSounds](https://cran.r-project.org/package=NatureSounds) or
downloaded from the web (sounds must be ‘Wave’ objects or local ‘.wav’
or ‘.mp3’ files, so recordings are downloaded first).

Layers can be stored in an object and modified by adding more layers.
Arguments of
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)
and style layers are combined with the previous ones (so only the
settings that change need to be supplied), while other panel and player
layers replace previous ones of the same kind, and annotations and
captions are added. Settings not supplied take the defaults of each
style.
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
sets the frequency range shown (`flim`, in kHz) and the part of the
sound used (`tlim`, in s: a single value takes the first seconds, two
values a time interval).

## Spectrograms

[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)
sets how the spectrogram is computed (window length `wl`, overlap
`ovlp`, window function `wn`) and drawn (color palette `pal`, amplitude
levels of the colors `collevels`, background `col.bg`). Other arguments
are passed to
[`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html):

``` r

ds_scythebill <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1))

# a longer recording for scrolling spectrograms
ds_antshrike <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))

# palette and amplitude range
ds_scythebill + ds_spectrogram(pal = mako, collevels = seq(-100, 0, 1))
```

![Spectrogram of a 3.05 s sound](NA)

Black and white spectrogram with a wider amplitude range (fainter sounds
are shown):

``` r

ds_scythebill + ds_spectrogram(pal = reverse.gray.colors.1, collevels = seq(-100, 0, 5))
```

![Spectrogram of a 3.05 s sound](NA)

Palettes can also be the name of a ‘viridis’ palette (e.g. `"magma"`),
or two or more colors for a gradient, from low to high amplitude:

``` r

# from low to high amplitude
ds_scythebill +
  ds_spectrogram(
    pal = c("white", "#FCE51E", "#F68D45", "#B5367A", "#2C115F"),
    collevels = seq(-50, 0, 1)
  )
```

![Spectrogram of a 3.05 s sound](NA)

By default the spectrogram background is the color of the lowest
amplitude of the palette (dark purple for ‘viridis’), so quiet parts of
the sound and the background look the same. `col.bg` sets another
spectrogram background:

``` r

ds_scythebill + ds_spectrogram(pal = viridis, col.bg = "black")
```

![Spectrogram of a 3.05 s sound](NA)

[`ds_filter()`](https://marce10.github.io/dynaSpec/reference/ds_filter.md)
applies a bandpass filter to the sound, so the filtered sound is both
what is shown and what is heard (also with video files): removing noise
outside the frequency range of the sounds of interest gives not only a
cleaner spectrogram but also a cleaner sound to listen to. `bp` sets its
frequency limits (in kHz; `Inf` as upper limit makes a high-pass filter,
and 0 as lower limit a low-pass one):

``` r

# only the sound between 0.4 and 4 kHz
ds_antshrike + ds_filter(bp = c(0.4, 4))
```

![Spectrogram of a 16.78 s sound](NA)

In paged spectrograms `amp.trans` brightens the spectrogram (a nonlinear
transformation that collapses the difference between loud and quiet
sounds; 1 means no transformation):

``` r

ds_scythebill +
  ds_spectrogram(amp.trans = 3, collevels = seq(-35, 0, 1)) +
  ds_paged(t.display = 2)
```

![Spectrogram, page 1 of 2](NA)![Spectrogram, page 2 of 2](NA)

## Panels

Oscillograms and amplitude envelopes can go above or below the
spectrogram (`position`), with their own color (`col`) and height
relative to the spectrogram (`height`). Power spectra go on the left or
right side (`side`), with a given width (`width`, as a proportion of the
figure):

``` r

cols <- mako(10)

ds_scythebill +
  ds_spectrogram(pal = mako, collevels = seq(-100, 0, 1)) +
  ds_oscillogram(position = "above", col = cols[3], height = 0.3) +
  ds_spectrum(side = "left", col = cols[3], width = 0.12)
```

![Spectrogram, oscillogram and power spectrum of a 3.05 s sound](NA)

An amplitude envelope instead of the oscillogram (oscillograms and
envelopes show the same information, so they are better not combined):

``` r

ds_scythebill +
  ds_spectrogram(pal = mako, collevels = seq(-100, 0, 1)) +
  ds_envelope(col = cols[5], height = 0.3)
```

![Spectrogram and amplitude envelope of a 3.05 s sound](NA)

With several rows each row shows its own power spectrum, on the same
scale:

``` r


data("antshrike")

ds_antshrike <- dynaspec(antshrike, flim = c(0.5, 4)) +
   ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))

ds_antshrike + ds_spectrum(col = cols[5]) +
  ds_static(rows = 2, height = 350)
```

![Spectrogram and power spectrum of a 16.78 s sound](NA)

In scrolling spectrograms the lower panel can be an overview of the
whole sound
([`ds_overview()`](https://marce10.github.io/dynaSpec/reference/ds_overview.md),
with its relative `height`), an oscillogram or an envelope:

``` r

ds_antshrike +
  ds_envelope(col = cols[3]) +
  ds_scrolling(t.display = 2)
```

![Scrolling spectrogram](NA)![](NA)![](NA)

## Static spectrograms

Static spectrograms are the default style, so
[`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)
is only needed to change their settings or to switch back from another
style (the last style added is used).
[`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)
sets the size and resolution of each row (`width`, `height`, in pixels,
and `res`), the number of rows (`rows`), whether the play line is shown
(`play.line`) and the frames per second of videos (`fps`). The play line
(its color, also used for the active buttons, its type and width) is set
with
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
(`col.line`, `lty.line` and `lwd.line`), in all styles:

``` r

ds_scythebill +
  ds_static(width = 1200, height = 400, res = 120) +
  ds_theme(col.line = "#E69F00")
```

![Spectrogram of a 3.05 s sound](NA)

Without the moving line (clicking on the figure still plays the sound
from that point):

``` r

ds_scythebill + ds_static(play.line = FALSE)
```

![Spectrogram of a 3.05 s sound](NA)

## Scrolling spectrograms

[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
sets the time window shown (`t.display`), whether the window stays
within the sound (`still.ends`; with `FALSE` the play line stays at the
center all the time, and the sound enters and leaves at the center),
whether the time axis moves along with the spectrogram
(`fix.time = FALSE`), the speed of videos (`speed`; web pages start at
the first speed of `ds_player(speeds)`, as in all styles), a delay
before the spectrogram starts moving (`buffer`, in s), looping (`loop`),
the size and resolution of the frames (`width`, `height`, `res`) and the
frames per second of videos (`fps`):

``` r

ds_antshrike +
  ds_spectrogram(pal = viridis, col.bg = "black") +
  ds_oscillogram(col = "#31688E99") +
  ds_scrolling(
    t.display = 2,
    fix.time = FALSE,
    width = 1000,
    height = 500,
    res = 120
  ) +
  ds_player(speeds = c(0.5, 1, 0.25, 0.125)) +
  ds_theme(col.line = "white", lty.line = 1)
```

![Scrolling spectrogram](NA)![](NA)![](NA)

Long-billed hermit song at 1/5 speed, without axes and looping (web
pages loop until paused):

``` r

data("Phae.long1", package = "NatureSounds")

dynaspec(Phae.long1, flim = c(2, 9)) +
  ds_spectrogram(
    wl.ms = 13,
    ovlp = 90,
    pal = magma,
    collevels = seq(-50, 0, 5),
    col.bg = "black"
  ) +
  ds_scrolling(
    t.display = 1.2,
    width = 1000,
    height = 400,
    res = 120,
    loop = 3
  ) +
  ds_player(speeds = c(0.2, 0.5, 1))
```

![Scrolling spectrogram](NA)![](NA)

The first 20 s of a northern nightingale wren recording from
[xeno-canto](https://www.xeno-canto.org) using a custom color palette:

``` r

nightingale_wren <- tempfile(fileext = ".mp3")
download.file(
  "https://xeno-canto.org/518334/download",
  nightingale_wren,
  mode = "wb"
)

custom_pal <- colorRampPalette(c(
  "#2d2d86",
  "#2d2d86",
  reverse.terrain.colors(10)[5:10]
))

dynaspec(nightingale_wren, flim = c(0, 8), tlim = 20) +
  ds_spectrogram(
    wl.ms = 12.5,
    ovlp = 95,
    pal = custom_pal,
    collevels = seq(-40, 0, 5),
    col.bg = "#2d2d86"
  ) +
  ds_scrolling(
    t.display = 3,
    width = 700,
    height = 350,
    res = 100
  ) +
  ds_theme(col.line = "#FFFFFFE6")
```

![Scrolling spectrogram](NA)![](NA)

Spix’s disc-winged bat inquiry call slowed down 16 times (starting at a
speed of 0.0625, the first speed of `ds_player(speeds)` and the slowest
one web browsers can play):

``` r

data("thyroptera.est", package = "NatureSounds")

# extract one call
thy_wav <- attributes(thyroptera.est)$wave.objects[[12]]

# convert to 16 bits (as the silence added below)
thy_wav <- tuneR::normalize(thy_wav, unit = "16")

# add silence at both sides
thy_wav <- pastew(
  tuneR::silence(
    duration = 0.05,
    samp.rate = thy_wav@samp.rate,
    xunit = "time"
  ),
  thy_wav,
  output = "Wave"
)
thy_wav <- pastew(
  thy_wav,
  tuneR::silence(
    duration = 0.04,
    samp.rate = thy_wav@samp.rate,
    xunit = "time"
  ),
  output = "Wave"
)

dynaspec(thy_wav, flim = c(12, 45)) +
  ds_spectrogram(
    wl.ms = 2,
    ovlp = 95,
    pal = inferno,
    collevels = seq(-40, 0, 5),
    col.bg = "black"
  ) +
  ds_oscillogram(col = inferno(10, alpha = 0.9)[3]) +
  ds_scrolling(
    t.display = 0.08,
    width = 700,
    height = 250,
    res = 100,
    loop = 4
  ) +
  ds_player(speeds = c(0.0625, 0.125, 0.25, 0.5, 1)) +
  ds_theme(col.line = "#FFFFFFE6", lty.line = 1)
```

![Scrolling spectrogram](NA)![](NA)![](NA)

Other options of
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
are a custom spectrogram function (`spectro.call`, a call made with
[`base::call()`](https://rdrr.io/r/base/call.html)) and spectral
derivatives (`derivative = TRUE`, requires the package ‘imager’).

## Paged spectrograms

[`ds_paged()`](https://marce10.github.io/dynaSpec/reference/styles.md)
sets the duration of each page (`t.display`), the opacity of the
highlight box (`highlight.alpha`; its color and that of the cursor are
set with `ds_theme(col.highlight, col.line)`), a title (`title`), an
amplitude color scale (`legend = TRUE`), the size and resolution of the
pages (`width`, `height`, in pixels, and `res`) and the frames per
second of videos (`fps`). Other arguments set the preparation of the
pages (e.g. `resample.rate` or `amp.thresh`):

``` r

ds_scythebill +
  ds_spectrogram(amp.trans = 3, collevels = seq(-35, 0, 1)) +
  ds_paged(
    t.display = 2,
    title = "Brown-billed scythebill",
    highlight.alpha = 0.4
  ) +
  ds_theme(col.highlight = "#2d2d86", col.line = "#FFEA46")
```

![Spectrogram, page 1 of 2](NA)![Spectrogram, page 2 of 2](NA)

The first 12 s of a humpback whale song, with 3 s pages and the
frequency range used by these whales (0-700 Hz):

``` r

whale <- tempfile(fileext = ".wav")
download.file(
  "http://www.oceanmammalinst.org/songs/hmpback3.wav",
  whale,
  mode = "wb"
)

dynaspec(whale, flim = c(0.1, 2), tlim = 9) +
  ds_spectrogram(amp.trans = 3, wl.ms = 133, ovlp = 90, collevels = seq(-50, 0, 5)) +
  ds_paged(t.display = 3)
```

![Spectrogram, page 1 of 3](NA)![Spectrogram, page 2 of
3](NA)![Spectrogram, page 3 of 3](NA)

A yellow-throaded toucan song from
[xeno-canto](https://www.xeno-canto.org) with a two-color palette:

``` r

toucan <- tempfile(fileext = ".mp3")
download.file("https://xeno-canto.org/154168/download", toucan, mode = "wb")

dynaspec(toucan, tlim = c(3.2, 15.2), flim = c(0, 8)) +
  ds_spectrogram(pal = c("white", "black"), collevels = seq(-100, 0, 5)) +
  ds_paged(t.display = 3) +
  ds_theme(col.highlight = "#d1b0ff", col.line = "#7817ff")
```

![Spectrogram, page 1 of 4](NA)![Spectrogram, page 2 of
4](NA)![Spectrogram, page 3 of 4](NA)![Spectrogram, page 4 of 4](NA)

## Annotations

[`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md)
highlights parts of the sound with translucent boxes. It takes a data
frame with the start and end of each selection (in s), as the selection
tables of [warbleR](https://cran.r-project.org/package=warbleR) and
[ohun](https://cran.r-project.org/package=ohun). If it has ‘bottom.freq’
and ‘top.freq’ columns (in kHz) the boxes only span that frequency
range, and colors and labels can be taken from its columns:

``` r

# note types
nts <- data.frame(
  start = c(0.34, 1.13, 1.44, 1.75, 2.32),
  end = c(1.03, 1.36, 1.68, 2.28, 2.56),
  bottom.freq = c(2.5, 2.4, 2.3, 2.1, 2.1),
  top.freq = c(3.9, 4, 3.8, 4, 3.7) + 0.5,
  labels = c("A", "B", "B", "C", "D"),
  colors = viridis::mako(n = 7)[1:5]
)

ds_scythebill +
  ds_annotations(nts, col = "colors", labels = "labels")
```

![Spectrogram of a 3.05 s sound](NA)

Annotations are also shown in overview spectrograms:

``` r


ds_scythebill +
  ds_scrolling() +
  ds_annotations(nts, col = "colors", labels = "labels") +
  ds_overview()
```

![Scrolling spectrogram](NA)![](NA)

The box borders are set with `border` (a color or the name of a column
with the color of each box), `lwd` (width) and `lty` (line type):

``` r

ds_scythebill +
  ds_annotations(
    nts,
    col = "colors",
    alpha = 0.1,
    border = "colors",
    lwd = 2,
    lty = "dotted",
    labels = "labels"
  )
```

![Spectrogram of a 3.05 s sound](NA)

`type = "spectrogram"` colors the spectrogram itself within each
selection, with a gradient from the spectrogram background to the color
of the selection (as
[`warbleR::color_spectro()`](https://marce10.github.io/warbleR/reference/color_spectro.html)),
instead of drawing boxes. A narrower amplitude range (`collevels`) keeps
the background noise uncolored:

``` r

# house wren male-female duet recording
hs_wren <- tempfile(fileext = ".wav")
download.file(
  "https://github.com/maRce10/example_sounds/raw/refs/heads/main/house_wren_male_female_duet.wav",
  hs_wren,
  mode = "wb"
)

# and selection table
st <- read.csv(
  "https://github.com/maRce10/example_sounds/raw/refs/heads/main/house_wren_male_female_duet.csv"
)

# colors and labels
st$colors <- viridis(10)[c(3, 8)]
st$labels <- c("male", "female")

duet <- dynaspec(hs_wren, flim = c(1, 14)) +
  ds_spectrogram(
    wl.ms = 11.6,
    pal = reverse.gray.colors.1,
    collevels = seq(-120, 0, 1)
  ) +
  ds_scrolling(t.display = 1.2, loop = 3, width = 1000, height = 500, res = 120)


duet +
  ds_spectrogram(collevels = seq(-60, 0, 1)) +
  ds_annotations(st, col = "colors", type = "spectrogram") +
  # labels at a fixed position of the figure while each bird sings (captions do not scroll)
  ds_captions(
    "male",
    start = st$start[1],
    end = st$end[1],
    y = 0.93,
    col = st$colors[1],
    cex = 2,
    font = 2
  ) +
  ds_captions(
    "female",
    start = st$start[2],
    end = st$end[2],
    y = 0.93,
    col = st$colors[2],
    cex = 2,
    font = 2
  )
```

![Scrolling spectrogram](NA)![](NA)

`type = "line"` draws a horizontal line from the start to the end of
each selection, at the frequency set with `line.freq` (in kHz, or the
name of a column with the frequency of each line), as is common in
figures of duets. Lines are thick and opaque by default (`lwd = 8`,
`alpha = 1`), and labels (`labels`) are shown above them. In scrolling
spectrograms labels move along with the spectrogram, so here the singers
are named with captions instead (see [Captions](#captions)), which stay
at a fixed position while each bird sings:

``` r

# one line height for each individual
st$line.freq <- c(12, 11.2)

duet +
  ds_annotations(st, col = "colors", type = "line", line.freq = "line.freq") +
  # labels at a fixed position of the figure while each bird sings (captions do not scroll)
  ds_captions(
    "male",
    start = st$start[1],
    end = st$end[1],
    y = 0.93,
    col = st$colors[1],
    cex = 2,
    font = 2
  ) +
  ds_captions(
    "female",
    start = st$start[2],
    end = st$end[2],
    y = 0.93,
    col = st$colors[2],
    cex = 2,
    font = 2
  )
```

![Scrolling spectrogram](NA)![](NA)

Selection tables made with
[warbleR](https://cran.r-project.org/package=warbleR) can be used
directly. When they have selections of several sound files, only those
of the sound shown are used (matching the file name), or the sound file
is chosen with `sound.file` (e.g. for ‘Wave’ objects):

``` r

data("lbh_selec_table", package = "warbleR")
data("Phae.long1", package = "NatureSounds")

dynaspec(Phae.long1, flim = c(1, 10)) +
  ds_spectrogram(wl.ms = 13) +
  ds_annotations(lbh_selec_table, sound.file = "Phae.long1.wav", labels = "selec", col = "tomato")
```

![Spectrogram of a 2.5 s sound](NA)

### Frequency contours

Annotations can also show frequency contours, drawn over the spectrogram
within each selection. With `contour = "dominant"` or
`contour = "fundamental"` they are measured on the sound, within the
frequency range of the selection (`bottom.freq` and `top.freq`, if
available) or the frequency limits of
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md).
`contour.col` and `contour.lwd` set the color and width of the lines:

``` r

st <- data.frame(
  start = c(0.34, 1.13, 1.44, 2.31),
  end = c(1.03, 1.36, 1.68, 2.55),
  labels = c("long note", "arched", "arched", "arched"),
  colors = c("#4C78A8", "#B279A2", "#B279A2", "#B279A2")
)

ds_scythebill +
  ds_annotations(
    st,
    col = "colors",
    labels = "labels",
    contour = "dominant",
    contour.col = "#E69F00",
    contour.lwd = 3
  )
```

![Spectrogram of a 3.05 s sound](NA)

Contours can be shown without boxes with `alpha = 0`, and work in all
styles. `contour.args` sets how they are measured: the window length
(`wl`, by default that of the spectrogram), overlap (`ovlp`), amplitude
threshold below which no frequency is measured (`threshold`) and
smoothing (`smooth`):

``` r

ds_scythebill +
  ds_annotations(
    st,
    alpha = 0,
    contour = "dominant",
    contour.col = "#E69F00",
    contour.args = list(threshold = 10, smooth = 9)
  ) +
  ds_scrolling(t.display = 1, width = 700, height = 300, res = 90)
```

![Scrolling spectrogram](NA)![](NA)

Dominant frequency contours jump when another frequency becomes the
loudest one (e.g. in fast trills), and fundamental frequency contours
are coarse for high-pitched sounds at low sampling rates, as in this
recording.

Contours measured elsewhere can be supplied as a list with one element
per selection: a data frame with the time (in s, as `start` and `end`)
and frequency (in kHz) of each point, a vector of frequencies evenly
spaced along the selection, or `NULL` for no contour. Supplied contours
are drawn as they are, and `NA` frequencies leave gaps in the line:

``` r

my_contours <- list(
  data.frame(
    time = c(0.34, 0.45, 0.5, 0.8, 1.03),
    freq = c(3.7, 3.2, 2.8, 2.9, 2.65)
  ),
  c(2.6, 3.2, 3.7, 3.3, 3, 2.8),
  NULL,
  c(2.2, 2.7, 3.1, 3.2, 3.1, 2.7)
)

ds_scythebill +
  ds_annotations(
    st,
    col = "colors",
    labels = "labels",
    contour = my_contours,
    contour.col = "#E69F00",
    contour.lwd = 3
  )
```

![Spectrogram of a 3.05 s sound](NA)

They can also be a table with one row per selection and the frequencies
in numeric columns, such as the output of
[`warbleR::freq_ts()`](https://marce10.github.io/warbleR/reference/freq_ts.html):

``` r

# freq_ts() reads the sound from a file
data("Phae.long1", package = "NatureSounds")
data("lbh_selec_table", package = "warbleR")

sound_dir <- tempfile()
dir.create(sound_dir)
tuneR::writeWave(Phae.long1, file.path(sound_dir, "Phae.long1.wav"))

hermit_st <- lbh_selec_table[lbh_selec_table$sound.files == "Phae.long1.wav", ]

hermit_contours <- warbleR::freq_ts(
  hermit_st,
  type = "dominant",
  wl = 300,
  length.out = 30,
  img = FALSE,
  pb = FALSE,
  path = sound_dir
)

dynaspec(Phae.long1, flim = c(1, 10)) +
  ds_spectrogram(wl.ms = 13) +
  ds_annotations(
    hermit_st,
    alpha = 0.1,
    contour = hermit_contours,
    contour.col = "tomato"
  )
```

![Spectrogram of a 2.5 s sound](NA)

## Captions

[`ds_captions()`](https://marce10.github.io/dynaSpec/reference/ds_captions.md)
adds text at a fixed position of the figure (`x` and `y`, as proportions
of the figure width and height), shown during a time range of the sound
(`start` and `end`, in s). `fading` adds fade-in and fade-out effects
(in s), and `col`, `cex` and `font` set the color, size and font.
Several captions can be shown one after the other, and several caption
layers can be added at different positions. The following code labels
the frog species calling in a recording from Costa Rica:

``` r

# frog recording from GitHub
frogs <- tempfile(fileext = ".wav")
download.file(
  "https://github.com/maRce10/example_sounds/raw/refs/heads/main/CostaRican_frogs.wav",
  frogs,
  mode = "wb"
)

dynaspec(frogs, flim = c(0, 5.5), tlim = c(35.3, 50.5)) +
  ds_spectrogram(
    wl.ms = 11.6,
    ovlp = 95,
    pal = cividis,
    collevels = seq(-60, 0, 5),
    col.bg = "black"
  ) +
  ds_oscillogram(col = "#31688E", height = 1 / 3) +
  ds_captions(
    c("Dendropsophus ebraccatus", "Eleutherodactylus coqui"),
    start = c(35.7, 42.3),
    end = c(40.8, 50.1),
    x = 0.33,
    y = 0.86,
    col = "#FFEA46CC",
    font = 3,
    fading = 0.6
  ) +
  ds_scrolling(
    t.display = 1.1,
    loop = 3,
    width = 1200,
    height = 550,
    res = 200
  ) +
  ds_theme(col.line = "#FFFFFFCC", lty.line = 3)
```

![Scrolling spectrogram](NA)![](NA)![](NA)

Note that times of annotations and captions refer to the original
recording, even when only part of it is used (`tlim`).

## Player

[`ds_player()`](https://marce10.github.io/dynaSpec/reference/ds_player.md)
sets the speeds offered by the speed slider of web pages (the first one
is the initial speed; a single speed hides the slider), whether the
pitch is kept when slowing down by default and the additional controls
shown (`buttons`). A player with only the play button, for instance to
embed in a presentation:

``` r

ds_scythebill + ds_player(speeds = 1, buttons = character())
```

![Spectrogram of a 3.05 s sound](NA)

A player with all controls, starting at half speed and looping (the loop
button starts pressed when `ds_scrolling(loop)` is larger than 1):

``` r

ds_antshrike +
  ds_player(
    speeds = c(0.5, 1, 0.25),
    buttons = c("time", "loop", "volume", "download", "fullscreen")
  ) +
  ds_scrolling(t.display = 2, loop = 2)
```

![Scrolling spectrogram](NA)![](NA)

In scrolling spectrograms, `buttons = "zoom"` adds a slider (with -
and + buttons) to stretch or shrink the spectrogram, from 4 times less
to 4 times more time than `t.display` (at most the whole sound). The
spectrogram is drawn at twice the resolution so zooming in stays sharp,
which makes web pages larger:

``` r

ds_antshrike +
  ds_overview() +
  ds_player(buttons = c("zoom", "volume", "time")) +
  ds_scrolling(t.display = 3, fix.time = FALSE)
```

![Scrolling spectrogram](NA)![](NA)

The download button saves the sound embedded in the web page (named
after the sound file or ‘Wave’ object), and the full screen button
enlarges the player to the size of the screen (e.g. for presentations;
web browsers may not allow full screen in embedded frames).

## Comparing sounds

[`ds_compare()`](https://marce10.github.io/dynaSpec/reference/ds_compare.md)
shows two or more sounds one on top of the other and plays them in
sequence, from top to bottom. Layers are added as with
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
and apply to all sounds. `gap` sets the silence between sounds (in s).
Web pages have one set of controls for all sounds: speed, pitch, volume
and zoom apply to all of them, the time readout shows the number of the
sound being played and its time, the seek bar spans all sounds and the
loop button repeats the whole sequence. Clicking on a spectrogram plays
that sound from that point, and then the following ones:

``` r

data("female_barn_swallow")
data("male_barn_swallow")

ds_compare(female_barn_swallow, male_barn_swallow, flim = c(0, 10), gap = 0.3) +
  ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1)) +
  ds_oscillogram() +
  ds_player(buttons = c("progress", "time", "loop"))
```

![Spectrogram and oscillogram of a 4.75 s sound](NA)

![Spectrogram and oscillogram of a 4.75 s sound](NA)

Any number of sounds can be compared. All of them use the same frequency
axis (the lowest Nyquist frequency if their sampling rates differ, as in
this example) and the same time scale, so a second takes the same width
in all of them. `same.scale = FALSE` fits each sound to the width of the
figure instead:

``` r

three_songs <- ds_compare(
  female_barn_swallow,
  male_barn_swallow,
  scythebill
) +
  ds_spectrogram(wl.ms = 12, collevels = seq(-50, 0, 1)) +
  ds_static(height = 300)

three_songs
```

![Spectrogram of a 4.75 s sound](NA)

![Spectrogram of a 4.75 s sound](NA)

![Spectrogram of a 4.75 s sound](NA)

All styles and panels can be used, as long as all sounds use the same
style:

``` r

three_songs + ds_scrolling(t.display = 1, width = 700, height = 250, res = 90)
```

![Scrolling spectrogram](NA)![](NA)

![Scrolling spectrogram](NA)![](NA)

![Scrolling spectrogram](NA)![](NA)

``` r


three_songs + ds_paged(t.display = 1)
```

![Spectrogram, page 1 of 2](NA)![Spectrogram, page 2 of 2](NA)

![Spectrogram, page 1 of 5](NA)![Spectrogram, page 2 of
5](NA)![Spectrogram, page 3 of 5](NA)![Spectrogram, page 4 of
5](NA)![Spectrogram, page 5 of 5](NA)

![Spectrogram, page 1 of 4](NA)![Spectrogram, page 2 of
4](NA)![Spectrogram, page 3 of 4](NA)![Spectrogram, page 4 of 4](NA)

Sounds can also be
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
objects, for settings or layers of a single sound (e.g. its own time
range, annotations or captions). Layers added to the comparison apply to
all sounds:

``` r

swallow_st <- data.frame(start = 0.2, end = 0.75, labels = "first notes")

female <- dynaspec(female_barn_swallow) +
  ds_annotations(swallow_st, labels = "labels")

male <- dynaspec(male_barn_swallow, tlim = c(0, 2.5))

ds_compare(female, male, flim = c(0, 10)) +
  ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))
```

![Spectrogram of a 2.5 s sound](NA)

![Spectrogram of a 2.5 s sound](NA)

Amplitudes are scaled within each sound, so the colors of the
spectrograms and the height of the oscillograms do not compare the
amplitude of the sounds. Videos (`.mp4`) and still images (`.png`) stack
those of each sound: in videos, each sound keeps its first frame until
it is played, and its last frame afterwards.

## Videos

Videos
([`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md)
with a file name ending in `.mp4`) use the same settings as web pages.
Videos of scrolling spectrograms render every frame, which can be sped
up by using several cores (`parallel`) and lowering the frames per
second (`fps`):

``` r

save_dynaspec(duet + ds_scrolling(parallel = 4, fps = 30), "house_wren_duet.mp4")
```

### Spectrograms of videos

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
also takes video files with sound (e.g. ‘.mp4’, ‘.mov’, ‘.mkv’ or
‘.webm’; reading them requires [ffmpeg](https://ffmpeg.org/)). Their
sound is used as the sound of the dynamic spectrogram, and
[`ds_video()`](https://marce10.github.io/dynaSpec/reference/ds_video.md)
shows the video above (`position = "above"`, default) or below the
spectrogram, playing along with the sound. `tlim` also cuts the video.
This clip shows a three-wattled bellbird (*Procnias tricarunculatus*)
singing (video by David Jimenez):

``` r


bellbird <- dynaspec("bellbird_song.mp4", flim = c(0.7, 8.4)) +
    ds_spectrogram(wl.ms = 21, collevels = seq(-30, 0, 1)) +
    ds_scrolling() +
    ds_theme_minimal(
        pal = function(n) viridis::mako(n, direction = -1),
        xlab = "Tiempo (s)",
        ylab = "Frec (kHz)",
        cex.lab = 1.5,
        col.lab = "black"
    ) +
    ds_video(position = "above", height = 2.5) + 
  ds_filter(bp  = c(1, Inf))

bellbird
```

![Scrolling spectrogram](NA)![](NA)

Videos work with all styles, and keep in step with the sound when it is
slowed down:

``` r

bellbird +
  ds_video(position = "below") +
  ds_scrolling(t.display = 1.5, width = 700, height = 250, res = 90)
```

![Scrolling spectrogram](NA)![](NA)

Saved as a video, the video and the spectrogram are shown one on top of
the other, with the sound of the video:

``` r

save_dynaspec(bellbird, "bellbird_song_spectrogram.mp4")
```

Web pages embed a silent copy of the video (up to 960 pixels wide), so
cut long videos with `tlim` to keep web pages small.

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
    #> [1] seewave_2.2.4       viridis_0.6.5       viridisLite_0.4.3  
    #> [4] dynaSpec_2.0.0.9000
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] NatureSounds_1.0.5 sass_0.4.10        tiff_0.1-12        jpeg_0.1-11       
    #>  [5] stringi_1.8.9      digest_0.6.39      magrittr_2.0.5     evaluate_1.0.5    
    #>  [9] grid_4.6.2         RColorBrewer_1.1-3 fastmap_1.2.0      jsonlite_2.0.0    
    #> [13] backports_1.5.1    tuneR_1.4.7        gridExtra_2.3.1    httr_1.4.9        
    #> [17] purrr_1.2.2        scales_1.4.0       pbapply_1.7-5      warbleR_1.1.38    
    #> [21] textshaping_1.0.5  jquerylib_0.1.4    cli_3.6.6          rlang_1.3.0       
    #> [25] fftw_1.0-9         cachem_1.1.0       yaml_2.3.12        otel_0.3.0        
    #> [29] parallel_4.6.2     tools_4.6.2        checkmate_2.3.4    ggplot2_4.0.3     
    #> [33] curl_8.1.0         vctrs_0.7.3        R6_2.6.1           png_0.1-9         
    #> [37] bmp_0.3.1          proxy_0.4-29       dtw_1.23-3         lifecycle_1.0.5   
    #> [41] stringr_1.6.0      fs_2.1.0           MASS_7.3-66        ragg_1.5.2        
    #> [45] pkgconfig_2.0.3    desc_1.4.3         pkgdown_2.2.1      bslib_0.12.0      
    #> [49] gtable_0.3.6       glue_1.8.1         Rcpp_1.1.2         systemfonts_1.3.2 
    #> [53] xfun_0.62          knitr_1.52         farver_2.1.2       readbitmap_0.1.5  
    #> [57] htmltools_0.5.9    imager_1.0.8       igraph_2.3.4       rmarkdown_2.32    
    #> [61] signal_1.8-1       compiler_4.6.2     S7_0.2.2
