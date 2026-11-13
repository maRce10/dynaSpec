# Annotation layer of dynamic spectrograms

`ds_annotations()` highlights parts of the sound in a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with translucent boxes (e.g. calls, songs or the parts of a
duet), optionally with borders and labels, or with horizontal lines
above the sounds (e.g. to show which individual sings each part of a
duet).

## Usage

``` r
ds_annotations(
  X,
  col = NULL,
  alpha = 0.3,
  type = "box",
  border = NULL,
  lwd = 1,
  lty = 1,
  labels = NULL,
  cex = 1,
  freq = TRUE,
  sound.file = NULL,
  line.freq = NULL,
  contour = NULL,
  contour.col = NULL,
  contour.lwd = 2,
  contour.args = list()
)
```

## Arguments

- X:

  Data frame with the selections: columns 'start' and 'end' (in s) and,
  optionally, 'bottom.freq' and 'top.freq' (in kHz) to restrict boxes to
  a frequency range. Selection tables from 'warbleR' (or tables from
  'ohun' or 'Rraven') can be used directly (see details). Extended
  selection tables are not supported.

- col:

  Color of the boxes, or the name of a column of 'X' with the color of
  each box. Default is `NULL`: 'col.annotation' of
  [`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
  or "#E69F00".

- alpha:

  Opacity of the boxes or lines (0-1). Default is 0.3 (1 for lines).

- type:

  Character string: "box" (translucent boxes), "spectrogram" (the
  spectrogram is colored within each selection, with a gradient from the
  spectrogram background to the color of the selection, as
  [`warbleR::color_spectro()`](https://marce10.github.io/warbleR/reference/color_spectro.html);
  only borders and labels are drawn on top) or "line" (a horizontal line
  from the start to the end of each selection at the frequency set by
  'line.freq', drawn on spectrograms only; labels are shown above the
  lines). Default is "box".

- border:

  Color of the box borders, or the name of a column of 'X' with the
  border color of each box. Default is `NULL` (no borders).

- lwd:

  Width of the box borders, or of the lines when `type = "line"`.
  Default is 1 (8 for lines).

- lty:

  Line type of the box borders (or of the lines), as in
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html): an integer
  (0-6) or a name ("solid", "dashed", "dotted", "dotdash", "longdash" or
  "twodash"). Default is 1 (solid).

- labels:

  Name of a column of 'X' with labels to show at the top of each box (or
  above each line). Default is `NULL` (no labels).

- cex:

  Size of the labels. Default is 1.

- freq:

  Logical to control if the frequency range of selections is used (when
  'X' has 'bottom.freq' and 'top.freq' columns). If `FALSE` boxes span
  the whole frequency range. Default is `TRUE`.

- sound.file:

  Name of the sound file whose selections are shown, when 'X' has a
  'sound.files' column with several sound files. Default is `NULL`: when
  the sound of
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
  is a file, the selections of that file are shown (matching the file
  name); otherwise 'X' must contain a single sound file.

- line.freq:

  Frequency (in kHz) at which lines are drawn when `type = "line"`, or
  the name of a column of 'X' with the frequency of each line (e.g. a
  different height for each individual of a duet). Default is `NULL`:
  near the top of the spectrogram (90% of the frequency range shown).

- contour:

  Frequency contours to draw over the spectrogram within each selection.
  Default is `NULL` (no contours). It can be:

  - "dominant" or "fundamental": contours measured on the sound with
    [`seewave::dfreq()`](https://rdrr.io/pkg/seewave/man/dfreq.html) or
    [`seewave::fund()`](https://rdrr.io/pkg/seewave/man/fund.html),
    within the frequency range of the selection ('bottom.freq' and
    'top.freq' columns, if 'freq' is `TRUE`) or the frequency limits of
    [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md).

  - A list with one element per selection (`NULL` for no contour): a
    numeric vector of frequencies (in kHz) evenly spaced from the start
    to the end of the selection, or a data frame or matrix with the time
    (in s, as in 'start' and 'end') and frequency (in kHz) of each
    point, in that order or in columns named 'time' and 'freq'.

  - A data frame or matrix with one row per selection and the
    frequencies (in kHz) evenly spaced from its start to its end in
    numeric columns, as the output of
    [`warbleR::freq_ts()`](https://marce10.github.io/warbleR/reference/freq_ts.html)
    (columns 'sound.files' and 'selec' are ignored).

  `NA` frequencies leave gaps in the contour.

- contour.col:

  Color of the contours, or the name of a column of 'X' with the color
  of each contour. Default is `NULL` (the color of the selection,
  opaque).

- contour.lwd:

  Width of the contours. Default is 2.

- contour.args:

  List with the settings used to measure contours: 'wl.ms' (window
  length, in ms; takes precedence over 'wl') or 'wl' (in samples);
  default is the window of
  [`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md),
  or 11.61 ms, 'ovlp' (percent overlap; default 90), 'threshold'
  (amplitude threshold, as a percentage, below which no frequency is
  measured; default 5) and 'smooth' (number of consecutive values of the
  running median and mean used to smooth contours; default 5, and 1 for
  no smoothing).

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`. Several annotation layers can be added.

## Details

Tables with a 'sound.files' column (e.g. 'warbleR' selection tables) can
contain selections of several sound files; only those of the sound shown
are used (see 'sound.file').

Contours are only drawn on spectrograms. To show contours without boxes
use `alpha = 0`. Dominant frequency contours jump between frequencies
when another frequency becomes the loudest one (e.g. fast trills), and
fundamental frequency contours are coarse for high-pitched sounds at low
sampling rates (they are measured in steps of the sampling period).

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`ds_captions()`](https://marce10.github.io/dynaSpec/reference/ds_captions.md)

## Examples

``` r
data(scythebill)

# note types
st <- data.frame(start = c(0.34, 1.13, 1.44, 1.75), end = c(1.03, 1.36, 1.68, 2.28),
                 labels = c("A", "B", "B", "C"),
                 colors = c("#E69F00", "#56B4E9", "#56B4E9", "#009E73"))

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
  ds_annotations(st, col = "colors", labels = "labels")

# dashed borders of the same color as the boxes
x <- x + ds_annotations(st, col = "colors", alpha = 0.1, border = "colors", lwd = 2, lty = "dashed")

# spectrogram colored within the selections
x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
  ds_annotations(st, col = "colors", type = "spectrogram")

# lines above the notes (one height per note type)
st$line.freq <- c(4.6, 4.9, 4.9, 5.2)
x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
  ds_annotations(st, col = "colors", type = "line", line.freq = "line.freq", labels = "labels")

# frequency contours
x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
  ds_annotations(st, col = "colors", labels = "labels", contour = "dominant",
                 contour.col = "#E69F00")
```
