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
  line.freq = NULL
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

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`. Several annotation layers can be added.

## Details

Tables with a 'sound.files' column (e.g. 'warbleR' selection tables) can
contain selections of several sound files; only those of the sound shown
are used (see 'sound.file').

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
  ds_spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
  ds_annotations(st, col = "colors", labels = "labels")

# dashed borders of the same color as the boxes
x <- x + ds_annotations(st, col = "colors", alpha = 0.1, border = "colors", lwd = 2, lty = "dashed")

# spectrogram colored within the selections
x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
  ds_annotations(st, col = "colors", type = "spectrogram")

# lines above the notes (one height per note type)
st$line.freq <- c(4.6, 4.9, 4.9, 5.2)
x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
  ds_annotations(st, col = "colors", type = "line", line.freq = "line.freq", labels = "labels")
```
