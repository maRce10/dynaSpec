# Caption layer of dynamic spectrograms

`ds_captions()` adds text to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object at a fixed position of the figure, shown (fading in and out)
during a time range of the sound, like subtitles. They are shown in the
same way in all styles, in web pages and videos. To label parts of the
spectrogram use the 'labels' argument of
[`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md)
instead.

## Usage

``` r
ds_captions(
  labels,
  start,
  end,
  x = 0.5,
  y = 0.9,
  fading = 0,
  col = "black",
  cex = 1,
  font = 1
)
```

## Arguments

- labels:

  Character vector with the captions.

- start, end:

  Numeric vectors with the start and end times (in s) of each caption.

- x, y:

  Position of the center of the captions as proportions of the figure
  width and height (0-1, from the left and bottom). Defaults are 0.5 and
  0.9.

- fading:

  Duration (in s) of the fade-in and fade-out of captions. Default is 0.

- col, cex, font:

  Color, size and font (1 = plain, 2 = bold, 3 = italic, 4 = bold
  italic) of the captions. Defaults are "black", 1 and 1.

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`. Several caption layers can be added (e.g. at different
positions).

## Details

Each caption layer shows one caption at a time; when the time ranges of
its captions overlap the most visible one is shown.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md)

## Examples

``` r
data(scythebill)

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
  ds_captions(c("long note", "arched notes", "trill"), start = c(0.34, 1.13, 1.75),
              end = c(1.03, 1.68, 2.28), fading = 0.2)
```
