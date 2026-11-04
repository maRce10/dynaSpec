# Styles of dynamic spectrograms

Set the style of a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object:

- `ds_scrolling()`: the spectrogram scrolls from right to left along a
  fixed play line.

- `ds_paged()`: the spectrogram is split into pages, and a translucent
  box reveals the part of each page being played.

- `ds_static()`: a play line moves over the whole spectrogram (and
  oscillogram), which can be split in several rows (default style).

## Usage

``` r
ds_scrolling(
  t.display,
  fix.time,
  buffer,
  loop,
  lcol,
  lty,
  lwd,
  speed,
  width,
  height,
  res,
  fps,
  parallel,
  ...
)

ds_paged(
  t.display,
  title,
  highlight.col,
  highlight.alpha,
  lcol,
  plot.legend,
  spec.width,
  spec.height,
  fps,
  parallel,
  ...
)

ds_static(rows, width, height, res, lcol, alt, fps, play.line)
```

## Arguments

- t.display:

  Time (in s) shown at once: the time window of scrolling spectrograms
  (default 1.5) or the duration of each page of paged spectrograms
  (default: the whole sound, up to 5 s).

- fix.time:

  Logical: does the time axis stay fixed (`TRUE`, default) or move along
  with the scrolling spectrogram?

- buffer:

  Time (in s) before the scrolling spectrogram starts moving. Default is
  0.

- loop:

  Number of times the sound is repeated in scrolling videos (web pages
  loop until paused when \> 1). Default is 1.

- lcol:

  Color of the play line (or cursor of paged spectrograms). Defaults are
  "#07889B99" (scrolling), "white" (paged) and `viridis::mako(10)[9]`
  (static).

- lty, lwd:

  Type and width of the play line of scrolling spectrograms (defaults 2
  and 2).

- speed:

  Initial playback speed of scrolling spectrograms (videos are played at
  this speed). Default is 1.

- width, height, res:

  Size (in pixels) and resolution of the figures: frames of scrolling
  spectrograms (defaults 700, 400 and 70), and each row of static
  spectrograms (defaults 1000, 500 and 100).

- fps:

  Frames per second of videos. Defaults are 50 (scrolling) and 30 (paged
  and static).

- parallel:

  Number of cores used to render videos of scrolling and paged
  spectrograms. Default is 1.

- ...:

  Other arguments of the style:

  - `ds_scrolling()`: 'hop.size' (window length in ms, used when 'wl' is
    not set in
    [`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md);
    default 11.6), 'height.prop' (relative heights of the scrolling
    spectrogram and the lower panel; default `c(5, 1)`), 'ggspectro'
    (use
    [`seewave::ggspectro()`](https://rdrr.io/pkg/seewave/man/ggspectro.html);
    default `FALSE`), 'derivative' (spectral derivatives; requires
    'imager'; default `FALSE`) and 'spectro.call' (a call to a
    spectrogram function replacing the default one, made with
    [`base::call()`](https://rdrr.io/r/base/call.html)).

  - `ds_paged()`: 'resample.rate' (sampling rate in Hz used for the
    spectrogram; default 15000), 'amp.thresh' (amplitude threshold, as a
    percentage; default 0), 'filter' (frequency range in kHz to filter
    out), 'dest.path' (folder for temporary files) and 'delete.temp'
    (delete temporary files; default `TRUE`).

- title:

  Title of paged spectrograms.

- highlight.col, highlight.alpha:

  Color and opacity of the highlight box of paged spectrograms (defaults
  "#4B0C6BFF" and 0.6).

- plot.legend:

  Logical: show the amplitude color scale of paged spectrograms? Default
  is `FALSE`.

- spec.width, spec.height:

  Size (in inches) of the pages of paged spectrograms (defaults 9 and
  3).

- rows:

  Number of rows in which static spectrograms are split (each row shows
  a consecutive segment of the same duration). Default is 1.

- alt:

  Alternative text of static spectrograms in web pages (for screen
  readers).

- play.line:

  Logical: show the line moving along the sound in static spectrograms?
  With `FALSE` web pages show the figure without the moving line
  (clicking on it still plays the sound from that point) and videos show
  the figure with the sound. Default is `TRUE`.

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## Details

Only the arguments supplied are used; other settings take the defaults
shown.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md)

## Examples

``` r
data(scythebill)

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl = 200, collevels = seq(-50, 0, 1))

if (FALSE) { # \dontrun{
data(antshrike)
y <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl = 256, collevels = seq(-50, 0, 1))
y + ds_scrolling(t.display = 2, axis.type = "minimal")
x + ds_paged(t.display = 1, axis.type = "standard")
x + ds_oscillogram() + ds_static(rows = 2, height = 350)
} # }
```
