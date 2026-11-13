# Styles of dynamic spectrograms

Set the style of a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object:

- `ds_scrolling()`: the spectrogram scrolls from right to left past the
  play line, which stays at the center (it only moves at the start and
  the end, while the spectrogram is still, so the window shown is always
  full of sound; see 'still.ends').

- `ds_paged()`: the spectrogram is split into pages, and a translucent
  box reveals the part of each page being played.

- `ds_static()`: a play line moves over the whole spectrogram (and
  oscillogram), which can be split in several rows (default style).

## Usage

``` r
ds_scrolling(
  t.display = 1.5,
  still.ends = TRUE,
  fix.time = TRUE,
  buffer = 0,
  loop = 1,
  speed = 1,
  width = 700,
  height = 400,
  res = 70,
  fps = 50,
  parallel = 1,
  ...
)

ds_paged(
  t.display = NULL,
  title = NULL,
  highlight.alpha = 0.6,
  legend = FALSE,
  width = 1350,
  height = 450,
  res = 150,
  fps = 30,
  parallel = 1,
  ...
)

ds_static(
  rows = 1,
  width = 1000,
  height = 500,
  res = 100,
  alt = NULL,
  fps = 30,
  play.line = TRUE,
  ...
)
```

## Arguments

- t.display:

  Time (in s) shown at once: the time window of scrolling spectrograms
  (default 1.5) or the duration of each page of paged spectrograms
  (default: the whole sound, up to 5 s).

- still.ends:

  Logical: keep the window shown within the sound in scrolling
  spectrograms? With `TRUE` the spectrogram is still at the start, while
  the play line moves from the left edge to the center, then scrolls
  with the line at the center, and is still again at the end, while the
  line moves to the right edge (so the window never shows the empty room
  before the start or after the end of the sound). With `FALSE` the line
  stays at the center and the sound enters from the center and leaves at
  the center. Looping spectrograms (`loop > 1`) always scroll
  continuously. Default is `TRUE`.

- fix.time:

  Logical: does the time axis stay fixed (`TRUE`, default) or move along
  with the scrolling spectrogram?

- buffer:

  Time (in s) before the scrolling spectrogram starts moving. Default is
  0.

- loop:

  Number of times the sound is repeated in scrolling videos (web pages
  loop until paused when \> 1). Default is 1.

- speed:

  Playback speed of videos of scrolling spectrograms (e.g. 0.5 for half
  speed). Web pages start at the first speed of
  [`ds_player()`](https://marce10.github.io/dynaSpec/reference/ds_player.md)
  ('speeds'), as in all styles. Default is 1.

- width, height, res:

  Size (in pixels) and resolution of the figures: frames of scrolling
  spectrograms (defaults 700, 400 and 70), pages of paged spectrograms
  (defaults 1350, 450 and 150; web pages show them at a higher
  resolution), and each row of static spectrograms (defaults 1000, 500
  and 100).

- fps:

  Frames per second of videos. Defaults are 50 (scrolling) and 30 (paged
  and static).

- parallel:

  Number of cores used to render videos of scrolling and paged
  spectrograms. Default is 1.

- ...:

  Other arguments of the style:

  - `ds_static()`: none.

  - `ds_scrolling()`: 'ggspectro' (use
    [`seewave::ggspectro()`](https://rdrr.io/pkg/seewave/man/ggspectro.html);
    default `FALSE`), 'derivative' (spectral derivatives; requires
    'imager'; default `FALSE`) and 'spectro.call' (a call to a
    spectrogram function replacing the default one, made with
    [`base::call()`](https://rdrr.io/r/base/call.html)).

  - `ds_paged()`: 'resample.rate' (sampling rate in Hz used for the
    spectrogram; default 15000), 'amp.thresh' (amplitude threshold, as a
    percentage; default 0), 'dest.path' (folder for temporary files) and
    'delete.temp' (delete temporary files; default `TRUE`).

- title:

  Title of paged spectrograms.

- highlight.alpha:

  Opacity of the highlight box of paged spectrograms (default 0.6).

- legend:

  Logical: show the amplitude color scale of paged spectrograms? Default
  is `FALSE`.

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

Only the arguments supplied are used: the defaults shown are those of
each style, which apply when an argument is not supplied (adding the
style again keeps the arguments supplied before and changes those
supplied again).

The play line (its color, type and width) and the color of the highlight
box of paged spectrograms are set with
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
('col.line', 'lty.line', 'lwd.line' and 'col.highlight'), for all
styles.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md),
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)

## Examples

``` r
data(scythebill)

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1))

if (FALSE) { # \dontrun{
data(antshrike)
y <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))
y + ds_scrolling(t.display = 2, axis.type = "minimal")
x + ds_paged(t.display = 1, axis.type = "standard")
x + ds_oscillogram() + ds_static(rows = 2, height = 350)
x + ds_theme(col.line = "red", lty.line = "dashed", lwd.line = 3)
} # }
```
