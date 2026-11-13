# Theme of dynamic spectrograms

`ds_theme()` sets the look of a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object (axes, text, panels, colors and player controls), in the same way
for all styles. Adding it again changes the settings supplied (other
settings are kept). Preset themes (e.g. `ds_theme_dark()`) set several
settings at once.

## Usage

``` r
ds_theme(
  xlab = NULL,
  ylab = NULL,
  cex.lab = NULL,
  cex.axis = NULL,
  font.lab = NULL,
  col.lab = NULL,
  col.axis = NULL,
  axis.type = NULL,
  bg = NULL,
  family = NULL,
  las = NULL,
  tick.length = NULL,
  box = NULL,
  col.box = NULL,
  grid = NULL,
  col.grid = NULL,
  col.panel = NULL,
  col.title = NULL,
  cex.title = NULL,
  pal = NULL,
  col.bg = NULL,
  col.wave = NULL,
  col.line = NULL,
  lty.line = NULL,
  lwd.line = NULL,
  col.highlight = NULL,
  col.annotation = NULL,
  col.bar = NULL,
  bg.bar = NULL,
  bar.style = NULL
)

ds_theme_dark(...)

ds_theme_presentation(...)

ds_theme_paper(...)

ds_theme_minimal(...)

ds_theme_transparent(...)

ds_theme_void(...)

ds_theme_macaulay(...)

ds_theme_xenocanto(...)

ds_theme_mako(...)
```

## Arguments

- xlab, ylab:

  Titles of the time and frequency axes. Defaults are "Time (s)" and
  "Frequency (kHz)".

- cex.lab, cex.axis:

  Size of axis titles and axis labels, relative to the style default
  (e.g. 1.5 is 50% larger). Defaults are 1 (1.2 for axis titles of
  static spectrograms).

- font.lab:

  Font of axis titles: 1 (plain), 2 (bold), 3 (italic) or 4 (bold
  italic). Defaults are 1 (static and scrolling) and 2 (paged).

- col.lab:

  Color of axes, axis labels and titles. Default is black (paged
  spectrograms: picked to contrast with the background).

- col.axis:

  Color of the axis labels (tick labels), when they should differ from
  the axis titles and lines. Default is 'col.lab'.

- axis.type:

  Character string: "standard" (axes with labels and titles), "inside"
  (no axis titles; frequency labels inside the spectrogram, e.g. "8k",
  and time labels below the lowest panel, e.g. "2s"), "compact" (no axis
  titles; frequency labels below short ticks in a narrow column on the
  left, with the unit at the bottom, and time labels in minutes and
  seconds, e.g. "0:02", above the figure), "minimal" (1 s and 1 kHz
  scale bars; scrolling spectrograms only) or "none" (no axes). Defaults
  are "standard" (static and scrolling) and "none" (paged).

- bg:

  Background color of the figure around the panels ("transparent" for a
  transparent background). Default is white. The backgrounds of the
  panels are set separately: 'col.bg' (spectrograms, see
  [`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md))
  and 'col.panel' (other panels).

- family:

  Font family of all text, including captions and annotation labels:
  "sans", "serif", "mono" or the name of a font installed in the system.
  Default is "sans".

- las:

  Orientation of the axis labels (tick labels), as in
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html): 0 (parallel
  to the axis), 1 (horizontal), 2 (perpendicular to the axis) or 3
  (vertical). Applies to both axes (e.g. 2 makes time labels vertical
  and frequency labels horizontal) with standard axes ('axis.type').
  Defaults are 1 (static and paged: horizontal labels) and 0 (scrolling:
  vertical frequency labels).

- tick.length:

  Length of axis ticks, in lines of text (0 hides ticks). Defaults are
  0.5 (static and scrolling) and 0.3 (paged).

- box:

  Logical argument to control whether boxes are drawn around panels.
  Default is `TRUE`.

- col.box:

  Color of the boxes around panels. Default is 'col.lab'.

- grid:

  Logical argument to control whether horizontal grid lines are drawn at
  the frequency axis ticks of spectrograms. Default is `FALSE`.

- col.grid:

  Color of grid lines. Default is a translucent gray.

- col.panel:

  Background color of oscillogram, envelope and power spectrum panels.
  Default is 'bg' (the figure background shows through).

- col.title, cex.title:

  Color and size of titles (the title of paged spectrograms and the
  'Amp.' label of power spectra). Defaults are 'col.lab' and 1.

- pal, col.bg:

  Color palette and background of spectrograms (see
  [`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)).
  Used when not supplied in
  [`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md).

- col.wave:

  Color of oscillograms, envelopes and power spectra. Used when not
  supplied in
  [`ds_oscillogram()`](https://marce10.github.io/dynaSpec/reference/ds_oscillogram.md),
  [`ds_envelope()`](https://marce10.github.io/dynaSpec/reference/ds_envelope.md)
  or
  [`ds_spectrum()`](https://marce10.github.io/dynaSpec/reference/ds_spectrum.md).

- col.line:

  Color of the play line (and of the cursor of paged spectrograms and
  the active player buttons). Defaults are "#07889B99" (scrolling),
  "white" (paged) and `viridis::mako(10)[9]` (static).

- lty.line, lwd.line:

  Type and width of the play line of scrolling and static spectrograms,
  as in [`graphics::par()`](https://rdrr.io/r/graphics/par.html):
  'lty.line' is an integer (0-6) or a name ("solid", "dashed", "dotted",
  "dotdash", "longdash", "twodash" or "blank"), and 'lwd.line' a width
  in which 1 is 1/96 inch (scaled with the resolution). Defaults are 2
  and 2 (scrolling), and a solid line 2 pixels wide (static).

- col.highlight:

  Color of the highlight box of paged spectrograms (default "#4B0C6BFF")
  and of the translucent boxes dimming the parts of the
  [`ds_overview()`](https://marce10.github.io/dynaSpec/reference/ds_overview.md)
  outside the window shown in scrolling spectrograms. Default is gray in
  overviews.

- col.annotation:

  Color of the boxes of
  [`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md).
  Used when 'col' is not supplied in
  [`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md).

- col.bar, bg.bar:

  Color of the text and background of the player controls of web pages
  (play button, sliders, ...). Defaults are dark gray and transparent
  (the page background).

- bar.style:

  Style of the player controls of web pages: "standard" (rectangular
  buttons), "rounded" (round play button, rounded controls, a seek bar
  and a frame with rounded corners around the figure; 'bg.bar' is used
  as the background of the whole player) or "square" (flat bar with a
  square play button and sliders in the color of the play line). With
  "rounded"
  [`ds_player()`](https://marce10.github.io/dynaSpec/reference/ds_player.md)
  shows the seek bar and the time by default
  (`buttons = c("progress", "time")`), and with "square" the volume
  control and the time (`buttons = c("volume", "time")`). Default is
  "standard".

- ...:

  Settings of `ds_theme()` that replace those of the preset theme.

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## Details

Only the settings supplied are used: settings left as `NULL` take the
defaults of each style (given for each setting above). Settings supplied
in other layers (e.g. the palette in
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md))
take precedence over those of the theme.

Preset themes are complete themes: adding one replaces previous themes
(use `ds_theme()` afterwards to change some of its settings, or supply
them in the preset):

- `ds_theme_dark()`: black figure background, white text, 'mako' palette
  (the spectrogram background is its darkest color, as in all presets).

- `ds_theme_mako()`: light theme with the 'mako' palette (dark blue
  background and light sounds) and 'mako' colors for panels,
  annotations, play line and player controls.

- `ds_theme_presentation()`: large bold text for slides and projectors.

- `ds_theme_paper()`: black and white with serif fonts, for printed
  figures.

- `ds_theme_minimal()`: no boxes, light gray axes and short ticks.

- `ds_theme_transparent()`: transparent background, to blend in slides
  or web pages of any color.

- `ds_theme_void()`: no axes, titles or boxes (only the sound).

- `ds_theme_macaulay()`: inspired by the player of the 'Macaulay
  Library': gray spectrogram, compact axes (time in minutes and seconds
  above the figure) and a black player bar with a square play button.
  Best with
  [`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
  and an
  [`ds_overview()`](https://marce10.github.io/dynaSpec/reference/ds_overview.md)
  (e.g. `ds_scrolling(t.display = 1.5, fix.time = FALSE)`).

- `ds_theme_xenocanto()`: inspired by the player of the 'Xeno-canto' web
  site: gray spectrogram, labels inside the figure, solid play line and
  rounded player controls with a seek bar. Best with
  [`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
  and an
  [`ds_oscillogram()`](https://marce10.github.io/dynaSpec/reference/ds_oscillogram.md)
  (e.g. `ds_scrolling(t.display = 1.5, fix.time = FALSE)`).

## See also

[`ds_set_theme()`](https://marce10.github.io/dynaSpec/reference/ds_set_theme.md)
to set a theme for all spectrograms,
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)

## Examples

``` r
data(scythebill)

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
  ds_theme(xlab = "Time (s)", ylab = "Frequency (kHz)", cex.lab = 1.4, font.lab = 2)

# preset theme with larger axis titles
x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) + ds_oscillogram() +
  ds_theme_dark(cex.lab = 1.4)

# Macaulay Library look
data(antshrike)
x <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1)) +
  ds_overview() + ds_theme_macaulay() + ds_scrolling(t.display = 3, fix.time = FALSE)

# Xeno-canto look (and its color version)
x <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1)) + ds_oscillogram(height = 0.15) +
  ds_theme_xenocanto() + ds_scrolling(t.display = 3, fix.time = FALSE)
x <- x + ds_theme_xenocanto(pal = viridis::inferno, col.bg = "black", bg = "black",
  col.panel = "black", col.wave = "white", col.lab = "white", col.line = "white")
```
