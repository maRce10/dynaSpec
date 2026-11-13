# Spectrogram layer of dynamic spectrograms

`ds_spectrogram()` adds a spectrogram to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object and sets how it is computed and drawn. Adding it again changes
its settings.

## Usage

``` r
ds_spectrogram(
  wl = NULL,
  wl.ms = NULL,
  ovlp = NULL,
  wn = NULL,
  pal = NULL,
  collevels = NULL,
  col.bg = NULL,
  amp.trans = 1,
  ...
)
```

## Arguments

- wl:

  Window length (in samples), as in
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html).
  Used when 'wl.ms' is not supplied.

- wl.ms:

  Window length in milliseconds (as 'Window length' in Praat). Takes
  precedence over 'wl'. When neither is supplied the window lasts 11.61
  ms (512 samples at 44.1 kHz) in all styles. The window length in
  samples is computed from the sampling rate of the sound (for paged
  spectrograms, after resampling), so sounds with different sampling
  rates (e.g. in
  [`ds_compare()`](https://marce10.github.io/dynaSpec/reference/ds_compare.md))
  get windows of the same duration and the same time resolution.

- ovlp:

  Percent overlap between consecutive windows, as in
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html).

- wn:

  Window function name, as in
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html)
  (e.g. "hanning" or "blackman").

- pal:

  Color palette of the spectrogram, from low to high amplitude: a
  palette function (e.g.
  [`viridis::viridis()`](https://sjmgarnier.github.io/viridis/reference/reexports.html)
  or
  [`seewave::reverse.gray.colors.1()`](https://rdrr.io/pkg/seewave/man/seewave.internal.html)),
  the name of a 'viridis' palette (e.g. "magma"), or two or more colors
  for a gradient (e.g. `c("white", "black")` or
  `c("white", "gold", "red", "black")`).

- collevels:

  Numeric vector with the amplitude levels (in dB) of the colors, as in
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html)
  (e.g. `seq(-40, 0, 1)`).

- col.bg:

  Background color of the spectrogram, shown where the amplitude is
  below the lowest level of 'collevels'. Default is the color of the
  lowest amplitude of the palette (e.g. white for
  [`seewave::reverse.gray.colors.1()`](https://rdrr.io/pkg/seewave/man/seewave.internal.html),
  the default palette, or dark blue for
  [`viridis::mako()`](https://sjmgarnier.github.io/viridis/reference/reexports.html)),
  so quiet parts and the background look the same.

- amp.trans:

  Amplitude transformation to brighten paged spectrograms (values \> 1
  brighten quiet sounds; only used by
  [`ds_paged()`](https://marce10.github.io/dynaSpec/reference/styles.md)).

- ...:

  Additional arguments passed to
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html).

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## Details

Only the arguments supplied are used. Arguments with default `NULL` take
the defaults of each style
([`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md),
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
or
[`ds_paged()`](https://marce10.github.io/dynaSpec/reference/styles.md)):

- 'wl.ms' (or 'wl'): 11.61 ms (512 samples at 44.1 kHz) in all styles.

- 'ovlp': 70 (static and scrolling) and 90 (paged).

- 'wn': "hanning" (static and scrolling) and "blackman" (paged).

- 'pal':
  [`seewave::reverse.gray.colors.1()`](https://rdrr.io/pkg/seewave/man/seewave.internal.html)
  (static and scrolling) and "inferno" (paged).

- 'collevels': `seq(-40, 0, 1)` (static), 30 levels from the lowest to
  the highest amplitude, as in
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html)
  (scrolling) and `seq(-30, 0, 1)` (paged).

- 'col.bg': the color of the lowest amplitude of the palette.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)

## Examples

``` r
data(scythebill)

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, pal = viridis::mako, collevels = seq(-60, 0, 1))

# window length in milliseconds (the same duration for any sampling rate)
x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 15, collevels = seq(-50, 0, 1))
```
