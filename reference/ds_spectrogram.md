# Spectrogram layer of dynamic spectrograms

`ds_spectrogram()` adds a spectrogram to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object and sets how it is computed and drawn. Adding it again changes
its settings.

## Usage

``` r
ds_spectrogram(wl, ovlp, wn, pal, collevels, colbg, amp.trans, ...)
```

## Arguments

- wl:

  Window length (in samples), as in
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html).

- ovlp:

  Percent overlap between consecutive windows, as in
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html).

- wn:

  Window function name, as in
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html)
  (e.g. "hanning" or "blackman").

- pal:

  Color palette function (e.g.
  [`viridis::viridis()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html)
  or
  [`seewave::reverse.gray.colors.1()`](https://rdrr.io/pkg/seewave/man/seewave.internal.html)).
  Paged spectrograms also take the names of 'viridis' palettes (e.g.
  "magma") and 2 colors for a gradient (e.g. `c("white", "black")`).

- collevels:

  Numeric vector with the amplitude levels (in dB) of the colors, as in
  [`seewave::spectro()`](https://rdrr.io/pkg/seewave/man/spectro.html)
  (e.g. `seq(-40, 0, 1)`).

- colbg:

  Background color of the spectrogram, shown where the amplitude is
  below the lowest level of 'collevels'. Default is the color of the
  lowest amplitude of the palette (e.g. white for
  [`seewave::reverse.gray.colors.1()`](https://rdrr.io/pkg/seewave/man/seewave.internal.html),
  the default palette, or dark blue for
  [`viridis::mako()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html)),
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

Only the arguments supplied are used; other settings take the defaults
of each style (see
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md),
[`ds_paged()`](https://marce10.github.io/dynaSpec/reference/styles.md)
and
[`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)).

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)

## Examples

``` r
data(scythebill)

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl = 200, pal = viridis::mako, collevels = seq(-60, 0, 1))
```
