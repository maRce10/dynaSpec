# Power spectrum layer of dynamic spectrograms

`ds_spectrum()` adds the mean power spectrum of the sound next to the
spectrogram of a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object, sharing its frequency axis. Power spectra are shown by the
[`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)
style (with several rows, each row shows the spectrum of its segment, on
the same scale).

## Usage

``` r
ds_spectrum(side = "right", col = NULL, width = 0.1)
```

## Arguments

- side:

  Character string: "right" or "left" of the spectrogram. Default is
  "right".

- col:

  Color of the spectrum. Default is `NULL`: 'col.wave' of
  [`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
  or black (as oscillograms).

- width:

  Width of the spectrum panel as a proportion of the figure width.
  Default is 0.1.

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)

## Examples

``` r
data(scythebill)

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) + ds_spectrum(side = "left")
```
