# Amplitude envelope layer of dynamic spectrograms

`ds_envelope()` adds the amplitude envelope of the sound (absolute
amplitude smoothed with a 5 ms moving window) to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object. Envelopes are shown by
[`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)
(alone, or above or below the spectrogram, along with oscillograms) and
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
(below the spectrogram, instead of the oscillogram) styles.

## Usage

``` r
ds_envelope(position = "below", col = NULL, height = NULL)
```

## Arguments

- position:

  Character string: "below" or "above" the spectrogram (only for
  [`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)).
  Default is "below".

- col:

  Color of the envelope. Default is `NULL`: 'col.wave' of
  [`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
  or black (as oscillograms).

- height:

  Numeric vector of length 1 with the height of the envelope panel
  relative to that of the spectrogram. Default is `NULL` (half the
  height of the spectrogram).

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`ds_oscillogram()`](https://marce10.github.io/dynaSpec/reference/ds_oscillogram.md)

## Examples

``` r
data(scythebill)

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
  ds_envelope(position = "above")
```
