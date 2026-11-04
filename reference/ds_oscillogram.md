# Oscillogram layer of dynamic spectrograms

`ds_oscillogram()` adds an oscillogram (waveform) to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object. Oscillograms are shown by
[`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)
(alone, or above or below the spectrogram) and
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
(below the spectrogram) styles.

## Usage

``` r
ds_oscillogram(position = "below", col = NULL, height = NULL)
```

## Arguments

- position:

  Character string: "below" or "above" the spectrogram (only for
  [`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md);
  [`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
  always shows it below). Default is "below".

- col:

  Color of the oscillogram. Default is `NULL`: 'col.wave' of
  [`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md),
  or "black".

- height:

  Numeric vector of length 1 with the height of the oscillogram relative
  to that of the spectrogram (e.g. 0.5 is half as high). Default is
  `NULL` (style default).

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
