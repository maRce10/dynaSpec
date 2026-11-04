# Add layers to dynamic spectrograms

Add layers to dynamic spectrograms

## Usage

``` r
# S3 method for class 'dynaspec'
e1 + e2
```

## Arguments

- e1:

  A
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
  object.

- e2:

  A layer (e.g.
  [`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md),
  [`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md)
  or
  [`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)).

## Value

A
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object. Spectrogram, oscillogram, envelope, spectrum, overview, player
and style layers replace previous ones of the same kind (arguments of
spectrogram, theme and style layers are combined); annotations and
captions are added.
