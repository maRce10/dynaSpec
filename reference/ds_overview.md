# Overview layer of scrolling spectrograms

`ds_overview()` adds a lower panel to
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
spectrograms with the spectrogram of the whole sound, in which the part
being shown is highlighted.

## Usage

``` r
ds_overview(height = NULL)
```

## Arguments

- height:

  Numeric vector of length 1 with the height of the overview panel
  relative to that of the scrolling spectrogram. Default is `NULL` (a
  fifth of it).

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## Details

Scrolling spectrograms show a single lower panel: an overview, an
oscillogram or an envelope (the first one added). Other styles do not
show overviews (static spectrograms show the whole sound already).

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)

## Examples

``` r
data(antshrike)

x <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl = 256, collevels = seq(-50, 0, 1)) +
  ds_overview() + ds_scrolling(t.display = 2)
```
