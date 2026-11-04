# Set the theme of all dynamic spectrograms

`ds_set_theme()` sets a theme used by all
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
objects created or shown afterwards in the R session (e.g. in all the
code chunks of an R Markdown or Quarto document), as
[`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html).
`ds_get_theme()` returns the current theme.

## Usage

``` r
ds_set_theme(theme = NULL)

ds_get_theme()
```

## Arguments

- theme:

  A theme:
  [`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
  or a preset theme (e.g.
  [`ds_theme_dark()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)).
  `NULL` removes the theme (the defaults of each style are used).

## Value

`ds_set_theme()` returns the previous theme invisibly (so it can be
restored); `ds_get_theme()` returns the current theme (`NULL` if none
was set).

## Details

Themes added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+` take precedence:
[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)
changes some settings of the global theme, while preset themes (e.g.
`+ ds_theme_paper()`) are complete themes that replace it.

## See also

[`ds_theme()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)

## Examples

``` r
data(scythebill)

old <- ds_set_theme(ds_theme_dark(cex.lab = 1.3))

# uses the dark theme
x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl = 200, collevels = seq(-50, 0, 1)) + ds_oscillogram()

# dark theme with other axis titles
x <- x + ds_theme(xlab = "Time of the song (s)")

# restore the previous theme
ds_set_theme(old)
```
