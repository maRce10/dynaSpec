# Defunct functions in dynaSpec

These functions of dynaSpec 1.x were removed in version 2.0.0, which
creates dynamic spectrograms with layers (see
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
and
[`vignette("dynaSpec")`](https://marce10.github.io/dynaSpec/articles/dynaSpec.md)).
Calling them gives an error showing the new syntax:

- `scrolling_spectro()`: use
  `dynaspec(wave) + ds_spectrogram() + ds_scrolling()`.

- `prep_static_ggspectro()` and `prepStaticSpec()`: use
  `dynaspec(wave) + ds_spectrogram() + ds_paged()` (save the pages with
  `save_dynaspec(x, "pages.png")`).

- `paged_spectro()` and `pagedSpec()`: use
  `dynaspec(wave) + ds_spectrogram() + ds_paged()` (save the video with
  `save_dynaspec(x, "paged.mp4")`).

## Usage

``` r
scrolling_spectro(...)

prep_static_ggspectro(...)

prepStaticSpec(...)

paged_spectro(...)

pagedSpec(...)
```

## Arguments

- ...:

  Ignored.

## Value

None (an error is raised).
