# Save dynamic spectrograms

`save_dynaspec()` saves a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object as an interactive web page, a video or still images.

## Usage

``` r
save_dynaspec(x, file.name = NULL, play = TRUE, verbose = TRUE)
```

## Arguments

- x:

  A
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
  or
  [`ds_compare()`](https://marce10.github.io/dynaSpec/reference/ds_compare.md)
  object.

- file.name:

  Character string with the name of the file. The extension sets the
  output:

  - '.html': interactive web page.

  - '.mp4': video (requires 'ffmpeg').

  - '.png': still image of static spectrograms, or one image per page of
    paged spectrograms (named after 'file.name', followed by the page
    number).

  Default is `NULL`, which returns the HTML code of the web page without
  saving it.

- play:

  Logical argument to control if the file is opened after it is saved
  (or the web page is shown in the viewer when 'file.name' is `NULL`, in
  interactive sessions). Default is `TRUE`.

- verbose:

  Logical argument to control if progress messages are printed. Default
  is `TRUE`.

## Value

Invisibly returns the path to the file, or the HTML code of the web page
(a character string of class 'dynaSpec_html') when 'file.name' is `NULL`
or ends in '.html'. When knitting R Markdown or Quarto documents the web
page is embedded in the document.

## Details

Videos of scrolling spectrograms render every frame, which takes time
(it can be sped up with the 'parallel' and 'fps' arguments of
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)).
Videos of paged and static spectrograms and all web pages are created in
seconds.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)

## Examples

``` r
data(scythebill)

x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl = 200, collevels = seq(-50, 0, 1)) + ds_oscillogram()

save_dynaspec(x, file.path(tempdir(), "scythebill.html"), play = FALSE)
save_dynaspec(x, file.path(tempdir(), "scythebill.png"), play = FALSE)

if (FALSE) { # \dontrun{
# videos (require ffmpeg)
save_dynaspec(x, file.path(tempdir(), "scythebill.mp4"))
data(antshrike)
y <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl = 256, collevels = seq(-50, 0, 1))
save_dynaspec(y + ds_scrolling(t.display = 2), file.path(tempdir(), "antshrike_scrolling.mp4"))
} # }
```
