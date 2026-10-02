# Make a paged dynamic spectrogram similar to spectral display in Adobe Audition

This function works on an object generated with
[`prep_static_ggspectro`](https://marce10.github.io/dynaSpec/reference/prep_static_ggspectro.md),
an alias for prepStaticSpec(). Video generation is very time consuming,
and all the desired spectrogram parameters should be set in the prep
step. The output is an mp4 video of a dynamic spectrogram video. If the
input sound file was segmented in the prep step, the resulting video
will be a concatenation of multiple dynamic spectrogram "pages." Each
page has a sliding window revealing the part of the static spectrogram
being played. Temporal width of each page is defined by the xLim
parameter in
[`prep_static_ggspectro`](https://marce10.github.io/dynaSpec/reference/prep_static_ggspectro.md).
You can also output temporary segmented files, if desired.

## Usage

``` r
paged_spectro(
  specParams,
  destFolder = NULL,
  vidName = NULL,
  framerate = 30,
  highlightCol = "#4B0C6BFF",
  highlightAlpha = 0.6,
  cursorCol = "white",
  delete_temp_files = TRUE
)
```

## Arguments

- specParams:

  an object returned from
  [`prep_static_ggspectro`](https://marce10.github.io/dynaSpec/reference/prep_static_ggspectro.md)

- destFolder:

  destination of output video; this setting overwrites setting from
  specParams object

- vidName:

  expects "FileName", .mp4 not necessary; if not supplied, will be named
  after the file you used in prep_static_ggspectro()

- framerate:

  by default, set to 30 (currently this is not supported, as animate
  doesn't honor the setting)

- highlightCol:

  default "#4B0C6BFF" (a purple color to match the default viridis
  'inferno' palette)

- highlightAlpha:

  opacity of the highlight box; default is 0.6

- cursorCol:

  Color of the leading edge of the highlight box; default "white"

- delete_temp_files:

  Default= TRUE, deletes temporary files (specs & WAV files used to
  create concatenated video)

## Value

Nothing is returned, though progress and file save locations are output
to user. Video should play after rendering.

## References

Araya-Salas M & Wilkins M R. (2020). \*dynaSpec: dynamic spectrogram
visualizations in R\*. R package version 1.0.0.

## See also

[`prep_static_ggspectro`](https://marce10.github.io/dynaSpec/reference/prep_static_ggspectro.md)

## Author

Matthew R Wilkins (<matt@galacticpolymath.com>)

## Examples

``` r
if (FALSE) { # \dontrun{
#show wav files included with dynaSpec
f <- list.files(pattern=".wav", full.names = TRUE,
     path = system.file(package="dynaSpec"))

femaleBarnSwallow<-prep_static_ggspectro(f[1],destFolder=tempdir(),
                   onlyPlotSpec = FALSE, bgFlood= TRUE)
paged_spectro(femaleBarnSwallow,destFolder=tempdir())

maleBarnSwallow<-prep_static_ggspectro(f[2],destFolder=tempdir(),
                 onlyPlotSpec = FALSE, bgFlood= TRUE,min_dB=-40)

paged_spectro(femaleBarnSwallow,destFolder=tempdir())

# Make a multipage dynamic spec of a humpback whale song
# Note, we're saving PNGs of our specs in the working directory; to add
# axis labels, we set onlyPlotSpec to F, and to make the same background
# color for the entire figure, we set bgFlood= TRUE;
# The yLim is set to only go to 0.7kHz, where the sounds are for these big whales;
#also applying an amplitude transform to boost signal.
#This is a longer file, so we're taking the first 12 seconds with crop=12
#xLim=3 means each "page" will be 3 seconds, so we'll have 4 dynamic spec pages that get combined

humpback <- prep_static_ggspectro(
"http://www.oceanmammalinst.org/songs/hmpback3.wav",destFolder=tempdir(),savePNG= FALSE,
onlyPlotSpec=FALSE,bgFlood= TRUE,yLim=c(0,.7),crop=12,xLim=3,ampTrans=3)

#to generate multipage dynamic spec (movie), run the following
paged_spectro(humpback,destFolder=tempdir())

# see more examples at https://marce10.github.io/dynaSpec/
} # }
```
