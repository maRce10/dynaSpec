# Generate ggplot2-based spectrogram(s), which can be passed to paged_spectro

Can be used to generate single or segmented static spectrograms. Works
as standalone, but the returned object is also intended to feed into
[`paged_spectro`](https://marce10.github.io/dynaSpec/reference/paged_spectro.md).
Workflow: 1) use prep_static_ggspectro to crop, filter, segment and
tweak all spectrogram parameters; 2) pass these settings to
[`paged_spectro`](https://marce10.github.io/dynaSpec/reference/paged_spectro.md)
to generate dynamic spectrogram video.

## Usage

``` r
prep_static_ggspectro(
  soundFile,
  destFolder = NULL,
  outFilename = NULL,
  savePNG = FALSE,
  colPal = "inferno",
  crop = NULL,
  bg = NULL,
  filter = NULL,
  xLim = NULL,
  yLim = c(0, 10),
  title = NULL,
  plotLegend = FALSE,
  onlyPlotSpec = TRUE,
  ampTrans = 1,
  resampleRate = 15000,
  min_dB = -30,
  wl = 512,
  ovlp = 90,
  wn = "blackman",
  specWidth = 9,
  specHeight = 3,
  colbins = 30,
  ampThresh = 0,
  bgFlood = FALSE,
  fontAndAxisCol = NULL,
  optim = NULL,
  ...
)
```

## Arguments

- soundFile:

  should work with URLs, full and relative paths; handles .mp3 and .wav

- destFolder:

  path to directory to save output. Needs to be like
  "figures/spectrograms/" to be relative to working directory.
  Default=parent folder of soundFile. Specify "wd" to output to the
  working directory, gotten from \[get_wd()\]

- outFilename:

  name for output PNG. default=NULL will use input name in output
  filename.

- savePNG:

  logical; Save static spectrograms as PNGs? They will be exported to
  destFolder.

- colPal:

  color palette; one of "viridis","magma","plasma","inferno","cividis"
  from the
  [`viridis`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html)
  package OR a 2 value vector (e.g. c("white","black")), defining the
  start and end of a custom color gradient

- crop:

  subset of recording to include; default crop=NULL will use whole file,
  up to 10 sec; if a number, interpreted as crop first X.X sec; if
  c(X1,X2), interpreted as trimming out a specific time interval in sec;
  if crop=FALSE, will not crop at all, even for recordings over 10 sec.

- bg:

  background color (defaults to 1st value of chosen palette)

- filter:

  apply a bandpass filter? Defaults to none (NULL). Expects 'c(0,2)'
  where sound from 0 to 2kHz would be filtered out

- xLim:

  the time limit (x-axis width) in seconds for all spectrograms; i.e.
  page width in seconds for multi-page dynamic spectrograms (defaults to
  WAV file length, unless file duration \>5s). To override the 5s limit,
  put xLim=Inf or specify the desired spectrogram x-axis limit.

- yLim:

  the frequency limits (y-axis); default is c(0,10) aka 0-10kHz

- title:

  string for title of plots; default=NULL

- plotLegend:

  logical; include a legend showing amplitude colors? default=FALSE

- onlyPlotSpec:

  logical; do you want to just plot the spec and leave out the legend,
  axes, and axis labels? default= TRUE

- ampTrans:

  amplitude transform for boosting spectrum contrast; default=1 (actual
  dB values); specify a decimal number for the lambda value of
  scales::modulus_trans(); 2.5 is a good place to start. (This amplifies
  your loud values the most, while not increasing background noise much
  at all)

- resampleRate:

  a number in Hz to downsample audio for spectrogram only. This will
  simplify audio data and speed up generation of spectrogram. Passed to
  \[tuneR::downsample()\]. Default=15000 shaves off a few seconds
  without losing much quality. Put NULL to keep original sample rate for
  spectrogram. Audiofile will not be resampled for MP4.

- min_dB:

  the minimum decibel (quietest sound) to include in the spec; defaults
  to -30 (-40 would include quieter sounds; -20 would cut out all but
  very loud sounds)

- wl:

  window length for the spectrogram (low values= higher temporal res;
  high values= higher freq. res). Default 512 is a good tradeoff; human
  speech would look better at 1024 or higher, giving higher frequency
  resolution.

- ovlp:

  how much overlap (as percent) between sliding windows to generate
  spec? Default 90 looks good, but takes longer

- wn:

  window name (slight tweaks on algorithm that affect smoothness of
  output) see [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html)

- specWidth:

  what width (in inches) would you like to make your PNG output be, if
  saving a static spec?

- specHeight:

  what height (in inches) would you like to make your PNG output be, if
  saving a static spec?

- colbins:

  default 30: increasing can smooth the color contours, but take longer
  to generate spec

- ampThresh:

  amplitude threshold as a percent to cut out of recording (try 5 to
  start); default= no filtering (high data loss with this; not
  recommended; play with min_dB and ampTrans first)

- bgFlood:

  do you want the background color to spill into the axis margins?
  Default=FALSE (i.e. white margins)

- fontAndAxisCol:

  the color of legend text if onlyPlotSpec=TRUE (since margins will be
  white, with black text); if bgFlood=TRUE, this will be the color of
  axis margins, labels and legend text. If you don't supply this, it
  will be picked automatically to be white or black based on supplied bg
  color

- optim:

  NULL by default; this is an experimental feature to simplify the
  dataframe of the FFT-processed waveform used to generate the
  spectrogram (currently does nothing)

- ...:

  Other arguments to be passed for rendering the spec (i.e. to
  seewave::spectro)

## Value

a list with all spectrogram parameters, segmented WAV files (segWavs)
and spectrograms spec; importantly, spec is a list of n=number of
"pages"/segments; the first page is displayed by default

## References

Araya-Salas M & Wilkins M R. (2020). \*dynaSpec: dynamic spectrogram
visualizations in R\*. R package version 1.0.0.

## See also

[`paged_spectro`](https://marce10.github.io/dynaSpec/reference/paged_spectro.md)

## Author

Matthew R Wilkins (<matt@galacticpolymath.com>)

## Examples
