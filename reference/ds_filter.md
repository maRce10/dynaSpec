# Frequency filter layer of dynamic spectrograms

`ds_filter()` applies a bandpass filter to the sound of a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object (also to the sound of video files), so the filtered sound is both
what is shown and what is heard, in all styles and outputs (web pages,
videos and images). Removing noise outside the frequency range of the
sounds of interest (e.g. wind, traffic or insects) gives a cleaner
spectrogram and also a cleaner sound to listen to. Adding it again
replaces the previous filter.

## Usage

``` r
ds_filter(bp)
```

## Arguments

- bp:

  Numeric vector of length 2 with the frequency limits (in kHz) of the
  bandpass filter (as 'bp' in 'warbleR' functions). `Inf` as upper limit
  makes a high-pass filter, and 0 as lower limit a low-pass filter.
  Upper limits at or above the Nyquist frequency of the sound (half its
  sampling rate) are not applied.

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## Details

The filter is applied to the whole sound before it is cut with 'tlim'
(so its edges are not affected), with
[`seewave::ffilter()`](https://rdrr.io/pkg/seewave/man/ffilter.html) (a
filter in the frequency domain with windows of 11.61 ms, the default
window length of spectrograms). Stereo sounds are filtered channel by
channel. Frequency contours measured by
[`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md)
are measured on the filtered sound.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)

## Examples

``` r
data(antshrike)

# sound between 0.5 and 4 kHz (the low-frequency noise is removed)
x <- dynaspec(antshrike, flim = c(0, 5.5), tlim = c(0, 5)) +
  ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1)) +
  ds_filter(bp = c(0.5, 4))

# high-pass filter
x <- x + ds_filter(bp = c(1, Inf))
```
