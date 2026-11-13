# Create dynamic spectrograms

`dynaspec()` starts a dynamic spectrogram of a sound. Layers are added
with `+`, as in 'ggplot2' graphs:
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)
and
[`ds_oscillogram()`](https://marce10.github.io/dynaSpec/reference/ds_oscillogram.md)
add the panels,
[`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md)
and
[`ds_captions()`](https://marce10.github.io/dynaSpec/reference/ds_captions.md)
highlight and label parts of the sound, and
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md),
[`ds_paged()`](https://marce10.github.io/dynaSpec/reference/styles.md)
or
[`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)
set the style. Printing the object shows an interactive web page
(embedded in R Markdown and Quarto documents when knitting), and
[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md)
saves it as a web page, a video or still images.

## Usage

``` r
dynaspec(wave, flim = NULL, tlim = NULL)
```

## Arguments

- wave:

  The sound: an object of class 'Wave', the path to a '.wav' or '.mp3'
  file, or the path to a video file with sound (e.g. '.mp4', '.mov',
  '.mkv', '.webm' or '.avi'; requires 'ffmpeg'). The sound of videos is
  used as the sound, and
  [`ds_video()`](https://marce10.github.io/dynaSpec/reference/ds_video.md)
  shows the video next to the spectrogram.

- flim:

  Numeric vector of length 2 with the frequency limits (in kHz). Default
  is `NULL` (from 0 to the Nyquist frequency, or the style default).
  `Inf` as upper limit means the Nyquist frequency of the sound (half
  its sampling rate). Any other upper limit higher than the Nyquist
  frequency gives a warning, and spectrograms are shown up to the
  Nyquist frequency.

- tlim:

  Numeric vector of length 1 or 2 with the time range (in s) of the
  sound to use: a single value takes the first seconds of the sound, and
  two values a time interval. `Inf` as upper limit means the end of the
  sound. Times of
  [`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md)
  and
  [`ds_captions()`](https://marce10.github.io/dynaSpec/reference/ds_captions.md)
  refer to the whole sound. Default is `NULL` (whole sound).

## Value

An object of class 'dynaspec'.

## Details

Panels are added as layers, as in 'ggplot2': only the panels added are
shown
([`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md),
[`ds_oscillogram()`](https://marce10.github.io/dynaSpec/reference/ds_oscillogram.md),
[`ds_envelope()`](https://marce10.github.io/dynaSpec/reference/ds_envelope.md)
or several of them, and
[`ds_spectrum()`](https://marce10.github.io/dynaSpec/reference/ds_spectrum.md)
next to the spectrogram). `dynaspec()` alone shows an empty panel with a
time axis and the play line (annotations and captions can be drawn on
it). Scrolling and paged spectrograms need a spectrogram.

The three styles show the sound in different ways:

- [`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md):
  the spectrogram scrolls past the play line (the line only moves at the
  start and the end of the sound).

- [`ds_paged()`](https://marce10.github.io/dynaSpec/reference/styles.md):
  the spectrogram is split into pages, and a translucent box reveals the
  part of each page being played.

- [`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md)
  (default): a play line moves over the whole figure (spectrogram,
  oscillogram or both), which can be split in several rows.

Web pages include buttons to slow down playback (see
[`ds_player()`](https://marce10.github.io/dynaSpec/reference/ds_player.md)),
and clicking on the spectrogram plays the sound from that point. Figures
and sounds are embedded in the HTML code, so web pages can be shared as
single files or pasted into any web page. Note that embedding sounds
makes web pages larger (about 1.4 times the size of the sound file), so
cut or downsample long recordings before creating web pages.

Settings not supplied in layers take the defaults of each style (e.g.
paged spectrograms use the 'inferno' color palette unless
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md)
sets another one).

## References

Araya-Salas M & Wilkins M R. (2020). *dynaSpec: dynamic spectrogram
visualizations in R*. R package version 1.0.0.

## See also

[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md),
[`ds_spectrogram()`](https://marce10.github.io/dynaSpec/reference/ds_spectrogram.md),
[`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
data(scythebill)

# empty panel with time axis (only the panels added as layers are shown)
dynaspec(scythebill)
#> <dynaspec: static spectrogram of 'scythebill'> (use save_dynaspec() to save it)

# static spectrogram with oscillogram (printing shows it in the viewer or browser)
x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
  ds_oscillogram()

# save as a web page
save_dynaspec(x, file.path(tempdir(), "scythebill.html"), play = FALSE)
#> ✔ File saved at: /tmp/Rtmp7mqEhF/scythebill.html

if (FALSE) { # \dontrun{
# show it
x

# paged style
x + ds_paged(t.display = 1)

# scrolling style (a longer recording)
data(antshrike)
y <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
  ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))
y + ds_scrolling(t.display = 2)

# video (requires ffmpeg)
save_dynaspec(y + ds_scrolling(t.display = 2), "antshrike.mp4")

# highlight and label parts of the song
st <- data.frame(start = c(0.34, 1.13, 1.44, 1.75), end = c(1.03, 1.36, 1.68, 2.28),
                 labels = c("A", "B", "B", "C"))
x + ds_annotations(st, labels = "labels") +
  ds_captions("Brown-billed scythebill", start = 0, end = 3)

# only the oscillogram
dynaspec(scythebill) + ds_oscillogram()
} # }
```
