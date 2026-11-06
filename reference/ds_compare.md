# Compare sounds with dynamic spectrograms

`ds_compare()` shows two or more sounds one on top of the other and
plays them in sequence, from top to bottom. Layers are added with `+`,
as in
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
and are applied to all sounds.

## Usage

``` r
ds_compare(..., flim = NULL, tlim = NULL, gap = 0.5, same.scale = TRUE)
```

## Arguments

- ...:

  The sounds (at least two), in the order they are shown and played:
  objects of class 'Wave', paths to '.wav' or '.mp3' files, or
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
  objects, or a single list of them. Use
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
  objects for settings or layers of a single sound (e.g. its own
  [`ds_annotations()`](https://marce10.github.io/dynaSpec/reference/ds_annotations.md),
  [`ds_captions()`](https://marce10.github.io/dynaSpec/reference/ds_captions.md)
  or time range).

- flim:

  Numeric vector of length 2 with the frequency limits (in kHz) of all
  sounds, as in
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md).
  Default is `NULL`: the limits set in
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
  objects; limits set for only some sounds are used for all of them, if
  they are the same. Sounds with different sampling rates are shown up
  to the lowest Nyquist frequency, so all use the same frequency axis
  (unless they have different limits set in
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
  objects).

- tlim:

  Numeric vector of length 1 or 2 with the time range (in s) of all
  sounds, as in
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md).
  Default is `NULL` (the whole sounds, or the time range set in
  [`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
  objects).

- gap:

  Numeric vector of length 1 with the silence (in s) between consecutive
  sounds. Default is 0.5.

- same.scale:

  Logical: use the same time scale for all sounds, so a second takes the
  same width in all of them? Static spectrograms leave empty room to the
  right of shorter sounds, and paged spectrograms without 't.display'
  use the duration of the longest sound as the duration of their pages
  (scrolling spectrograms and paged spectrograms with 't.display' always
  have the same scale). `FALSE` fits each sound to the width of the
  figure. Default is `TRUE`.

## Value

An object of class 'dynaspec_compare' (also of class 'dynaspec'), which
is shown and saved as any
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object, with layers added with `+`.

## Details

All styles, panels, themes and output formats of
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
objects can be used, and all sounds must use the same style:

- Web pages have a single set of controls below the sounds. Speed,
  pitch, volume and zoom apply to all sounds, the time readout shows the
  number of the sound being played and its time, the seek bar spans all
  sounds, the loop button repeats the sequence and the download button
  saves all sounds (browsers may ask for permission to download several
  files). Clicking on a spectrogram plays that sound from that point,
  and then the following ones (the sequence ends after the last sound).

- Videos ('.mp4', requires 'ffmpeg') stack the videos of the sounds:
  each sound keeps its first frame until it is played, and its last
  frame afterwards.

- Still images ('.png') stack the images of the sounds (paged
  spectrograms: one image per page, stacking the pages with the same
  number).

Annotations and captions added to a comparison are applied to all
sounds, with times referring to each sound. To add them to a single
sound, add them to its
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object. Layers added to a comparison replace (or are combined with)
those of the
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
objects, as in any
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object.

Amplitudes are scaled within each sound (as in
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)),
so the colors of the spectrograms and the height of the oscillograms do
not compare the amplitude of the sounds.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md)

## Examples

``` r
data(female_barn_swallow, male_barn_swallow)

x <- ds_compare(female_barn_swallow, male_barn_swallow, flim = c(0, 10)) +
  ds_spectrogram(wl = 512, collevels = seq(-50, 0, 1))

save_dynaspec(x, file.path(tempdir(), "swallows.html"), play = FALSE)

if (FALSE) { # \dontrun{
# show it
x

# other panels, styles and themes, as in dynaspec()
x + ds_oscillogram() + ds_theme_mako()
x + ds_scrolling(t.display = 1)
x + ds_paged(t.display = 1) + ds_player(buttons = c("progress", "time", "loop"))

# more than two sounds
data(scythebill)
ds_compare(female_barn_swallow, male_barn_swallow, scythebill) +
  ds_spectrogram(wl = 512)

# video (requires ffmpeg)
save_dynaspec(x, file.path(tempdir(), "swallows.mp4"))

# layers of a single sound
st <- data.frame(start = 0.5, end = 1, labels = "A")
a <- dynaspec(female_barn_swallow) + ds_annotations(st, labels = "labels")
ds_compare(a, male_barn_swallow, flim = c(0, 10), gap = 1) + ds_spectrogram(wl = 512)
} # }
```
