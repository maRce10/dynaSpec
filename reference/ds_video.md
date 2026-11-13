# Video layer of dynamic spectrograms

`ds_video()` shows the video of a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object made from a video file together with the spectrogram. The video
plays in step with the sound, also when playback is slowed down, in web
pages and videos ('.mp4') of all styles.

## Usage

``` r
ds_video(position = "above", height = NULL)
```

## Arguments

- position:

  Where the video goes, relative to the spectrogram:

  - "above" (default): the video on top and the spectrogram right below
    it.

  - "below": the spectrogram on top and the video below it.

  Other panels (e.g. oscillograms) stay with the spectrogram.

- height:

  Height of the video relative to the height of the figure with the
  spectrogram (including its axes and other panels), as 'height' in
  [`ds_oscillogram()`](https://marce10.github.io/dynaSpec/reference/ds_oscillogram.md):
  e.g. 3 makes the video three times as high as the figure, and 0.5 half
  as high. The video always takes the width of the figure (with its own
  proportions), so 'height' sets the height of the figure: it replaces
  'height' of the style
  ([`ds_static()`](https://marce10.github.io/dynaSpec/reference/styles.md),
  [`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)
  or
  [`ds_paged()`](https://marce10.github.io/dynaSpec/reference/styles.md)),
  keeping its width. Default is `NULL`: the figure keeps the height of
  its style.

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## Details

The sound of the dynamic spectrogram is the sound of the video, so
'tlim' in
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
also cuts the video.

Outputs of
[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md):

- Videos ('.mp4'): the video and the spectrogram are shown one on top of
  the other, with the sound of the video. In scrolling spectrograms the
  video also slows down with the sound ('speed' of
  [`ds_scrolling()`](https://marce10.github.io/dynaSpec/reference/styles.md)),
  waits during the 'buffer' and is repeated when looping.

- Web pages: a silent copy of the video (re-encoded at up to 960 pixels
  wide) is embedded in the page and follows the sound at any speed.
  Embedded videos make web pages larger (about 1 MB for each 10 s of
  video), so cut long videos with 'tlim'.

- Still images ('.png'): the video is not shown.

Videos are read with 'ffmpeg', so most formats can be used (e.g. '.mp4',
'.mov', '.mkv', '.webm' or '.avi', with H.264, H.265, VP9 or AV1 video,
as supported by the installed 'ffmpeg'). Videos must have sound (the
first sound track is used).

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md),
[`save_dynaspec()`](https://marce10.github.io/dynaSpec/reference/save_dynaspec.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# a video file with sound (requires ffmpeg)
x <- dynaspec("field_recording.mp4", flim = c(1, 8), tlim = c(0, 20)) +
  ds_spectrogram(wl.ms = 12) +
  ds_video()

# web page (video above the spectrogram)
x

# video below the spectrogram, as high as the figure
x + ds_video(position = "below", height = 1)

# video file of a scrolling spectrogram
save_dynaspec(x + ds_scrolling(t.display = 2), "field_recording_spectrogram.mp4")
} # }
```
