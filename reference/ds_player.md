# Player settings of dynamic spectrogram web pages

`ds_player()` sets the playback controls of web pages.

## Usage

``` r
ds_player(
  speeds = c(1, 0.5, 0.25, 0.125),
  keep.pitch = FALSE,
  buttons = "time"
)
```

## Arguments

- speeds:

  Numeric vector with the playback speeds offered by the speed slider
  (between 0.0625 and 16). The slider snaps to these values, ordered
  from the slowest (left) to the fastest (right). The first value is the
  initial speed. Use a single value to hide the slider. Default is
  `c(1, 0.5, 0.25, 0.125)`.

- keep.pitch:

  Logical argument to control if the pitch is initially kept when
  playback speed changes. Listeners can switch this with the 'keep
  pitch' button. If `FALSE` slowing down a sound also lowers its pitch
  (as when slowing down a tape), which makes fast sound features (e.g.
  pulses) easier to hear. If `TRUE` the web browser time-stretches the
  sound to keep its pitch. Default is `FALSE`.

- buttons:

  Character vector with the additional controls shown next to the play
  button:

  - `"time"`: current time and duration of the sound

  - `"loop"`: button to play the sound repeatedly

  - `"volume"`: mute button and volume slider

  - `"download"`: button to save the sound (as a '.wav' or '.mp3' file,
    as embedded in the web page)

  - `"fullscreen"`: button to show the player in full screen (e.g. in
    presentations)

  - `"progress"`: seek bar showing the part of the sound played (drag it
    to move to another time)

  - `"zoom"`: slider (with - and + buttons) to stretch or shrink
    scrolling spectrograms, showing from 4 times less to 4 times more
    time than 't.display' (at most the whole sound). Only in scrolling
    spectrograms; the scrolling spectrogram is drawn at twice the
    resolution so zooming in stays sharp, which makes web pages larger

  Use [`character()`](https://rdrr.io/r/base/character.html) to show
  only the play button and speed slider. Default is `"time"`
  (`c("progress", "time")` with themes with `bar.style = "rounded"`,
  e.g.
  [`ds_theme_xenocanto()`](https://marce10.github.io/dynaSpec/reference/ds_theme.md)).
  Buttons are only shown in web pages (not in videos).

## Value

A layer to be added to a
[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)
object with `+`.

## See also

[`dynaspec()`](https://marce10.github.io/dynaSpec/reference/dynaspec.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data("scythebill")

dynaspec(scythebill, flim = c(1.5, 5.5)) +
  ds_spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
  ds_player(buttons = c("time", "loop", "volume", "download", "fullscreen"))
} # }
```
