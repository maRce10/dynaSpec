# player controls of web pages

#' Player settings of dynamic spectrogram web pages
#'
#' `ds_player()` sets the playback controls of web pages.
#' @param speeds Numeric vector with the playback speeds offered by the speed slider (between 0.0625 and 16). The slider snaps to these values, ordered from the slowest (left) to the fastest (right). The first value is the initial speed. Use a single value to hide the slider. Default is `c(1, 0.5, 0.25, 0.125)`.
#' @param keep.pitch Logical argument to control if the pitch is initially kept when playback speed changes. Listeners can switch this with the 'keep pitch' button. If `FALSE` slowing down a sound also lowers its pitch (as when slowing down a tape), which makes fast sound features (e.g. pulses) easier to hear. If `TRUE` the web browser time-stretches the sound to keep its pitch. Default is `FALSE`.
#' @param buttons Character vector with the additional controls shown next to the play button:
#' \itemize{
#'   \item \code{"time"}: current time and duration of the sound
#'   \item \code{"loop"}: button to play the sound repeatedly
#'   \item \code{"volume"}: mute button and volume slider
#'   \item \code{"download"}: button to save the sound (as a '.wav' or '.mp3' file, as embedded in the web page)
#'   \item \code{"fullscreen"}: button to show the player in full screen (e.g. in presentations)
#'   \item \code{"progress"}: seek bar showing the part of the sound played (drag it to move to another time)
#'   \item \code{"zoom"}: slider (with - and + buttons) to stretch or shrink scrolling spectrograms, showing from 4 times less to 4 times more time than 't.display' (at most the whole sound). Only in scrolling spectrograms; the scrolling spectrogram is drawn at twice the resolution so zooming in stays sharp, which makes web pages larger
#' }
#' Use \code{character()} to show only the play button and speed slider. Default is \code{"time"} (\code{c("progress", "time")} with themes with \code{bar.style = "rounded"}, e.g. [ds_theme_xenocanto()]). Buttons are only shown in web pages (not in videos).
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()]
#' @export
#' @examples
#' \dontrun{
#' data("scythebill")
#'
#' dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
#'   ds_player(buttons = c("time", "loop", "volume", "download", "fullscreen"))
#' }
ds_player <- function(speeds = c(1, 0.5, 0.25, 0.125), keep.pitch = FALSE, buttons = "time") {
  # only supplied arguments are stored (the default buttons depend on the theme)
  args <- provided_args_int(match.call(), parent.frame())
  .report_assertions(.check_arguments("player", args))
  layer_int("player", args)
}


# attributes of web players with the ds_player() settings (speed buttons, keep pitch, additional buttons and name
# of downloaded sounds)
# (and colors of the controls and font family of the ds_theme())
player_attrs_int <- function(speeds, keep.pitch, buttons, download.name, theme = list()) {
  paste0(
    ' data-speeds="', paste(speeds, collapse = ","), '" data-keep-pitch="', tolower(keep.pitch),
    '" data-buttons="', paste(buttons, collapse = ","), '" data-name="', html_escape_int(download.name), '"',
    if (!is.null(theme$col.bar)) paste0(' data-bar-col="', col2hex_int(theme$col.bar), '" data-bar-line="', col2hex_int(grDevices::adjustcolor(theme$col.bar, 0.45)), '"'),
    if (!is.null(theme$bg.bar)) paste0(' data-bar-bg="', col2hex_int(theme$bg.bar), '"'),
    if (!is.null(theme$family)) paste0(' data-family="', html_escape_int(css_family_int(theme$family)), '"'),
    if (!is.null(theme$bar.style)) paste0(' data-bar-style="', theme$bar.style, '"')
  )
}
