# panel layers: spectrogram, oscillogram, envelope, power spectrum and overview

#' Spectrogram layer of dynamic spectrograms
#'
#' `ds_spectrogram()` adds a spectrogram to a [dynaspec()] object and sets how it is computed and drawn. Adding it again changes its settings.
#' @param wl Window length (in samples), as in [seewave::spectro()]. Used when 'wl.ms' is not supplied.
#' @param wl.ms Window length in milliseconds (as 'Window length' in Praat). Takes precedence over 'wl'. When neither is supplied the window lasts 11.61 ms (512 samples at 44.1 kHz) in all styles. The window length in samples is computed from the sampling rate of the sound (for paged spectrograms, after resampling), so sounds with different sampling rates (e.g. in [ds_compare()]) get windows of the same duration and the same time resolution.
#' @param ovlp Percent overlap between consecutive windows, as in [seewave::spectro()].
#' @param wn Window function name, as in [seewave::spectro()] (e.g. "hanning" or "blackman").
#' @param pal Color palette of the spectrogram, from low to high amplitude: a palette function (e.g. [viridis::viridis()] or [seewave::reverse.gray.colors.1()]), the name of a 'viridis' palette (e.g. "magma"), or two or more colors for a gradient (e.g. `c("white", "black")` or `c("white", "gold", "red", "black")`).
#' @param collevels Numeric vector with the amplitude levels (in dB) of the colors, as in [seewave::spectro()] (e.g. `seq(-40, 0, 1)`).
#' @param col.bg Background color of the spectrogram, shown where the amplitude is below the lowest level of 'collevels'. Default is the color of the lowest amplitude of the palette (e.g. white for [seewave::reverse.gray.colors.1()], the default palette, or dark blue for [viridis::mako()]), so quiet parts and the background look the same.
#' @param amp.trans Amplitude transformation to brighten paged spectrograms (values > 1 brighten quiet sounds; only used by [ds_paged()]).
#' @param ... Additional arguments passed to [seewave::spectro()].
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details Only the arguments supplied are used. Arguments with default `NULL` take the defaults of each style ([ds_static()], [ds_scrolling()] or [ds_paged()]):
#' - 'wl.ms' (or 'wl'): 11.61 ms (512 samples at 44.1 kHz) in all styles.
#' - 'ovlp': 70 (static and scrolling) and 90 (paged).
#' - 'wn': "hanning" (static and scrolling) and "blackman" (paged).
#' - 'pal': [seewave::reverse.gray.colors.1()] (static and scrolling) and "inferno" (paged).
#' - 'collevels': `seq(-40, 0, 1)` (static), 30 levels from the lowest to the highest amplitude, as in [seewave::spectro()] (scrolling) and `seq(-30, 0, 1)` (paged).
#' - 'col.bg': the color of the lowest amplitude of the palette.
#' @seealso [dynaspec()]
#' @export
#' @examples
#' data(scythebill)
#'
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, pal = viridis::mako, collevels = seq(-60, 0, 1))
#'
#' # window length in milliseconds (the same duration for any sampling rate)
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 15, collevels = seq(-50, 0, 1))
ds_spectrogram <- function(wl = NULL, wl.ms = NULL, ovlp = NULL, wn = NULL, pal = NULL, collevels = NULL, col.bg = NULL, amp.trans = 1, ...) {
  if ("colbg" %in% names(list(...)))
    stop2("'colbg' is no longer used: use 'col.bg'")
  checkmate::assert_number(wl.ms, lower = 0, finite = TRUE, null.ok = TRUE, .var.name = "wl.ms")
  layer_int("spectrogram", provided_args_int(match.call(), parent.frame()))
}


#' Frequency filter layer of dynamic spectrograms
#'
#' `ds_filter()` applies a bandpass filter to the sound of a [dynaspec()] object (also to the sound of video files), so the filtered sound is both what is shown and what is heard, in all styles and outputs (web pages, videos and images). Removing noise outside the frequency range of the sounds of interest (e.g. wind, traffic or insects) gives a cleaner spectrogram and also a cleaner sound to listen to. Adding it again replaces the previous filter.
#' @param bp Numeric vector of length 2 with the frequency limits (in kHz) of the bandpass filter (as 'bp' in 'warbleR' functions). `Inf` as upper limit makes a high-pass filter, and 0 as lower limit a low-pass filter. Upper limits at or above the Nyquist frequency of the sound (half its sampling rate) are not applied.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details The filter is applied to the whole sound before it is cut with 'tlim' (so its edges are not affected), with [seewave::ffilter()] (a filter in the frequency domain with windows of 11.61 ms, the default window length of spectrograms). Stereo sounds are filtered channel by channel. Frequency contours measured by [ds_annotations()] are measured on the filtered sound.
#' @seealso [dynaspec()], [ds_spectrogram()]
#' @export
#' @examples
#' data(antshrike)
#'
#' # sound between 0.5 and 4 kHz (the low-frequency noise is removed)
#' x <- dynaspec(antshrike, flim = c(0, 5.5), tlim = c(0, 5)) +
#'   ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1)) +
#'   ds_filter(bp = c(0.5, 4))
#'
#' # high-pass filter
#' x <- x + ds_filter(bp = c(1, Inf))
ds_filter <- function(bp) {
  checkmate::assert_numeric(bp, len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, .var.name = "bp")
  if (bp[1] == 0 && !is.finite(bp[2]))
    stop2("'bp': c(0, Inf) does not filter any frequency")
  layer_int("filter", list(bp = bp))
}

#' Oscillogram layer of dynamic spectrograms
#'
#' `ds_oscillogram()` adds an oscillogram (waveform) to a [dynaspec()] object. Oscillograms are shown by [ds_static()] (alone, or above or below the spectrogram) and [ds_scrolling()] (below the spectrogram) styles.
#' @param position Character string: "below" or "above" the spectrogram (only for [ds_static()]; [ds_scrolling()] always shows it below). Default is "below".
#' @param col Color of the oscillogram. Default is `NULL`: 'col.wave' of [ds_theme()], or "black".
#' @param height Numeric vector of length 1 with the height of the oscillogram relative to that of the spectrogram (e.g. 0.5 is half as high). Default is `NULL` (style default).
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()]
#' @export
ds_oscillogram <- function(position = "below", col = NULL, height = NULL) {
  check_results <- .check_arguments("oscillogram", list(colwave = col))
  checkmate::assert_choice(position, c("below", "above"), add = check_results, .var.name = "position")
  if (!is.null(height)) checkmate::assert_number(height, lower = 0.01, upper = 10, add = check_results, .var.name = "height")
  .report_assertions(check_results)
  layer_int("oscillogram", list(type = "osc", position = position, col = col, height = height))
}


#' Amplitude envelope layer of dynamic spectrograms
#'
#' `ds_envelope()` adds the amplitude envelope of the sound (absolute amplitude smoothed with a 5 ms moving window) to a [dynaspec()] object. Envelopes are shown by [ds_static()] (alone, or above or below the spectrogram, along with oscillograms) and [ds_scrolling()] (below the spectrogram, instead of the oscillogram) styles.
#' @param position Character string: "below" or "above" the spectrogram (only for [ds_static()]). Default is "below".
#' @param col Color of the envelope. Default is `NULL`: 'col.wave' of [ds_theme()], or black (as oscillograms).
#' @param height Numeric vector of length 1 with the height of the envelope panel relative to that of the spectrogram. Default is `NULL` (half the height of the spectrogram).
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()], [ds_oscillogram()]
#' @export
#' @examples
#' data(scythebill)
#'
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
#'   ds_envelope(position = "above")
ds_envelope <- function(position = "below", col = NULL, height = NULL) {
  check_results <- .check_arguments("envelope", list(colwave = col))
  checkmate::assert_choice(position, c("below", "above"), add = check_results, .var.name = "position")
  if (!is.null(height)) checkmate::assert_number(height, lower = 0.01, upper = 10, add = check_results, .var.name = "height")
  .report_assertions(check_results)
  layer_int("envelope", list(type = "env", position = position, col = col, height = height))
}


#' Power spectrum layer of dynamic spectrograms
#'
#' `ds_spectrum()` adds the mean power spectrum of the sound next to the spectrogram of a [dynaspec()] object, sharing its frequency axis. Power spectra are shown by the [ds_static()] style (with several rows, each row shows the spectrum of its segment, on the same scale).
#' @param side Character string: "right" or "left" of the spectrogram. Default is "right".
#' @param col Color of the spectrum. Default is `NULL`: 'col.wave' of [ds_theme()], or black (as oscillograms).
#' @param width Width of the spectrum panel as a proportion of the figure width. Default is 0.1.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()], [ds_spectrogram()]
#' @export
#' @examples
#' data(scythebill)
#'
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) + ds_spectrum(side = "left")
ds_spectrum <- function(side = "right", col = NULL, width = 0.1) {
  check_results <- .check_arguments("spectrum", list(colwave = col))
  checkmate::assert_choice(side, c("right", "left"), add = check_results, .var.name = "side")
  checkmate::assert_number(width, lower = 0.05, upper = 0.5, add = check_results, .var.name = "width")
  .report_assertions(check_results)
  layer_int("spectrum", list(side = side, col = col, width = width))
}


#' Overview layer of scrolling spectrograms
#'
#' `ds_overview()` adds a lower panel to [ds_scrolling()] spectrograms with the spectrogram of the whole sound, in which the part being shown is highlighted.
#' @param height Numeric vector of length 1 with the height of the overview panel relative to that of the scrolling spectrogram. Default is `NULL` (a fifth of it).
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details Scrolling spectrograms show a single lower panel: an overview, an oscillogram or an envelope (the first one added). Other styles do not show overviews (static spectrograms show the whole sound already).
#' @seealso [dynaspec()], [ds_scrolling()]
#' @export
#' @examples
#' data(antshrike)
#'
#' x <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1)) +
#'   ds_overview() + ds_scrolling(t.display = 2)
ds_overview <- function(height = NULL) {
  check_results <- checkmate::makeAssertCollection()
  if (!is.null(height)) checkmate::assert_number(height, lower = 0.01, upper = 0.99, add = check_results, .var.name = "height")
  .report_assertions(check_results)
  layer_int("overview", list(height = height))
}
