#' Create dynamic spectrograms
#'
#' `dynaspec()` starts a dynamic spectrogram of a sound. Layers are added with `+`, as in 'ggplot2' graphs: [spectrogram()] and [oscillogram()] add the panels, [annotations()] and [captions()] highlight and label parts of the sound, and [scrolling()], [paged()] or [static()] set the style. Printing the object shows an interactive web page (embedded in R Markdown and Quarto documents when knitting), and [save_dynaspec()] saves it as a web page, a video or still images.
#' @param wave The sound: an object of class 'Wave' or the path to a '.wav' or '.mp3' file.
#' @param flim Numeric vector of length 2 with the frequency limits (in kHz). Default is `NULL` (from 0 to the Nyquist frequency, or the style default).
#' @param tlim Numeric vector of length 1 or 2 with the time range (in s) of the sound to use: a single value takes the first seconds of the sound, and two values a time interval. Times of [annotations()] and [captions()] refer to the whole sound. Default is `NULL` (whole sound).
#' @return An object of class 'dynaspec'.
#' @details Panels are added as layers, as in 'ggplot2': only the panels added are shown ([spectrogram()], [oscillogram()], [envelope()] or several of them, and [spectrum()] next to the spectrogram). `dynaspec()` alone shows an empty panel with a time axis and the play line (annotations and captions can be drawn on it). Scrolling and paged spectrograms need a spectrogram.
#'
#' The three styles show the sound in different ways:
#' - [scrolling()]: the spectrogram scrolls along a fixed play line.
#' - [paged()]: the spectrogram is split into pages, and a translucent box reveals the part of each page being played.
#' - [static()] (default): a play line moves over the whole figure (spectrogram, oscillogram or both), which can be split in several rows.
#'
#' Web pages include buttons to slow down playback (see [player()]), and clicking on the spectrogram plays the sound from that point. Figures and sounds are embedded in the HTML code, so web pages can be shared as single files or pasted into any web page. Note that embedding sounds makes web pages larger (about 1.4 times the size of the sound file), so cut or downsample long recordings before creating web pages.
#'
#' Settings not supplied in layers take the defaults of each style (e.g. paged spectrograms use the 'inferno' color palette unless [spectrogram()] sets another one).
#' @seealso [save_dynaspec()], [spectrogram()], [scrolling()]
#' @export
#' @name dynaspec
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' # empty panel with time axis (only the panels added as layers are shown)
#' dynaspec(scaly_breasted_hummingbird)
#'
#' # static spectrogram with oscillogram (printing shows it in the viewer or browser)
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
#'   oscillogram()
#'
#' # save as a web page
#' save_dynaspec(x, file.path(tempdir(), "hummingbird.html"), play = FALSE)
#'
#' \dontrun{
#' # show it
#' x
#'
#' # scrolling and paged styles
#' x + scrolling(t.display = 1.7)
#' x + paged(t.display = 2)
#'
#' # videos (require ffmpeg)
#' save_dynaspec(x + scrolling(t.display = 1.7), "hummingbird.mp4")
#'
#' # highlight and label parts of the song
#' st <- data.frame(start = c(1, 6), end = c(2.9, 7.4), labels = c("start", "end"))
#' x + annotations(st, labels = "labels") +
#'   captions("Scaly-breasted hummingbird", start = 0, end = 8.3)
#'
#' # only the oscillogram
#' dynaspec(scaly_breasted_hummingbird) + oscillogram()
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#' @references
#' Araya-Salas M & Wilkins M R. (2020). *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.

dynaspec <- function(wave, flim = NULL, tlim = NULL) {
  check_results <- checkmate::makeAssertCollection()
  if (!isTRUE(res <- check_wave_int(wave)))
    check_results$push(paste0("'wave': ", res))
  if (!is.null(flim))
    checkmate::assert_numeric(flim, len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_results, .var.name = "flim")
  if (!is.null(tlim))
    checkmate::assert_numeric(tlim, min.len = 1, max.len = 2, lower = 0, finite = TRUE, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_results, .var.name = "tlim")
  .report_assertions(check_results)

  # name for output files: sound file name or name of the Wave object
  name <- if (is.character(wave)) tools::file_path_sans_ext(basename(wave)) else make.names(paste(deparse(substitute(wave)), collapse = ""))

  new_dynaspec_int(wave = wave, name = name, flim = flim, tlim = if (length(tlim) == 1) c(0, tlim) else tlim)
}

#' Spectrogram layer of dynamic spectrograms
#'
#' `spectrogram()` adds a spectrogram to a [dynaspec()] object and sets how it is computed and drawn. Adding it again changes its settings.
#' @param wl Window length (in samples), as in [seewave::spectro()].
#' @param ovlp Percent overlap between consecutive windows, as in [seewave::spectro()].
#' @param wn Window function name, as in [seewave::spectro()] (e.g. "hanning" or "blackman").
#' @param pal Color palette function (e.g. [viridis::viridis()] or [seewave::reverse.gray.colors.1()]). Paged spectrograms also take the names of 'viridis' palettes (e.g. "magma") and 2 colors for a gradient (e.g. `c("white", "black")`).
#' @param collevels Numeric vector with the amplitude levels (in dB) of the colors, as in [seewave::spectro()] (e.g. `seq(-40, 0, 1)`).
#' @param colbg Background color of the spectrogram, shown where the amplitude is below the lowest level of 'collevels'. Default is the color of the lowest amplitude of the palette (e.g. white for [seewave::reverse.gray.colors.1()], the default palette, or dark blue for [viridis::mako()]), so quiet parts and the background look the same.
#' @param amp.trans Amplitude transformation to brighten paged spectrograms (values > 1 brighten quiet sounds; only used by [paged()]).
#' @param ... Additional arguments passed to [seewave::spectro()].
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details Only the arguments supplied are used; other settings take the defaults of each style (see [scrolling()], [paged()] and [static()]).
#' @seealso [dynaspec()]
#' @export
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, pal = viridis::mako, collevels = seq(-60, 0, 1))
spectrogram <- function(wl, ovlp, wn, pal, collevels, colbg, amp.trans, ...) {
  layer_int("spectrogram", provided_args_int(match.call(), parent.frame()))
}

#' Oscillogram layer of dynamic spectrograms
#'
#' `oscillogram()` adds an oscillogram (waveform) to a [dynaspec()] object. Oscillograms are shown by [static()] (alone, or above or below the spectrogram) and [scrolling()] (below the spectrogram) styles.
#' @param position Character string: "below" or "above" the spectrogram (only for [static()]; [scrolling()] always shows it below). Default is "below".
#' @param col Color of the oscillogram. Default is `NULL`: 'col.wave' of [dynatheme()], or "black".
#' @param height Numeric vector of length 1 with the height of the oscillogram relative to that of the spectrogram (e.g. 0.5 is half as high). Default is `NULL` (style default).
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()]
#' @export
oscillogram <- function(position = "below", col = NULL, height = NULL) {
  check_results <- .check_arguments("oscillogram", list(colwave = col))
  checkmate::assert_choice(position, c("below", "above"), add = check_results, .var.name = "position")
  if (!is.null(height)) checkmate::assert_number(height, lower = 0.01, upper = 10, add = check_results, .var.name = "height")
  .report_assertions(check_results)
  layer_int("oscillogram", list(type = "osc", position = position, col = col, height = height))
}

#' Amplitude envelope layer of dynamic spectrograms
#'
#' `envelope()` adds the amplitude envelope of the sound (absolute amplitude smoothed with a 5 ms moving window) to a [dynaspec()] object. Envelopes are shown by [static()] (alone, or above or below the spectrogram, along with oscillograms) and [scrolling()] (below the spectrogram, instead of the oscillogram) styles.
#' @param position Character string: "below" or "above" the spectrogram (only for [static()]). Default is "below".
#' @param col Color of the envelope. Default is `NULL`: 'col.wave' of [dynatheme()], or "#31688E".
#' @param height Numeric vector of length 1 with the height of the envelope panel relative to that of the spectrogram. Default is `NULL` (half the height of the spectrogram).
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()], [oscillogram()]
#' @export
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
#'   oscillogram(position = "above") + envelope()
envelope <- function(position = "below", col = NULL, height = NULL) {
  check_results <- .check_arguments("envelope", list(colwave = col))
  checkmate::assert_choice(position, c("below", "above"), add = check_results, .var.name = "position")
  if (!is.null(height)) checkmate::assert_number(height, lower = 0.01, upper = 10, add = check_results, .var.name = "height")
  .report_assertions(check_results)
  layer_int("envelope", list(type = "env", position = position, col = col, height = height))
}

#' Power spectrum layer of dynamic spectrograms
#'
#' `spectrum()` adds the mean power spectrum of the sound next to the spectrogram of a [dynaspec()] object, sharing its frequency axis. Power spectra are shown by the [static()] style (with several rows, each row shows the spectrum of its segment, on the same scale).
#' @param side Character string: "right" or "left" of the spectrogram. Default is "right".
#' @param col Color of the spectrum. Default is `NULL`: 'col.wave' of [dynatheme()], or "#31688E".
#' @param width Width of the spectrum panel as a proportion of the figure width. Default is 0.1.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()], [spectrogram()]
#' @export
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) + spectrum(side = "left")
spectrum <- function(side = "right", col = NULL, width = 0.1) {
  check_results <- .check_arguments("spectrum", list(colwave = col))
  checkmate::assert_choice(side, c("right", "left"), add = check_results, .var.name = "side")
  checkmate::assert_number(width, lower = 0.05, upper = 0.5, add = check_results, .var.name = "width")
  .report_assertions(check_results)
  layer_int("spectrum", list(side = side, col = col, width = width))
}

#' Overview layer of scrolling spectrograms
#'
#' `overview()` adds a lower panel to [scrolling()] spectrograms with the spectrogram of the whole sound, in which the part being shown is highlighted.
#' @param height Numeric vector of length 1 with the height of the overview panel relative to that of the scrolling spectrogram. Default is `NULL` (a fifth of it).
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details Scrolling spectrograms show a single lower panel: an overview, an oscillogram or an envelope (the first one added). Other styles do not show overviews (static spectrograms show the whole sound already).
#' @seealso [dynaspec()], [scrolling()]
#' @export
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
#'   overview() + scrolling(t.display = 1.7)
overview <- function(height = NULL) {
  check_results <- checkmate::makeAssertCollection()
  if (!is.null(height)) checkmate::assert_number(height, lower = 0.01, upper = 0.99, add = check_results, .var.name = "height")
  .report_assertions(check_results)
  layer_int("overview", list(height = height))
}

#' Theme of dynamic spectrograms
#'
#' `dynatheme()` sets the look of a [dynaspec()] object (axes, text, panels, colors and player controls), in the same way for all styles. Adding it again changes the settings supplied (other settings are kept). Preset themes (e.g. `dynatheme_dark()`) set several settings at once.
#' @param xlab,ylab Titles of the time and frequency axes. Defaults are "Time (s)" and "Frequency (kHz)".
#' @param cex.lab,cex.axis Size of axis titles and axis labels, relative to the style default (e.g. 1.5 is 50% larger). Defaults are 1 (1.2 for axis titles of static spectrograms).
#' @param font.lab Font of axis titles: 1 (plain), 2 (bold), 3 (italic) or 4 (bold italic). Defaults are 1 (static and scrolling) and 2 (paged).
#' @param col.lab Color of axes, axis labels and titles. Default is black (paged spectrograms: picked to contrast with the background).
#' @param axis.type Character string: "standard" (axes with labels and titles), "inside" (no axis titles; frequency labels inside the spectrogram, e.g. "8k", and time labels below the lowest panel, e.g. "2s"), "compact" (no axis titles; frequency labels below short ticks in a narrow column on the left, with the unit at the bottom, and time labels in minutes and seconds, e.g. "0:02", above the figure), "minimal" (1 s and 1 kHz scale bars; scrolling spectrograms only) or "none" (no axes). Defaults are "standard" (static and scrolling) and "none" (paged).
#' @param bg Background color of the figure around the panels ("transparent" for a transparent background). Default is white (static spectrograms: 'colbg' of [spectrogram()] when it is supplied).
#' @param family Font family of all text, including captions and annotation labels: "sans", "serif", "mono" or the name of a font installed in the system. Default is "sans".
#' @param las Orientation of the frequency axis labels: 0 (parallel to the axis) or 1 (horizontal). Defaults are 1 (static and paged) and 0 (scrolling).
#' @param tick.length Length of axis ticks, in lines of text (0 hides ticks). Defaults are 0.5 (static and scrolling) and 0.3 (paged).
#' @param box Logical argument to control whether boxes are drawn around panels. Default is `TRUE`.
#' @param col.box Color of the boxes around panels. Default is 'col.lab'.
#' @param grid Logical argument to control whether horizontal grid lines are drawn at the frequency axis ticks of spectrograms. Default is `FALSE`.
#' @param col.grid Color of grid lines. Default is a translucent gray.
#' @param col.panel Background color of oscillogram, envelope and power spectrum panels. Default is 'bg' (white in static spectrograms).
#' @param col.title,cex.title Color and size of titles (the title of paged spectrograms and the 'Power' label of power spectra). Defaults are 'col.lab' and 1.
#' @param pal,colbg Color palette and background of spectrograms (see [spectrogram()]). Used when not supplied in [spectrogram()].
#' @param col.wave Color of oscillograms, envelopes and power spectra. Used when not supplied in [oscillogram()], [envelope()] or [spectrum()].
#' @param col.line Color of the play line (and cursor of paged spectrograms). Used when 'lcol' is not supplied in the style ([static()], [scrolling()] or [paged()]).
#' @param lty.line,lwd.line Type and width of the play line of scrolling spectrograms. Used when 'lty' and 'lwd' are not supplied in [scrolling()].
#' @param col.highlight Color of the highlight box of paged spectrograms (used when 'highlight.col' is not supplied in [paged()]) and of the translucent boxes dimming the parts of the [overview()] outside the window shown in scrolling spectrograms. Default is gray in overviews.
#' @param col.bar,bg.bar Color of the text and background of the player controls of web pages (play button, sliders, ...). Defaults are dark gray and transparent (the page background).
#' @param bar.style Style of the player controls of web pages: "standard" (rectangular buttons), "rounded" (round play button, rounded controls, a seek bar and a frame with rounded corners around the figure; 'bg.bar' is used as the background of the whole player) or "square" (flat bar with a square play button and sliders in the color of the play line). With "rounded" [player()] shows the seek bar and the time by default (`buttons = c("progress", "time")`), and with "square" the volume control and the time (`buttons = c("volume", "time")`). Default is "standard".
#' @param ... Settings of `dynatheme()` that replace those of the preset theme.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details Only the settings supplied are used; other settings take the defaults of each style. Settings supplied in other layers (e.g. the palette in [spectrogram()]) take precedence over those of the theme.
#'
#' Preset themes are complete themes: adding one replaces previous themes (use `dynatheme()` afterwards to change some of its settings, or supply them in the preset):
#' \itemize{
#'   \item `dynatheme_dark()`: black background, white text, 'mako' palette.
#'   \item `dynatheme_presentation()`: large bold text for slides and projectors.
#'   \item `dynatheme_paper()`: black and white with serif fonts, for printed figures.
#'   \item `dynatheme_minimal()`: no boxes, light gray axes and short ticks.
#'   \item `dynatheme_transparent()`: transparent background, to blend in slides or web pages of any color.
#'   \item `dynatheme_void()`: no axes, titles or boxes (only the sound).
#'   \item `dynatheme_macaulay()`: inspired by the player of the 'Macaulay Library': gray spectrogram, compact axes (time in minutes and seconds above the figure) and a black player bar with a square play button. Best with [scrolling()] and an [overview()] (e.g. `scrolling(t.display = 3, fix.time = FALSE)`).
#'   \item `dynatheme_xenocanto()`: inspired by the player of the 'Xeno-canto' web site: gray spectrogram, labels inside the figure, solid play line and rounded player controls with a seek bar. Best with [scrolling()] and an [oscillogram()] (e.g. `scrolling(t.display = 3, fix.time = FALSE)`).
#' }
#' @seealso [dynaspec()], [spectrogram()]
#' @export
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
#'   dynatheme(xlab = "Time (s)", ylab = "Frequency (kHz)", cex.lab = 1.4, font.lab = 2)
#'
#' # preset theme with larger axis titles
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) + oscillogram() +
#'   dynatheme_dark(cex.lab = 1.4)
#'
#' # Macaulay Library look
#' x <- dynaspec(scaly_breasted_hummingbird) + spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
#'   overview() + dynatheme_macaulay() + scrolling(t.display = 3, fix.time = FALSE)
#'
#' # Xeno-canto look (and its color version)
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(0, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) + oscillogram(height = 0.15) +
#'   dynatheme_xenocanto() + scrolling(t.display = 3, fix.time = FALSE)
#' x <- x + dynatheme_xenocanto(pal = viridis::inferno, colbg = "black", bg = "black",
#'   col.panel = "black", col.wave = "white", col.lab = "white", col.line = "white")
dynatheme <- function(xlab, ylab, cex.lab, cex.axis, font.lab, col.lab, axis.type, bg, family, las, tick.length, box, col.box,
                      grid, col.grid, col.panel, col.title, cex.title, pal, colbg, col.wave, col.line, lty.line, lwd.line, col.highlight,
                      col.bar, bg.bar, bar.style) {
  args <- provided_args_int(match.call(), parent.frame())
  layer_int("theme", check_theme_int(args))
}

#' @rdname dynatheme
#' @export
dynatheme_dark <- function(...) theme_preset_int(list(
  bg = "black", col.lab = "white", col.box = "gray60", col.panel = "black", pal = viridis::mako, colbg = "black",
  col.wave = "#A0DFB9", col.line = "white", col.highlight = "#3E356B", col.grid = "#FFFFFF40", col.bar = "white", bg.bar = "black"
), ...)

#' @rdname dynatheme
#' @export
dynatheme_presentation <- function(...) theme_preset_int(list(
  cex.lab = 1.6, cex.axis = 1.4, font.lab = 2, cex.title = 1.5, tick.length = 0.6
), ...)

#' @rdname dynatheme
#' @export
dynatheme_paper <- function(...) theme_preset_int(list(
  family = "serif", col.lab = "black", pal = seewave::reverse.gray.colors.1, colbg = "white", col.wave = "black",
  col.panel = "white", bg = "white", col.line = "black", las = 1, tick.length = 0.3, cex.axis = 0.9
), ...)

#' @rdname dynatheme
#' @export
dynatheme_minimal <- function(...) theme_preset_int(list(
  box = FALSE, col.lab = "gray30", tick.length = 0.25, las = 1, grid = TRUE, col.grid = "#00000014"
), ...)

#' @rdname dynatheme
#' @export
dynatheme_transparent <- function(...) theme_preset_int(list(
  bg = "transparent", col.panel = "transparent", bg.bar = "transparent"
), ...)

#' @rdname dynatheme
#' @export
dynatheme_void <- function(...) theme_preset_int(list(
  axis.type = "none", box = FALSE
), ...)

#' @rdname dynatheme
#' @export
dynatheme_macaulay <- function(...) theme_preset_int(list(
  axis.type = "compact", pal = c("#EBEBEB", "#111111"), colbg = "#EBEBEB", bg = "white", col.panel = "#EBEBEB",
  col.wave = "#333333", col.lab = "#555555", col.box = "#9E9E9E", cex.axis = 0.8, col.line = "#3D7299", lty.line = 1,
  lwd.line = 2, col.highlight = "#333333", bar.style = "square", col.bar = "white", bg.bar = "black"
), ...)

#' @rdname dynatheme
#' @export
dynatheme_xenocanto <- function(...) theme_preset_int(list(
  axis.type = "inside", box = FALSE, pal = seewave::reverse.gray.colors.1, colbg = "white", bg = "white", col.panel = "white",
  col.wave = "black", col.lab = "#333333", cex.axis = 0.85, tick.length = 0.3, col.line = "#333333", lty.line = 1, lwd.line = 2,
  col.highlight = "#F0F0F0", bar.style = "rounded", col.bar = "#333333", bg.bar = "#F0F0F0"
), ...)

#' Annotation layer of dynamic spectrograms
#'
#' `annotations()` highlights parts of the sound in a [dynaspec()] object with translucent boxes (e.g. calls, songs or the parts of a duet), optionally with borders and labels.
#' @param X Data frame with the selections: columns 'start' and 'end' (in s) and, optionally, 'bottom.freq' and 'top.freq' (in kHz) to restrict boxes to a frequency range. Selection tables from 'warbleR' (or tables from 'ohun' or 'Rraven') can be used directly (see details). Extended selection tables are not supported.
#' @param sound.file Name of the sound file whose selections are shown, when 'X' has a 'sound.files' column with several sound files. Default is `NULL`: when the sound of [dynaspec()] is a file, the selections of that file are shown (matching the file name); otherwise 'X' must contain a single sound file.
#' @param col Color of the boxes, or the name of a column of 'X' with the color of each box. Default is "#E69F00".
#' @param alpha Opacity of the boxes (0-1). Default is 0.3.
#' @param border Color of the box borders, or the name of a column of 'X' with the border color of each box. Default is `NULL` (no borders).
#' @param lwd Width of the box borders. Default is 1.
#' @param type Character string: "box" (translucent boxes) or "spectrogram" (the spectrogram is colored within each selection, with a gradient from the spectrogram background to the color of the selection, as [warbleR::color_spectro()]; only borders and labels are drawn on top). Default is "box".
#' @param lty Line type of the box borders, as in [graphics::par()]: an integer (0-6) or a name ("solid", "dashed", "dotted", "dotdash", "longdash" or "twodash"). Default is 1 (solid).
#' @param labels Name of a column of 'X' with labels to show at the top of each box. Default is `NULL` (no labels).
#' @param cex Size of the labels. Default is 1.
#' @param freq Logical to control if the frequency range of selections is used (when 'X' has 'bottom.freq' and 'top.freq' columns). If `FALSE` boxes span the whole frequency range. Default is `TRUE`.
#' @return A layer to be added to a [dynaspec()] object with `+`. Several annotation layers can be added.
#' @details Tables with a 'sound.files' column (e.g. 'warbleR' selection tables) can contain selections of several sound files; only those of the sound shown are used (see 'sound.file').
#' @seealso [dynaspec()], [captions()]
#' @export
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' st <- data.frame(start = c(1, 6), end = c(2.9, 7.4), labels = c("first notes", "last notes"),
#'                  colors = c("#E69F00", "#56B4E9"))
#'
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
#'   annotations(st, col = "colors", labels = "labels")
#'
#' # dashed borders of the same color as the boxes
#' x <- x + annotations(st, col = "colors", alpha = 0.1, border = "colors", lwd = 2, lty = "dashed")
#'
#' # spectrogram colored within the selections
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
#'   annotations(st, col = "colors", type = "spectrogram")
annotations <- function(X, col = "#E69F00", alpha = 0.3, type = "box", border = NULL, lwd = 1, lty = 1, labels = NULL, cex = 1, freq = TRUE, sound.file = NULL) {
  check_results <- checkmate::makeAssertCollection()
  if (inherits(X, "extended_selection_table"))
    check_results$push("'X': Extended selection tables are not supported; use a selection table (e.g. warbleR::selection_table()) or a data frame")
  checkmate::assert_data_frame(X, min.rows = 1, add = check_results, .var.name = "X")
  if (is.data.frame(X)) {
    checkmate::assert_names(names(X), must.include = c("start", "end"), add = check_results, .var.name = "X")
    if (!is.null(labels)) checkmate::assert_choice(labels, names(X), add = check_results, .var.name = "labels")
  }
  checkmate::assert_number(alpha, lower = 0, upper = 1, add = check_results, .var.name = "alpha")
  checkmate::assert_choice(type, c("box", "spectrogram"), add = check_results, .var.name = "type")
  checkmate::assert_flag(freq, add = check_results, .var.name = "freq")
  is_col <- function(y) all(vapply(y, function(z) !inherits(try(grDevices::col2rgb(z), silent = TRUE), "try-error"), logical(1)))
  if (is.data.frame(X) && !(length(col) == 1 && col %in% names(X)) && !is_col(col))
    check_results$push("'col': Must be a valid color or the name of a column of 'X'")
  if (is.data.frame(X) && !is.null(border) && !(length(border) == 1 && border %in% names(X)) && !is_col(border))
    check_results$push("'border': Must be a valid color or the name of a column of 'X'")
  checkmate::assert_number(lwd, lower = 0, add = check_results, .var.name = "lwd")
  if (!(checkmate::test_choice(lty, 0:6) || checkmate::test_choice(lty, c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))))
    check_results$push("'lty': Must be an integer between 0 and 6 or one of 'blank', 'solid', 'dashed', 'dotted', 'dotdash', 'longdash' or 'twodash'")
  if (!is.null(sound.file)) {
    checkmate::assert_string(sound.file, add = check_results, .var.name = "sound.file")
    if (is.data.frame(X) && !"sound.files" %in% names(X))
      check_results$push("'sound.file': 'X' has no 'sound.files' column")
    else if (is.data.frame(X) && checkmate::test_string(sound.file) && !sound.file %in% X$sound.files)
      check_results$push(sprintf("'sound.file': '%s' is not in the 'sound.files' column of 'X'", sound.file))
  }
  .report_assertions(check_results)
  # plain data frame (e.g. from warbleR selection tables; attributes are not needed)
  X <- data.frame(lapply(as.list(X), function(v) { attributes(v) <- NULL; v }), stringsAsFactors = FALSE)
  if (!is.null(sound.file)) X <- X[X$sound.files == sound.file, , drop = FALSE]
  layer_int("annotations", list(X = X, col = col, alpha = alpha, type = type, border = border, lwd = lwd, lty = lty, labels = labels, cex = cex, freq = freq, sound.file = sound.file))
}

#' Caption layer of dynamic spectrograms
#'
#' `captions()` adds text to a [dynaspec()] object at a fixed position of the figure, shown (fading in and out) during a time range of the sound, like subtitles. They are shown in the same way in all styles, in web pages and videos. To label parts of the spectrogram use the 'labels' argument of [annotations()] instead.
#' @param labels Character vector with the captions.
#' @param start,end Numeric vectors with the start and end times (in s) of each caption.
#' @param x,y Position of the center of the captions as proportions of the figure width and height (0-1, from the left and bottom). Defaults are 0.5 and 0.9.
#' @param fading Duration (in s) of the fade-in and fade-out of captions. Default is 0.
#' @param col,cex,font Color, size and font (1 = plain, 2 = bold, 3 = italic, 4 = bold italic) of the captions. Defaults are "black", 1 and 1.
#' @return A layer to be added to a [dynaspec()] object with `+`. Several caption layers can be added (e.g. at different positions).
#' @details Each caption layer shows one caption at a time; when the time ranges of its captions overlap the most visible one is shown.
#' @seealso [dynaspec()], [annotations()]
#' @export
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
#'   captions(c("first notes", "last notes"), start = c(1, 6), end = c(2.9, 7.4), fading = 0.2)
captions <- function(labels, start, end, x = 0.5, y = 0.9, fading = 0, col = "black", cex = 1, font = 1) {
  check_results <- checkmate::makeAssertCollection()
  checkmate::assert_character(labels, min.len = 1, any.missing = FALSE, add = check_results, .var.name = "labels")
  checkmate::assert_numeric(start, len = length(labels), lower = 0, any.missing = FALSE, add = check_results, .var.name = "start")
  checkmate::assert_numeric(end, len = length(labels), lower = 0, any.missing = FALSE, add = check_results, .var.name = "end")
  checkmate::assert_number(x, lower = 0, upper = 1, add = check_results, .var.name = "x")
  checkmate::assert_number(y, lower = 0, upper = 1, add = check_results, .var.name = "y")
  checkmate::assert_number(fading, lower = 0, add = check_results, .var.name = "fading")
  checkmate::assert_number(cex, lower = 0.01, add = check_results, .var.name = "cex")
  checkmate::assert_choice(font, 1:4, add = check_results, .var.name = "font")
  if (inherits(try(grDevices::col2rgb(col), silent = TRUE), "try-error"))
    check_results$push("'col': Must be a valid color")
  .report_assertions(check_results)
  layer_int("captions", list(labels = labels, start = start, end = end, x = x, y = y, fading = fading, col = col, cex = cex, font = font))
}

#' Player settings of dynamic spectrogram web pages
#'
#' `player()` sets the playback controls of web pages.
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
#' Use \code{character()} to show only the play button and speed slider. Default is \code{"time"} (\code{c("progress", "time")} with themes with \code{bar.style = "rounded"}, e.g. [dynatheme_xenocanto()]). Buttons are only shown in web pages (not in videos).
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()]
#' @export
#' @examples
#' \dontrun{
#' data("scaly_breasted_hummingbird")
#'
#' dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) +
#'   player(buttons = c("time", "loop", "volume", "download", "fullscreen"))
#' }
player <- function(speeds = c(1, 0.5, 0.25, 0.125), keep.pitch = FALSE, buttons = "time") {
  # only supplied arguments are stored (the default buttons depend on the theme)
  args <- provided_args_int(match.call(), parent.frame())
  .report_assertions(.check_arguments("player", args))
  layer_int("player", args)
}

#' Styles of dynamic spectrograms
#'
#' Set the style of a [dynaspec()] object:
#' - `scrolling()`: the spectrogram scrolls from right to left along a fixed play line.
#' - `paged()`: the spectrogram is split into pages, and a translucent box reveals the part of each page being played.
#' - `static()`: a play line moves over the whole spectrogram (and oscillogram), which can be split in several rows (default style).
#'
#' Only the arguments supplied are used; other settings take the defaults shown.
#' @param t.display Time (in s) shown at once: the time window of scrolling spectrograms (default 1.5) or the duration of each page of paged spectrograms (default: the whole sound, up to 5 s).
#' @param fix.time Logical: does the time axis stay fixed (`TRUE`, default) or move along with the scrolling spectrogram?
#' @param buffer Time (in s) before the scrolling spectrogram starts moving. Default is 0.
#' @param loop Number of times the sound is repeated in scrolling videos (web pages loop until paused when > 1). Default is 1.
#' @param lcol Color of the play line (or cursor of paged spectrograms). Defaults are "#07889B99" (scrolling), "white" (paged) and `viridis::mako(10)[9]` (static).
#' @param lty,lwd Type and width of the play line of scrolling spectrograms (defaults 2 and 2).
#' @param speed Initial playback speed of scrolling spectrograms (videos are played at this speed). Default is 1.
#' @param width,height,res Size (in pixels) and resolution of the figures: frames of scrolling spectrograms (defaults 700, 400 and 70), and each row of static spectrograms (defaults 1000, 500 and 100).
#' @param fps Frames per second of videos. Defaults are 50 (scrolling) and 30 (paged and static).
#' @param parallel Number of cores used to render videos of scrolling and paged spectrograms. Default is 1.
#' @param ... Other arguments of the style:
#' - `scrolling()`: 'hop.size' (window length in ms, used when 'wl' is not set in [spectrogram()]; default 11.6), 'height.prop' (relative heights of the scrolling spectrogram and the lower panel; default `c(5, 1)`), 'ggspectro' (use [seewave::ggspectro()]; default `FALSE`), 'derivative' (spectral derivatives; requires 'imager'; default `FALSE`) and 'spectro.call' (a call to a spectrogram function replacing the default one, made with [base::call()]).
#' - `paged()`: 'resample.rate' (sampling rate in Hz used for the spectrogram; default 15000), 'amp.thresh' (amplitude threshold, as a percentage; default 0), 'filter' (frequency range in kHz to filter out), 'dest.path' (folder for temporary files) and 'delete.temp' (delete temporary files; default `TRUE`).
#' @param title Title of paged spectrograms.
#' @param highlight.col,highlight.alpha Color and opacity of the highlight box of paged spectrograms (defaults "#4B0C6BFF" and 0.6).
#' @param plot.legend Logical: show the amplitude color scale of paged spectrograms? Default is `FALSE`.
#' @param spec.width,spec.height Size (in inches) of the pages of paged spectrograms (defaults 9 and 3).
#' @param rows Number of rows in which static spectrograms are split (each row shows a consecutive segment of the same duration). Default is 1.
#' @param alt Alternative text of static spectrograms in web pages (for screen readers).
#' @param play.line Logical: show the line moving along the sound in static spectrograms? With `FALSE` web pages show the figure without the moving line (clicking on it still plays the sound from that point) and videos show the figure with the sound. Default is `TRUE`.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()], [save_dynaspec()]
#' @name styles
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1))
#'
#' \dontrun{
#' x + scrolling(t.display = 1.7, axis.type = "minimal")
#' x + paged(t.display = 2, axis.type = "standard")
#' x + oscillogram() + static(rows = 2, height = 350)
#' }
NULL

#' @rdname styles
#' @export
scrolling <- function(t.display, fix.time, buffer, loop, lcol, lty, lwd, speed, width, height, res, fps, parallel, ...) {
  style_int("scrolling", provided_args_int(match.call(), parent.frame()))
}

#' @rdname styles
#' @export
paged <- function(t.display, title, highlight.col, highlight.alpha, lcol, plot.legend, spec.width, spec.height, fps, parallel, ...) {
  style_int("paged", provided_args_int(match.call(), parent.frame()))
}

#' @rdname styles
#' @export
static <- function(rows, width, height, res, lcol, alt, fps, play.line) {
  style_int("static", provided_args_int(match.call(), parent.frame()))
}

#' Add layers to dynamic spectrograms
#'
#' @param e1 A [dynaspec()] object.
#' @param e2 A layer (e.g. [spectrogram()], [annotations()] or [scrolling()]).
#' @return A [dynaspec()] object. Spectrogram, oscillogram, envelope, spectrum, overview, player and style layers replace previous ones of the same kind (arguments of spectrogram, theme and style layers are combined); annotations and captions are added.
#' @export
`+.dynaspec` <- function(e1, e2) {
  if (missing(e2) || !inherits(e2, "dynaspec_layer"))
    stop2("only dynaspec layers (e.g. spectrogram(), annotations() or scrolling()) can be added to dynaspec objects")
  if (!inherits(e1, "dynaspec"))
    stop2("layers must be added to a dynaspec object (see dynaspec())")

  switch(e2$type,
    spectrogram = e1$spectrogram <- utils::modifyList(if (is.null(e1$spectrogram)) list() else e1$spectrogram, e2$args),
    oscillogram = ,
    envelope = e1$panels <- c(Filter(function(p) p$type != e2$args$type, e1$panels), list(e2$args)),
    spectrum = e1$spectrum <- e2$args,
    overview = e1$overview <- e2$args,
    # (preset themes replace previous themes)
    theme = e1$theme <- if (isTRUE(e2$complete)) e2$args else utils::modifyList(if (is.null(e1$theme)) list() else e1$theme, e2$args),
    player = e1$player <- e2$args,
    annotations = e1$selections <- c(e1$selections, list(e2$args)),
    captions = e1$captions <- c(e1$captions, list(e2$args)),
    style = e1$style <- if (identical(e1$style$type, e2$style)) list(type = e2$style, args = utils::modifyList(e1$style$args, e2$args)) else list(type = e2$style, args = e2$args)
  )
  e1
}

#' Save dynamic spectrograms
#'
#' `save_dynaspec()` saves a [dynaspec()] object as an interactive web page, a video or still images.
#' @param x A [dynaspec()] object.
#' @param file.name Character string with the name of the file. The extension sets the output:
#' - '.html': interactive web page.
#' - '.mp4': video (requires 'ffmpeg').
#' - '.png': still image of static spectrograms, or one image per page of paged spectrograms (named after 'file.name', followed by the page number).
#'
#' Default is `NULL`, which returns the HTML code of the web page without saving it.
#' @param play Logical argument to control if the file is opened after it is saved (or the web page is shown in the viewer when 'file.name' is `NULL`, in interactive sessions). Default is `TRUE`.
#' @param verbose Logical argument to control if progress messages are printed. Default is `TRUE`.
#' @return Invisibly returns the path to the file, or the HTML code of the web page (a character string of class 'dynaSpec_html') when 'file.name' is `NULL` or ends in '.html'. When knitting R Markdown or Quarto documents the web page is embedded in the document.
#' @details Videos of scrolling spectrograms render every frame, which takes time (it can be sped up with the 'parallel' and 'fps' arguments of [scrolling()]). Videos of paged and static spectrograms and all web pages are created in seconds.
#' @seealso [dynaspec()]
#' @export
#' @examples
#' data(scaly_breasted_hummingbird)
#'
#' x <- dynaspec(scaly_breasted_hummingbird, flim = c(2, 11)) +
#'   spectrogram(wl = 200, collevels = seq(-50, 0, 1)) + oscillogram()
#'
#' save_dynaspec(x, file.path(tempdir(), "hummingbird.html"), play = FALSE)
#' save_dynaspec(x, file.path(tempdir(), "hummingbird.png"), play = FALSE)
#'
#' \dontrun{
#' # videos (require ffmpeg)
#' save_dynaspec(x, file.path(tempdir(), "hummingbird.mp4"))
#' save_dynaspec(x + scrolling(t.display = 1.7), file.path(tempdir(), "hummingbird_scrolling.mp4"))
#' }
save_dynaspec <- function(x, file.name = NULL, play = TRUE, verbose = TRUE) {
  if (!inherits(x, "dynaspec"))
    stop2("'x' must be a dynaspec object (see dynaspec())")
  check_results <- .check_arguments("save_dynaspec", list(play = play, verbose = verbose))
  if (!is.null(file.name))
    checkmate::assert_string(file.name, pattern = "\\.(html?|mp4|png)$", ignore.case = TRUE, add = check_results, .var.name = "file.name")
  .report_assertions(check_results)

  ext <- if (is.null(file.name)) "html" else tolower(tools::file_ext(file.name))
  if (ext == "htm") ext <- "html"
  if (ext == "mp4" && !have_ffmpeg_exec2())
    stop2("'ffmpeg' could not be found. It must be installed (and available in the system path) to create videos (see https://github.com/maRce10/dynaSpec#ffmpeg)")

  # settings of the theme not supplied in other layers (colors and font family)
  x <- theme_layers_int(x)

  # sound (cropped if requested; times of layers are shifted accordingly) and layers
  wave <- x$wave
  layers <- list(selections = lapply(x$selections, sound_file_selections_int, x = x), captions = x$captions)
  if (!is.null(x$tlim)) {
    w <- read_sound_int(wave)$wave
    wave <- seewave::cutw(w, from = x$tlim[1], to = min(x$tlim[2], seewave::duration(w)), output = "Wave")
    layers <- shift_layers_int(layers, -x$tlim[1])
  }

  type <- x$style$type
  if (ext == "png" && type == "scrolling")
    stop2("still images ('.png') are only available for static and paged spectrograms")
  if (type %in% c("scrolling", "paged") && is.null(x$spectrogram))
    stop2(type, " spectrograms need a spectrogram: add spectrogram() (e.g. dynaspec(wave) + spectrogram() + ", type, "())")

  # web pages without file are written to a temporary file
  out <- if (is.null(file.name)) tempfile(fileext = ".html") else file.name
  play_out <- play && !is.null(file.name)

  res <- switch(type,
    static = save_static_int(x, wave, layers, out, ext, play_out),
    scrolling = save_scrolling_int(x, wave, layers, out, play_out, verbose),
    paged = save_paged_int(x, wave, layers, out, ext, play_out, verbose)
  )

  # show web pages without file in the viewer
  if (is.null(file.name) && play && interactive() && !isTRUE(getOption("knitr.in.progress")) && inherits(res, "dynaSpec_html"))
    view_html_int(res)

  invisible(res)
}

#' @export
print.dynaspec <- function(x, ...) {
  if (interactive())
    save_dynaspec(x, play = TRUE, verbose = FALSE) else
      cat("<dynaspec: ", x$style$type, " spectrogram of '", x$name, "'> (use save_dynaspec() to save it)\n", sep = "")
  invisible(x)
}

#' @exportS3Method knitr::knit_print
knit_print.dynaspec <- function(x, ...) {
  # returned visibly: knitr does not include invisible values in documents
  res <- save_dynaspec(x, play = FALSE, verbose = FALSE)
  res
}

#' @export
print.dynaspec_layer <- function(x, ...) {
  cat("<dynaspec layer: ", if (x$type == "style") paste(x$style, "style") else x$type, "> (add it to a dynaspec object with +)\n", sep = "")
  invisible(x)
}

# print method for the HTML code returned by save_dynaspec()
#' @export
print.dynaSpec_html <- function(x, ...) {
  if (interactive())
    view_html_int(x) else
      cat("<dynaSpec HTML player (", format(structure(nchar(x, type = "bytes"), class = "object_size"), units = "auto"), ")>\n", sep = "")
  invisible(x)
}
