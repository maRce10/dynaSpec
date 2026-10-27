#' Create dynamic spectrograms
#'
#' `dynaspec()` starts a dynamic spectrogram of a sound. Layers are added with `+`, as in 'ggplot2' graphs: [spectrogram()] and [oscillogram()] set the panels, [selections()] and [annotations()] highlight and label parts of the sound, and [scrolling()], [paged()] or [static()] set the style. Printing the object shows an interactive web page (embedded in R Markdown and Quarto documents when knitting), and [save_dynaspec()] saves it as a web page, a video or still images.
#' @param wave The sound: an object of class 'Wave' or the path to a '.wav' or '.mp3' file.
#' @param flim Numeric vector of length 2 with the frequency limits (in kHz). Default is `NULL` (from 0 to the Nyquist frequency, or the style default).
#' @param crop Numeric vector of length 1 or 2 with the time range (in s) of the sound to use: a single value takes the first seconds of the sound, and two values a time interval. Times of [selections()] and [annotations()] refer to the original (uncropped) sound. Default is `NULL` (whole sound).
#' @return An object of class 'dynaspec'.
#' @details The three styles show the same spectrogram in different ways:
#' - [scrolling()]: the spectrogram scrolls along a fixed play line.
#' - [paged()]: the spectrogram is split into pages, and a translucent box reveals the part of each page being played.
#' - [static()] (default): a play line moves over the whole spectrogram (and oscillogram), which can be split in several rows.
#'
#' Web pages include buttons to slow down playback (see [player()]), and clicking on the spectrogram plays the sound from that point. Figures and sounds are embedded in the HTML code, so web pages can be shared as single files or pasted into any web page. Note that embedding sounds makes web pages larger (about 1.4 times the size of the sound file), so cut or downsample long recordings before creating web pages.
#'
#' Settings not supplied in layers take the defaults of each style (e.g. paged spectrograms use the 'inferno' color palette unless [spectrogram()] sets another one).
#' @seealso [save_dynaspec()], [spectrogram()], [scrolling()]
#' @export
#' @name dynaspec
#' @examples
#' data(canyon_wren)
#'
#' # static spectrogram with oscillogram (printing shows it in the viewer or browser)
#' x <- dynaspec(canyon_wren, flim = c(1, 10)) +
#'   spectrogram(wl = 300) +
#'   oscillogram()
#'
#' # save as a web page
#' save_dynaspec(x, file.path(tempdir(), "canyon_wren.html"), play = FALSE)
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
#' save_dynaspec(x + scrolling(t.display = 1.7), "canyon_wren.mp4")
#'
#' # highlight and label parts of the song
#' st <- data.frame(start = c(0.5, 3.5), end = c(2, 5), labels = c("start", "end"))
#' x + selections(st, labels = "labels") + annotations("canyon wren", start = 0, end = 5.3)
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#' @references
#' Araya-Salas M & Wilkins M R. (2020). *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.

dynaspec <- function(wave, flim = NULL, crop = NULL) {
  check_results <- checkmate::makeAssertCollection()
  if (!isTRUE(res <- check_wave_int(wave)))
    check_results$push(paste0("'wave': ", res))
  if (!is.null(flim))
    checkmate::assert_numeric(flim, len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_results, .var.name = "flim")
  if (!is.null(crop))
    checkmate::assert_numeric(crop, min.len = 1, max.len = 2, lower = 0, finite = TRUE, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_results, .var.name = "crop")
  .report_assertions(check_results)

  # name for output files: sound file name or name of the Wave object
  name <- if (is.character(wave)) tools::file_path_sans_ext(basename(wave)) else make.names(paste(deparse(substitute(wave)), collapse = ""))

  new_dynaspec_int(wave = wave, name = name, flim = flim, crop = if (length(crop) == 1) c(0, crop) else crop)
}

#' Spectrogram layer of dynamic spectrograms
#'
#' `spectrogram()` sets how the spectrogram of a [dynaspec()] object is computed and drawn.
#' @param wl Window length (in samples), as in [seewave::spectro()].
#' @param ovlp Percent overlap between consecutive windows, as in [seewave::spectro()].
#' @param wn Window function name, as in [seewave::spectro()] (e.g. "hanning" or "blackman").
#' @param pal Color palette function (e.g. [viridis::viridis()] or [seewave::reverse.gray.colors.1()]). Paged spectrograms also take the names of 'viridis' palettes (e.g. "magma") and 2 colors for a gradient (e.g. `c("white", "black")`).
#' @param collevels Numeric vector with the amplitude levels (in dB) of the colors, as in [seewave::spectro()] (e.g. `seq(-40, 0, 1)`).
#' @param colbg Background color.
#' @param amp.trans Amplitude transformation to brighten paged spectrograms (values > 1 brighten quiet sounds; only used by [paged()]).
#' @param ... Additional arguments passed to [seewave::spectro()].
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details Only the arguments supplied are used; other settings take the defaults of each style (see [scrolling()], [paged()] and [static()]).
#' @seealso [dynaspec()]
#' @export
#' @examples
#' data(canyon_wren)
#'
#' x <- dynaspec(canyon_wren, flim = c(1, 10)) +
#'   spectrogram(wl = 300, pal = viridis::mako, collevels = seq(-60, 0, 1))
spectrogram <- function(wl, ovlp, wn, pal, collevels, colbg, amp.trans, ...) {
  layer_int("spectrogram", provided_args_int(match.call(), parent.frame()))
}

#' Oscillogram layer of dynamic spectrograms
#'
#' `oscillogram()` adds an oscillogram (waveform) to a [dynaspec()] object. Oscillograms are shown by [static()] (above or below the spectrogram) and [scrolling()] (below the spectrogram) styles.
#' @param position Character string: "below" or "above" the spectrogram (only for [static()]; [scrolling()] always shows it below). Default is "below".
#' @param col Color of the oscillogram. Default is "black".
#' @param height Numeric vector of length 1 with the height of the oscillogram relative to that of the spectrogram (e.g. 0.5 is half as high). Default is `NULL` (style default).
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()]
#' @export
oscillogram <- function(position = "below", col = "black", height = NULL) {
  check_results <- .check_arguments("oscillogram", list(colwave = col))
  checkmate::assert_choice(position, c("below", "above"), add = check_results, .var.name = "position")
  if (!is.null(height)) checkmate::assert_number(height, lower = 0.01, upper = 10, add = check_results, .var.name = "height")
  .report_assertions(check_results)
  layer_int("oscillogram", list(position = position, col = col, height = height))
}

#' Selection layer of dynamic spectrograms
#'
#' `selections()` highlights parts of the sound in a [dynaspec()] object with translucent boxes (e.g. calls, songs or the parts of a duet).
#' @param X Data frame with the selections: columns 'start' and 'end' (in s) and, optionally, 'bottom.freq' and 'top.freq' (in kHz) to restrict boxes to a frequency range (e.g. a selection table from 'warbleR' or 'ohun').
#' @param col Color of the boxes, or the name of a column of 'X' with the color of each box. Default is "#E69F00".
#' @param alpha Opacity of the boxes (0-1). Default is 0.3.
#' @param border Color of the box borders. Default is `NULL` (no borders).
#' @param lwd Width of the box borders. Default is 1.
#' @param labels Name of a column of 'X' with labels to show at the top of each box. Default is `NULL` (no labels).
#' @param cex Size of the labels. Default is 1.
#' @param freq Logical to control if the frequency range of selections is used (when 'X' has 'bottom.freq' and 'top.freq' columns). If `FALSE` boxes span the whole frequency range. Default is `TRUE`.
#' @return A layer to be added to a [dynaspec()] object with `+`. Several selection layers can be added.
#' @seealso [dynaspec()], [annotations()]
#' @export
#' @examples
#' data(canyon_wren)
#'
#' st <- data.frame(start = c(0.4, 3.6), end = c(1.5, 4.8), labels = c("first notes", "last notes"),
#'                  colors = c("#E69F00", "#56B4E9"))
#'
#' x <- dynaspec(canyon_wren, flim = c(1, 10)) + spectrogram(wl = 300) +
#'   selections(st, col = "colors", labels = "labels")
selections <- function(X, col = "#E69F00", alpha = 0.3, border = NULL, lwd = 1, labels = NULL, cex = 1, freq = TRUE) {
  check_results <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(X, min.rows = 1, add = check_results, .var.name = "X")
  if (is.data.frame(X)) {
    checkmate::assert_names(names(X), must.include = c("start", "end"), add = check_results, .var.name = "X")
    if (!is.null(labels)) checkmate::assert_choice(labels, names(X), add = check_results, .var.name = "labels")
  }
  checkmate::assert_number(alpha, lower = 0, upper = 1, add = check_results, .var.name = "alpha")
  checkmate::assert_flag(freq, add = check_results, .var.name = "freq")
  if (is.data.frame(X) && !(length(col) == 1 && col %in% names(X)) && !all(vapply(col, function(y) !inherits(try(grDevices::col2rgb(y), silent = TRUE), "try-error"), logical(1))))
    check_results$push("'col': Must be a valid color or the name of a column of 'X'")
  .report_assertions(check_results)
  X <- as.data.frame(X)
  layer_int("selections", list(X = X, col = col, alpha = alpha, border = border, lwd = lwd, labels = labels, cex = cex, freq = freq))
}

#' Annotation layer of dynamic spectrograms
#'
#' `annotations()` adds text labels to a [dynaspec()] object, each one shown during a time range of the sound.
#' @param labels Character vector with the labels.
#' @param start,end Numeric vectors with the start and end times (in s) of each label.
#' @param x,y Position of the labels as proportions of the panel width and height (0-1). In [paged()] and [static()] styles labels are drawn at the center of their time range, so only 'y' is used. Defaults are 0.5 and 0.9.
#' @param fading Duration (in s) of the fade-in and fade-out of labels in [scrolling()] spectrograms. Default is 0.
#' @param col,cex,font Color, size and font (1 = plain, 2 = bold, 3 = italic, 4 = bold italic) of the labels.
#' @return A layer to be added to a [dynaspec()] object with `+`. Several annotation layers can be added.
#' @details In [scrolling()] spectrograms labels stay at a fixed position and fade in and out while their time range reaches the play line. In [paged()] and [static()] spectrograms they are drawn on the spectrogram, at the center of their time range.
#' @seealso [dynaspec()], [selections()]
#' @export
annotations <- function(labels, start, end, x = 0.5, y = 0.9, fading = 0, col = "black", cex = 1, font = 1) {
  check_results <- checkmate::makeAssertCollection()
  checkmate::assert_character(labels, min.len = 1, any.missing = FALSE, add = check_results, .var.name = "labels")
  checkmate::assert_numeric(start, len = length(labels), lower = 0, any.missing = FALSE, add = check_results, .var.name = "start")
  checkmate::assert_numeric(end, len = length(labels), lower = 0, any.missing = FALSE, add = check_results, .var.name = "end")
  checkmate::assert_number(x, lower = 0, upper = 1, add = check_results, .var.name = "x")
  checkmate::assert_number(y, lower = 0, upper = 1, add = check_results, .var.name = "y")
  checkmate::assert_number(fading, lower = 0, add = check_results, .var.name = "fading")
  .report_assertions(check_results)
  layer_int("annotations", list(labels = labels, start = start, end = end, x = x, y = y, fading = fading, col = col, cex = cex, font = font))
}

#' Player settings of dynamic spectrogram web pages
#'
#' `player()` sets the playback buttons of web pages.
#' @param speeds Numeric vector with the playback speeds offered as buttons (between 0.0625 and 16). The first value is the initial speed. Use a single value to hide speed buttons. Default is `c(1, 0.5, 0.25, 0.125)`.
#' @param keep.pitch Logical argument to control if the pitch is initially kept when playback speed changes. Listeners can switch this with the 'keep pitch' button. If `FALSE` slowing down a sound also lowers its pitch (as when slowing down a tape), which makes fast sound features (e.g. pulses) easier to hear. If `TRUE` the web browser time-stretches the sound to keep its pitch. Default is `FALSE`.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()]
#' @export
player <- function(speeds = c(1, 0.5, 0.25, 0.125), keep.pitch = FALSE) {
  .report_assertions(.check_arguments("player", list(speeds = speeds, keep.pitch = keep.pitch)))
  layer_int("player", list(speeds = speeds, keep.pitch = keep.pitch))
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
#' @param overview Logical: show the whole sound in a lower spectrogram, with the part being shown highlighted (scrolling spectrograms without oscillogram)? Default is `TRUE`.
#' @param buffer Time (in s) before the scrolling spectrogram starts moving. Default is 1 (0 when looping).
#' @param loop Number of times the sound is repeated in scrolling videos (web pages loop until paused when > 1). Default is 1.
#' @param axis.type Character string: "standard" (axes with labels), "minimal" (1 s and 1 kHz scale bars; scrolling only) or "none". Defaults are "standard" (scrolling) and "none" (paged).
#' @param lcol Color of the play line (or cursor of paged spectrograms). Defaults are "#07889B99" (scrolling), "white" (paged) and "#c0392b" (static).
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
#' @param bg.flood Logical: does the background color of paged spectrograms spill into the axis margins? Default is `FALSE`.
#' @param colaxis Color of axes and text of paged spectrograms (default: picked to contrast with the background).
#' @param plot.legend Logical: show the amplitude color scale of paged spectrograms? Default is `FALSE`.
#' @param spec.width,spec.height Size (in inches) of the pages of paged spectrograms (defaults 9 and 3).
#' @param rows Number of rows in which static spectrograms are split (each row shows a consecutive segment of the same duration). Default is 1.
#' @param cex.lab Size of the axis titles of static spectrograms. Default is 1.2.
#' @param alt Alternative text of static spectrograms in web pages (for screen readers).
#' @param spectro Logical: show the spectrogram in static spectrograms? (`FALSE` shows only the oscillogram). Default is `TRUE`.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @seealso [dynaspec()], [save_dynaspec()]
#' @name styles
#' @examples
#' data(canyon_wren)
#'
#' x <- dynaspec(canyon_wren, flim = c(1, 10)) + spectrogram(wl = 300)
#'
#' \dontrun{
#' x + scrolling(t.display = 1.7, axis.type = "minimal")
#' x + paged(t.display = 2, axis.type = "standard")
#' x + oscillogram() + static(rows = 2, height = 350)
#' }
NULL

#' @rdname styles
#' @export
scrolling <- function(t.display, fix.time, overview, buffer, loop, axis.type, lcol, lty, lwd, speed, width, height, res, fps, parallel, ...) {
  style_int("scrolling", provided_args_int(match.call(), parent.frame()))
}

#' @rdname styles
#' @export
paged <- function(t.display, axis.type, title, highlight.col, highlight.alpha, lcol, bg.flood, colaxis, plot.legend, spec.width, spec.height, fps, parallel, ...) {
  style_int("paged", provided_args_int(match.call(), parent.frame()))
}

#' @rdname styles
#' @export
static <- function(rows, width, height, res, cex.lab, lcol, alt, fps, spectro) {
  style_int("static", provided_args_int(match.call(), parent.frame()))
}

#' Add layers to dynamic spectrograms
#'
#' @param e1 A [dynaspec()] object.
#' @param e2 A layer (e.g. [spectrogram()], [selections()] or [scrolling()]).
#' @return A [dynaspec()] object. Spectrogram, oscillogram, player and style layers replace previous ones (arguments of spectrogram and style layers are combined); selections and annotations are added.
#' @export
`+.dynaspec` <- function(e1, e2) {
  if (missing(e2) || !inherits(e2, "dynaspec_layer"))
    stop2("only dynaspec layers (e.g. spectrogram(), selections() or scrolling()) can be added to dynaspec objects")
  if (!inherits(e1, "dynaspec"))
    stop2("layers must be added to a dynaspec object (see dynaspec())")

  switch(e2$type,
    spectrogram = e1$spectrogram <- utils::modifyList(e1$spectrogram, e2$args),
    oscillogram = e1$oscillogram <- e2$args,
    player = e1$player <- e2$args,
    selections = e1$selections <- c(e1$selections, list(e2$args)),
    annotations = e1$annotations <- c(e1$annotations, list(e2$args)),
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
#' data(canyon_wren)
#'
#' x <- dynaspec(canyon_wren, flim = c(1, 10)) + spectrogram(wl = 300) + oscillogram()
#'
#' save_dynaspec(x, file.path(tempdir(), "canyon_wren.html"), play = FALSE)
#' save_dynaspec(x, file.path(tempdir(), "canyon_wren.png"), play = FALSE)
#'
#' \dontrun{
#' # videos (require ffmpeg)
#' save_dynaspec(x, file.path(tempdir(), "canyon_wren.mp4"))
#' save_dynaspec(x + scrolling(t.display = 1.7), file.path(tempdir(), "canyon_wren_scrolling.mp4"))
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

  # sound (cropped if requested; times of layers are shifted accordingly) and layers
  wave <- x$wave
  layers <- list(selections = x$selections, annotations = x$annotations)
  if (!is.null(x$crop)) {
    w <- read_sound_int(wave)$wave
    wave <- seewave::cutw(w, from = x$crop[1], to = min(x$crop[2], seewave::duration(w)), output = "Wave")
    layers <- shift_layers_int(layers, -x$crop[1])
  }

  type <- x$style$type
  if (ext == "png" && type == "scrolling")
    stop2("still images ('.png') are only available for static and paged spectrograms")
  if (!is.null(x$oscillogram) && type == "paged")
    warning2("oscillograms are not shown in paged spectrograms")

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

# print method for the HTML code returned by save_dynaspec() and sound_buttons()
#' @export
print.dynaSpec_html <- function(x, ...) {
  if (interactive())
    view_html_int(x) else
      cat("<dynaSpec HTML player (", format(structure(nchar(x, type = "bytes"), class = "object_size"), units = "auto"), ")>\n", sep = "")
  invisible(x)
}
