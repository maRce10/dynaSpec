# dynaspec(): dynamic spectrogram objects, adding layers with + and printing

#' Create dynamic spectrograms
#'
#' `dynaspec()` starts a dynamic spectrogram of a sound. Layers are added with `+`, as in 'ggplot2' graphs: [ds_spectrogram()] and [ds_oscillogram()] add the panels, [ds_annotations()] and [ds_captions()] highlight and label parts of the sound, and [ds_scrolling()], [ds_paged()] or [ds_static()] set the style. Printing the object shows an interactive web page (embedded in R Markdown and Quarto documents when knitting), and [save_dynaspec()] saves it as a web page, a video or still images.
#' @param wave The sound: an object of class 'Wave', the path to a '.wav' or '.mp3' file, or the path to a video file with sound (e.g. '.mp4', '.mov', '.mkv', '.webm' or '.avi'; requires 'ffmpeg'). The sound of videos is used as the sound, and [ds_video()] shows the video next to the spectrogram.
#' @param flim Numeric vector of length 2 with the frequency limits (in kHz). Default is `NULL` (from 0 to the Nyquist frequency, or the style default). `Inf` as upper limit means the Nyquist frequency of the sound (half its sampling rate). Any other upper limit higher than the Nyquist frequency gives a warning, and spectrograms are shown up to the Nyquist frequency.
#' @param tlim Numeric vector of length 1 or 2 with the time range (in s) of the sound to use: a single value takes the first seconds of the sound, and two values a time interval. `Inf` as upper limit means the end of the sound. Times of [ds_annotations()] and [ds_captions()] refer to the whole sound. Default is `NULL` (whole sound).
#' @return An object of class 'dynaspec'.
#' @details Panels are added as layers, as in 'ggplot2': only the panels added are shown ([ds_spectrogram()], [ds_oscillogram()], [ds_envelope()] or several of them, and [ds_spectrum()] next to the spectrogram). `dynaspec()` alone shows an empty panel with a time axis and the play line (annotations and captions can be drawn on it). Scrolling and paged spectrograms need a spectrogram.
#'
#' The three styles show the sound in different ways:
#' - [ds_scrolling()]: the spectrogram scrolls past the play line (the line only moves at the start and the end of the sound).
#' - [ds_paged()]: the spectrogram is split into pages, and a translucent box reveals the part of each page being played.
#' - [ds_static()] (default): a play line moves over the whole figure (spectrogram, oscillogram or both), which can be split in several rows.
#'
#' Web pages include buttons to slow down playback (see [ds_player()]), and clicking on the spectrogram plays the sound from that point. Figures and sounds are embedded in the HTML code, so web pages can be shared as single files or pasted into any web page. Note that embedding sounds makes web pages larger (about 1.4 times the size of the sound file), so cut or downsample long recordings before creating web pages.
#'
#' Settings not supplied in layers take the defaults of each style (e.g. paged spectrograms use the 'inferno' color palette unless [ds_spectrogram()] sets another one).
#' @seealso [save_dynaspec()], [ds_spectrogram()], [ds_scrolling()]
#' @export
#' @name dynaspec
#' @examples
#' data(scythebill)
#'
#' # empty panel with time axis (only the panels added as layers are shown)
#' dynaspec(scythebill)
#'
#' # static spectrogram with oscillogram (printing shows it in the viewer or browser)
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
#'   ds_oscillogram()
#'
#' # save as a web page
#' save_dynaspec(x, file.path(tempdir(), "scythebill.html"), play = FALSE)
#'
#' \dontrun{
#' # show it
#' x
#'
#' # paged style
#' x + ds_paged(t.display = 1)
#'
#' # scrolling style (a longer recording)
#' data(antshrike)
#' y <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))
#' y + ds_scrolling(t.display = 2)
#'
#' # video (requires ffmpeg)
#' save_dynaspec(y + ds_scrolling(t.display = 2), "antshrike.mp4")
#'
#' # highlight and label parts of the song
#' st <- data.frame(start = c(0.34, 1.13, 1.44, 1.75), end = c(1.03, 1.36, 1.68, 2.28),
#'                  labels = c("A", "B", "B", "C"))
#' x + ds_annotations(st, labels = "labels") +
#'   ds_captions("Brown-billed scythebill", start = 0, end = 3)
#'
#' # only the oscillogram
#' dynaspec(scythebill) + ds_oscillogram()
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#' @references
#' Araya-Salas M & Wilkins M R. (2020). *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.

dynaspec <- function(wave, flim = NULL, tlim = NULL) {
  check_results <- checkmate::makeAssertCollection()
  if (!isTRUE(res <- check_wave_int(wave, video = TRUE)))
    check_results$push(paste0("'wave': ", res))
  if (!is.null(flim))
    checkmate::assert_numeric(flim, len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_results, .var.name = "flim")
  if (!is.null(tlim))
    checkmate::assert_numeric(tlim, min.len = 1, max.len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_results, .var.name = "tlim")
  .report_assertions(check_results)

  # videos: the sound is extracted (and the video is kept for ds_video())
  video <- NULL
  if (is.character(wave) && is_video_int(wave)) {
    video <- normalizePath(wave)
    name <- tools::file_path_sans_ext(basename(wave))
    wave <- video_sound_int(video)
  }

  # frequencies above the Nyquist frequency (half the sampling rate) are not in the sound ('Inf' asks for all of them)
  if (!is.null(flim) && is.finite(flim[2])) {
    nyquist <- samp_rate_int(wave) / 2000
    if (isTRUE(flim[2] > nyquist))
      warning2("the upper limit of 'flim' (", flim[2], " kHz) is higher than the Nyquist frequency of the sound (", round(nyquist, 2),
               " kHz, half its sampling rate); spectrograms are shown up to ", round(nyquist, 2), " kHz")
  }

  # name for output files: sound or video file name, or name of the Wave object
  if (is.null(video))
    name <- if (is.character(wave)) tools::file_path_sans_ext(basename(wave)) else make.names(paste(deparse(substitute(wave)), collapse = ""))

  x <- new_dynaspec_int(wave = wave, name = name, flim = flim, tlim = if (length(tlim) == 1) c(0, tlim) else tlim)
  x$video.file <- video
  x
}


#' Add layers to dynamic spectrograms
#'
#' @param e1 A [dynaspec()] object.
#' @param e2 A layer (e.g. [ds_spectrogram()], [ds_annotations()] or [ds_scrolling()]).
#' @return A [dynaspec()] object. Spectrogram, oscillogram, envelope, spectrum, overview, player and style layers replace previous ones of the same kind (arguments of spectrogram, theme and style layers are combined); annotations and captions are added.
#' @export
`+.dynaspec` <- function(e1, e2) {
  if (missing(e2) || !inherits(e2, "dynaspec_layer"))
    stop2("only dynaspec layers (e.g. ds_spectrogram(), ds_annotations() or ds_scrolling()) can be added to dynaspec objects")
  if (!inherits(e1, "dynaspec"))
    stop2("layers must be added to a dynaspec object (see dynaspec())")

  # comparisons: layers are added to all sounds
  if (inherits(e1, "dynaspec_compare")) {
    e1$sounds <- lapply(e1$sounds, function(s) s + e2)
    e1$style <- e1$sounds[[1]]$style
    return(e1)
  }

  switch(e2$type,
    spectrogram = e1$spectrogram <- utils::modifyList(if (is.null(e1$spectrogram)) list() else e1$spectrogram, e2$args),
    oscillogram = ,
    envelope = e1$panels <- c(Filter(function(p) p$type != e2$args$type, e1$panels), list(e2$args)),
    spectrum = e1$spectrum <- e2$args,
    overview = e1$overview <- e2$args,
    # (preset themes replace previous themes)
    theme = {
      e1$theme <- if (isTRUE(e2$complete)) e2$args else utils::modifyList(if (is.null(e1$theme)) list() else e1$theme, e2$args)
      # (complete themes also replace the theme set with ds_set_theme())
      if (isTRUE(e2$complete)) e1$theme_complete <- TRUE
    },
    player = e1$player <- e2$args,
    video = e1$video.layer <- e2$args,
    filter = e1$filter <- e2$args,
    annotations = e1$selections <- c(e1$selections, list(e2$args)),
    captions = e1$captions <- c(e1$captions, list(e2$args)),
    style = e1$style <- if (identical(e1$style$type, e2$style)) list(type = e2$style, args = utils::modifyList(e1$style$args, e2$args)) else list(type = e2$style, args = e2$args)
  )
  e1
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


# dynaspec object (without checks; see dynaspec())
new_dynaspec_int <- function(wave, name, flim = NULL, tlim = NULL) {
  structure(
    list(wave = wave, name = name, flim = flim, tlim = tlim,
         spectrogram = NULL, panels = list(), spectrum = NULL, overview = NULL, theme = list(), selections = list(), captions = list(), player = list(),
         style = list(type = "static", args = list())),
    class = "dynaspec"
  )
}


# layer and style objects
layer_int <- function(type, args) structure(list(type = type, args = args), class = "dynaspec_layer")


style_int <- function(style, args) structure(list(type = "style", style = style, args = args), class = "dynaspec_layer")


# arguments supplied in a call (evaluated), so layers only set the arguments supplied by users
provided_args_int <- function(call, env) {
  args <- as.list(call)[-1]
  lapply(args, eval, envir = env)
}
