# save_dynaspec(): web pages, videos and images of dynaspec objects

#' Save dynamic spectrograms
#'
#' `save_dynaspec()` saves a [dynaspec()] object as an interactive web page, a video or still images.
#' @param x A [dynaspec()] or [ds_compare()] object.
#' @param file.name Character string with the name of the file. The extension sets the output:
#' - '.html': interactive web page.
#' - '.mp4': video (requires 'ffmpeg').
#' - '.png': still image of static spectrograms, or one image per page of paged spectrograms (named after 'file.name', followed by the page number).
#'
#' Default is `NULL`, which returns the HTML code of the web page without saving it.
#' @param play Logical argument to control if the file is opened after it is saved (or the web page is shown in the viewer when 'file.name' is `NULL`, in interactive sessions). Default is `TRUE`.
#' @param verbose Logical argument to control if progress messages are printed. Default is `TRUE`.
#' @return Invisibly returns the path to the file, or the HTML code of the web page (a character string of class 'dynaSpec_html') when 'file.name' is `NULL` or ends in '.html'. When knitting R Markdown or Quarto documents the web page is embedded in the document.
#' @details Videos of scrolling spectrograms render every frame, which takes time (it can be sped up with the 'parallel' and 'fps' arguments of [ds_scrolling()]). Videos of paged and static spectrograms and all web pages are created in seconds.
#' @seealso [dynaspec()]
#' @export
#' @examples
#' data(scythebill)
#'
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) + ds_oscillogram()
#'
#' save_dynaspec(x, file.path(tempdir(), "scythebill.html"), play = FALSE)
#' save_dynaspec(x, file.path(tempdir(), "scythebill.png"), play = FALSE)
#'
#' \dontrun{
#' # videos (require ffmpeg)
#' save_dynaspec(x, file.path(tempdir(), "scythebill.mp4"))
#' data(antshrike)
#' y <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))
#' save_dynaspec(y + ds_scrolling(t.display = 2), file.path(tempdir(), "antshrike_scrolling.mp4"))
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

  # web pages without file are written to a temporary file
  # (absolute paths: some engines write temporary files in other folders, e.g. lists of video segments for ffmpeg)
  out <- if (is.null(file.name)) tempfile(fileext = ".html") else file.path(normalizePath(dirname(file.name), mustWork = TRUE), basename(file.name))
  play_out <- play && !is.null(file.name)

  res <- if (inherits(x, "dynaspec_compare")) save_compare_int(x, out, ext, play_out, verbose) else save_sound_int(x, out, ext, play_out, verbose)

  # files saved
  if (verbose && !is.null(file.name) && !isFALSE(getOption("dynaSpec.report"))) {
    files <- if (ext == "png" && is.character(res) && !inherits(res, "dynaSpec_html")) res else out
    message_dyn_int(if (length(files) > 1) "Files saved at: " else "File saved at: ", paste(files, collapse = ", "), type = "success")
  }

  # show web pages without file in the viewer
  if (is.null(file.name) && play && interactive() && !isTRUE(getOption("knitr.in.progress")) && inherits(res, "dynaSpec_html"))
    view_html_int(res)

  invisible(res)
}


# save a dynaspec object of a single sound
save_sound_int <- function(x, out, ext, play, verbose) {
  # settings of the theme not supplied in other layers (colors and font family)
  x <- theme_layers_int(global_theme_int(x))

  # bandpass filter (ds_filter()): the whole sound, before contours are measured and the sound is cut
  if (!is.null(x$filter)) x$wave <- filter_wave_int(read_sound_int(x$wave)$wave, x$filter$bp)

  # sound (cropped if requested; times of layers are shifted accordingly) and layers
  wave <- x$wave
  layers <- list(selections = lapply(lapply(x$selections, sound_file_selections_int, x = x), measure_contours_int, x = x), captions = x$captions)
  if (!is.null(x$tlim)) {
    w <- read_sound_int(wave)$wave
    wave <- seewave::cutw(w, from = x$tlim[1], to = min(x$tlim[2], seewave::duration(w)), output = "Wave")
    layers <- shift_layers_int(layers, -x$tlim[1])
  }

  type <- x$style$type
  if (ext == "png" && type == "scrolling")
    stop2("still images ('.png') are only available for static and paged spectrograms")
  if (type %in% c("scrolling", "paged") && is.null(x$spectrogram))
    stop2(type, " spectrograms need a spectrogram: add ds_spectrogram() (e.g. dynaspec(wave) + ds_spectrogram() + ", type, "())")

  # comparisons: figures can show silence after the sound (static ones up to the duration of the longest sound with the
  # same time scale, paged ones up to the end of the last page), but web pages embed the sound itself, so the next sound
  # starts when it ends
  audio <- NULL
  if (isTRUE(x$compare) && type != "scrolling") audio <- wave
  if (!is.null(x$pad.to) && type == "static")
    wave <- pad_wave_int(read_sound_int(wave)$wave, x$pad.to)

  render <- function(out, play) switch(type,
    static = save_static_int(x, wave, layers, out, ext, play, audio = audio),
    scrolling = save_scrolling_int(x, wave, layers, out, play, verbose),
    paged = save_paged_int(x, wave, layers, out, ext, play, verbose, audio = audio)
  )

  # videos (ds_video()): rendered with the spectrogram and then combined
  if (!is.null(x$video.layer)) {
    if (is.null(x$video.file))
      stop2("ds_video() needs a video: use the path to a video file in dynaspec() (e.g. dynaspec(\"recording.mp4\"))")
    if (ext == "png")
      warning2("videos are not shown in still images ('.png')")
    else {
      # (the figure goes right below a video above it)
      if (!identical(x$video.layer$position, "below")) x$theme$top.margin <- FALSE
      # (ds_video(height): the figure is as high as the video at its width, divided by 'height')
      if (!is.null(x$video.layer$height)) x <- video_figure_size_int(x)
      dur <- seewave::duration(if (is.null(audio)) wave else read_sound_int(audio)$wave)
      st <- x$style$args
      if (ext == "mp4") {
        spec_file <- tempfile(fileext = ".mp4")
        on.exit(unlink(spec_file), add = TRUE)
        render(spec_file, FALSE)
        timing <- if (type == "scrolling") list(speed = if (is.null(st$speed)) 1 else st$speed, buffer = if (is.null(st$buffer)) 0 else st$buffer,
                                                loop = if (is.null(st$loop)) 1 else st$loop) else list(speed = 1, buffer = 0, loop = 1)
        video_stack_int(x, spec_file, out, dur, timing)
        if (play) open_file_int(normalizePath(out))
        return(invisible(out))
      }
      # (web pages: the code of the player is completed with the video before it is saved or embedded)
      op <- options(knitr.in.progress = FALSE)
      html <- tryCatch(render(tempfile(fileext = ".html"), FALSE), finally = options(op))
      html <- video_html_int(html, x, dur, offset = if (type == "scrolling" && !is.null(st$buffer)) st$buffer else 0)
      return(output_html_int(html, file.name = out, play = play))
    }
  }

  render(out, play)
}
