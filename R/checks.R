# argument checks

# arguments set to NULL are not checked
.check_arguments <- function(fun, args) {
  # make function name a character
  fun <- as.character(fun)[1]

  # environment so `$` only matches exact names (lists do partial matching, e.g. args$res -> resampleRate)
  args <- list2env(args)

  # create object to store check results
  check_collection <- checkmate::makeAssertCollection()

  # add a custom message
  push <- function(name, msg) check_collection$push(sprintf("'%s': %s", name, msg))

  # check that all values are valid colors
  check_color <- function(x, name, len = NULL) {
    if (is.null(x)) return(invisible(NULL))
    if (!is.null(len) && length(x) != len)
      return(push(name, sprintf("Must have length %i, but has length %i", len, length(x))))
    valid <- vapply(x, function(y) !inherits(try(grDevices::col2rgb(y), silent = TRUE), "try-error"), logical(1))
    if (length(x) == 0 || !all(valid))
      push(name, "Must be a valid color (see grDevices::colors())")
  }

  # strictly positive single number
  check_positive <- function(x, name) {
    if (is.null(x)) return(invisible(NULL))
    checkmate::assert_number(x, finite = TRUE, add = check_collection, .var.name = name)
    if (checkmate::test_number(x) && x <= 0) push(name, "Must be > 0")
  }

  ### general arguments -------------------------------------------------------

  for (i in c("fix.time", "still.ends", "play", "ggspectro", "lower.spectro", "derivative", "osc", "save.png", "plot.legend", "bg.flood", "delete.temp", "plot", "verbose", "keep.pitch", "spectro", "play.line")) {
    if (!is.null(args[[i]]))
      checkmate::assert_flag(args[[i]], add = check_collection, .var.name = i)
  }

  # (t.display of prep_paged_spectro() can be Inf, checked below)
  for (i in setdiff(c("cex.lab", "hop.size", "speed", "fps", "t.display", "lwd", "amp.trans", "resample.rate", "spec.width", "spec.height"), if (fun == "prep_paged_spectro") "t.display"))
    check_positive(args[[i]], i)

  for (i in c("rows", "res", "width", "height", "parallel", "loop", "col.bins")) {
    if (!is.null(args[[i]]))
      checkmate::assert_count(args[[i]], positive = TRUE, add = check_collection, .var.name = i)
  }

  for (i in c("lcol", "colwave", "colbg", "colaxis", "highlight.col"))
    check_color(args[[i]], i, len = 1)

  for (i in c("dest.path", "png.name", "title")) {
    if (!is.null(args[[i]]))
      checkmate::assert_string(args[[i]], min.chars = 1, add = check_collection, .var.name = i)
  }

  if (!is.null(args$wl)) {
    checkmate::assert_int(args$wl, lower = 2, add = check_collection, .var.name = "wl")
  }

  if (!is.null(args$ovlp)) {
    checkmate::assert_number(args$ovlp, lower = 0, upper = 99, add = check_collection, .var.name = "ovlp")
  }

  ### scrolling_spectro() arguments ------------------------------------------

  # sound input (Wave object or sound file), used by all functions
  for (i in c("wave")) {
    if (!is.null(args[[i]]) && !isTRUE(res <- check_wave_int(args[[i]])))
      push(i, res)
  }

  if (!is.null(args$file.name)) {
    checkmate::assert_string(args$file.name, pattern = "\\.(mp4|html?)$", ignore.case = TRUE, add = check_collection, .var.name = "file.name")
  }

  if (!is.null(args$flim)) {
    checkmate::assert_numeric(args$flim, len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_collection, .var.name = "flim")
  }

  # palette function (prep_paged_spectro() also takes viridis palette names and two or more colors for a gradient)
  if (!is.null(args$pal)) {
    viridis_pals <- c("viridis", "magma", "plasma", "inferno", "cividis", "rocket", "mako", "turbo")
    if (fun == "prep_paged_spectro" && !is.function(args$pal)) {
      if (length(args$pal) == 1)
        checkmate::assert_choice(args$pal, choices = viridis_pals, add = check_collection, .var.name = "pal") else
          check_color(args$pal, "pal")
    } else
      checkmate::assert_function(args$pal, add = check_collection, .var.name = "pal")
  }

  if (!is.null(args$buffer)) {
    checkmate::assert_number(args$buffer, lower = 0, finite = TRUE, add = check_collection, .var.name = "buffer")
  }

  if (!is.null(args$axis.type)) {
    checkmate::assert_choice(args$axis.type, choices = if (fun == "prep_paged_spectro") c("standard", "inside", "compact", "none") else c("standard", "minimal", "inside", "compact", "none"), add = check_collection, .var.name = "axis.type")
  }

  if (!is.null(args$lty)) {
    lty_names <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    if (length(args$lty) != 1 || !(args$lty %in% c(0:6, lty_names)))
      push("lty", paste0("Must be an integer between 0 and 6 or one of {'", paste(lty_names, collapse = "','"), "'}"))
  }

  if (!is.null(args$height.prop)) {
    checkmate::assert_numeric(args$height.prop, len = 2, lower = 0, finite = TRUE, any.missing = FALSE, add = check_collection, .var.name = "height.prop")
    if (fun == "scrolling_spectro" && checkmate::test_numeric(args$height.prop, len = 2, any.missing = FALSE) && args$height.prop[1] <= args$height.prop[2])
      push("height.prop", "First value (scrolling spectrogram) must be larger than the second one (lower panel)")
  }

  if (!is.null(args$spectro.call)) {
    checkmate::assert_class(args$spectro.call, "call", add = check_collection, .var.name = "spectro.call")
  }

  if (!is.null(args$annotation.call)) {
    checkmate::assert_class(args$annotation.call, "call", add = check_collection, .var.name = "annotation.call")

    if (is.call(args$annotation.call)) {
      ann <- as.list(args$annotation.call)
      missing_args <- setdiff(c("labels", "start", "end"), names(ann))
      if (length(missing_args) > 0)
        push("annotation.call", paste0("Must include argument(s) ", paste0("'", missing_args, "'", collapse = ", ")))
      else if (length(eval(ann$start)) != length(eval(ann$labels)) || length(eval(ann$end)) != length(eval(ann$labels)))
        push("annotation.call", "'start' and 'end' must have one value per label")
    }
  }

  # wave must be longer than t.display (only checked for Wave objects; files are checked once read)
  if (fun == "scrolling_spectro" && checkmate::test_class(args$wave, "Wave") && checkmate::test_number(args$t.display) && seewave::duration(args$wave) <= args$t.display)
    push("t.display", "Must be shorter than the duration of 'wave'")

  ### static_spectro() arguments ------------------------------------------------

  if (!is.null(args$html.file.name)) {
    checkmate::assert_string(args$html.file.name, pattern = if (fun == "static_spectro") "\\.(html?|mp4|png)$" else "\\.html?$", ignore.case = TRUE, add = check_collection, .var.name = "file.name")
  }

  if (!is.null(args$speeds)) {
    checkmate::assert_numeric(args$speeds, lower = 0.0625, upper = 16, min.len = 1, any.missing = FALSE, unique = TRUE, add = check_collection, .var.name = "speeds")
  }

  if (!is.null(args$buttons)) {
    checkmate::assert_character(args$buttons, any.missing = FALSE, unique = TRUE, add = check_collection, .var.name = "buttons")
    checkmate::assert_subset(args$buttons, c("time", "loop", "volume", "download", "fullscreen", "progress", "zoom"), add = check_collection, .var.name = "buttons")
  }

  if (!is.null(args$collevels)) {
    checkmate::assert_numeric(args$collevels, min.len = 2, finite = TRUE, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_collection, .var.name = "collevels")
  }

  if (!is.null(args$osc.position)) {
    checkmate::assert_choice(args$osc.position, choices = c("below", "above"), add = check_collection, .var.name = "osc.position")
  }

  if (!is.null(args$alt)) {
    checkmate::assert_string(args$alt, add = check_collection, .var.name = "alt")
  }

  ### prep_paged_spectro() arguments --------------------------------------

  if (!is.null(args$crop) && !isFALSE(args$crop)) {
    checkmate::assert_numeric(args$crop, min.len = 1, max.len = 2, lower = 0, finite = TRUE, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_collection, .var.name = "crop")
    if (checkmate::test_number(args$crop) && args$crop == 0)
      push("crop", "Must be > 0 (or FALSE to keep the whole file)")
  }

  if (!is.null(args$filter)) {
    checkmate::assert_numeric(args$filter, len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_collection, .var.name = "filter")
  }

  if (fun == "prep_paged_spectro" && !is.null(args$t.display)) {
    checkmate::assert_numeric(args$t.display, min.len = 1, max.len = 2, lower = 0, any.missing = FALSE, add = check_collection, .var.name = "t.display")
    if (checkmate::test_numeric(args$t.display, any.missing = FALSE) && utils::tail(args$t.display, 1) <= 0)
      push("t.display", "Must be > 0")
  }

  if (!is.null(args$min.db)) {
    checkmate::assert_number(args$min.db, finite = TRUE, add = check_collection, .var.name = "min.db")
    if (checkmate::test_number(args$min.db) && args$min.db >= 0) push("min.db", "Must be < 0")
  }

  if (!is.null(args$wn)) {
    checkmate::assert_choice(args$wn, choices = c("bartlett", "blackman", "flattop", "hamming", "hanning", "rectangle", "gaussian"), add = check_collection, .var.name = "wn")
  }

  if (!is.null(args$amp.thresh)) {
    checkmate::assert_number(args$amp.thresh, lower = 0, upper = 100, add = check_collection, .var.name = "amp.thresh")
  }

  ### paged_spectro() arguments ----------------------------------------------

  if (!is.null(args$specParams)) {
    spec_names <- c("destFolder", "outFilename", "xLim", "yLim", "bg", "spec", "newWav", "segWavs", "specWidth", "specHeight", "onlyPlotSpec", "bgFlood", "autoFontCol", "fontAndAxisCol")
    if (!checkmate::test_list(args$specParams) || !checkmate::test_names(names(args$specParams), must.include = spec_names))
      push("specParams", "Must be the output of prep_paged_spectro()")
  }

  if (!is.null(args$highlight.alpha)) {
    checkmate::assert_number(args$highlight.alpha, lower = 0, upper = 1, add = check_collection, .var.name = "highlight.alpha")
  }

  return(check_collection)
}


# custom reporting of errors
.report_assertions <- function(collection) {
  checkmate::assert_class(collection, "AssertCollection")
  if (!collection$isEmpty()) {
    msgs <- collection$getMessages()

    msgs <- gsub("^Variable ", "", msgs)

    context <- "%i input argument(s) failed:"
    err <- c(sprintf(context, length(msgs)), unlist(lapply(msgs, strwrap, initial = " * ", prefix = "   ")))
    stop(simpleError(paste0(err, collapse = "\n"), call = sys.call(1L)))
  }
  invisible(TRUE)
}


# check the 'wave' argument (used by .check_arguments())
check_wave_int <- function(x, video = FALSE) {
  if (inherits(x, "Wave"))
    return(TRUE)
  if (!checkmate::test_string(x, min.chars = 1))
    return(if (video) "Must be a 'Wave' object or the path to a '.wav' or '.mp3' file, or to a video file" else "Must be a 'Wave' object or the path to a '.wav' or '.mp3' file")
  if (video && is_video_int(x)) {
    if (!file.exists(x)) return(sprintf("File '%s' does not exist", x))
    if (!have_ffmpeg_exec2()) return("'ffmpeg' could not be found; it must be installed (and available in the system path) to read videos (see https://github.com/maRce10/dynaSpec#ffmpeg)")
    return(TRUE)
  }
  if (!checkmate::test_file_exists(x, extension = c("wav", "mp3")))
    return(sprintf(if (video) "File '%s' does not exist or is not a '.wav', '.mp3' or video file" else "File '%s' does not exist or is not a '.wav' or '.mp3' file", x))
  TRUE
}
