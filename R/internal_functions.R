# internal functions not to be called by users
# stop function that doesn't print call
stop2 <- function (...)
{
  stop(..., call. = FALSE)
}

# stop function that doesn't print call
warning2 <- function (...)
{
  warning(..., call. = FALSE)
}

# functions adopted from ari to check if ffmpeg is installed
ffmpeg_exec2 <- function(quote = FALSE) {
  candidates <- c(Sys.getenv("ffmpeg"), Sys.which("ffmpeg"))
  ffmpeg <- candidates[nchar(candidates) > 0][1]
  if (is.na(ffmpeg)) {
    stop("Could not find ffmpeg")
  }
  if (!ffmpeg %in% c("ffmpeg", "ffmpeg.exe")) {
    ffmpeg = normalizePath(ffmpeg, winslash = "/")
  }
  if (quote) {
    ffmpeg = shQuote(ffmpeg)
  }
  return(ffmpeg)
}

have_ffmpeg_exec2 <- function(){
  exec = try({
    ffmpeg_exec2()
  }, silent = TRUE)
  !inherits(exec, "try-error")
}

# run ffmpeg with the supplied arguments and stop with an informative error if it fails
run_ffmpeg_int <- function(args, step) {
  out <- suppressWarnings(system2(ffmpeg_exec2(), args = paste("-hide_banner -loglevel error", args), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0)
    stop2("ffmpeg failed when ", step, " (exit status ", status, "):\n", paste(utils::tail(out, 5), collapse = "\n"))
  invisible(out)
}

# open a file with the default application of the operating system
open_file_int <- function(file) {
  if (.Platform$OS.type == "windows") {
    shell.exec(file)
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    system2("open", shQuote(file), wait = FALSE)
  } else {
    utils::browseURL(file)
  }
}

# black or white, whichever contrasts best with the background color
contrast_col_int <- function(bg) {
  bgRGB <- grDevices::col2rgb(bg)
  if (bgRGB["red", 1] * 0.299 + bgRGB["green", 1] * 0.587 + bgRGB["blue", 1] * 0.114 > 149)
    "#000000" else "#ffffff"
}

## function to check arguments (as in suwo)
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

  for (i in c("fix.time", "pb", "play", "ggspectro", "lower.spectro", "derivative", "osc", "savePNG", "plotLegend", "onlyPlotSpec", "bgFlood", "delete_temp_files", "plot", "verbose")) {
    if (!is.null(args[[i]]))
      checkmate::assert_flag(args[[i]], add = check_collection, .var.name = i)
  }

  for (i in c("hop.size", "speed", "fps", "t.display", "lwd", "ampTrans", "resampleRate", "specWidth", "specHeight"))
    check_positive(args[[i]], i)

  for (i in c("res", "width", "height", "parallel", "loop", "framerate", "colbins")) {
    if (!is.null(args[[i]]))
      checkmate::assert_count(args[[i]], positive = TRUE, add = check_collection, .var.name = i)
  }

  for (i in c("lcol", "colwave", "colbg", "bg", "fontAndAxisCol", "highlightCol", "cursorCol"))
    check_color(args[[i]], i, len = 1)

  for (i in c("destFolder", "outFilename", "vidName", "title")) {
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

  if (!is.null(args$wave)) {
    checkmate::assert_class(args$wave, "Wave", add = check_collection, .var.name = "wave")
  }

  if (!is.null(args$file.name)) {
    checkmate::assert_string(args$file.name, pattern = "\\.mp4$", ignore.case = TRUE, add = check_collection, .var.name = "file.name")
  }

  if (!is.null(args$flim)) {
    checkmate::assert_numeric(args$flim, len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_collection, .var.name = "flim")
  }

  if (!is.null(args$pal)) {
    checkmate::assert_function(args$pal, add = check_collection, .var.name = "pal")
  }

  if (!is.null(args$buffer)) {
    checkmate::assert_number(args$buffer, lower = 0, finite = TRUE, add = check_collection, .var.name = "buffer")
  }

  if (!is.null(args$axis.type)) {
    checkmate::assert_choice(args$axis.type, choices = c("standard", "minimal", "none"), add = check_collection, .var.name = "axis.type")
  }

  if (!is.null(args$lty)) {
    lty_names <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    if (length(args$lty) != 1 || !(args$lty %in% c(0:6, lty_names)))
      push("lty", paste0("Must be an integer between 0 and 6 or one of {'", paste(lty_names, collapse = "','"), "'}"))
  }

  if (!is.null(args$height.prop)) {
    checkmate::assert_numeric(args$height.prop, len = 2, lower = 0, finite = TRUE, any.missing = FALSE, add = check_collection, .var.name = "height.prop")
    if (checkmate::test_numeric(args$height.prop, len = 2, any.missing = FALSE) && args$height.prop[1] <= args$height.prop[2])
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

  # wave must be longer than t.display
  if (fun == "scrolling_spectro" && checkmate::test_class(args$wave, "Wave") && checkmate::test_number(args$t.display) && seewave::duration(args$wave) <= args$t.display)
    push("t.display", "Must be shorter than the duration of 'wave'")

  ### prep_static_ggspectro() arguments --------------------------------------

  if (!is.null(args$soundFile)) {
    checkmate::assert_string(args$soundFile, min.chars = 1, add = check_collection, .var.name = "soundFile")
    if (checkmate::test_string(args$soundFile) && !is.url(args$soundFile))
      checkmate::assert_file_exists(args$soundFile, extension = c("wav", "mp3"), add = check_collection, .var.name = "soundFile")
  }

  if (!is.null(args$colPal)) {
    viridis_pals <- c("viridis", "magma", "plasma", "inferno", "cividis", "rocket", "mako", "turbo")
    if (length(args$colPal) == 1)
      checkmate::assert_choice(args$colPal, choices = viridis_pals, add = check_collection, .var.name = "colPal") else
        check_color(args$colPal, "colPal", len = 2)
  }

  if (!is.null(args$crop) && !isFALSE(args$crop)) {
    checkmate::assert_numeric(args$crop, min.len = 1, max.len = 2, lower = 0, finite = TRUE, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_collection, .var.name = "crop")
    if (checkmate::test_number(args$crop) && args$crop == 0)
      push("crop", "Must be > 0 (or FALSE to keep the whole file)")
  }

  if (!is.null(args$filter)) {
    checkmate::assert_numeric(args$filter, len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_collection, .var.name = "filter")
  }

  if (!is.null(args$xLim)) {
    checkmate::assert_numeric(args$xLim, min.len = 1, max.len = 2, lower = 0, any.missing = FALSE, add = check_collection, .var.name = "xLim")
    if (checkmate::test_numeric(args$xLim, any.missing = FALSE) && utils::tail(args$xLim, 1) <= 0)
      push("xLim", "Upper limit must be > 0")
  }

  if (!is.null(args$yLim)) {
    checkmate::assert_numeric(args$yLim, min.len = 1, max.len = 2, lower = 0, finite = TRUE, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_collection, .var.name = "yLim")
    if (checkmate::test_number(args$yLim) && args$yLim == 0)
      push("yLim", "Upper limit must be > 0")
  }

  if (!is.null(args$min_dB)) {
    checkmate::assert_number(args$min_dB, finite = TRUE, add = check_collection, .var.name = "min_dB")
    if (checkmate::test_number(args$min_dB) && args$min_dB >= 0) push("min_dB", "Must be < 0")
  }

  if (!is.null(args$wn)) {
    checkmate::assert_choice(args$wn, choices = c("bartlett", "blackman", "flattop", "hamming", "hanning", "rectangle", "gaussian"), add = check_collection, .var.name = "wn")
  }

  if (!is.null(args$ampThresh)) {
    checkmate::assert_number(args$ampThresh, lower = 0, upper = 100, add = check_collection, .var.name = "ampThresh")
  }

  ### paged_spectro() arguments ----------------------------------------------

  if (!is.null(args$specParams)) {
    spec_names <- c("destFolder", "outFilename", "xLim", "yLim", "bg", "spec", "newWav", "segWavs", "specWidth", "specHeight", "onlyPlotSpec", "bgFlood", "autoFontCol", "fontAndAxisCol")
    if (!checkmate::test_list(args$specParams) || !checkmate::test_names(names(args$specParams), must.include = spec_names))
      push("specParams", "Must be the output of prep_static_ggspectro()")
  }

  if (!is.null(args$highlightAlpha)) {
    checkmate::assert_number(args$highlightAlpha, lower = 0, upper = 1, add = check_collection, .var.name = "highlightAlpha")
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
