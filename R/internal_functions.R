# internal functions not to be called by users

# apply FUN to X with an optional progress bar (cli) and parallel computing (adapted from suwo). X is a sequence
# of indices
.pbapply_dyn <- function(X, FUN, cl = 1, pbar = TRUE, title = "", ...) {
  FUN <- match.fun(FUN)
  cl <- as.integer(cl)

  if (identical(Sys.getenv("TESTTHAT"), "true"))
    cl <- 1L

  # no parallel
  if (cl == 1) {
    if (!pbar)
      return(lapply(X, FUN, ...))
    return(lapply(X = cli::cli_progress_along(name = title, x = X, clear = FALSE), FUN = function(i) FUN(X[i], ...)))
  }

  # parallel (in chunks of 'cl' elements, so the progress bar is updated after each chunk)
  is_windows <- .Platform$OS.type == "windows"
  if (is_windows) {
    clust <- parallel::makePSOCKcluster(cl)
    on.exit(parallel::stopCluster(clust), add = TRUE)
  }
  run_chunk <- function(idx) {
    if (is_windows) parallel::parLapply(clust, X[idx], FUN, ...) else parallel::mclapply(X[idx], FUN, ..., mc.cores = cl)
  }
  chunks <- split(seq_along(X), ceiling(seq_along(X) / cl))

  out <- vector("list", length(X))
  steps <- if (pbar) cli::cli_progress_along(name = title, x = chunks, clear = FALSE) else seq_along(chunks)
  for (i in steps)
    out[chunks[[i]]] <- run_chunk(chunks[[i]])
  out
}
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

  for (i in c("fix.time", "play", "ggspectro", "lower.spectro", "derivative", "osc", "save.png", "plot.legend", "bg.flood", "delete.temp", "plot", "verbose", "keep.pitch", "spectro", "play.line")) {
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

  # palette function (prep_paged_spectro() also takes viridis palette names and 2 colors for a gradient)
  if (!is.null(args$pal)) {
    viridis_pals <- c("viridis", "magma", "plasma", "inferno", "cividis", "rocket", "mako", "turbo")
    if (fun == "prep_paged_spectro" && !is.function(args$pal)) {
      if (length(args$pal) == 1)
        checkmate::assert_choice(args$pal, choices = viridis_pals, add = check_collection, .var.name = "pal") else
          check_color(args$pal, "pal", len = 2)
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

### sound input ('wave' argument of all functions) ------------------------------------------------------

# read a sound: a 'Wave' object or a path to a .wav or .mp3 file. Returns the Wave object ('wave'), the file it
# was read from ('file', NULL for Wave objects) and a name to use for output files ('name')
read_sound_int <- function(x, name = NULL, verbose = FALSE) {
  if (inherits(x, "Wave"))
    return(list(wave = x, file = NULL, name = if (is.null(name)) "sound" else name))

  ext <- tolower(tools::file_ext(x))
  if (verbose && ext == "mp3") message("Converting mp3 to wav")

  list(
    wave = if (ext == "mp3") tuneR::readMP3(x) else tuneR::readWave(x),
    file = x,
    name = tools::file_path_sans_ext(basename(x))
  )
}

# check the 'wave' argument (used by .check_arguments())
check_wave_int <- function(x) {
  if (inherits(x, "Wave"))
    return(TRUE)
  if (!checkmate::test_string(x, min.chars = 1))
    return("Must be a 'Wave' object or the path to a '.wav' or '.mp3' file")
  if (!checkmate::test_file_exists(x, extension = c("wav", "mp3")))
    return(sprintf("File '%s' does not exist or is not a '.wav' or '.mp3' file", x))
  TRUE
}

### HTML players (static_spectro(), and web pages of scrolling_spectro() and paged_spectro()) ---

# sound to embed: wave object (for plotting) and data uri
# sound files are embedded as they are; Wave objects are written as wav
sound_data_int <- function(x, read = TRUE) {
  if (inherits(x, "Wave")) {
    wav_file <- tempfile(fileext = ".wav")
    on.exit(unlink(wav_file), add = TRUE)
    # wav files must be integer (normalize non-integer waves)
    w <- if (x@pcm) x else tuneR::normalize(x, unit = "16")
    tuneR::writeWave(w, filename = wav_file)
    return(list(wave = x, uri = base64_uri_int(wav_file, "audio/wav")))
  }

  ext <- tolower(tools::file_ext(x))
  list(
    wave = if (read) read_sound_int(x)$wave,
    uri = base64_uri_int(x, if (ext == "mp3") "audio/mpeg" else "audio/wav")
  )
}

# oscillogram downsampled to min/max amplitude per pixel column
oscillo_player_int <- function(wave, xlim, colwave, n, axisX = FALSE, cex.lab = 1, offset = 0, max.amp = max(abs(wave@left), 1e-12), empty = FALSE, theme = list()) {
  amp <- wave@left / max.amp
  tm <- offset + (seq_along(amp) - 1) / wave@samp.rate

  # small vertical margin so peaks do not touch the box
  panel_frame_int(xlim, c(-1.05, 1.05), theme)
  on.exit(theme_box_int(theme), add = TRUE)
  if (!empty && !isTRUE(theme$hide))
    graphics::mtext("Amp.", side = 2, line = 0.5, cex = cex.lab, las = 1, xpd = NA, font = theme_int(theme, "font.lab", 1), col = theme_int(theme, "col.lab", "black"))

  if (axisX)
    do.call(time_axis_int, c(list(to_x = identity, xlim = xlim, cex.lab = cex.lab, inside = identical(theme$axis.type, "inside")), theme[intersect(names(theme), c("xlab", "font.lab", "col.lab", "cex.axis"))]))

  # empty panel (time axis only)
  if (empty)
    return(invisible(NULL))

  if (length(amp) > 4 * n) {
    bins <- cut(tm, breaks = n, labels = FALSE)
    mins <- vapply(split(amp, bins), min, numeric(1))
    maxs <- vapply(split(amp, bins), max, numeric(1))
    x <- vapply(split(tm, bins), mean, numeric(1))
    graphics::segments(x0 = x, y0 = mins, y1 = maxs, col = colwave)
  } else
    graphics::lines(tm, amp, col = colwave)
}

# theme setting (supplied in dynatheme()) or its default
theme_int <- function(theme, name, default) if (is.null(theme[[name]])) default else theme[[name]]

# check the settings of dynatheme() (only those supplied)
check_theme_int <- function(args) {
  check_results <- .check_arguments("dynatheme", args[intersect(names(args), c("cex.lab", "axis.type", "bg"))])
  for (i in intersect(names(args), c("xlab", "ylab", "family")))
    checkmate::assert_string(args[[i]], add = check_results, .var.name = i)
  for (i in intersect(names(args), c("cex.axis", "cex.title")))
    checkmate::assert_number(args[[i]], lower = 0.01, add = check_results, .var.name = i)
  if (!is.null(args$tick.length)) checkmate::assert_number(args$tick.length, lower = 0, add = check_results, .var.name = "tick.length")
  if (!is.null(args$font.lab)) checkmate::assert_choice(args$font.lab, 1:4, add = check_results, .var.name = "font.lab")
  if (!is.null(args$las)) checkmate::assert_choice(args$las, 0:1, add = check_results, .var.name = "las")
  if (!is.null(args$lwd.line)) checkmate::assert_number(args$lwd.line, lower = 0.01, add = check_results, .var.name = "lwd.line")
  if (!is.null(args$lty.line)) {
    lty_names <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    if (length(args$lty.line) != 1 || !(args$lty.line %in% c(0:6, lty_names)))
      check_results$push(paste0("'lty.line': Must be an integer between 0 and 6 or one of {'", paste(lty_names, collapse = "','"), "'}"))
  }
  if (!is.null(args$bar.style)) checkmate::assert_choice(args$bar.style, c("standard", "rounded", "square"), add = check_results, .var.name = "bar.style")
  for (i in intersect(names(args), c("box", "grid")))
    checkmate::assert_flag(args[[i]], add = check_results, .var.name = i)
  for (i in intersect(names(args), c("col.lab", "bg", "col.box", "col.grid", "col.panel", "col.title", "colbg", "col.wave", "col.line", "col.highlight", "col.bar", "bg.bar")))
    if (!checkmate::test_string(args[[i]]) || inherits(try(grDevices::col2rgb(args[[i]]), silent = TRUE), "try-error"))
      check_results$push(paste0("'", i, "': Must be a valid color"))
  if (!is.null(args$pal) && !is.function(args$pal) && !checkmate::test_character(args$pal, min.len = 1, max.len = 2))
    check_results$push("'pal': Must be a color palette function, a palette name or two colors (paged spectrograms)")
  .report_assertions(check_results)
  args
}

# preset themes: complete themes (replace previous themes when added) whose settings can be changed in '...'
theme_preset_int <- function(preset, ...) {
  th <- layer_int("theme", utils::modifyList(preset, check_theme_int(list(...))))
  th$complete <- TRUE
  th
}

# CSS font family of web pages for an R font family
css_family_int <- function(family) {
  switch(family,
    sans = "Helvetica, Arial, sans-serif",
    serif = "\"Times New Roman\", Times, serif",
    mono = "\"Courier New\", Courier, monospace",
    paste0("\"", family, "\", sans-serif")
  )
}

# graphical parameters of the theme for base R devices (font family and tick length)
theme_par_int <- function(theme, tick.length = 0.5) {
  graphics::par(family = theme_int(theme, "family", "sans"), tcl = -theme_int(theme, "tick.length", tick.length))
}

# box around the current panel (unless box = FALSE in the theme)
theme_box_int <- function(theme, default = "black") {
  if (!isFALSE(theme$box))
    graphics::box(col = theme_int(theme, "col.box", theme_int(theme, "col.lab", default)))
}

# grid lines at the frequency axis ticks (when grid = TRUE in the theme)
theme_grid_int <- function(theme, at = graphics::axTicks(2)) {
  if (isTRUE(theme$grid))
    graphics::abline(h = at, col = theme_int(theme, "col.grid", "#7F7F7F66"), lty = 3)
}

# empty panel (no axes) with the panel background of the theme
panel_frame_int <- function(xlim, ylim, theme, bg = theme_int(theme, "col.panel", NA)) {
  graphics::plot(NA, xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")
  usr <- graphics::par("usr")
  if (!is.na(bg))
    graphics::rect(usr[1], usr[3], usr[2], usr[4], col = bg, border = NA)
}

# description of the panels of a static spectrogram (alternative text of web pages)
panel_names_int <- function(spectro, panels, spectrum) {
  nms <- c(osc = "oscillogram", env = "amplitude envelope")[vapply(panels, `[[`, character(1), "type")]
  nms <- c(if (spectro) "spectrogram", nms, if (!is.null(spectrum)) "power spectrum")
  out <- if (length(nms) > 1) paste(paste(nms[-length(nms)], collapse = ", "), "and", nms[length(nms)]) else nms
  paste0(toupper(substring(out, 1, 1)), substring(out, 2))
}

# amplitude envelope of a sound (absolute amplitude smoothed with a moving window), and its times
envelope_int <- function(wave, n = 2000) {
  amp <- abs(wave@left)
  # window of ~5 ms (at least 1 sample)
  w <- max(1, round(wave@samp.rate * 0.005))
  # centered moving average (cumulative sums; shorter windows at both ends)
  cs <- c(0, cumsum(amp))
  lo <- pmax(seq_along(amp) - w %/% 2, 1)
  hi <- pmin(seq_along(amp) + w %/% 2, length(amp))
  env <- (cs[hi + 1] - cs[lo]) / (hi - lo + 1)
  # keep at most 'n' points per sound (maximum of each bin)
  if (length(env) > n) {
    bins <- cut(seq_along(env), breaks = n, labels = FALSE)
    tm <- vapply(split(seq_along(env), bins), mean, numeric(1))
    env <- vapply(split(env, bins), max, numeric(1))
  } else tm <- seq_along(env)
  list(time = (tm - 1) / wave@samp.rate, env = env)
}

# amplitude envelope panel (time in s on the x axis)
envelope_player_int <- function(wave, xlim, col, axisX = FALSE, cex.lab = 1, offset = 0, max.env = NULL, theme = list()) {
  e <- envelope_int(wave)
  y <- e$env / if (is.null(max.env)) max(e$env, 1e-12) else max.env
  panel_frame_int(xlim, c(0, 1.05), theme)
  if (!isTRUE(theme$hide))
    graphics::mtext("Env.", side = 2, line = 0.5, cex = cex.lab, las = 1, xpd = NA, font = theme_int(theme, "font.lab", 1), col = theme_int(theme, "col.lab", "black"))
  if (axisX)
    do.call(time_axis_int, c(list(to_x = identity, xlim = xlim, cex.lab = cex.lab, inside = identical(theme$axis.type, "inside")), theme[intersect(names(theme), c("xlab", "font.lab", "col.lab", "cex.axis"))]))
  x <- offset + e$time
  graphics::polygon(c(x[1], x, x[length(x)]), c(0, y, 0), col = grDevices::adjustcolor(col, 0.6), border = col)
  theme_box_int(theme)
}

# mean power spectrum of a sound (frequency in kHz, linear amplitude), restricted to the frequency limits
spectrum_int <- function(wave, wl = 512, flim = NULL) {
  sp <- seewave::meanspec(wave, f = wave@samp.rate, wl = wl, plot = FALSE, norm = FALSE)
  if (!is.null(flim)) sp <- sp[sp[, 1] >= flim[1] & sp[, 1] <= flim[2], , drop = FALSE]
  list(freq = sp[, 1], amp = sp[, 2])
}

# power spectrum panel next to the spectrogram (frequency on the y axis, same range as the spectrogram)
# ('axis': frequency axis on its left side, used when the spectrum is on the left of the spectrogram)
spectrum_player_int <- function(sp, ylim, side = "right", col = "black", max.amp = NULL, label = TRUE, cex.lab = 1, axis = FALSE, theme = list()) {
  x <- sp$amp / if (is.null(max.amp)) max(sp$amp, 1e-12) else max.amp
  col_lab <- theme_int(theme, "col.lab", "black")
  panel_frame_int(if (side == "left") c(1.05, 0) else c(0, 1.05), ylim, theme)
  theme_grid_int(theme, at = pretty(ylim))
  graphics::polygon(c(0, x, 0), c(sp$freq[1], sp$freq, sp$freq[length(sp$freq)]), col = grDevices::adjustcolor(col, 0.6), border = col)
  if (axis && identical(theme$axis.type, "compact"))
    compact_freq_axis_int(ylim, col = col_lab, cex = theme_int(theme, "cex.axis", 1))
  else if (axis) {
    # as the frequency axis of the spectrogram
    graphics::axis(2, las = theme_int(theme, "las", 1), col = col_lab, col.axis = col_lab, cex.axis = theme_int(theme, "cex.axis", 1))
    graphics::title(ylab = theme_int(theme, "ylab", "Frequency (kHz)"), cex.lab = cex.lab, col.lab = col_lab, font.lab = theme_int(theme, "font.lab", 1))
  }
  theme_box_int(theme)
  if (label)
    graphics::mtext("Power", side = 3, line = 0.1, cex = 0.8 * cex.lab * theme_int(theme, "cex.title", 1), col = theme_int(theme, "col.title", col_lab))
}

# spectrogram panel of static spectrograms: computed with seewave::spectro() and drawn in the current panel
# (seewave::spectro() plots set the background of the whole page to 'colbg', covering other panels and the figure
# background). As in seewave, analysis windows are spread evenly from 0 to the duration of the sound. Other
# arguments are passed to seewave::spectro()
spectro_int <- function(wave, f, wl, ovlp, flim, pal, collevels, colbg = "white", dBref = NULL, axisY = TRUE,
                        flab = "Frequency (kHz)", cex.lab = 1, cex.axis = 1, col.lab = "black", theme = list(), ...) {
  sp <- seewave::spectro(wave, f = f, wl = wl, ovlp = ovlp, flim = flim, norm = FALSE, dBref = dBref, plot = FALSE, ...)
  graphics::plot.new()
  graphics::plot.window(xlim = range(sp$time), ylim = range(sp$freq), xaxs = "i", yaxs = "i")
  usr <- graphics::par("usr")
  graphics::rect(usr[1], usr[3], usr[2], usr[4], col = colbg, border = NA)
  graphics::.filled.contour(sp$time, sp$freq, t(sp$amp), levels = collevels, col = pal(length(collevels) - 1))
  theme_grid_int(theme)
  theme_box_int(theme, col.lab)
  if (axisY && identical(theme$axis.type, "inside"))
    inside_freq_axis_int(range(sp$freq), col = col.lab, cex = cex.axis)
  else if (axisY && identical(theme$axis.type, "compact"))
    compact_freq_axis_int(range(sp$freq), col = col.lab, cex = cex.axis)
  else if (axisY) {
    graphics::axis(2, las = theme_int(theme, "las", 1), col = col.lab, col.axis = col.lab, cex.axis = cex.axis)
    graphics::title(ylab = flab, cex.lab = cex.lab, col.lab = col.lab)
  }
  invisible(sp)
}

# time axis; 'to_x' converts times to x axis coordinates
time_axis_int <- function(to_x, xlim, cex.lab, xlab = "Time (s)", font.lab = 1, col.lab = "black", cex.axis = 1, inside = FALSE) {
  # ticks slightly outside the plotted range are kept (the spectrogram starts and ends half a window inside the sound)
  tol <- diff(xlim) * 0.01
  at <- pretty(xlim)
  at <- at[at >= xlim[1] - tol & at <= xlim[2] + tol]
  # axis() drops ticks outside the plotting region, so those are moved to its edge (less than a pixel away)
  usr <- graphics::par("usr")
  if (inside) {
    inside_time_axis_int(pmin(pmax(to_x(at), usr[1]), usr[2]), at, col = col.lab, cex = cex.axis)
    return(invisible(NULL))
  }
  graphics::axis(1, at = pmin(pmax(to_x(at), usr[1]), usr[2]), labels = at, col = col.lab, col.axis = col.lab, cex.axis = cex.axis)
  graphics::mtext(xlab, side = 1, line = max(3, 1.8 + cex.lab), cex = cex.lab, font = font.lab, col = col.lab)
}

# time axis of axis.type = "inside": no axis line or title, labels with units (e.g. "2s") close to short ticks.
# Labels too close to the figure edges are dropped (ticks are kept)
inside_time_axis_int <- function(x, at, col = "black", cex = 1) {
  usr <- graphics::par("usr")
  graphics::axis(1, at = x, labels = FALSE, lwd = 0, lwd.ticks = 1, col = col)
  half <- graphics::strwidth(paste0(at, "s"), cex = cex) / 2
  room <- graphics::grconvertX(c(0, 1), "ndc", "user")
  keep <- x - half >= room[1] & x + half <= room[2]
  if (any(keep))
    graphics::axis(1, at = x[keep], labels = paste0(at[keep], "s"), lwd = 0, tick = FALSE, col.axis = col, cex.axis = cex,
                   mgp = c(0, max(0, -graphics::par("tcl")) - 0.1, 0))
}

# times as minutes and seconds (e.g. "1:05"), with the decimals needed (e.g. "0:02.5")
mmss_int <- function(t) {
  d <- 0
  while (d < 3 && any(abs(round(t, d) - t) > 1e-6)) d <- d + 1
  t <- round(t, d)
  m <- floor(t / 60 + 1e-9)
  paste0(m, ":", formatC(t - 60 * m, format = "f", digits = d, width = if (d > 0) 3 + d else 2, flag = "0"))
}

# axes of axis.type = "compact": frequency labels below short ticks in a narrow column on the left of the panel
# (with the unit at the bottom) and time labels (minutes and seconds) next to ticks in the top margin
compact_freq_axis_int <- function(ylim, col = "black", cex = 1, bottom = graphics::par("usr")[3]) {
  lw <- graphics::grconvertX(1, "lines", "user") - graphics::grconvertX(0, "lines", "user")
  x0 <- graphics::grconvertX(0, "nfc", "user") + 0.3 * lw
  sh <- graphics::strheight("8", cex = cex)
  at <- pretty(ylim, n = 6)
  at <- at[at > bottom + 2.6 * sh & at <= ylim[2]]
  graphics::segments(x0, at, x0 + 0.9 * lw, at, col = col, xpd = NA)
  graphics::text(x0, at - 0.35 * sh, labels = at, adj = c(0, 1), col = col, cex = cex, xpd = NA)
  graphics::text(x0, bottom + 0.35 * sh, labels = "kHz", adj = c(0, 0), col = col, cex = cex, xpd = NA)
}

# compact time axis above the current panel of a static spectrogram ('to_x' converts times to x axis coordinates)
compact_row_axis_int <- function(to_x, xlim, theme, cex) {
  usr <- graphics::par("usr")
  # (ticks slightly outside the plotted range are kept at its edge, as in time_axis_int())
  tol <- diff(xlim) * 0.01
  at <- pretty(xlim)
  at <- at[at >= xlim[1] - tol & at <= xlim[2] + tol]
  compact_time_axis_int(pmin(pmax(to_x(at), usr[1]), usr[2]), at, col = theme_int(theme, "col.lab", "black"), cex = cex)
}

compact_time_axis_int <- function(x, at, col = "black", cex = 1, band = NULL) {
  if (length(x) == 0) return(invisible(NULL))
  usr <- graphics::par("usr")
  # (ticks span the top margin, or 'band' lines of text above the panel)
  top <- if (is.null(band)) graphics::grconvertY(1, "nfc", "user") else
    usr[4] + band * graphics::par("csi") * diff(graphics::grconvertY(c(0, 1), "inches", "user"))
  labs <- mmss_int(at)
  pad <- 0.3 * graphics::strwidth("0", cex = cex)
  graphics::segments(x, usr[4], x, top, col = col, xpd = NA)
  keep <- x + pad + graphics::strwidth(labs, cex = cex) <= graphics::grconvertX(1, "nfc", "user")
  if (any(keep))
    graphics::text(x[keep] + pad, (usr[4] + top) / 2, labels = labs[keep], adj = c(0, 0.5), col = col, cex = cex, xpd = NA)
}

# frequency labels of axis.type = "inside": labels with units (e.g. "8k") followed by a short tick, inside the
# left side of the spectrogram (labels closer than half a line of text to the top or bottom are dropped)
inside_freq_axis_int <- function(ylim, col = "black", cex = 1) {
  usr <- graphics::par("usr")
  at <- pretty(ylim, n = 6)
  pad <- graphics::strheight("8", cex = cex)
  at <- at[at >= ylim[1] + pad & at <= ylim[2] - pad]
  if (length(at) == 0) return(invisible(NULL))
  labs <- paste0(at, "k")
  ch <- graphics::strwidth("0", cex = cex)
  x_lab <- usr[1] + 0.8 * ch + max(graphics::strwidth(labs, cex = cex))
  tick <- max(0, -graphics::par("tcl")) * (graphics::grconvertX(1, "lines", "user") - graphics::grconvertX(0, "lines", "user"))
  graphics::text(x_lab, at, labels = labs, adj = c(1, 0.5), col = col, cex = cex, xpd = NA)
  graphics::segments(x_lab + 0.4 * ch, at, x_lab + 0.4 * ch + tick, at, col = col)
}

# web page of scrolling_spectro(): a frame with the parts that do not move (axes, play line, lower spectrogram),
# with the full spectrogram (and oscillogram) below it, moved by the web page in sync with the sound
scrolling_html_int <- function(frame_file, spectro_file, osc_file, lower, plt, height.prop, wave, wave_sil, t.display, buffer,
                               step_time, step_time_low, fps, loop, fix.time, axis, zoom = FALSE, inside = FALSE, compact = FALSE, width, res, captions = NULL,
                               speeds, keep.pitch, buttons = "time", download.name = "sound", lcol, theme = list()) {
  # plotting region (proportions of the frame from the top); the upper part has the scrolling spectrogram
  split <- plt[3] + (plt[4] - plt[3]) * height.prop[2] / height.prop[1]
  dur_wave <- seewave::duration(wave)

  # sound with the silence added at the start (as in videos)
  if (buffer > 0)
    wave <- seewave::pastew(wave2 = tuneR::silence(duration = buffer, samp.rate = wave@samp.rate, xunit = "time"), wave1 = wave, f = wave@samp.rate, output = "Wave")
  snd <- sound_data_int(wave)

  tick_step <- diff(pretty(c(0, t.display)))[1]

  cfg <- list(
    duration = seewave::duration(wave), durSil = seewave::duration(wave_sil), durWave = dur_wave, tDisplay = t.display,
    buffer = buffer, k = step_time * fps, kl = step_time_low * fps / t.display, loop = loop > 1, lower = lower,
    left = plt[1], right = plt[2], upTop = 1 - plt[4], upBottom = 1 - split, lowTop = 1 - split, lowBottom = 1 - plt[3],
    movingAxis = axis && (!fix.time || zoom), fixTime = fix.time, zoom = zoom, tickStep = tick_step, tickDigits = max(0, -floor(log10(tick_step) + 1e-9)),
    # (ticks and labels as in R axes: tick length and line of the labels, in lines of text; labels with units inside)
    unit = if (inside) "s" else "", top = compact, tickLines = theme_int(theme, "tick.length", 0.5), labLine = if (inside) max(0, theme_int(theme, "tick.length", 0.5) - 0.1) else 1,
    width = width, res = res,
    bg = col2hex_int(theme_int(theme, "bg", "white")), colLab = col2hex_int(theme_int(theme, "col.lab", "black")),
    cexAxis = theme_int(theme, "cex.axis", 1),
    dim = col2hex_int(grDevices::adjustcolor(theme_int(theme, "col.highlight", "gray"), 0.42), alpha = TRUE)
  )

  paste0(
    '<div class="dynaspec-scroll"', player_attrs_int(speeds, keep.pitch, buttons, download.name, theme),
    ' data-color="', col2hex_int(lcol), '" data-text-color="', contrast_col_int(lcol), '">\n',
    '<script type="application/json">', json_int(cfg), '</script>\n',
    '<img class="ds-frame" src="', base64_uri_int(frame_file, "image/png"), '" alt="Scrolling spectrogram">\n',
    '<img class="ds-strip" src="', base64_uri_int(spectro_file, "image/png"), '" alt="">\n',
    if (!is.null(osc_file)) paste0('<img class="ds-strip ds-osc" src="', base64_uri_int(osc_file, "image/png"), '" alt="">\n'),
    '<audio controls preload="auto"', if (loop > 1) " loop", ' src="', snd$uri, '"></audio>\n',
    captions_json_int(captions, res = res, width = width, offset = buffer),
    '</div>\n',
    player_js_int()
  )
}

# minimal JSON for settings passed to the web pages (named lists are objects, vectors of length > 1 are arrays)
json_int <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    vals <- vapply(x, json_int, character(1))
    return(if (!is.null(names(x))) paste0("{", paste0('"', names(x), '":', vals, collapse = ","), "}") else paste0("[", paste(vals, collapse = ","), "]"))
  }
  vals <- if (is.character(x)) {
    # escape quotes, backslashes and "<" (so labels cannot close the script element)
    paste0('"', gsub("<", "\\\\u003c", gsub('(["\\\\])', "\\\\\\1", x)), '"')
  } else if (is.logical(x)) tolower(x) else vapply(unname(x), format, character(1), digits = 10, scientific = FALSE)
  if (length(x) == 1) vals else paste0("[", paste(vals, collapse = ","), "]")
}

# web page of paged_spectro(): one image per page ('pages': list of page images (data uris) and the extents of
# their highlight boxes)
paged_html_int <- function(pages, wave, highlightCol, highlightAlpha, cursorCol, speeds, keep.pitch, buttons = "time", download.name = "sound",
                           captions = NULL, res = 100, width = 1000, theme = list()) {
  ext <- do.call(rbind, lapply(pages, `[[`, "ext"))
  snd <- sound_data_int(wave)
  n_pages <- length(pages)

  paste0(
    '<div class="dynaspec-paged" data-duration="', seewave::duration(wave),
    '" data-rows="', paste(apply(round(ext, 5), 1, paste, collapse = ","), collapse = ";"),
    '" data-highlight="', col2hex_int(highlightCol), '" data-alpha="', highlightAlpha, '" data-cursor="', col2hex_int(cursorCol),
    '"', player_attrs_int(speeds, keep.pitch, buttons, download.name, theme),
    ' data-color="', col2hex_int(highlightCol), '" data-text-color="', contrast_col_int(highlightCol), '">\n',
    paste0('<img src="', vapply(pages, `[[`, character(1), "uri"), '" alt="Spectrogram, page ', seq_len(n_pages), ' of ', n_pages, '">', collapse = "\n"),
    '\n<audio controls preload="auto" src="', snd$uri, '"></audio>\n',
    captions_json_int(captions, res = res, width = width),
    '</div>\n',
    player_js_int()
  )
}

# video of one page of a paged spectrogram: the translucent highlight box (from the cursor to the end of the
# page) and the cursor move over the page image along the sound ('ext': time range and box extents)
paged_video_int <- function(png_file, wave, ext, file.name, highlightCol, highlightAlpha, cursorCol, fps, captions = NULL, res = 100) {
  wav_file <- tempfile(fileext = ".wav")
  on.exit(unlink(wav_file), add = TRUE)
  tuneR::writeWave(if (wave@pcm) wave else tuneR::normalize(wave, unit = "16"), filename = wav_file)

  dims <- dim(png::readPNG(png_file))
  W <- dims[2]
  H <- dims[1]
  dur <- ext[["t1"]] - ext[["t0"]]
  x0 <- round(ext[["left"]] * W)
  x1 <- round(ext[["right"]] * W)
  y0 <- round(ext[["top"]] * H)
  bh <- max(1, round((ext[["bottom"]] - ext[["top"]]) * H))
  lw <- max(2, round(W / 400))
  speed <- (x1 - x0) / dur

  # the box moves right as the sound plays; the part of the frame right of the page end is put back on top
  filter <- paste0(
    "[0]split=2[a][b];",
    "[1]format=rgba,colorchannelmixer=aa=", highlightAlpha, "[box];",
    "[a][box]overlay=x='", x0, "+", speed, "*t':y=", y0, "[v1];",
    "[b]crop=", W - x1, ":", H, ":", x1, ":0[r];",
    "[v1][r]overlay=x=", x1, ":y=0[v2];",
    "[v2][2]overlay=x='", x0 - lw / 2, "+", speed, "*t':y=", y0, "[v3]"
  )

  # captions of this page (times relative to the start of the page; inputs after the audio)
  caps <- caption_overlays_int(captions, width = W, height = H, res = res, fps = fps, first = 4, in_label = "v3", offset = ext[["t0"]], dur = dur)
  filter <- paste(c(filter, caps$filters, paste0("[", caps$out, "]pad=ceil(iw/2)*2:ceil(ih/2)*2,format=yuv420p[v]")), collapse = ";")

  run_ffmpeg_int(
    paste0(
      '-y -loop 1 -framerate ', fps, ' -i "', png_file, '"',
      ' -f lavfi -i "color=c=', sub("#", "0x", col2hex_int(highlightCol)), ':s=', x1 - x0, 'x', bh, ':r=', fps, '"',
      ' -f lavfi -i "color=c=', sub("#", "0x", col2hex_int(cursorCol)), ':s=', lw, 'x', bh, ':r=', fps, '"',
      ' -i "', wav_file, '" ', paste(caps$inputs, collapse = " "), ' -filter_complex "', filter, '" -map "[v]" -map 3:a',
      ' -c:v libx264 -tune stillimage -r ', fps, ' -c:a aac -b:a 192k -ac ', if (wave@stereo) 2 else 1, ' -t ', dur,
      ' "', file.name, '"'
    ),
    "creating the video"
  )

  invisible(file.name)
}

# video of a static_spectro() figure with the play line moving along the sound
# 'ext' has one row per segment (t0, t1, and left, right, top and bottom as proportions of the figure)
playhead_video_int <- function(png_file, wave, ext, file.name, lcol, fps, width, height, line = TRUE, captions = NULL, res = 100) {
  wav_file <- tempfile(fileext = ".wav")
  on.exit(unlink(wav_file), add = TRUE)
  tuneR::writeWave(if (wave@pcm) wave else tuneR::normalize(wave, unit = "16"), filename = wav_file)

  dur <- utils::tail(ext[, "t1"], 1)
  lw <- max(2, round(width / 500))
  col <- sub("#", "0x", col2hex_int(lcol))

  # one line (color source) per row, shown while that row plays (no lines when 'line' is FALSE). The overlay
  # filter evaluates the position expressions on every frame (unlike drawbox in ffmpeg < 5)
  n_rows <- if (line) nrow(ext) else 0
  lines <- vapply(seq_len(n_rows), function(k) {
    e <- ext[k, ]
    line_h <- round((e[["bottom"]] - e[["top"]]) * height)
    x0 <- e[["left"]] * width - lw / 2
    dx <- (e[["right"]] - e[["left"]]) * width / (e[["t1"]] - e[["t0"]])
    enable <- if (k < n_rows) sprintf("gte(t,%f)*lt(t,%f)", e[["t0"]], e[["t1"]]) else sprintf("gte(t,%f)", e[["t0"]])
    sprintf("[v%d][%d]overlay=x='%f+%f*(t-%f)':y=%d:enable='%s'[v%d]", k - 1, k + 1, x0, dx, e[["t0"]], round(e[["top"]] * height), enable, k)
  }, character(1))

  sources <- vapply(seq_len(n_rows), function(k) {
    sprintf('-f lavfi -i "color=c=%s:s=%dx%d:r=%s"', col, lw, max(1, round((ext[k, "bottom"] - ext[k, "top"]) * height)), fps)
  }, character(1))

  # captions over the video (inputs after the line sources)
  caps <- caption_overlays_int(captions, width = width, height = height, res = res, fps = fps, first = n_rows + 2, in_label = paste0("v", n_rows), dur = dur)

  # video dimensions must be even
  filter <- paste(c("[0]pad=ceil(iw/2)*2:ceil(ih/2)*2:color=white[v0]", lines, caps$filters), collapse = ";")

  run_ffmpeg_int(
    paste0(
      '-y -loop 1 -framerate ', fps, ' -i "', png_file, '" -i "', wav_file, '" ', paste(c(sources, caps$inputs), collapse = " "),
      ' -filter_complex "', filter, '" -map "[', caps$out, ']" -map 1:a',
      ' -c:v libx264 -tune stillimage -pix_fmt yuv420p -r ', fps,
      # explicit number of channels (the aac encoder rejects the channel layout of mono files written by tuneR)
      ' -c:a aac -b:a 192k -ac ', if (wave@stereo) 2 else 1, ' -t ', dur,
      ' -movflags +faststart "', file.name, '"'
    ),
    "creating the video"
  )

  invisible(file.name)
}

# captions (captions() layers) of web pages: settings read by the player script. 'res' and 'width' are those of the
# figure (text sizes are scaled to the size of the figure), and 'offset' is added to their times
captions_json_int <- function(captions, res, width, offset = 0) {
  if (length(captions) == 0)
    return("")
  layers <- lapply(captions, function(cp) list(
    labels = as.character(cp$labels), start = cp$start + offset, end = cp$end + offset, fading = cp$fading,
    x = cp$x, y = 1 - cp$y, col = col2hex_int(cp$col), cex = cp$cex, font = cp$font, family = css_family_int(theme_int(cp, "family", "sans"))
  ))
  paste0('<script type="application/json" class="ds-captions">', json_int(list(layers = layers, res = res, width = width)), '</script>\n')
}

# captions of videos: each label is drawn in a transparent image of the size of the frame, which ffmpeg fades in and
# out over the video. Returns the ffmpeg inputs and the filters overlaying them on the video labelled 'in_label'
# (inputs are numbered from 'first'); times are shifted by 'offset' (e.g. the start of a page)
caption_overlays_int <- function(captions, width, height, res, fps, first, in_label, offset = 0, dur = Inf) {
  inputs <- character(0)
  filters <- character(0)
  label <- in_label
  k <- first
  for (cp in captions) {
    for (i in seq_along(cp$labels)) {
      t0 <- cp$start[i] - offset
      t1 <- cp$end[i] - offset
      if (t1 <= 0 || t0 >= dur) next
      png_file <- tempfile(fileext = ".png")
      grDevices::png(png_file, width = width, height = height, res = res, bg = "transparent")
      graphics::par(mar = rep(0, 4))
      graphics::plot.new()
      graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
      graphics::text(cp$x, cp$y, labels = cp$labels[i], col = cp$col, cex = cp$cex, font = cp$font, family = theme_int(cp, "family", "sans"))
      grDevices::dev.off()

      inputs <- c(inputs, sprintf('-loop 1 -framerate %s -i "%s"', fps, png_file))
      fade <- if (cp$fading > 0) sprintf(",fade=t=in:st=%f:d=%f:alpha=1,fade=t=out:st=%f:d=%f:alpha=1", max(t0, 0), cp$fading, max(t1 - cp$fading, 0), cp$fading) else ""
      out <- sprintf("cap%d", k)
      filters <- c(filters, sprintf("[%d]format=rgba%s[c%d]", k, fade, k),
                   sprintf("[%s][c%d]overlay=0:0:enable='between(t,%f,%f)'[%s]", label, k, t0, t1, out))
      label <- out
      k <- k + 1
    }
  }
  list(inputs = inputs, filters = filters, out = label)
}

# base64 encode a file as a data uri
base64_uri_int <- function(file, mime) {
  paste0("data:", mime, ";base64,", base64_enc_int(readBin(file, "raw", n = file.size(file))))
}

# base64 encoding of a raw vector
base64_enc_int <- function(x) {
  chars <- c(LETTERS, letters, 0:9, "+", "/")
  n <- length(x)
  if (n == 0) return("")
  pad <- (3 - n %% 3) %% 3
  m <- matrix(c(as.integer(x), integer(pad)), nrow = 3)
  v <- m[1, ] * 65536L + m[2, ] * 256L + m[3, ]
  out <- chars[rbind(v %/% 262144L, (v %/% 4096L) %% 64L, (v %/% 64L) %% 64L, v %% 64L) + 1L]
  if (pad > 0) out[length(out) - seq_len(pad) + 1] <- "="
  paste(out, collapse = "")
}

# color as hex
col2hex_int <- function(col, alpha = FALSE) {
  rgb <- grDevices::col2rgb(col, alpha = alpha)
  if (alpha) grDevices::rgb(rgb[1], rgb[2], rgb[3], rgb[4], maxColorValue = 255) else grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
}

# attributes of web players with the player() settings (speed buttons, keep pitch, additional buttons and name
# of downloaded sounds)
# (and colors of the controls and font family of the dynatheme())
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

html_escape_int <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

# return html (embedded when knitting), save it as a web page and open it
output_html_int <- function(html, file.name, play) {
  html <- structure(html, class = "dynaSpec_html")

  if (!is.null(file.name))
    write_page_int(html, file.name)

  if (isTRUE(getOption("knitr.in.progress")) && requireNamespace("knitr", quietly = TRUE))
    return(knitr::asis_output(paste0("\n```{=html}\n", html, "\n```\n")))

  if (play) {
    if (!is.null(file.name))
      open_file_int(normalizePath(file.name))
    else if (interactive())
      view_html_int(html)
  }

  invisible(html)
}

# write html as a standalone web page
write_page_int <- function(html, file.name, title = tools::file_path_sans_ext(basename(file.name))) {
  writeLines(c(
    "<!DOCTYPE html>",
    '<html lang="en">',
    "<head>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    paste0("<title>", html_escape_int(title), "</title>"),
    "<style>body{font-family:system-ui,sans-serif;max-width:1000px;margin:2em auto;padding:0 16px;background:#fff;color:#222}</style>",
    "</head>",
    "<body>",
    html,
    "</body>",
    "</html>"
  ), con = file.name, useBytes = TRUE)
}

# preview html in the IDE viewer (e.g. RStudio) or the web browser
view_html_int <- function(html) {
  # the viewer only shows files in the session temporary directory
  view_dir <- tempfile("dynaSpec_view_")
  dir.create(view_dir)
  file <- file.path(view_dir, "index.html")
  write_page_int(html, file, title = "dynaSpec player")

  viewer <- getOption("viewer")
  if (is.function(viewer)) viewer(file) else utils::browseURL(file)
}

# javascript for the players (defined once per page, even if included several times)
player_js_int <- function() {
  '<script>
(function () {
  if (!window.dynaSpecPlayers) {
    var all = [];
    var label = function (s) {
      var f = {1: "1", 0.5: "\\u00BD", 0.25: "\\u00BC", 0.125: "\\u215B"};
      return (f[s] || String(s)) + "\\u00D7";
    };
    var css = document.createElement("style");
    css.textContent =
      ".dynaspec-player .ds-wrap,.dynaspec-paged .ds-wrap{position:relative;cursor:pointer}" +
      ".dynaspec-player img,.dynaspec-paged img{display:block;width:100%;height:auto;margin:0}" +
      ".dynaspec-player audio,.dynaspec-paged audio{display:none}" +
      ".ds-box{position:absolute;pointer-events:none}" +
      // reset image styles of the hosting page (e.g. backgrounds, borders or padding added by R Markdown themes)
      ".dynaspec-player img,.dynaspec-paged img,.dynaspec-scroll img{background:transparent!important;border:0!important;padding:0!important;border-radius:0!important;box-shadow:none!important;margin:0!important}" +
      ".dynaspec-scroll audio{display:none}" +
      ".dynaspec-scroll .ds-wrap{position:relative;cursor:pointer;background:#fff;overflow:hidden}" +
      ".dynaspec-scroll img.ds-frame{position:relative;z-index:2;display:block;width:100%;height:auto;margin:0}" +
      ".ds-vp{position:absolute;overflow:hidden;z-index:1}" +
      ".ds-vp img.ds-strip{position:absolute;top:0;height:100%;max-width:none;margin:0;display:block}" +
      ".ds-over{position:absolute;z-index:3;pointer-events:none}" +
      ".ds-text{position:absolute;z-index:4;pointer-events:none;white-space:nowrap;font-family:var(--ds-font,Helvetica,Arial,sans-serif);line-height:1}" +
      ".ds-bar .ds-page{font-variant-numeric:tabular-nums;color:var(--ds-fg,#555)}" +
      ".ds-head{position:absolute;width:2px;margin-left:-1px;pointer-events:none}" +
      ".ds-bar{display:flex;flex-wrap:wrap;gap:.4em;align-items:center;margin:.4em 0 1.2em;font-size:.9em}" +
      ".ds-bar button{border:1px solid var(--ds-line,#bbb);background:var(--ds-bg,#fff);color:var(--ds-fg,#222);border-radius:4px;padding:.15em .7em;line-height:1.5;cursor:pointer}" +
      ".ds-bar .ds-play{min-width:5.5em;font-weight:600}" +
      ".ds-bar .ds-time{font-variant-numeric:tabular-nums;color:var(--ds-fg,#555);margin-left:.3em}" +
      ".ds-bar .ds-sep{color:var(--ds-fg,#999);opacity:.75;margin:0 .2em}" +
      ".ds-bar .ds-rate{color:var(--ds-fg,inherit)}" +
      ".ds-bar .ds-speed{display:inline-flex;align-items:center;gap:.4em}" +
      ".ds-bar .ds-speed input{margin:0}" +
      ".ds-bar .ds-zoom{display:inline-flex;align-items:center;gap:.4em;margin-left:.6em}" +
      ".ds-bar .ds-zoom input{width:6em;margin:0}" +
      ".ds-bar .ds-zoom .ds-rate{min-width:3.2em}" +
      ".ds-bar .ds-rate{font-variant-numeric:tabular-nums;min-width:2.2em;font-weight:600}" +
      ".ds-bar .ds-pitch{margin-left:.6em}" +
      ".ds-bar .ds-loop{margin-left:.6em}" +
      ".ds-bar .ds-vol{display:inline-flex;align-items:center;gap:.3em;margin-left:.6em}" +
      ".ds-bar .ds-vol input{width:5em;margin:0}" +
      ".ds-bar .ds-fs{margin-left:auto}" +
      ".ds-bar .ds-progress{flex:1 1 8em;min-width:6em;margin:0 .3em}" +
      // rounded controls (dynatheme(bar.style = "rounded")): the whole player on the bar background, the figure in a
      // frame with rounded corners, round play button, pill-shaped controls and flat sliders
      ".ds-round{background:var(--ds-bg,#F0F0F0);padding:12px;border-radius:10px;margin-bottom:1.2em}" +
      ".ds-round .ds-wrap{border:1px solid color-mix(in srgb,var(--ds-fg,#333) 16%,transparent);border-radius:8px;overflow:hidden}" +
      ".ds-round .ds-bar{margin:10px 0 0;gap:10px;font-size:14px;color:var(--ds-fg,#333);font-family:var(--ds-font,inherit)}" +
      ".ds-round .ds-bar button{border:1px solid color-mix(in srgb,var(--ds-fg,#333) 18%,transparent);background:color-mix(in srgb,var(--ds-fg,#333) 9%,transparent);color:var(--ds-fg,#333);border-radius:22px;padding:.35em .9em;line-height:1.3;font-family:inherit}" +
      ".ds-round .ds-bar .ds-play{width:44px;height:44px;min-width:0;padding:0;border-radius:50%;font-size:15px;font-weight:400}" +
      ".ds-round .ds-bar .ds-time{background:color-mix(in srgb,var(--ds-fg,#333) 9%,transparent);border-radius:4px;padding:5px 10px;margin:0;color:var(--ds-fg,#333)}" +
      ".ds-round .ds-bar .ds-speed,.ds-round .ds-bar .ds-zoom{background:color-mix(in srgb,var(--ds-fg,#333) 9%,transparent);border-radius:22px;padding:5px;gap:6px}" +
      ".ds-round .ds-bar .ds-sep{display:none}" +
      ".ds-round .ds-bar .ds-step{width:28px;height:28px;padding:0;border:0;border-radius:50%;background:color-mix(in srgb,var(--ds-fg,#333) 18%,transparent);font-size:18px;line-height:1}" +
      ".ds-round .ds-bar .ds-rate{padding-right:6px;font-weight:400}" +
      ".ds-round .ds-bar input[type=range]{-webkit-appearance:none;appearance:none;height:6px;border-radius:3px;background:color-mix(in srgb,var(--ds-fg,#333) 18%,transparent);outline:none;margin:0}" +
      ".ds-round .ds-bar input[type=range]::-webkit-slider-thumb{-webkit-appearance:none;appearance:none;width:16px;height:16px;border-radius:50%;background:var(--ds-fg,#333);border:0;cursor:pointer}" +
      ".ds-round .ds-bar input[type=range]::-moz-range-thumb{width:16px;height:16px;border-radius:50%;background:var(--ds-fg,#333);border:0;cursor:pointer}" +
      ".ds-round .ds-bar input.ds-progress{height:8px;border-radius:4px}" +
      // square controls (dynatheme(bar.style = "square")): flat bar, square play button and sliders in the accent
      // color (the play line color)
      ".ds-square .ds-wrap{border:1px solid #9E9E9E}" +
      ".ds-square .ds-bar{gap:14px;color:var(--ds-fg,#fff);font-family:var(--ds-font,inherit)}" +
      ".ds-square .ds-bar button{border:0;background:transparent;color:var(--ds-fg,#fff);border-radius:3px;padding:.3em .6em;font-family:inherit}" +
      ".ds-square .ds-bar .ds-play{width:56px;height:40px;min-width:0;padding:0;background:var(--ds-accent,#3D7299);color:#fff;font-size:20px;font-weight:400}" +
      ".ds-square .ds-bar .ds-time{color:var(--ds-fg,#fff);margin:0}" +
      ".ds-square .ds-bar .ds-speed,.ds-square .ds-bar .ds-zoom{background:color-mix(in srgb,var(--ds-fg,#fff) 16%,transparent);border-radius:4px;padding:4px 8px;gap:6px}" +
      ".ds-square .ds-bar .ds-sep{display:none}" +
      ".ds-square .ds-bar .ds-step{padding:0 .35em;font-size:18px;line-height:1}" +
      ".ds-square .ds-bar .ds-rate{font-weight:400}" +
      ".ds-square .ds-bar input[type=range]{-webkit-appearance:none;appearance:none;height:4px;border-radius:2px;background:color-mix(in srgb,var(--ds-fg,#fff) 35%,transparent);outline:none;margin:0}" +
      ".ds-square .ds-bar input[type=range]::-webkit-slider-thumb{-webkit-appearance:none;appearance:none;width:16px;height:16px;border-radius:50%;background:var(--ds-accent,#3D7299);border:2px solid var(--ds-fg,#fff);cursor:pointer}" +
      ".ds-square .ds-bar input[type=range]::-moz-range-thumb{width:12px;height:12px;border-radius:50%;background:var(--ds-accent,#3D7299);border:2px solid var(--ds-fg,#fff);cursor:pointer}" +
      // full screen: player centered, as large as possible while showing the whole figure and the buttons
      ".dynaspec-player:fullscreen,.dynaspec-paged:fullscreen,.dynaspec-scroll:fullscreen{background:var(--ds-bg,#fff);padding:1em;box-sizing:border-box;display:flex;flex-direction:column;justify-content:center;overflow:auto}" +
      ".dynaspec-player:fullscreen>*,.dynaspec-paged:fullscreen>*,.dynaspec-scroll:fullscreen>*{width:100%;margin-left:auto;margin-right:auto}";
    document.head.appendChild(css);

    var setup = function (a, keep) {
      a.preservesPitch = a.mozPreservesPitch = a.webkitPreservesPitch = keep;
      a.addEventListener("play", function () {
        all.forEach(function (o) { if (o !== a) o.pause(); });
      });
      all.push(a);
    };

    // times as minutes and seconds (as mmss_int() in R)
    var mmss = function (t, d) {
      t = +t.toFixed(d);
      var m = Math.floor(t / 60 + 1e-9), sec = (t - 60 * m).toFixed(d);
      return m + ":" + (sec.length < (d ? 3 + d : 2) ? "0" : "") + sec;
    };

    // text of the play button (only the symbol with rounded and square controls)
    var setPlay = function (b, on) {
      var round = b.closest(".ds-round,.ds-square");
      b.textContent = round ? (on ? "\\u275A\\u275A" : "\\u25B6") : (on ? "\\u275A\\u275A pause" : "\\u25B6 play");
      b.title = on ? "pause" : "play";
      b.setAttribute("aria-label", on ? "pause" : "play");
    };

    var paintBtn = function (b, on, color) {
      b.style.background = on ? color[0] : "";
      b.style.borderColor = on ? color[0] : "";
      b.style.color = on ? color[1] : "";
      // outline so active buttons are visible even with colors similar to the page background
      b.style.boxShadow = on ? "inset 0 0 0 1px rgba(0,0,0,0.35)" : "";
    };

    // speed slider (snapping to the speeds of player(), slowest on the left) and keep pitch toggle; returns a
    // function giving the current speed
    var nSliders = 0;
    var speedGroup = function (bar, speeds, color, keep, audios, onChange) {
      var rate = speeds[0];
      if (speeds.length < 2) return function () { return rate; };
      var sorted = speeds.slice().sort(function (x, y) { return x - y; });
      var group = document.createElement("span");
      group.className = "ds-speed";
      var sep = document.createElement("span");
      sep.className = "ds-sep";
      sep.textContent = "speed";
      var slider = document.createElement("input");
      slider.type = "range";
      slider.min = 0; slider.max = sorted.length - 1; slider.step = 1;
      slider.value = sorted.indexOf(rate);
      slider.style.accentColor = color[0];
      slider.style.width = 1.6 * sorted.length + "em";
      slider.setAttribute("aria-label", "playback speed");
      // tick marks at the fixed speeds
      var ticks = document.createElement("datalist");
      ticks.id = "ds-speeds-" + (++nSliders);
      sorted.forEach(function (s, i) { var o = document.createElement("option"); o.value = i; ticks.appendChild(o); });
      slider.setAttribute("list", ticks.id);
      var out = document.createElement("span");
      out.className = "ds-rate";
      var show = function () {
        out.textContent = label(rate);
        slider.title = rate === 1 ? "real time" : rate < 1 ? "slowed down" : "sped up";
        slider.setAttribute("aria-valuetext", label(rate));
      };
      slider.addEventListener("input", function () { rate = sorted[+slider.value]; show(); onChange(rate); });
      show();
      [sep, slider, ticks, out].forEach(function (x) { group.appendChild(x); });
      bar.appendChild(group);

      // keep pitch toggle
      var p = document.createElement("button");
      p.type = "button";
      p.className = "ds-pitch";
      p.textContent = "keep pitch";
      p.setAttribute("aria-pressed", keep);
      var setPitch = function () {
        audios.forEach(function (a) { a.preservesPitch = a.mozPreservesPitch = a.webkitPreservesPitch = keep; });
        p.setAttribute("aria-pressed", keep);
        p.title = keep ? "pitch is kept when speed changes (click to let pitch follow speed)" : "pitch follows speed, as when slowing down a tape (click to keep pitch)";
        paintBtn(p, keep, color);
      };
      p.addEventListener("click", function () { keep = !keep; setPitch(); });
      setPitch();
      bar.appendChild(p);

      return function () { return rate; };
    };

    // additional controls set by player(buttons): time readout, loop, volume, download and full screen. time is
    // the time readout of the player and redraw updates the player (e.g. after entering full screen)
    var extraButtons = function (div, bar, wrap, a, time, color, redraw, dur) {
      var d = div.dataset, opts = d.buttons ? d.buttons.split(",") : [];
      var has = function (x) { return opts.indexOf(x) > -1; };
      var round = d.barStyle === "rounded", square = d.barStyle === "square", play = bar.querySelector(".ds-play");
      if (round) div.classList.add("ds-round");
      if (square) div.classList.add("ds-square");
      div.style.setProperty("--ds-accent", color[0]);
      setPlay(play, !a.paused);
      // colors of the controls and font family of the dynatheme()
      if (d.barCol) { div.style.setProperty("--ds-fg", d.barCol); div.style.setProperty("--ds-line", d.barLine); }
      if (d.barBg && round) div.style.setProperty("--ds-bg", d.barBg);
      else if (d.barBg) {
        div.style.setProperty("--ds-bg", d.barBg);
        bar.style.background = d.barBg;
        bar.style.padding = ".35em .5em";
        bar.style.marginTop = "0";
      }
      if (d.family) div.style.setProperty("--ds-font", d.family);
      var button = function (cls, text, title) {
        var b = document.createElement("button");
        b.type = "button";
        b.className = cls;
        b.textContent = text;
        b.title = title;
        bar.appendChild(b);
        return b;
      };
      if (!has("time")) time.style.display = "none";

      // seek bar (after the play button, followed by the time)
      if (has("progress")) {
        var prog = document.createElement("input");
        prog.type = "range";
        prog.className = "ds-progress";
        prog.min = 0; prog.max = dur; prog.step = "any"; prog.value = 0;
        prog.style.accentColor = color[0];
        prog.setAttribute("aria-label", "position");
        play.insertAdjacentElement("afterend", prog);
        prog.insertAdjacentElement("afterend", time);
        var dragging = false, rafP;
        var paint = function () {
          if (!dragging) prog.value = Math.min(a.currentTime, dur);
          // (rounded and square controls: the part played is filled)
          if (round || square) {
            var q = 100 * prog.value / dur;
            prog.style.background = "linear-gradient(to right, " + (round ? "var(--ds-fg,#333) " : "var(--ds-accent) ") + q + "%, color-mix(in srgb,var(--ds-fg,#333) " + (round ? 18 : 35) + "%,transparent) " + q + "%)";
          }
        };
        var follow = function () { paint(); if (!a.paused) rafP = requestAnimationFrame(follow); };
        prog.addEventListener("input", function () { dragging = true; a.currentTime = Math.min(dur - 0.001, +prog.value); paint(); });
        prog.addEventListener("change", function () { dragging = false; });
        a.addEventListener("play", follow);
        a.addEventListener("pause", function () { cancelAnimationFrame(rafP); paint(); });
        a.addEventListener("seeked", paint);
        a.addEventListener("ended", paint);
        paint();
      }

      // rounded and square controls: buttons to step through the speeds on both sides of the speed slider
      var group = (round || square) && bar.querySelector(".ds-speed");
      if (group) {
        var sl = group.querySelector("input");
        var step = function (txt, by, title) {
          var b = document.createElement("button");
          b.type = "button";
          b.className = "ds-step";
          b.textContent = txt;
          b.title = title;
          b.addEventListener("click", function () {
            var v = Math.max(0, Math.min(+sl.max, +sl.value + by));
            if (v !== +sl.value) { sl.value = v; sl.dispatchEvent(new Event("input")); }
          });
          return b;
        };
        group.insertBefore(step("\\u2212", -1, "slower"), sl);
        sl.insertAdjacentElement("afterend", step("+", 1, "faster"));
      }

      if (has("loop")) {
        var lp = button("ds-loop", "\\u21BB loop", "");
        var setLoop = function () {
          lp.setAttribute("aria-pressed", a.loop);
          lp.title = a.loop ? "playing repeatedly (click to stop looping)" : "click to play repeatedly";
          paintBtn(lp, a.loop, color);
        };
        lp.addEventListener("click", function () { a.loop = !a.loop; setLoop(); });
        setLoop();
      }

      if (has("volume")) {
        var vol = document.createElement("span");
        vol.className = "ds-vol";
        var mute = document.createElement("button");
        mute.type = "button";
        var slider = document.createElement("input");
        slider.type = "range";
        slider.min = 0; slider.max = 1; slider.step = 0.05; slider.value = a.volume;
        slider.style.accentColor = color[0];
        slider.setAttribute("aria-label", "volume");
        var setVol = function () {
          var off = a.muted || a.volume === 0;
          mute.textContent = off ? "\\uD83D\\uDD07" : "\\uD83D\\uDD0A";
          mute.title = off ? "unmute" : "mute";
          mute.setAttribute("aria-pressed", off);
          slider.value = a.muted ? 0 : a.volume;
        };
        mute.addEventListener("click", function () {
          if (a.volume === 0) { a.volume = 1; a.muted = false; } else a.muted = !a.muted;
          setVol();
        });
        slider.addEventListener("input", function () { a.volume = +slider.value; a.muted = false; setVol(); });
        vol.appendChild(mute);
        vol.appendChild(slider);
        bar.appendChild(vol);
        setVol();
      }

      if (has("download")) {
        var mp3 = a.src.indexOf("data:audio/mpeg") === 0;
        button("ds-dl", "\\u2913 download", "save the sound").addEventListener("click", function () {
          var link = document.createElement("a");
          link.href = a.src;
          link.download = (d.name || "sound") + (mp3 ? ".mp3" : ".wav");
          document.body.appendChild(link);
          link.click();
          link.remove();
        });
      }

      if (has("fullscreen") && div.requestFullscreen && document.fullscreenEnabled) {
        var fs = button("ds-fs", "\\u26F6 full screen", "show in full screen");
        fs.addEventListener("click", function () {
          // the request can be refused (e.g. in embedded frames)
          if (document.fullscreenElement === div) document.exitFullscreen(); else div.requestFullscreen().catch(function () {});
        });
        document.addEventListener("fullscreenchange", function () {
          var on = document.fullscreenElement === div, img = wrap.querySelector("img");
          fs.textContent = on ? "\\u2715 exit full screen" : "\\u26F6 full screen";
          fs.title = on ? "exit full screen" : "show in full screen";
          // widest size at which the figure and the buttons fit in the screen
          var w = on && img.naturalHeight ? Math.max(200, (window.innerHeight - bar.offsetHeight - 64) * img.naturalWidth / img.naturalHeight) + "px" : "";
          wrap.style.maxWidth = bar.style.maxWidth = w;
          redraw();
        });
      }
    };

    // captions: text at a fixed position of the figure shown (fading in and out) during a time range. Settings
    // are in a script element of class ds-captions; returns a function that updates captions at time t
    var captionsInit = function (div, wrap) {
      var el = div.querySelector("script.ds-captions");
      if (!el) return function () {};
      var c = JSON.parse(el.textContent);
      var arr = function (x) { return Array.isArray(x) ? x : [x]; };
      var items = c.layers.map(function (l) {
        var node = document.createElement("div");
        node.className = "ds-text";
        node.style.color = l.col;
        node.style.fontWeight = l.font === 2 || l.font === 4 ? "bold" : "normal";
        node.style.fontStyle = l.font === 3 || l.font === 4 ? "italic" : "normal";
        if (l.family) node.style.fontFamily = l.family;
        node.style.left = 100 * l.x + "%";
        node.style.top = 100 * l.y + "%";
        node.style.transform = "translate(-50%,-50%)";
        wrap.appendChild(node);
        return {node: node, l: l, labels: arr(l.labels), start: arr(l.start), end: arr(l.end)};
      });
      return function (t) {
        // text size follows the size of the figure (12 points at the resolution of the figure)
        var px = 12 * c.res / 72 * wrap.clientWidth / c.width;
        items.forEach(function (it) {
          var op = 0, txt = "", f = Math.max(it.l.fading, 1e-3);
          it.labels.forEach(function (lab, i) {
            var t0 = it.start[i], t1 = it.end[i];
            if (t >= t0 && t <= t1) { var o = Math.min(1, (t - t0) / f, (t1 - t) / f); if (o > op) { op = o; txt = lab; } }
          });
          it.node.textContent = txt;
          it.node.style.opacity = op;
          it.node.style.fontSize = it.l.cex * px + "px";
        });
      };
    };

    var initPlayer = function (div) {
      var d = div.dataset, img = div.querySelector("img"), a = div.querySelector("audio");
      var dur = +d.duration;
      // one row per sound segment: start and end time, and extents of the panels (as proportions of the figure)
      var rows = d.rows.split(";").map(function (r) {
        r = r.split(",").map(Number);
        return {t0: r[0], t1: r[1], left: r[2], right: r[3], top: r[4], bottom: r[5]};
      });
      var rowAt = function (t) {
        for (var i = 0; i < rows.length - 1; i++) if (t < rows[i].t1) return rows[i];
        return rows[rows.length - 1];
      };
      var speeds = d.speeds.split(",").map(Number);
      var keep = d.keepPitch === "true";
      setup(a, keep);
      a.playbackRate = speeds[0];

      var wrap = document.createElement("div");
      wrap.className = "ds-wrap";
      img.parentNode.insertBefore(wrap, img);
      wrap.appendChild(img);
      var head = document.createElement("div");
      head.className = "ds-head";
      head.style.background = d.color;
      // no moving line when play.line is FALSE (clicking on the figure still plays from that point)
      if (d.line === "false") head.style.display = "none";
      wrap.appendChild(head);

      var bar = document.createElement("div");
      bar.className = "ds-bar";
      var play = document.createElement("button");
      play.type = "button";
      play.className = "ds-play";
      play.textContent = "\\u25B6 play";
      bar.appendChild(play);
      speedGroup(bar, speeds, [d.color, d.textColor], keep, [a], function (r) { a.playbackRate = r; });
      var time = document.createElement("span");
      time.className = "ds-time";
      bar.appendChild(time);
      wrap.insertAdjacentElement("afterend", bar);
      var captions = captionsInit(div, wrap);

      var raf, fmt = function (t) { return dur < 5 ? t.toFixed(3) : t.toFixed(1); };
      var draw = function () {
        var t = Math.min(a.currentTime, dur), r = rowAt(t);
        captions(t);
        head.style.left = 100 * (r.left + (r.right - r.left) * (t - r.t0) / (r.t1 - r.t0)) + "%";
        head.style.top = 100 * r.top + "%";
        head.style.height = 100 * (r.bottom - r.top) + "%";
        time.textContent = fmt(t) + " / " + fmt(dur) + " s";
        if (!a.paused) raf = requestAnimationFrame(draw);
      };
      draw();
      extraButtons(div, bar, wrap, a, time, [d.color, d.textColor], draw, dur);
      a.addEventListener("play", function () { setPlay(play, true); draw(); });
      a.addEventListener("pause", function () { setPlay(play, false); cancelAnimationFrame(raf); draw(); });
      a.addEventListener("ended", function () { a.currentTime = 0; draw(); });
      a.addEventListener("seeked", draw);
      play.addEventListener("click", function () { a.paused ? a.play() : a.pause(); });
      wrap.addEventListener("click", function (e) {
        var b = wrap.getBoundingClientRect(), y = (e.clientY - b.top) / b.height;
        // row clicked (or the closest one)
        var r = rows.reduce(function (best, x) {
          var dist = y < x.top ? x.top - y : y > x.bottom ? y - x.bottom : 0;
          return dist < best.dist ? {row: x, dist: dist} : best;
        }, {row: rows[0], dist: Infinity}).row;
        var f = ((e.clientX - b.left) / b.width - r.left) / (r.right - r.left);
        a.currentTime = Math.max(0, Math.min(dur - 0.001, r.t0 + f * (r.t1 - r.t0)));
        a.play();
      });
    };

    // paged spectrograms: one image per page, a highlight box over the part of the page not played yet
    // and a cursor at its leading edge (as in paged_spectro() videos)
    var initPaged = function (div) {
      var d = div.dataset, imgs = Array.prototype.slice.call(div.querySelectorAll("img")), a = div.querySelector("audio");
      var dur = +d.duration;
      var pages = d.rows.split(";").map(function (r) {
        r = r.split(",").map(Number);
        return {t0: r[0], t1: r[1], left: r[2], right: r[3], top: r[4], bottom: r[5]};
      });
      var pageAt = function (t) {
        for (var i = 0; i < pages.length - 1; i++) if (t < pages[i].t1) return i;
        return pages.length - 1;
      };
      var speeds = d.speeds.split(",").map(Number);
      var keep = d.keepPitch === "true";
      setup(a, keep);
      a.playbackRate = speeds[0];

      var wrap = document.createElement("div");
      wrap.className = "ds-wrap";
      imgs[0].parentNode.insertBefore(wrap, imgs[0]);
      imgs.forEach(function (img, i) { wrap.appendChild(img); img.style.display = i ? "none" : "block"; });
      var box = document.createElement("div");
      box.className = "ds-box";
      box.style.background = d.highlight;
      box.style.opacity = d.alpha;
      wrap.appendChild(box);
      var cursor = document.createElement("div");
      cursor.className = "ds-box";
      cursor.style.background = d.cursor;
      cursor.style.width = "4px";
      cursor.style.marginLeft = "-2px";
      wrap.appendChild(cursor);

      var bar = document.createElement("div");
      bar.className = "ds-bar";
      var play = document.createElement("button");
      play.type = "button";
      play.className = "ds-play";
      play.textContent = "\\u25B6 play";
      bar.appendChild(play);
      var prev = document.createElement("button"), next = document.createElement("button"), label = document.createElement("span");
      prev.type = next.type = "button";
      prev.textContent = "\\u25C0";
      prev.title = "previous page";
      next.textContent = "\\u25B6";
      next.title = "next page";
      label.className = "ds-page";
      if (pages.length > 1) [prev, label, next].forEach(function (x) { bar.appendChild(x); });
      speedGroup(bar, speeds, [d.color, d.textColor], keep, [a], function (r) { a.playbackRate = r; });
      var time = document.createElement("span");
      time.className = "ds-time";
      bar.appendChild(time);
      wrap.insertAdjacentElement("afterend", bar);
      var captions = captionsInit(div, wrap);

      var raf, shown = 0, fmt = function (t) { return dur < 5 ? t.toFixed(3) : t.toFixed(1); };
      var draw = function () {
        var t = Math.min(a.currentTime, dur), k = pageAt(t), p = pages[k];
        captions(t);
        if (k !== shown) { imgs[shown].style.display = "none"; imgs[k].style.display = "block"; shown = k; }
        var x = p.left + (p.right - p.left) * Math.min(1, (t - p.t0) / (p.t1 - p.t0));
        [box, cursor].forEach(function (el) {
          el.style.top = 100 * p.top + "%";
          el.style.height = 100 * (p.bottom - p.top) + "%";
          el.style.left = 100 * x + "%";
        });
        box.style.width = 100 * Math.max(0, p.right - x) + "%";
        label.textContent = (k + 1) + " / " + pages.length;
        time.textContent = fmt(t) + " / " + fmt(dur) + " s";
        if (!a.paused) raf = requestAnimationFrame(draw);
      };
      draw();
      extraButtons(div, bar, wrap, a, time, [d.color, d.textColor], draw, dur);
      var go = function (k) {
        a.currentTime = pages[Math.max(0, Math.min(pages.length - 1, k))].t0;
        draw();
      };
      prev.addEventListener("click", function () { go(pageAt(a.currentTime) - 1); });
      next.addEventListener("click", function () { go(pageAt(a.currentTime) + 1); });
      a.addEventListener("play", function () { setPlay(play, true); draw(); });
      a.addEventListener("pause", function () { setPlay(play, false); cancelAnimationFrame(raf); draw(); });
      a.addEventListener("ended", function () { a.currentTime = 0; draw(); });
      a.addEventListener("seeked", draw);
      play.addEventListener("click", function () { a.paused ? a.play() : a.pause(); });
      wrap.addEventListener("click", function (e) {
        var b = wrap.getBoundingClientRect(), p = pages[shown];
        var f = Math.max(0, Math.min(1, ((e.clientX - b.left) / b.width - p.left) / (p.right - p.left)));
        a.currentTime = Math.min(dur - 0.001, p.t0 + f * (p.t1 - p.t0));
        a.play();
      });
    };

    // scrolling spectrograms: the full spectrogram (and oscillogram) moves below a frame with the parts that do
    // not move (axes, play line, lower spectrogram), as in scrolling_spectro() videos
    var initScrolling = function (div) {
      var d = div.dataset, c = JSON.parse(div.querySelector("script[type=\'application/json\']").textContent);
      var frame = div.querySelector("img.ds-frame"), strips = div.querySelectorAll("img.ds-strip"), a = div.querySelector("audio");
      var speeds = d.speeds.split(",").map(Number), keep = d.keepPitch === "true";
      setup(a, keep);
      a.playbackRate = speeds[0];
      var pct = function (x) { return 100 * x + "%"; };
      var place = function (el, left, right, top, bottom) {
        el.style.left = pct(left); el.style.width = pct(Math.max(0, right - left));
        el.style.top = pct(top); el.style.height = pct(Math.max(0, bottom - top));
      };

      var wrap = document.createElement("div");
      wrap.className = "ds-wrap";
      wrap.style.background = c.bg;
      frame.parentNode.insertBefore(wrap, frame);
      wrap.appendChild(frame);

      // scrolling strips (viewports clip the part shown)
      var viewport = function (strip, top, bottom) {
        var vp = document.createElement("div");
        vp.className = "ds-vp";
        place(vp, c.left, c.right, top, bottom);
        vp.appendChild(strip);
        wrap.appendChild(vp);
        return strip;
      };
      // zoom: duration shown at once is tDisplay * zoom (strips stretch or shrink around the play line)
      var zoom = 1;
      var moving = [viewport(strips[0], c.upTop, c.upBottom)];
      if (c.lower === "osc") moving.push(viewport(strips[1], c.lowTop, c.lowBottom));

      // window in the lower spectrogram: gray boxes outside the window and a thin box around it
      var over = function (bg, border) {
        var el = document.createElement("div");
        el.className = "ds-over";
        if (bg) el.style.background = bg;
        if (border) el.style.border = "1px solid rgba(0,0,0,0.55)";
        el.style.boxSizing = "border-box";
        wrap.appendChild(el);
        return el;
      };
      var grays = [], boxes = [];
      if (c.lower === "spectro") {
        grays = [over(c.dim), over(c.dim)];
        boxes = [over(null, true), over(null, true)];
      }
      // horizontal segment [x0, x1] of the lower spectrogram (proportions of its width)
      var seg = function (el, x0, x1) {
        if (x1 <= x0) { el.style.display = "none"; return; }
        el.style.display = "block";
        place(el, c.left + (c.right - c.left) * x0, c.left + (c.right - c.left) * x1, c.lowTop, c.lowBottom);
      };

      // moving time axis (when time is not fixed)
      var ticks = [];
      var label = function () {
        var el = document.createElement("div");
        el.className = "ds-text";
        el.style.transform = "translateX(-50%)";
        var tick = document.createElement("div");
        tick.className = "ds-over";
        tick.style.width = "1px";
        tick.style.background = c.colLab;
        el.style.color = c.colLab;
        wrap.appendChild(el);
        wrap.appendChild(tick);
        return {text: el, tick: tick};
      };

      var bar = document.createElement("div");
      bar.className = "ds-bar";
      var play = document.createElement("button");
      play.type = "button";
      play.className = "ds-play";
      play.textContent = "\\u25B6 play";
      bar.appendChild(play);
      speedGroup(bar, speeds, [d.color, d.textColor], keep, [a], function (r) { a.playbackRate = r; });
      // zoom slider (player(buttons = "zoom")): from showing up to 4 times longer (at most the whole sound) to 4
      // times shorter; slider values are -log2(zoom), so zooming in is to the right
      if (c.zoom) {
        var zg = document.createElement("span"), zs = document.createElement("input"), zv = document.createElement("span");
        zg.className = "ds-zoom";
        var zsep = document.createElement("span");
        zsep.className = "ds-sep";
        zsep.textContent = "zoom";
        zs.type = "range";
        zs.min = -Math.log2(Math.max(1, Math.min(4, c.durWave / c.tDisplay))); zs.max = 2; zs.step = 0.25; zs.value = 0;
        zs.style.accentColor = d.color;
        zs.setAttribute("aria-label", "zoom");
        zv.className = "ds-rate";
        var setZoom = function (v) {
          v = Math.max(+zs.min, Math.min(+zs.max, v));
          zs.value = v;
          zoom = Math.pow(2, -v);
          var T = c.tDisplay * zoom;
          zv.textContent = (T < 1 ? T.toFixed(2) : T < 10 ? T.toFixed(1) : T.toFixed(0)) + " s";
          zs.title = "time shown: " + zv.textContent;
          draw();
        };
        var zstep = function (txt, by, title) {
          var b = document.createElement("button");
          b.type = "button";
          b.className = "ds-step";
          b.textContent = txt;
          b.title = title;
          b.addEventListener("click", function () { setZoom(+zs.value + by); });
          return b;
        };
        zs.addEventListener("input", function () { setZoom(+zs.value); });
        [zsep, zstep("\\u2212", -0.5, "zoom out"), zs, zstep("+", 0.5, "zoom in"), zv].forEach(function (x) { zg.appendChild(x); });
        bar.appendChild(zg);
      }

      var time = document.createElement("span");
      time.className = "ds-time";
      bar.appendChild(time);
      wrap.insertAdjacentElement("afterend", bar);
      var captions = captionsInit(div, wrap);

      // start of the window shown (time of the sound with the half windows added at both ends), with zoom
      var winStart = function (t) {
        var s0 = Math.min(Math.max(0, t * c.k - c.buffer), c.durSil - c.tDisplay);
        return s0 + c.tDisplay * (1 - zoom) / 2;
      };
      // tick step of the time labels (as R pretty() for the window of the figure)
      var tickStep = function (T) {
        if (zoom === 1) return c.tickStep;
        var raw = T / 5, mag = Math.pow(10, Math.floor(Math.log10(raw))), r = raw / mag;
        return (r < 1.5 ? 1 : r < 3 ? 2 : r < 7 ? 5 : 10) * mag;
      };

      var dur = c.duration, raf, fmt = function (t) { return dur < 5 ? t.toFixed(3) : t.toFixed(1); };
      var draw = function () {
        var t = Math.min(a.currentTime, dur), T = c.tDisplay * zoom;
        // text size follows the size of the figure (12 points at the resolution of the frame)
        var px = 12 * c.res / 72 * wrap.clientWidth / c.width;

        var s = winStart(t);
        moving.forEach(function (strip) { strip.style.width = pct(c.durSil / T); strip.style.left = pct(-s / T); });

        if (c.lower === "spectro") {
          var w = T / c.durWave, mid = Math.max(0, t * c.kl - c.buffer / c.durWave), x0 = mid - w / 2, x1 = mid + w / 2;
          if (c.loop && x1 > 1) {
            seg(boxes[0], x0, 1); seg(boxes[1], 0, x1 - 1); seg(grays[0], x1 - 1, x0); seg(grays[1], 0, 0);
          } else if (c.loop && x0 < 0) {
            seg(boxes[0], 0, x1); seg(boxes[1], 1 + x0, 1); seg(grays[0], x1, 1 + x0); seg(grays[1], 0, 0);
          } else {
            seg(boxes[0], Math.max(0, x0), Math.min(1, x1)); seg(boxes[1], 0, 0);
            seg(grays[0], 0, Math.max(0, x0)); seg(grays[1], Math.min(1, x1), 1);
          }
        }

        if (c.movingAxis) {
          // labels show the time of the sound (the strip starts with half a window of silence, not labeled), or the
          // time of the window when time is fixed
          var step = tickStep(T), digits = Math.max(0, -Math.floor(Math.log10(step) + 1e-9));
          var half = c.fixTime ? s : c.tDisplay / 2, first = Math.ceil((s - half) / step - 1e-9), n = Math.floor((s + T - half) / step + 1e-9) - first + 1;
          while (ticks.length < n) ticks.push(label());
          ticks.forEach(function (tk, i) {
            var v = (first + i) * step, on = i < n && v >= -1e-9 && (c.fixTime || v <= c.durWave + 1e-9), x = c.left + (c.right - c.left) * (v + half - s) / T;
            tk.text.style.display = tk.tick.style.display = on ? "block" : "none";
            if (!on) return;
            // (labels with units without trailing zeros, e.g. "3s", as in R; compact axes: minutes and seconds)
            tk.text.textContent = c.top ? mmss(v, digits) : c.unit ? parseFloat(v.toFixed(digits)) + c.unit : v.toFixed(digits);
            tk.text.style.fontSize = c.cexAxis * px + "px";
            tk.text.style.left = pct(x);
            // labels and ticks placed as in R axes (measured on frames drawn by R; a line of text is 1.2 px)
            tk.text.style.top = "calc(" + pct(c.lowBottom) + " + " + (1.2 * c.labLine + 0.15) * px + "px)";
            // (labels with units are hidden close to the figure edges, as in R)
            if (c.unit) tk.text.style.visibility = x < 0.02 || x > 0.98 ? "hidden" : "visible";
            tk.tick.style.left = pct(x);
            tk.tick.style.top = pct(c.lowBottom);
            tk.tick.style.height = 1.2 * c.tickLines * px + "px";
            // compact axes: ticks across the top margin and labels to their right
            if (c.top) {
              tk.text.style.transform = "translateY(-50%)";
              tk.text.style.left = "calc(" + pct(x) + " + " + 0.2 * px + "px)";
              tk.text.style.top = pct(c.upTop / 2);
              tk.text.style.visibility = x > 0.96 ? "hidden" : "visible";
              tk.tick.style.top = "0";
              tk.tick.style.height = pct(c.upTop);
            }
          });
        }

        captions(t);

        time.textContent = fmt(t) + " / " + fmt(dur) + " s";
        if (!a.paused) raf = requestAnimationFrame(draw);
      };
      draw();
      if (c.zoom) setZoom(0);
      window.addEventListener("resize", draw);
      extraButtons(div, bar, wrap, a, time, [d.color, d.textColor], draw, dur);
      a.addEventListener("play", function () { setPlay(play, true); draw(); });
      a.addEventListener("pause", function () { setPlay(play, false); cancelAnimationFrame(raf); draw(); });
      a.addEventListener("ended", function () { a.currentTime = 0; draw(); });
      a.addEventListener("seeked", draw);
      play.addEventListener("click", function () { a.paused ? a.play() : a.pause(); });

      // clicking on the scrolling spectrogram moves that point to the play line; clicking on the lower
      // spectrogram plays from that point of the sound
      wrap.addEventListener("click", function (e) {
        var b = wrap.getBoundingClientRect(), x = (e.clientX - b.left) / b.width, y = (e.clientY - b.top) / b.height;
        var f = Math.max(0, Math.min(1, (x - c.left) / (c.right - c.left))), orig;
        if (c.lower !== "none" && y > c.lowTop) {
          orig = f * c.durWave;
        } else {
          orig = winStart(a.currentTime) + f * c.tDisplay * zoom - c.tDisplay / 2;
        }
        a.currentTime = Math.max(0, Math.min(dur - 0.001, (Math.max(0, orig) + c.buffer) / c.k));
        a.play();
      });
    };


    window.dynaSpecPlayers = function () {
      document.querySelectorAll(".dynaspec-player:not([data-ready])").forEach(function (div) {
        div.setAttribute("data-ready", "");
        initPlayer(div);
      });
      document.querySelectorAll(".dynaspec-paged:not([data-ready])").forEach(function (div) {
        div.setAttribute("data-ready", "");
        initPaged(div);
      });
      document.querySelectorAll(".dynaspec-scroll:not([data-ready])").forEach(function (div) {
        div.setAttribute("data-ready", "");
        initScrolling(div);
      });
    };
  }
  window.dynaSpecPlayers();
})();
</script>'
}

### scrolling_spectro() helpers -------------------------------------------------------------------------


#internal dynaSpec function called by scrolling_spectro. Modified from seewave::oscillo. Not to be called by users.

oscillo_dynaspec_int <- function (wave, f, channel = 1, tlab = "Time (s)", alab = "Amplitude",  colwave = "black", coltitle = "black", 
          cextitle = 1.2, fonttitle = 2, collab = "black", cexlab = 1, fontlab = 1, colline = "black", colaxis = "black", cexaxis = 1, 
          fontaxis = 1, coly0 = "lightgrey", tcl = 0.5, cex = 1,
          xaxt = "s", yaxt = "n", type = "l", bty = "l", labels = FALSE, bg = "white") 
{
  input <- seewave::inputw(wave = wave, f = f, channel = channel)
  wave <- input$w
  f <- input$f
  rm(input)
    a <- 0
    b <- length(wave)
    from <- 0
    to <- length(wave)/f
  
  wave <- as.matrix(wave[a:b, ])
  n <- nrow(wave)
  
  
    alim <- max(abs(wave))
      
        op <- graphics::par(tcl = tcl, col.axis = colaxis, cex.axis = cexaxis, 
                  font.axis = fontaxis, col = colline, las = 0, bg = bg)

        on.exit(graphics::par(op))
        
                plot(x = seq(from, to, length.out = n), y = wave, 
             col = colwave, type = type, cex = cex, xaxs = "i", 
             yaxs = "i", xlab = "", ylab = "", ylim = c(-alim, 
                                                        alim), xaxt = xaxt, yaxt = yaxt, cex.lab = 0.8, 
             font.lab = 2, bty = bty)
        if (bty == "l" | bty == "o") {
          graphics::axis(side = 1, col = colline, labels = FALSE)
          graphics::axis(side = 2, at = max(abs(wave), na.rm = TRUE), 
               col = colline, labels = FALSE)
        }
        if (labels) {
          graphics::mtext(tlab, col = collab, font = fontlab, 
                side = 1, line = 3, cex = cexlab)
          graphics::mtext(alab, col = collab, font = fontlab, 
                cex = cexlab, side = 2, line = 3)
        }
                graphics::abline(h = 0, col = coly0, lty = 2)
    
}

### prep_paged_spectro() and paged_spectro() helpers ----------------------------------------------------


# processSound: filter, crop, and segment wav (internal function)

processSound <- function(wav0, filter, ampThresh, crop, xLim, verbose = TRUE, ...) {

  smplRt <- wav0@samp.rate
  
  fileDur0 <- max(length(wav0@left), length(wav0@right)) / smplRt
  
  ##Figure out crop & spectrogram temporal window width
  
  #If crop provided as single digit, interpret as first X sec
  if (!is.null(crop)) {
    if (length(crop) == 1) {
      if (crop == F) {
        crop <- c(0, fileDur0)
      } else{
        crop <- c(0, crop)
      }
    }
    #If supplied crop end is longer than wav, crop end set to duration
    if (crop[2] > fileDur0) {
      crop[2] <- fileDur0
      if (crop[1] >= crop[2]) crop[1] <- 0
      if (verbose) message("Crop longer than file duration (", round(fileDur0, 2), " s), crop end set to file duration")
    }
  } else{
    #If crop not supplied, default crop to duration, unless >10 sec, warning user
    if (fileDur0 > 10) {
      crop = c(0, 10)
      if (verbose) message("Cropping to 1st 10 sec due to slow processing of large WAV files. To override, set crop = FALSE or crop = c(startInSec, stopInSec)")
    } else{
      crop = c(0, fileDur0)
    }
  }
  
  #crop is set now for all cases
  
  
  # Deal with xLim for segmenting wavs --------------------------------------
  # if xLim not provided, default to smaller of 5sec or wav duration
  cropped_dur <- crop[2] - crop[1]
  
  if (is.null(xLim)) {
    xLim <- c(0, min(5, cropped_dur))
    if (xLim[2] == 5) {
      if (verbose) message("xLim set to 5 sec by default; define to override")
    }
  } else{
    #If xLim provided as single digit, interpret as X sec
    if (length(xLim) == 1) {
      xLim <- c(0, xLim)
    }
  }
  
  #If user wants to have xLim be Infinite (override 5sec max for long files)
  if (xLim[2] %in% c(Inf, "Inf")) {
    xLim[2] <- cropped_dur
  }
  
  
  #assign new wave file for use going forward
  #the bit=wav0@bit is REALLY important! Should be standard, but it doesn't work
  
  if (crop[1] == 0 && crop[2] == fileDur0) {
    wav <- wav0
  } else{
    wav <- seewave::cutw(
      wav0,
      from = crop[1],
      to = crop[2],
      output = "Wave",
      bit = wav0@bit
    )
  }
  wavDur <- max(length(wav@left), length(wav@right)) / wav@samp.rate
  
  #Apply filters
  if (!is.null(filter)) {
    wav = seewave::ffilter(
      wave = wav,
      from = filter[1] * 1000,
      to = filter[2] * 1000,
      bandpass = F,
      output = "Wave",
      rescale = T
    )
  }
  if (ampThresh != 0) {
    wav <-
      seewave::afilter(
        wav,
        f = smplRt,
        threshold = ampThresh,
        plot = F,
        output = "Wave"
      )
  }
  
  
  
  # Make wav file match page xlims ------------------------------------------
  #Given xLim, what is the full length the recording needs to be?
  max_page_dur <- ceiling( round(wavDur,digits=2) / round(xLim[2],digits=2)) * xLim[2]
  
  timeRemainder <-
    (max_page_dur - wavDur)
  
  is_timeRemainder <- timeRemainder > 0
  
  
  #If file is longer than max_page_dur, trim it
  if (wavDur > max_page_dur) {
    #trim the wav file again to match a multiple of xLim pages (wav is already cropped, so start at 0)
    wav <- seewave::cutw(
      wav,
      from = 0,
      to = max_page_dur,
      output = "Wave",
      bit = wav0@bit
    )
    
    #Else fill out remainders with filler silence
  } else if (is_timeRemainder) {
    fillerWAV <-
      tuneR::silence(
        duration = timeRemainder,
        samp.rate = smplRt,
        xunit = "time",
        pcm = T,
        bit = wav@bit
      )
    wav <- tuneR::bind(wav, fillerWAV)
  }
  
    #pastew results in intermittent problems! Don't use! bind seems much more dependable
    #seewave::pastew(wave1=fillerWAV,wave2=wav,at="end",output="Wave",join=T,bit=wav0@bit)
    wavDur <- max(length(wav@left), length(wav@right)) / wav@samp.rate
  
  #Segment wav or make list of 1 if no segmentation
  #The ceiling code here is to deal with e.g. duration of 29.999 and xLim=10 that would only produce 2 segments
  
  segLens <- seq(0, ceiling(wavDur / xLim[2]) * xLim[2], xLim[2])
  indx <- 1:(length(segLens) - 1)
  segWavs <-
    lapply(indx, function(i)
      seewave::cutw(
        wav,
        from = segLens[i],
        to = segLens[i + 1],
        output = "Wave",
        bit = wav0@bit
      ))
  n_pages = length(segWavs)
  if (verbose) message(
    "Segmented original file into ",
    n_pages,
    " WAV files, each ",
    diff(xLim),
    " seconds long."
  )
  return(
    list(
      newWav = wav,
      n_pages = n_pages,
      segWavs = segWavs,
      wavDur = wavDur,
      segLens = segLens,
      xLim = xLim
    )
  )
}#End processSound

# ggSpec: helper function to create ggplot spectrogram of wave file

# spectrogram data of each page of a paged spectrogram: a list with time (s), frequency (kHz) and amplitude
# (dB, relative to the loudest point of the whole sound) of each page, restricted to the frequency limits
paged_spec_data_int <- function(wav, resampleRate, yLim, segLens, wl, ovlp, wn, verbose = TRUE, ...) {
  if (!is.null(resampleRate)) {
    # keep the Nyquist frequency at or above the upper frequency limit
    if (resampleRate < yLim[2] * 2000) {
      resampleRate <- yLim[2] * 2000
      if (verbose) message("resample.rate increased to ", resampleRate, " Hz so the spectrogram reaches the upper frequency limit (", yLim[2], " kHz)")
    }

    # only downsample (upsampling is not supported), and only when it reduces the sound noticeably
    # (tuneR::downsample() does not filter, so small rate changes add artifacts without saving time)
    if (resampleRate < 0.9 * wav@samp.rate) {
      if (verbose) message("Original wav (", wav@samp.rate, " samples/sec) data points: ", length(wav@left))
      wav <- tuneR::downsample(wav, samp.rate = resampleRate)
      if (verbose) message("Downsampled wav (", wav@samp.rate, " samples/sec) data points: ", length(wav@left))
    }
  }

  sp <- seewave::spectro(wav, plot = FALSE, ovlp = ovlp, wl = wl, wn = wn, ...)
  in_flim <- sp$freq >= yLim[1] & sp$freq <= yLim[2]

  n_pages <- length(segLens) - 1
  lapply(seq_len(n_pages), function(i) {
    # last page is inclusive on both bounds, all other pages on the left one
    in_page <- sp$time >= segLens[i] & (if (i == n_pages) sp$time <= segLens[i + 1] else sp$time < segLens[i + 1])
    list(time = sp$time[in_page], freq = sp$freq[in_flim], amp = sp$amp[in_flim, in_page, drop = FALSE])
  })
}

# colors of a paged spectrogram palette (viridis palette name, 2 colors for a gradient, or palette function)
paged_palette_int <- function(colPal, n = 256) {
  if (is.function(colPal)) colPal(n) else if (length(colPal) == 1) viridis::viridis(n, option = colPal) else grDevices::colorRampPalette(colPal)(n)
}

# modulus transformation (as scales::modulus_trans()), used to brighten spectrograms ('amp.trans')
modulus_int <- function(x, p) {
  if (abs(p) < 1e-07) sign(x) * log(abs(x) + 1) else sign(x) * ((abs(x) + 1)^p - 1) / p
}

# draw page i of a paged spectrogram (output of prep_paged_spectro()) on the current device. Text sizes are
# in points, so pages look the same at any resolution for a given size in inches ('spec.width' x 'spec.height').
# Returns the time range of the page and the extents of the highlight box (proportions of the figure, from the
# top left corner)
draw_page_int <- function(sp, i) {
  d <- sp$spec[[i]]
  range_i <- c((i - 1) * sp$xLim[2], i * sp$xLim[2])

  # colors of the amplitude bands (from min.db to 0 dB), along the transformed amplitude scale
  levels <- seq(sp$min_dB, 0, length.out = sp$colbins + 1)
  mids <- (levels[-1] + levels[-length(levels)]) / 2
  tr <- modulus_int(c(mids, sp$min_dB, 0), sp$ampTrans)
  pos <- (tr[seq_along(mids)] - tr[length(mids) + 1]) / (tr[length(mids) + 2] - tr[length(mids) + 1])
  ramp <- grDevices::colorRamp(paged_palette_int(sp$colPal), space = "Lab")
  cols <- grDevices::rgb(ramp(pmin(pmax(pos, 0), 1)), maxColorValue = 255)

  # text color: supplied, contrasting with the background when it floods the figure, or black
  axes <- !sp$onlyPlotSpec
  flood <- sp$onlyPlotSpec || sp$bgFlood
  th <- if (is.null(sp$theme)) list() else sp$theme
  lab_cex <- theme_int(th, "cex.lab", 1)
  fig_bg <- theme_int(th, "bg", if (flood) sp$bg else "white")
  txt_col <- theme_int(th, "col.lab", if (!sp$autoFontCol) sp$fontAndAxisCol else contrast_col_int(fig_bg))
  has_title <- !is.null(sp$title) && nzchar(sp$title)
  # labels inside the figure (no axis titles) or compact axes
  inside <- axes && identical(th$axis.type, "inside")
  compact <- axes && identical(th$axis.type, "compact")
  cex_axis <- 16 / 12 * theme_int(th, "cex.axis", 1)
  tcl <- -theme_int(th, "tick.length", 0.3)

  op <- graphics::par(
    bg = fig_bg,
    mar = if (inside) c(0.45 + cex_axis, 0, if (has_title) 3 else 0, if (sp$plotLegend) 7 else 0) else
      if (compact) c(0.3, 0.6 + 1.6 * cex_axis, 0.4 + 1.1 * cex_axis + if (has_title) 2.4 else 0, if (sp$plotLegend) 7 else 0.3) else
      c(if (axes) 2.2 + 2 * lab_cex else 0, if (axes) 2.8 + 2.6 * lab_cex else 0, if (has_title) 3 else if (axes) 1.2 else 0, if (sp$plotLegend) 7 else if (axes) 1.2 else 0),
    col.axis = txt_col, col.lab = txt_col, col.main = theme_int(th, "col.title", txt_col), fg = txt_col, las = theme_int(th, "las", 1),
    family = theme_int(th, "family", "sans"), tcl = tcl
  )
  on.exit(graphics::par(op), add = TRUE)
  box_col <- theme_int(th, "col.box", "gray20")

  # axis ranges expanded 5% on each side (as in previous versions)
  graphics::plot.new()
  graphics::plot.window(xlim = range_i + c(-1, 1) * 0.05 * diff(range_i), ylim = sp$yLim + c(-1, 1) * 0.05 * diff(sp$yLim), xaxs = "i", yaxs = "i")
  usr <- graphics::par("usr")
  graphics::rect(usr[1], usr[3], usr[2], usr[4], col = sp$bg, border = NA)

  if (length(d$time) > 1 && length(d$freq) > 1)
    graphics::.filled.contour(x = d$time, y = d$freq, z = t(d$amp), levels = levels, col = cols)
  theme_grid_int(th, at = pretty(sp$yLim))

  # selections
  if (length(d$time) > 1 && length(d$freq) > 1)
    color_annotations_int(sp$layers, d$time, d$freq, t(d$amp), levels = levels, colbg = sp$bg, ylim = usr[3:4], pos = pos)
  draw_layers_int(sp$layers, panel = "spectro", ylim = usr[3:4])

  if (compact) {
    compact_freq_axis_int(sp$yLim, col = txt_col, cex = cex_axis)
    at <- pretty(range_i)
    at <- at[at >= usr[1] & at <= usr[2]]
    compact_time_axis_int(at, at, col = txt_col, cex = cex_axis, band = 0.4 + 1.1 * cex_axis)
    if (!isFALSE(th$box)) graphics::box(col = box_col)
  } else if (inside) {
    inside_freq_axis_int(sp$yLim, col = txt_col, cex = cex_axis)
    at <- pretty(range_i)
    at <- at[at >= usr[1] & at <= usr[2]]
    inside_time_axis_int(at, at, col = txt_col, cex = cex_axis)
    if (!isFALSE(th$box)) graphics::box(col = box_col)
  } else if (axes) {
    graphics::axis(1, cex.axis = cex_axis, tcl = tcl, mgp = c(3, 0.6, 0), col = box_col, col.axis = txt_col)
    graphics::axis(2, cex.axis = cex_axis, tcl = tcl, mgp = c(3, 0.6, 0), col = box_col, col.axis = txt_col)
    font_lab <- theme_int(th, "font.lab", 2)
    cex_lab <- 20 / 12 * lab_cex
    graphics::title(xlab = theme_int(th, "xlab", "Time (s)"), cex.lab = cex_lab, font.lab = font_lab, line = 1.2 + 1.5 * lab_cex)
    # the frequency axis title is shrunk if it is longer than the figure height
    ylab <- theme_int(th, "ylab", "Frequency (kHz)")
    # (it is centered on the plotting region, so it can extend twice the distance to the closest figure edge)
    plt <- graphics::par("plt")
    room <- 2 * min(mean(plt[3:4]), 1 - mean(plt[3:4])) * graphics::par("din")[2]
    cex_y <- min(cex_lab, cex_lab * 0.95 * room / graphics::strwidth(ylab, units = "inches", cex = cex_lab, font = font_lab))
    graphics::title(ylab = ylab, cex.lab = cex_y, font.lab = font_lab, line = 1.2 + 1.5 * lab_cex)
    if (!isFALSE(th$box)) graphics::box(col = box_col)
  }

  if (has_title)
    graphics::title(main = sp$title, cex.main = 20 / 12 * theme_int(th, "cex.title", 1), font.main = 2, adj = 0, line = if (compact) 1.6 + 1.1 * cex_axis else 1)

  # amplitude color bar in the right margin
  if (sp$plotLegend) {
    xin <- graphics::grconvertX(1, "lines", "user") - graphics::grconvertX(0, "lines", "user")
    x0 <- usr[2] + xin
    yb <- seq(sp$yLim[1], sp$yLim[2], length.out = length(cols) + 1)
    graphics::rect(x0, yb[-length(yb)], x0 + 1.2 * xin, yb[-1], col = cols, border = NA, xpd = NA)
    labs <- pretty(c(sp$min_dB, 0))
    labs <- labs[labs >= sp$min_dB & labs <= 0]
    graphics::text(x0 + 1.5 * xin, sp$yLim[1] + (labs - sp$min_dB) / -sp$min_dB * diff(sp$yLim), labels = labs, pos = 4, cex = 1, xpd = NA, col = txt_col)
    graphics::text(x0, sp$yLim[2] + 0.1 * diff(sp$yLim), labels = "Amplitude (dB)", adj = c(0, 0), cex = 1, font = 2, xpd = NA, col = txt_col)
  }

  ndc_y <- function(y) 1 - graphics::grconvertY(y, "user", "ndc")
  c(t0 = range_i[1], t1 = range_i[2],
    left = graphics::grconvertX(range_i[1], "user", "ndc"), right = graphics::grconvertX(range_i[2], "user", "ndc"),
    top = ndc_y(sp$yLim[2]), bottom = ndc_y(sp$yLim[1]))
}

# draw page i of a paged spectrogram in a png file, 'spec.width' x 'spec.height' inches at resolution 'res'
page_png_int <- function(sp, i, file, res) {
  grDevices::png(file, width = sp$specWidth, height = sp$specHeight, units = "in", res = res, bg = "white")
  dev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dev))
  draw_page_int(sp, i)
}

### layers of dynaspec objects drawn by the engines ---------------------------------------------------------

# draw selections (boxes and their labels) of a dynaspec object in the current plot. 'to_x' converts
# times (s) of the sound to x coordinates of the plot. 'panel' is "spectro" (selections use their frequency
# range if available, and labels are drawn) or "osc" (selections span the whole panel)
# spectrogram colored within annotations with type = "spectrogram" (as warbleR::color_spectro()): within each
# selection the spectrogram is drawn again with a gradient from the spectrogram background to the color of the
# selection. 'z' is the amplitude matrix as used by .filled.contour() (time in rows) and 'pos' the position of each
# amplitude level along the gradient (default: evenly spaced)
color_annotations_int <- function(layers, x, y, z, levels, colbg, to_x = identity, ylim = graphics::par("usr")[3:4], pos = NULL) {
  sels <- Filter(function(sel) identical(sel$type, "spectrogram"), layers$selections)
  if (length(sels) == 0)
    return(invisible(NULL))
  usr <- graphics::par("usr")
  on.exit(graphics::clip(usr[1], usr[2], usr[3], usr[4]), add = TRUE)
  if (is.null(pos)) pos <- seq(0, 1, length.out = length(levels) - 1)
  for (sel in sels) {
    X <- sel$X
    has_freq <- sel$freq && all(c("bottom.freq", "top.freq") %in% names(X))
    cols <- if (length(sel$col) == 1 && sel$col %in% names(X)) X[[sel$col]] else rep_len(sel$col, nrow(X))
    for (i in seq_len(nrow(X))) {
      x0 <- to_x(X$start[i])
      x1 <- to_x(X$end[i])
      y0 <- if (has_freq) max(X$bottom.freq[i], ylim[1]) else ylim[1]
      y1 <- if (has_freq) min(X$top.freq[i], ylim[2]) else ylim[2]
      if (x1 <= usr[1] || x0 >= usr[2] || y1 <= y0) next
      ramp <- grDevices::colorRamp(c(colbg, cols[i]), space = "Lab")
      graphics::clip(max(x0, usr[1]), min(x1, usr[2]), y0, y1)
      graphics::rect(x0, y0, x1, y1, col = colbg, border = NA)
      graphics::.filled.contour(x, y, z, levels = levels, col = grDevices::rgb(ramp(pmin(pmax(pos, 0), 1)), maxColorValue = 255))
    }
  }
  invisible(NULL)
}

draw_layers_int <- function(layers, to_x = identity, panel = "spectro", ylim = graphics::par("usr")[3:4]) {
  if (is.null(layers))
    return(invisible(NULL))

  for (sel in layers$selections) {
    X <- sel$X
    has_freq <- panel == "spectro" && sel$freq && all(c("bottom.freq", "top.freq") %in% names(X))
    # boxes are clipped to the frequency range shown (so labels at the top of the boxes are visible)
    bottom <- if (has_freq) pmax(X$bottom.freq, ylim[1]) else rep(ylim[1], nrow(X))
    top <- if (has_freq) pmin(X$top.freq, ylim[2]) else rep(ylim[2], nrow(X))
    cols <- if (length(sel$col) == 1 && sel$col %in% names(X)) X[[sel$col]] else rep_len(sel$col, nrow(X))
    borders <- if (is.null(sel$border)) NA else if (length(sel$border) == 1 && sel$border %in% names(X)) X[[sel$border]] else sel$border
    # (type = "spectrogram": the spectrogram is colored instead, see color_annotations_int(); only borders are drawn)
    fill <- if (identical(sel$type, "spectrogram")) NA else grDevices::adjustcolor(cols, sel$alpha)
    graphics::rect(to_x(X$start), bottom, to_x(X$end), top, col = fill,
                   border = borders, lwd = sel$lwd, lty = if (is.null(sel$lty)) 1 else sel$lty)

    # selection labels at the top of each box
    if (panel %in% c("spectro", "empty") && !is.null(sel$labels) && sel$labels %in% names(X))
      graphics::text(to_x((X$start + X$end) / 2), top - 0.04 * diff(ylim), labels = X[[sel$labels]], adj = c(0.5, 1),
                     col = cols, cex = sel$cex, font = 2)
  }

  invisible(NULL)
}

### engines (static, scrolling and paged spectrograms) -------------------------------------------------------

# static spectrogram engine (dynaspec() + static()): figure with a play line moving over it. 'layers' are the
# selections of the dynaspec object
static_spectro_int <- function(wave,
                           file.name = NULL,
                           wl = 512,
                           ovlp = 70,
                           flim = NULL,
                           pal = seewave::reverse.gray.colors.1,
                           collevels = seq(-40, 0, 1),
                           spectro = TRUE,
                           osc = TRUE,
                           osc.position = "below",
                           cex.lab = 1.2,
                           height.prop = c(2, 1),
                           colwave = "black",
                           colbg = NULL,
                           res = 100,
                           width = 1000,
                           height = 500,
                           rows = 1,
                           lcol = viridis::mako(10)[9],
                           speeds = c(1, 0.5, 0.25, 0.125),
                           keep.pitch = FALSE,
                           buttons = "time",
                           download.name = "sound",
                           alt = NULL,
                           fps = 30,
                           play = TRUE,
                           play.line = TRUE,
                           layers = NULL,
                           panels = NULL,
                           spectrum = NULL,
                           theme = list(),
                           ...)
{
  # check arguments
  check_results <- .check_arguments(
    fun = "static_spectro",
    args = list(wave = wave, html.file.name = file.name, wl = wl, ovlp = ovlp, flim = flim, pal = pal, collevels = collevels, spectro = spectro, osc = osc, osc.position = osc.position, cex.lab = cex.lab, height.prop = height.prop, colwave = colwave, colbg = colbg, res = res, width = width, height = height, rows = rows, lcol = lcol, speeds = speeds, keep.pitch = keep.pitch, buttons = buttons, alt = alt, fps = fps, play = play, play.line = play.line)
  )

  # report errors
  .report_assertions(check_results)

  # spectrogram background: color of the lowest amplitude of the palette, unless supplied. The figure background
  # is 'colbg' only when it is supplied (white otherwise)
  fig_bg_default <- if (is.null(colbg)) "white" else colbg
  if (is.null(colbg)) colbg <- low_col_int(pal, length(collevels) - 1)

  dots <- list(...)
  if (any(names(dots) %in% c("scale", "osc", "axisX", "axisY", "cexlab", "tlim", "tlab", "norm", "dBref")))
    stop2("'scale', 'osc', 'axisX', 'axisY', 'cexlab', 'tlim', 'tlab', 'norm' and 'dBref' cannot be supplied through '...' (use 'cex.lab' for the size of axis titles)")

  video <- !is.null(file.name) && grepl("\\.mp4$", file.name, ignore.case = TRUE)
  if (video && !have_ffmpeg_exec2())
    stop2("'ffmpeg' could not be found. It must be installed (and available in the system path) to create videos (see https://github.com/maRce10/dynaSpec#ffmpeg)")

  # without spectrogram and oscillogram an empty panel with time axis is shown (drawn as an oscillogram without
  # waveform)
  empty <- isFALSE(spectro) && isFALSE(osc)
  if (empty) osc <- TRUE

  # read sound and audio data to embed
  snd <- sound_data_int(wave)
  wav <- tuneR::mono(snd$wave, which = "left")
  dur <- seewave::duration(wav)

  # check flim
  nyquist <- wav@samp.rate / 2000
  if (!is.null(flim) && flim[1] >= nyquist)
    stop2("'flim' lower limit must be lower than the Nyquist frequency (", nyquist, " kHz)")
  if (!is.null(flim))
    flim[2] <- min(flim[2], nyquist)

  # sample limits of the rows
  n <- length(wav@left)
  f <- wav@samp.rate
  row_lims <- round(seq(0, n, length.out = rows + 1))
  if (spectro && min(diff(row_lims)) <= wl)
    stop2("rows are too short for the window length (decrease 'rows' or 'wl')")


  # plot figure and get the panel extents (as proportions of the figure)
  png_file <- tempfile(fileext = ".png")
  on.exit(unlink(png_file), add = TRUE)

  # vertical panels of each row (top to bottom): panels above the spectrogram, the spectrogram and panels below it.
  # Panels other than the spectrogram are oscillograms ("osc"), amplitude envelopes ("env") or an empty panel with
  # time axis ("empty"); 'height' is relative to the height of the spectrogram
  if (is.null(panels))
    panels <- if (osc && !empty) list(list(type = "osc", position = osc.position, col = colwave, height = height.prop[2] / height.prop[1])) else list()
  if (empty)
    panels <- list(list(type = "empty", position = "below", col = colwave, height = 1))
  above <- Filter(function(p) spectro && p$position == "above", panels)
  below_panels <- Filter(function(p) !spectro || p$position != "above", panels)
  stack <- c(above, if (spectro) list(list(type = "spectro", height = 1)), below_panels)
  n_stack <- length(stack)
  rel <- vapply(stack, function(p) if (is.null(p$height)) 0.5 else p$height, numeric(1))
  spec_slot <- which(vapply(stack, `[[`, character(1), "type") == "spectro")
  if (!spectro) spectrum <- NULL

  # margins (in lines) of each slot: the bottom panel of the last row has the time axis; the other rows have a
  # small gap instead
  axes <- theme_int(theme, "axis.type", "standard") != "none"
  # (labels inside the figure: no axis titles and narrow margins)
  inside <- identical(theme$axis.type, "inside")
  # (compact axes: narrow left column with the frequency labels and time labels above each row)
  compact <- identical(theme$axis.type, "compact")
  cex_axis <- theme_int(theme, "cex.axis", 1)
  # (left and bottom margins grow with the size of axis titles)
  lab_lines <- if (inside) 0 else if (compact) 0.6 + 1.6 * cex_axis else max(4, 2.8 + cex.lab)
  # (with a power spectrum on the left side, the frequency axis is drawn on the left of the spectrum instead)
  spec_left <- !is.null(spectrum) && spectrum$side == "left"
  # (the time axis title is placed at line 1.8 + cex.lab, and the 'Power' label of spectra above the top panel)
  bottom_lines <- if (inside) 0.45 + cex_axis else if (compact) 0 else max(4, 1.9 + 1.75 * cex.lab)
  top_lines <- if (compact) 0.4 + 1.1 * cex_axis else if (!is.null(spectrum) && spec_slot == 1) max(1, 0.1 + 0.68 * cex.lab * theme_int(theme, "cex.title", 1)) else if (inside) 0 else 1
  flush <- inside || compact
  # (the gap between the spectrogram and the power spectrum, 'spec_gap' lines on each side, is as wide as the gap
  # between stacked panels)
  spec_gap <- 0.1
  slot_mar <- function(i, axis) c(if (i < n_stack) 0.2 else if (axis && axes) bottom_lines else if (flush) 0 else 0.5, if (spec_left) spec_gap else if (axes) lab_lines else if (inside) 0 else 1, if (i == 1) top_lines else 0, if (is.null(spectrum) || spec_left) (if (flush) 0 else 1) else spec_gap)

  # slot heights (in inches) that keep relative heights between plotting regions, with the same plotting
  # regions in all rows ('height' is the height of a row with time axis)
  lines_in <- 0.2 # height of a line of text in the png device (12 points * 1.2)
  slot_marg <- function(axis) vapply(seq_len(n_stack), function(i) sum(slot_mar(i, axis)[c(1, 3)]), numeric(1)) * lines_in
  plot_in <- height / res - sum(slot_marg(TRUE))
  if (plot_in <= 0)
    stop2("'height' is too small to fit the figure (increase 'height' or decrease 'res')")
  slot_heights <- function(axis) plot_in * rel / sum(rel) + slot_marg(axis)
  fig_height <- round(res * ((rows - 1) * sum(slot_heights(FALSE)) + sum(slot_heights(TRUE))))

  # maximum image size of the png device
  if (fig_height > 32767)
    stop2("the figure is too tall (decrease 'height' or 'rows')")

  # layout: within a row the spectrogram is plotted first (its time range is used by the other panels), then the
  # power spectrum (in its own column, next to the spectrogram) and then the other panels
  plot_order <- c(spec_slot, setdiff(seq_len(n_stack), spec_slot))
  n_per_row <- n_stack + !is.null(spectrum)
  main_col <- unlist(lapply(seq_len(rows) - 1, function(k) k * n_per_row + match(seq_len(n_stack), plot_order) + if (is.null(spectrum)) 0 else (seq_len(n_stack) != spec_slot)))
  lay <- if (is.null(spectrum)) matrix(main_col, ncol = 1) else {
    spec_col <- unlist(lapply(seq_len(rows) - 1, function(k) ifelse(seq_len(n_stack) == spec_slot, k * n_per_row + 2, 0)))
    if (spectrum$side == "left") cbind(spec_col, main_col) else cbind(main_col, spec_col)
  }
  # (a spectrum on the left side also takes the margin of the frequency axis)
  spec_width <- if (spec_left && axes && !inside) spectrum$width + (lab_lines - 1) * lines_in * res / width else spectrum$width
  widths <- if (is.null(spectrum)) 1 else if (spec_left) c(spec_width, 1 - spec_width) else c(1 - spec_width, spec_width)

  fig_bg <- theme_int(theme, "bg", fig_bg_default)
  grDevices::png(filename = png_file, width = width, height = fig_height, res = res, bg = fig_bg)
  dev <- grDevices::dev.cur()
  theme_par_int(theme)
  ext <- tryCatch({
    graphics::layout(lay, heights = unlist(lapply(seq_len(rows), function(k) slot_heights(k == rows))), widths = widths)

    # layout() shrinks text when there are 3 or more panels; keep the same size as with a single row
    graphics::par(cex = 1)

    # reference for dB, amplitude, envelopes and spectra (so all rows share the same scale)
    if (spectro)
      max_amp <- max(seewave::spectro(wav, f = f, wl = wl, ovlp = ovlp, plot = FALSE, norm = FALSE, dB = NULL,
                                      wn = if (is.null(dots$wn)) "hanning" else dots$wn, zp = if (is.null(dots$zp)) 0 else dots$zp)$amp)
    max_wave <- max(abs(wav@left), 1e-12)
    if (any(vapply(panels, `[[`, character(1), "type") == "env"))
      max_env <- max(envelope_int(wav)$env, 1e-12)
    if (!is.null(spectrum)) {
      spectra <- lapply(seq_len(rows), function(k) spectrum_int(wav[(row_lims[k] + 1):row_lims[k + 1]], wl = wl, flim = flim))
      max_spec <- max(unlist(lapply(spectra, `[[`, "amp")), 1e-12)
    }

    ndc_y <- function(y) 1 - graphics::grconvertY(y, "user", "ndc")

    t(vapply(seq_len(rows), function(k) {
      seg <- wav[(row_lims[k] + 1):row_lims[k + 1]]
      t0 <- row_lims[k] / f
      t1 <- row_lims[k + 1] / f
      axis <- k == rows
      tops <- bottoms <- numeric(n_stack)

      for (i in plot_order) {
        p <- stack[[i]]
        bottom_slot <- i == n_stack
        graphics::par(mar = slot_mar(i, axis), bg = fig_bg, font.lab = theme_int(theme, "font.lab", 1))

        if (p$type == "spectro") {
          col_lab <- theme_int(theme, "col.lab", "black")
          spg <- spectro_int(seg, f = f, wl = wl, ovlp = ovlp, flim = flim, pal = pal, collevels = collevels, colbg = colbg, dBref = max_amp,
                      axisY = axes && (!spec_left || inside), flab = theme_int(theme, "ylab", "Frequency (kHz)"), cex.lab = cex.lab,
                      cex.axis = theme_int(theme, "cex.axis", 1), col.lab = col_lab, theme = theme, ...)

          # seewave::spectro() spreads the analysis windows evenly from 0 to the duration of the sound, but the
          # center of the first window is at wl / 2 and that of the last one is wl / 2 before the end. Times are
          # converted between the spectrogram x axis and the sound so the play line matches the sound
          n_seg <- length(seg@left)
          last_step <- utils::tail(seq(1, n_seg + 1 - wl, wl - (ovlp * wl / 100)), 1) - 1
          to_x <- function(t) (t - t0 - wl / (2 * f)) * n_seg / last_step
          to_time <- function(x) t0 + wl / (2 * f) + x * last_step / n_seg

          usr <- graphics::par("usr")

          # selections (spectrogram colored within annotations with type = "spectrogram")
          if (!is.null(layers)) {
            color_annotations_int(layers, spg$time, spg$freq, t(spg$amp), levels = collevels, colbg = colbg, to_x = to_x, ylim = usr[3:4])
            theme_box_int(theme, col_lab)
          }
          draw_layers_int(layers, to_x = to_x, panel = "spectro", ylim = usr[3:4])

          # time axis (only if the spectrogram is the bottom panel)
          if (bottom_slot && axis && axes && !compact)
            do.call(time_axis_int, c(list(to_x = to_x, xlim = to_time(usr[1:2]), cex.lab = cex.lab, inside = inside), theme[intersect(names(theme), c("xlab", "font.lab", "col.lab", "cex.axis"))]))

          left <- graphics::grconvertX(to_x(t0), "user", "ndc")
          right <- graphics::grconvertX(to_x(t1), "user", "ndc")
          xlim <- to_time(usr[1:2])

          # compact time axis above the top panel of each row
          if (compact && axes && i == 1)
            compact_row_axis_int(to_x, xlim, theme, cex_axis)

          # power spectrum next to the spectrogram (same frequency axis)
          if (!is.null(spectrum)) {
            graphics::par(mar = c(slot_mar(i, axis)[c(1, 3)], if (spec_left) c(if (axes && !inside) lab_lines else if (flush) 0 else 1, spec_gap) else c(spec_gap, if (flush) 0 else 1))[c(1, 3, 2, 4)])
            spectrum_player_int(spectra[[k]], ylim = usr[3:4], side = spectrum$side, col = spectrum$col, max.amp = max_spec, label = k == 1 && !compact, cex.lab = cex.lab, axis = spec_left && axes && !inside, theme = theme)
          }
        } else {
          if (!spectro) xlim <- c(t0, t1)
          if (p$type == "env")
            envelope_player_int(seg, xlim = xlim, col = p$col, axisX = bottom_slot && axis && axes && !compact, cex.lab = cex.lab, offset = t0, max.env = max_env, theme = if (axes && !flush) theme else c(theme, list(hide = TRUE)))
          else
            oscillo_player_int(seg, xlim = xlim, colwave = p$col, n = width, axisX = bottom_slot && axis && axes && !compact, cex.lab = cex.lab, offset = t0, max.amp = max_wave, empty = p$type == "empty", theme = if (axes && !flush) theme else c(theme, list(hide = TRUE)))
          draw_layers_int(layers, to_x = identity, panel = if (p$type == "empty") "empty" else "osc", ylim = graphics::par("usr")[3:4])
          if (compact && axes && i == 1)
            compact_row_axis_int(identity, xlim, theme, cex_axis)
          if (!spectro) {
            left <- graphics::grconvertX(t0, "user", "ndc")
            right <- graphics::grconvertX(t1, "user", "ndc")
          }
        }
        u <- graphics::par("usr")
        tops[i] <- ndc_y(u[4])
        bottoms[i] <- ndc_y(u[3])
      }

      # the play line spans all panels of the row
      c(t0 = t0, t1 = t1, left = left, right = right, top = tops[1], bottom = bottoms[n_stack])
    }, numeric(6)))
  }, finally = grDevices::dev.off(dev))

  # still image
  if (!is.null(file.name) && grepl("\\.png$", file.name, ignore.case = TRUE)) {
    file.copy(png_file, file.name, overwrite = TRUE)
    if (play) open_file_int(normalizePath(file.name))
    return(invisible(file.name))
  }

  # video of the figure with the play line
  if (video) {
    playhead_video_int(png_file = png_file, wave = snd$wave, ext = ext, file.name = file.name, lcol = lcol, fps = fps, width = width, height = fig_height, line = play.line, captions = layers$captions, res = res)
    if (play)
      open_file_int(normalizePath(file.name))
    return(invisible(file.name))
  }

  # player html
  html <- paste0(
    '<div class="dynaspec-player" data-duration="', dur,
    '" data-rows="', paste(apply(round(ext, 5), 1, paste, collapse = ","), collapse = ";"),
    '"', player_attrs_int(speeds, keep.pitch, buttons, download.name, theme), ' data-line="', tolower(play.line), '" data-color="', col2hex_int(lcol), '" data-text-color="', contrast_col_int(lcol), '">\n',
    '<img src="', base64_uri_int(png_file, "image/png"), '" alt="', html_escape_int(if (is.null(alt)) paste0(if (empty) "Time axis" else panel_names_int(spectro, panels, spectrum), " of a ", round(dur, 2), " s sound") else alt), '">\n',
    '<audio controls preload="auto" src="', snd$uri, '"></audio>\n',
    captions_json_int(layers$captions, res = res, width = width),
    '</div>\n',
    player_js_int()
  )

  output_html_int(html, file.name = file.name, play = play)
}

# scrolling spectrogram engine (dynaspec() + scrolling()): videos and web pages of spectrograms scrolling along a
# play line. 'layers' are the selections and 'captions' the captions of the dynaspec object
scrolling_spectro_int <- function(wave, file.name = "scroll.spectro.mp4", hop.size = 11.6, wl = NULL, ovlp = 70, flim = NULL, pal = seewave::reverse.gray.colors.1, speed = 1, fps = 50, t.display = 1.5, fix.time = TRUE, res = 70, width = 700, height = 400, parallel = 1, verbose = TRUE, play = TRUE, loop = 1, lcol = "#07889B99", lty = 2, lwd = 2, axis.type = "standard", buffer = 0, ggspectro = FALSE, lower.spectro = TRUE, height.prop = c(5, 1), derivative = FALSE, osc = FALSE, colwave = "black", colbg = NULL, spectro.call = NULL, annotation.call = NULL, speeds = c(1, 0.5, 0.25, 0.125), keep.pitch = FALSE, buttons = "time", download.name = "sound", layers = NULL, osc.type = "osc", theme = list(), captions = NULL, ...)
{

  # check arguments
  check_results <- .check_arguments(
    fun = "scrolling_spectro",
    args = list(wave = wave, file.name = file.name, hop.size = hop.size, wl = wl, ovlp = ovlp, flim = flim, pal = pal, speed = speed, fps = fps, t.display = t.display, fix.time = fix.time, res = res, width = width, height = height, parallel = parallel, verbose = verbose, play = play, loop = loop, lcol = lcol, lty = lty, lwd = lwd, axis.type = axis.type, buffer = buffer, ggspectro = ggspectro, lower.spectro = lower.spectro, height.prop = height.prop, derivative = derivative, osc = osc, colwave = colwave, colbg = colbg, spectro.call = spectro.call, annotation.call = annotation.call, speeds = speeds, keep.pitch = keep.pitch, buttons = buttons)
  )

  # report errors
  .report_assertions(check_results)

  # other arguments in '...' are passed to seewave::spectro()
  spectro_args <- list(...)

  # spectrogram background: color of the lowest amplitude of the palette, unless supplied
  if (is.null(colbg)) colbg <- low_col_int(pal, if (is.null(spectro_args$collevels)) 30 else length(spectro_args$collevels) - 1)

  # no grid lines (as in static spectrograms) unless requested
  # grid lines (spectrogram(grid = TRUE) or dynatheme(grid = TRUE))
  if (is.null(spectro_args$grid)) spectro_args$grid <- isTRUE(theme$grid)
  if (isTRUE(spectro_args$grid) && is.null(spectro_args$colgrid)) spectro_args$colgrid <- theme_int(theme, "col.grid", "#7F7F7F66")

  # web page instead of video
  html <- grepl("\\.html?$", file.name, ignore.case = TRUE)

  # zoom slider of web pages: the scrolling strips are drawn at twice the resolution (so zooming in stays sharp) and
  # the time labels are added by the web page
  zoom <- html && "zoom" %in% buttons

  # stop if ffmpeg is not found
  if (!html && !have_ffmpeg_exec2())
    stop2("'ffmpeg' could not be found. It must be installed (and available in the system path) to create videos")

  # read sound files
  if (!inherits(wave, "Wave")) {
    wave <- read_sound_int(wave)$wave

    if (seewave::duration(wave) <= t.display)
      stop2("'t.display' must be shorter than the duration of 'wave'")
  }

  # error message if imager is not installed
  if (derivative & !requireNamespace("imager", quietly = TRUE))
    stop2("must install 'imager' when using spectral derivatives (derivative = TRUE)")

  if (derivative & ggspectro) warning2("spectral derivatives (derivative = TRUE) are not allowed with 'ggspectro'. 'derivative' will be ignored")

  # only warn if user explicitly asked for both
  if (osc & lower.spectro & !missing(lower.spectro)) warning2("lower.spectro (lower.spectro = TRUE)  and oscillogram (osc = TRUE) are mutually exclusive. 'lower.spectro' will be ignored")

  # change lower.spectro if osc = T
  if (osc) lower.spectro <- FALSE

  # turn background gray when derivatives
  if (derivative)
  colbg <- "gray"

  # set buffer to 0 if loop > 1 (only warn if buffer was supplied by user)
  if (buffer > 0 & loop > 1) {
    if (!missing(buffer))
      warning2("buffer cannot be used (> 0) when loop is > 1. Buffer was set to 0")

    buffer <- 0
    }

  # set height prop to 0, 1
  if (!lower.spectro & !osc) height.prop <- c(1, 0)

  ## create a segment to add at the beggining and end
  if (loop == 1)
    add_sgm_end <- add_sgm_strt <- tuneR::silence(duration = t.display / 2, samp.rate = wave@samp.rate,
                   xunit = "time") else {

                     add_sgm_strt <- seewave::cutw(wave = wave, from = seewave::duration(wave) - t.display / 2, to = seewave::duration(wave), output = "Wave")

                     add_sgm_end <- seewave::cutw(wave = wave, from = 0, to = t.display / 2, output = "Wave")
                   }

  # add silence to start and end
  wave_sil <- seewave::pastew(wave2 = add_sgm_strt, wave1 = wave, f = wave@samp.rate,
               output = "Wave")

  wave_sil <- seewave::pastew(wave1 = add_sgm_end, wave2 = wave_sil, f = wave_sil@samp.rate,
               output = "Wave")

  # adjust wl based on hop.size
  if (is.null(wl))
    wl <- round(wave_sil@samp.rate * hop.size / 1000, 0)

  # make wl even if odd
  if (!(wl %% 2) == 0) wl <- wl + 1

  # make width and height even if odd
  if (!(width %% 2) == 0) width <- width + 1
  if (!(height %% 2) == 0) height <- height + 1

  # number of frames
  frames <- round((seewave::duration(wave) + buffer) * fps, 0)

  # time increase between frames
  step_time <- (seewave::duration(wave_sil) - t.display) / (frames  - (buffer * fps))

  # time increase for lower spectrogram
  step_time_low <- t.display / (frames  - (buffer * fps))

  # relative size of white rectangle in lower spectrogram
  white_window <- (t.display) / (seewave::duration(wave)) * t.display

  # labels given as a call to text() ('annotation.call') are converted to captions (positions as proportions of the figure)
  if (!is.null(annotation.call)) {
    a <- lapply(as.list(annotation.call)[-1], eval)
    captions <- c(captions, list(list(labels = as.character(a$labels), start = a$start, end = a$end,
      x = a$x[1], y = a$y[1], fading = if (is.null(a$fading)) 0 else a$fading, col = if (is.null(a$col)) "black" else a$col[1],
      cex = if (is.null(a$cex)) 1 else a$cex[1], font = if (is.null(a$font)) 1 else a$font[1])))
    annotation.call <- NULL
  }

  # make vector with name of images
  img_names <- paste0(sprintf(paste0("%0",nchar(frames) + 2, "d"), 1:(frames * loop)), ".temp.img.png")

  # flim for spectro() (NULL = full range; c(0, Nyquist) makes seewave index past the last frequency bin)
  spectro_flim <- flim

  if (is.null(flim)) # flim on original wave
    flim <- c(0, wave@samp.rate / 2000)

  # temporary file names (tempdir of this session, also used by parallel workers)
  temp_dir <- tempdir()
  temp.audio <- file.path(temp_dir, "audio.scroll.spectro.wav")
  temp.audio.rsmp <- file.path(temp_dir, "audio.scroll.spectro.rsmp.wav")
  temp.video <- file.path(temp_dir, "scroll.spectro.temp.mp4")

  # reset margins at the end
  opar <- graphics::par(mar = graphics::par("mar"), bg = graphics::par("bg"), no.readonly = TRUE)
  on.exit(graphics::par(opar), add = TRUE)

  # remove temporary files at the end
  on.exit(try(unlink(c(temp.video, temp.audio, temp.audio.rsmp, list.files(path = temp_dir, full.names = TRUE, pattern = "\\.temp.img.png$"))), silent = TRUE), add = TRUE)

  on.exit(try(unlink(c(file.path(path = temp_dir, "temp.full.spectro.png"), file.path(path = temp_dir, "temp.full.oscillo.png"))), silent = TRUE), add = TRUE)

  wdt <- width * seewave::duration(wave_sil) / t.display * if (zoom) 2 else 1
  if (wdt > 32767) wdt <- 32767

  if (!ggspectro){
  # save full spectrogram of wave
  grDevices::png(filename = file.path(temp_dir, "temp.full.spectro.png"), height = height, width = wdt, res = res)

  # set plot margins for spectrogram
  graphics::par(mar = rep(0, 4))

  # plot spectro
  if (is.null(spectro.call))
  suppressMessages(do.call(seewave::spectro, c(list(wave = wave_sil, f = wave_sil@samp.rate, wl = wl, ovlp = ovlp, axisX = FALSE, axisY = FALSE, scale = FALSE, flim = spectro_flim, palette = pal, osc = FALSE, colbg = colbg), spectro_args))) else {

    # modify wave in call
    spectro.call$wave <- wave_sil

    # fix times in selection table if present in spectro.call
    if (!is.null(spectro.call$X)) {

      X <- spectro.call$X
      wave_dur <- seewave::duration(wave)

      # add selections to repeated parts when looping
      if (loop > 1) {
        # selections at the start are repeated at the end
        end.X <- X[X$start <= (t.display / 2), ]
        end.X$start <- end.X$start + wave_dur
        end.X$end <- end.X$end + wave_dur

        # selections at the end are repeated at the start
        start.X <- X[X$end >= wave_dur - (t.display / 2), ]
        start.X$start <- start.X$start - wave_dur
        start.X$end <- start.X$end - wave_dur

        X <- rbind(start.X, X, end.X)
      }

      # shift to account for the segment added at the start
      X$start <- X$start + (t.display / 2)
      X$end <- X$end + (t.display / 2)

      spectro.call$X <- X
    }
    # evaluate call
    eval(spectro.call)
  }

  # seewave::spectro() draws a box around the spectrogram, which is covered when the theme has no boxes
  if (isFALSE(theme$box))
    graphics::box(col = colbg, lwd = 3)

  # selections (times shifted by the half window added at the start; also repeated at both ends when looping)
  # (spectrogram colored within annotations with type = "spectrogram"; computed as in seewave::spectro() above)
  if (is.null(spectro.call) && any(vapply(layers$selections, function(sel) identical(sel$type, "spectrogram"), logical(1)))) {
    spc_args <- spectro_args[intersect(names(spectro_args), c("wn", "zp", "dB", "dBref", "norm", "fftw", "correction"))]
    spg <- do.call(seewave::spectro, c(list(wave = wave_sil, f = wave_sil@samp.rate, wl = wl, ovlp = ovlp, flim = spectro_flim, plot = FALSE), spc_args))
    lev <- if (is.null(spectro_args$collevels)) seq(round(max(spg$amp)) - 30, round(max(spg$amp)), by = 1) else spectro_args$collevels
    color_annotations_int(loop_layers_int(layers, seewave::duration(wave), loop > 1), spg$time, spg$freq, t(spg$amp), levels = lev,
                          colbg = colbg, to_x = function(t) t + t.display / 2, ylim = graphics::par("usr")[3:4])
  }
  draw_layers_int(loop_layers_int(layers, seewave::duration(wave), loop > 1), to_x = function(t) t + t.display / 2, panel = "spectro", ylim = graphics::par("usr")[3:4])

  # close plot
  grDevices::dev.off()

  # spectral derivatives
  if (derivative){

    # read spectrogram image
  spc_img <- imager::load.image(file.path(temp_dir, "temp.full.spectro.png"))

  # resave with derivatives
  grDevices::png(filename = file.path(temp_dir, "temp.full.spectro.png"), height = height, width = wdt, res = res)

  # remove margins and make background gray
  graphics::par(mar = rep(0, 4), bg = "gray")

  # calculate 2 order derivative
  der.im <- imager::deriche(spc_img, sigma = 3, order = 2, axis="y")

  # plot
  graphics::plot(der.im)

  grDevices::dev.off()
  }

  graphics::par(bg = "white")

  } else {
  level <- NULL

    suppressMessages(ggsp <- seewave::ggspectro(wave = wave_sil, f = wave_sil@samp.rate, wl = wl, ovlp = ovlp) +
      ggplot2::stat_contour(geom = "polygon", ggplot2::aes(fill = ggplot2::after_stat(level)), bins = 30, na.rm = TRUE) +
      ggplot2::scale_fill_gradientn(name="Amplitude\n(dB)\n", limits = c(-42, 0), guide = "none", na.value = "transparent", colours = pal(30)) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = flim) +
      ggplot2::theme(axis.line = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), legend.position = "none"))

    ggplot2::ggsave(plot = ggsp, filename = file.path(temp_dir, "temp.full.spectro.png"), height = height / res, width = wdt / res, units = "in", dpi = res, device = "png", limitsize = FALSE)
  }

  # plot oscillogram
  if (osc){
    grDevices::png(filename = file.path(temp_dir, "temp.full.oscillo.png"), height = height, width = wdt, res = res)

    # set plot margins for spectrogram
    graphics::par(mar = rep(0, 4))

    # plot spectro
    # oscillogram or amplitude envelope ('osc.type')
    panel_bg <- theme_int(theme, "col.panel", colbg)
    if (osc.type == "env") {
      graphics::par(bg = panel_bg)
      e <- envelope_int(wave_sil, n = round(wdt))
      graphics::plot(NA, xlim = c(0, seewave::duration(wave_sil)), ylim = c(0, 1.05), xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")
      y <- e$env / max(e$env, 1e-12)
      graphics::polygon(c(e$time[1], e$time, e$time[length(e$time)]), c(0, y, 0), col = grDevices::adjustcolor(colwave, 0.6), border = colwave)
    } else
      oscillo_dynaspec_int(wave = wave_sil, f = wave_sil@samp.rate, xaxt = "n", yaxt = "n", colwave = colwave, bg = panel_bg, bty = if (isFALSE(theme$box)) "n" else "l")
    draw_layers_int(loop_layers_int(layers, seewave::duration(wave), loop > 1), to_x = function(t) t + t.display / 2, panel = "osc", ylim = graphics::par("usr")[3:4])

    # close plot
    grDevices::dev.off()

    # read image
    osc_img <- png::readPNG(source = file.path(temp_dir, "temp.full.oscillo.png"))

    }

  # read image
  spc_img <- png::readPNG(source = file.path(temp_dir, "temp.full.spectro.png"))

  # calculate pixels per second
  px.per.s <- dim(spc_img)[2] / seewave::duration(wave_sil)

  # color for minimal axis (black or white depending on background)
  axis_col <- contrast_col_int(colbg)

  # draw frame x of the video in 'file'. With 'html_frame = TRUE' draw only the parts that do not move (for
  # web pages, which add the scrolling spectrogram, window rectangles, captions and moving time axis).
  # Returns the plotting region ('plt') of the frame
  draw_frame <- function(x, file, html_frame = FALSE) {
    # time limit
    tlim <- c((x - 1) * step_time, (x - 1) * step_time) - buffer

    # fix if negative
    if (tlim[1] < 0) tlim <- c(0, 0)

    # add t.display to tlim 2
    tlim[2] <- tlim[2] + t.display

    # pixel limt
    img.x.lim <- round(tlim * px.per.s, 0)

    # tlim_low are limits for white rectangle in lower spectrogram
    tlim_low <- c((x - 1) * step_time_low, (x - 1) * step_time_low) - buffer * t.display / seewave::duration(wave)

    # fix if negative
    if (tlim_low[1] < 0) tlim_low <- c(0, 0)

    tlim_low <- tlim_low + c(white_window * -1, white_window) / 2

    # start png device (tiff files from R are not readable by recent ffmpeg versions)
    grDevices::png(file, res = res, width = width, height = height, bg = if (html_frame) "transparent" else theme_int(theme, "bg", "white"))

    # axis colors, fonts and sizes (dynatheme())
    col_lab <- theme_int(theme, "col.lab", "black")
    # (parts of the overview outside the window shown are dimmed)
    dim_col <- grDevices::adjustcolor(theme_int(theme, "col.highlight", "gray"), 0.42)
    graphics::par(col.axis = col_lab, col.lab = col_lab, fg = col_lab, font.lab = theme_int(theme, "font.lab", 1),
                  cex.lab = theme_int(theme, "cex.lab", 1), cex.axis = theme_int(theme, "cex.axis", 1))
    theme_par_int(theme)

    # set regular margins
    if (axis.type == "compact")
      graphics::par(mar = c(0, 0.6 + 1.6 * theme_int(theme, "cex.axis", 1), 0.4 + 1.1 * theme_int(theme, "cex.axis", 1), 0)) else
    if (axis.type == "inside")
      graphics::par(mar = c(0.45 + theme_int(theme, "cex.axis", 1), 0, 0, 0)) else
    if (axis.type != "standard")
      graphics::par(mar =  rep(0, 4)) else
    graphics::par(mar =  c(4.2, 4.2, 1, 1) + 0.1)

    # keep original values
    org.flim <- flim

    # modify flim if lower spectro
    if (lower.spectro | osc){

      # add space at the bottom
      flim[1] <- flim[1] - (flim[2] - flim[1]) * ((height.prop[2]) / (height.prop[1] - height.prop[2]))
    }

    # time labels drawn in the frame (in web pages they are added by the page when time moves along or with zoom)
    frame_time <- !html_frame || (fix.time && !zoom)

    # plot anything at specific time and freq
    # (time axis labels move along when time is not fixed, so they are added by the web page in html frames)
    graphics::plot(0,0, type = "n", xlim = if(fix.time) c(0, t.display) else tlim, ylim = flim, xlab = if (axis.type == "standard") theme_int(theme, "xlab", "Time (s)") else "", ylab = if (axis.type == "standard") theme_int(theme, "ylab", "Frequency (kHz)") else "", xaxs = "i", yaxs = "i", bty = "n", xaxt = if (axis.type == "standard" && fix.time && frame_time) "s" else "n", yaxt = "n")

    # get plotting area coordinates
    plt <- graphics::par("plt")

    # get plotting area in original units
    usr <- graphics::par("usr")

    # add spectrogram segment to plot (scrolling parts are added by the web page in html frames)
    if (!html_frame)
    grid::grid.raster(spc_img[, img.x.lim[1]:img.x.lim[2], ], x = plt[1], y =  plt[3] + (plt[4] - plt[3]) * height.prop[2] / height.prop[1], height = (plt[4] - plt[3]) * (1 - height.prop[2] / height.prop[1]), width = plt[2] - plt[1], hjust = 0, vjust = 0)

    # add oscillogram
    if (osc && !html_frame) {
      grid::grid.raster(osc_img[, img.x.lim[1]:img.x.lim[2], ], x = plt[1], y =  plt[3], height = (plt[4] - plt[3]) * (height.prop[2] / height.prop[1]), width = plt[2] - plt[1], hjust = 0, vjust = 0)
    }

    # add play line
    graphics::abline(v = usr[1] + (usr[2] - usr[1]) / 2, lty = lty, col = lcol, lwd = lwd)

    # add spectro at bottom
    if (lower.spectro) {
      grid::grid.raster(spc_img[, round((t.display / 2) * px.per.s, 0):round(((t.display / 2) + seewave::duration(wave)) * px.per.s, 0), ], x = plt[1], y =  plt[3], height = (plt[4] - plt[3]) * height.prop[2] / height.prop[1], width = plt[2] - plt[1], hjust = 0, vjust = 0)

    # window rectangles (moving parts are added by the web page in html frames)
    if (!html_frame) {
    #left gray rectangle
    graphics::rect(xleft = if (tlim_low[2] + usr[1] > usr[2] & loop > 1) tlim_low[2] + 2 * usr[1] - usr[2] else 0, ybottom = flim[1], ytop = flim[1] + (flim[2] - flim[1]) * height.prop[2] / height.prop[1], xright = tlim_low[1] + usr[1], col = dim_col, border = NA)

    # right gray rectangle
    if (fix.time)
    graphics::rect(xleft = tlim_low[2] + usr[1], ybottom = flim[1], ytop = flim[1] + (flim[2] - flim[1]) * height.prop[2] / height.prop[1], xright = if(loop > 1 & tlim_low[1] - usr[1] <= 0) usr[2] - (white_window - (tlim_low[2] - usr[1])) else usr[2], col = dim_col, border = NA) else # different time limits when looping
      graphics::rect(xleft = tlim_low[2] + usr[1], ybottom = flim[1], ytop = flim[1] + (flim[2] - flim[1]) * height.prop[2] / height.prop[1], xright = if(loop > 1 & tlim_low[1] - usr[1] <= 0) {usr[2] - (white_window - (tlim_low[2] - usr[1]))
        + if (tlim_low[1] - usr[1] <= 0) tlim_low[2] + usr[1] + (seewave::duration(wave_sil) - white_window) / 2 else 0
      }  else usr[2], col = dim_col, border = NA)

    # box in playing area
    graphics::rect(xleft = tlim_low[1] + usr[1], ybottom = flim[1], ytop = flim[1] + (flim[2] - flim[1]) * height.prop[2] / height.prop[1], xright = tlim_low[2] + usr[1], col = "transparent", lwd = 0.4)

    # rectangle start at the same time that the other disapears
    if (tlim_low[2] + usr[1] > usr[2] & loop > 1)
      graphics::rect(xleft = -1, ybottom = flim[1], ytop = flim[1] + (flim[2] - flim[1]) * height.prop[2] / height.prop[1], xright = tlim_low[2] + 2 * usr[1] - usr[2], col = "transparent", lwd = 0.4)
    }
    }

    # add y axis
    if (axis.type == "standard") {
      labs <- pretty(org.flim, n = 5)
      labs <- labs[labs >= org.flim[1] & labs <= org.flim[2]]

      graphics::axis(side = 2, at = labs, labels = labs, las = theme_int(theme, "las", 0))
    }

    # time labels: times of the window (fixed time) or of the sound (time moving along; the x axis is the time of
    # the sound with half a window of silence added at the start, which is not labeled)
    if (frame_time) {
      half <- if (fix.time) 0 else t.display / 2
      t_at <- pretty(usr[1:2] - half)
      t_at <- t_at[t_at + half >= usr[1] & t_at + half <= usr[2] & (fix.time | (t_at >= 0 & t_at <= seewave::duration(wave) + 1e-9))]
      if (axis.type == "standard" && !fix.time)
        graphics::axis(1, at = t_at + half, labels = t_at)
    }

    # compact axes (time labels move along in html frames when time is not fixed)
    if (axis.type == "compact") {
      compact_freq_axis_int(org.flim, col = col_lab, cex = theme_int(theme, "cex.axis", 1), bottom = org.flim[1])
      if (frame_time) {
        compact_time_axis_int(t_at + half, t_at, col = col_lab, cex = theme_int(theme, "cex.axis", 1))
      }
    }

    # labels inside the figure (time labels move along in html frames when time is not fixed)
    if (axis.type == "inside") {
      inside_freq_axis_int(org.flim, col = col_lab, cex = theme_int(theme, "cex.axis", 1))
      if (frame_time) {
        inside_time_axis_int(t_at + half, t_at, col = col_lab, cex = theme_int(theme, "cex.axis", 1))
      }
    }

    # add 1 s / 1 kHz scale bars
    if (axis.type == "minimal") {
      x0 <- usr[1] + (usr[2] - usr[1]) * 0.03
      y0 <- org.flim[1] + (org.flim[2] - org.flim[1]) * 0.05
      # use a shorter time bar for short display windows
      bar_t <- if (t.display >= 1.5) 1 else 0.1
      graphics::segments(x0 = c(x0, x0), y0 = c(y0, y0), x1 = c(x0 + bar_t, x0), y1 = c(y0, y0 + 1), col = axis_col, lwd = 2, lend = 1)
      graphics::text(x = x0 + bar_t / 2, y = y0, labels = paste(bar_t, "s"), pos = 3, col = axis_col, cex = 0.8)
      graphics::text(x = x0, y = y0 + 0.5, labels = "1 kHz", pos = 4, col = axis_col, cex = 0.8)
    }

    # add lower spectro division line
    if ((lower.spectro | osc) && !isFALSE(theme$box))
      graphics::abline(h = org.flim[1], col = theme_int(theme, "col.box", col_lab))

    # captions (sound times + buffer; fading in and out)
    if (!html_frame)
      for (cp in captions) {
        tt <- (x - 1) / fps
        for (i in seq_along(cp$labels)) {
          t0 <- cp$start[i] + buffer
          t1 <- cp$end[i] + buffer
          if (tt < t0 || tt > t1) next
          alpha <- if (cp$fading > 0) min(1, (tt - t0) / cp$fading, (t1 - tt) / cp$fading) else 1
          graphics::text(graphics::grconvertX(cp$x, "ndc", "user"), graphics::grconvertY(cp$y, "ndc", "user"), labels = cp$labels[i],
                         col = grDevices::adjustcolor(cp$col, alpha.f = alpha), cex = cp$cex, font = cp$font, family = theme_int(cp, "family", "sans"), xpd = NA)
        }
      }

    # reprint box
    theme_box_int(theme, col_lab)

    grDevices::dev.off()

    invisible(plt)
  }

  # web page
  if (html) {
    frame_file <- tempfile(fileext = ".png")
    on.exit(unlink(frame_file), add = TRUE)
    plt <- draw_frame(1, frame_file, html_frame = TRUE)

    page <- scrolling_html_int(
      frame_file = frame_file, spectro_file = file.path(temp_dir, "temp.full.spectro.png"),
      osc_file = if (osc) file.path(temp_dir, "temp.full.oscillo.png"), lower = if (osc) "osc" else if (lower.spectro) "spectro" else "none",
      plt = plt, height.prop = height.prop, wave = wave, wave_sil = wave_sil, t.display = t.display, buffer = buffer,
      step_time = step_time, step_time_low = step_time_low, fps = fps, loop = loop, fix.time = fix.time,
      axis = axis.type %in% c("standard", "inside", "compact"), zoom = zoom, inside = axis.type == "inside", compact = axis.type == "compact", width = width, res = res, captions = captions,
      speeds = unique(c(speed, speeds)), keep.pitch = keep.pitch, buttons = buttons, download.name = download.name,
      lcol = lcol, theme = theme
    )

    return(output_html_int(page, file.name = file.name, play = play))
  }

  #loop to create image files (with progress bar)
  out <- .pbapply_dyn(X = seq_len(frames), cl = parallel, pbar = verbose, title = "Rendering video frames", FUN = function(x){

    draw_frame(x, file.path(temp_dir, img_names[x]))

    # copy frames for each additional loop
    if (loop > 1)
      for (i in seq_len(loop - 1))
      file.copy(file.path(temp_dir, img_names[x]), file.path(temp_dir, img_names[x + (frames * i)]))
  })

  # add buffer as silence
  if (buffer > 0)
    wave <- seewave::pastew(wave2 = tuneR::silence(duration = buffer, samp.rate = wave@samp.rate, xunit = "time"), wave1 = wave, f = wave@samp.rate,output = "Wave")

  # change speed
  if (speed != 1)
  {
    # change rate
    wave@samp.rate <- wave@samp.rate * speed
    fps <- fps * speed
  }

  # add waves as many time as loop
  if (loop > 1)
    for (i in 1:(loop - 1))
    {
      # non-repeated wave
      wave1 <- wave
      wave <- seewave::pastew(wave2 = wave1, wave1 = wave, f = wave@samp.rate,output = "Wave")
}

  suppressWarnings(tuneR::writeWave(object = wave, filename = temp.audio, extensible = FALSE))

  # resample to 44.1 kHz
  if (wave@samp.rate != 44100){
    cll_resample <- paste("-i", shQuote(temp.audio), "-ar 44100 -y", shQuote(temp.audio.rsmp))
    run_ffmpeg_int(cll_resample, "resampling audio")
    temp.audio <- temp.audio.rsmp # overwrite name
  }

  # put together call for ffmpeg
  cll1 <- paste("-framerate", fps, "-i", shQuote(file.path(temp_dir, paste0("%0", nchar(frames) + 2, "d.temp.img.png"))), "-c:v libx264 -profile:v high -crf 2 -pix_fmt yuv420p -y", shQuote(temp.video))

  # run ffmpeg to create video
  run_ffmpeg_int(cll1, "creating video from frames")

  # add audio
  cll2 <- paste("-i", shQuote(temp.video), "-i", shQuote(temp.audio), "-vcodec libx264 -acodec libmp3lame -shortest -y", shQuote(file.name))

  # run ffmpeg to add audio
  run_ffmpeg_int(cll2, "adding audio to video")

  # reproduce
  if (play)
    open_file_int(file.name)

  invisible(file.name)
}

# selections repeated at both ends of looping scrolling spectrograms (the end of the sound is added at the start
# and the start at the end)
loop_layers_int <- function(layers, dur, loop) {
  if (is.null(layers) || !loop)
    return(layers)
  layers$selections <- lapply(layers$selections, function(sel) {
    X <- sel$X
    shift <- function(d) { Y <- X; Y$start <- Y$start + d; Y$end <- Y$end + d; Y }
    sel$X <- rbind(shift(-dur), X, shift(dur))
    sel
  })
  layers
}

# paged spectrogram preparation (spectrogram of each page). Formerly prep_static_ggspectro()
prep_paged_int <-
  function(wave,
           ...,
           dest.path = NULL,
           png.name = NULL,
           save.png = FALSE,
           pal = "inferno",
           crop = NULL,
           colbg = NULL,
           filter = NULL,
           t.display = NULL,
           flim = c(0, 10),
           title = NULL,
           plot.legend = FALSE,
           axis.type = "none",
           amp.trans = 1,
           resample.rate = 15000,
           min.db = -30,
           wl = 512,
           ovlp = 90,
           wn = "blackman",
           spec.width = 9,
           spec.height = 3,
           col.bins = 30,
           amp.thresh = 0,
           bg.flood = FALSE,
           colaxis = NULL,
           plot = TRUE,
           verbose = TRUE)
  {
    dots <- list(...)

    # name for output files when 'wave' is a Wave object ('sound.name', or the name of the object), and layers
    # (selections and captions) of dynaspec objects
    wave_name <- if (!is.null(dots$sound.name)) dots$sound.name else if (!missing(wave) && inherits(wave, "Wave")) make.names(paste(deparse(substitute(wave)), collapse = "")) else NULL
    layers <- dots$layers
    theme <- dots$theme
    dots$sound.name <- dots$layers <- dots$theme <- NULL

    # single values of flim are upper limits
    if (length(flim) == 1) flim <- c(0, flim)

    # check arguments
    check_results <- .check_arguments(
      fun = "prep_paged_spectro",
      args = list(wave = wave, dest.path = dest.path, png.name = png.name, save.png = save.png, pal = pal, crop = crop, colbg = colbg, filter = filter, t.display = t.display, flim = flim, title = title, plot.legend = plot.legend, axis.type = axis.type, amp.trans = amp.trans, resample.rate = resample.rate, min.db = min.db, wl = wl, ovlp = ovlp, wn = wn, spec.width = spec.width, spec.height = spec.height, col.bins = col.bins, amp.thresh = amp.thresh, bg.flood = bg.flood, colaxis = colaxis, plot = plot, verbose = verbose)
    )

    # report errors
    .report_assertions(check_results)

    # internal names (used in the rest of the code and in the output)
    destFolder <- dest.path
    outFilename <- png.name
    savePNG <- save.png
    colPal <- pal
    bg <- colbg
    xLim <- t.display
    yLim <- flim
    plotLegend <- plot.legend
    onlyPlotSpec <- axis.type == "none"
    ampTrans <- amp.trans
    resampleRate <- resample.rate
    min_dB <- min.db
    specWidth <- spec.width
    specHeight <- spec.height
    colbins <- col.bins
    ampThresh <- amp.thresh
    bgFlood <- bg.flood
    fontAndAxisCol <- colaxis

    #Put in the folder of the sound file if unspecified (working directory for Wave objects)
    if (is.null(destFolder)) {
      if (inherits(wave, "Wave")) {
        destFolder = getwd()
      } else{
        destFolder = dirname(tools::file_path_as_absolute(wave))
      }
    } else if (identical(destFolder, "wd")) {
      #Put files in working dir if requested
      destFolder <- getwd()
    } else if (!fs::is_absolute_path(destFolder)) {
      #relative paths are relative to the working directory
      destFolder <- file.path(getwd(), destFolder)
    }

    #create destination folder if needed
    if (!dir.exists(destFolder)) {
      fs::dir_create(destFolder)
      if (verbose) message("Created destination folder: ", destFolder)
    }

    if (!grepl("/$", destFolder)) {
      destFolder = paste0(destFolder, "/")
    }#if destFolder missing terminal /, add it

    # read sound
    snd <- read_sound_int(wave, name = wave_name, verbose = verbose)
    soundFile <- if (is.null(snd$file)) snd$name else snd$file

    #Handle file naming for spec
    if (is.null(outFilename)) {
      outFilename = paste0(tools::file_path_sans_ext(basename(soundFile)), ".png")
    }
    if (!grepl(".png|PNG", outFilename)) {
      outFilename = paste0(outFilename, ".png")
    }#if user didn't put suffix onto output filename, add .png
    
    #Are we dealing with a custom or a viridis palette?
    # (palette functions are used as custom gradients)
    isViridis <- !is.function(colPal) && length(colPal) == 1
    
    #set background color as palette level 1 if missing
    if (is.null(bg)) {
      if (isViridis) {
        bg = viridis::viridis(1, option = colPal)
      } else if (is.function(colPal)) {
        bg = colPal(2)[1]
      } else{
        bg = colPal[1]
      }
    }
    
    wav0 <- snd$wave
    
    #Make sure it's mono (arbitrarily using left channel)
    if (wav0@stereo) {
      wav0 <- tuneR::mono(wav0, which = "left")
    }
    
    #######
    #crop, filter, normalize, deal with missing parameters for the sound
    prepped <-
      processSound(
        wav0,
        crop = crop,
        xLim = xLim,
        filter = filter,
        ampThresh = ampThresh,
        verbose = verbose
      )
    
    if (length(yLim) == 1) {
      yLim = c(0, yLim)
    }
    
    # text color picked automatically unless supplied
    autoFontCol <- is.null(fontAndAxisCol)
    if (autoFontCol)
      fontAndAxisCol <- contrast_col_int(bg)

    # spectrogram data of each page
    spec <- do.call(paged_spec_data_int, c(list(
      wav = prepped$newWav,
      resampleRate = resampleRate,
      yLim = yLim,
      segLens = prepped$segLens,
      wl = wl,
      ovlp = ovlp,
      wn = wn,
      verbose = verbose
    ), dots))

    #Make list of all info needed to recreate spec look in videos
    specParams = list(
      soundFile = soundFile,
      destFolder = destFolder,
      outFilename = outFilename,
      n_pages = length(prepped$segWavs),
      crop = crop,
      colPal = colPal,
      isViridis = isViridis,
      xLim = prepped$xLim,
      yLim = yLim,
      title = title,
      plotLegend = plotLegend,
      onlyPlotSpec = onlyPlotSpec,
      ampTrans = ampTrans,
      ampThresh = ampThresh,
      min_dB = min_dB,
      bg = bg,
      wl = wl,
      ovlp = ovlp,
      wn = wn,
      specWidth = specWidth,
      specHeight = specHeight,
      colbins = colbins,
      bgFlood = bgFlood,
      autoFontCol = autoFontCol,
      fontAndAxisCol = fontAndAxisCol,
      spec = spec,
      newWav = prepped$newWav,
      segWavs = prepped$segWavs,
      layers = layers,
      theme = theme
    )

    # save pages as png files
    if (savePNG) {
      subDest <- fs::path(destFolder, paste0(basename(tools::file_path_sans_ext(soundFile)), "_static_specs"))
      dir.create(subDest, showWarnings = FALSE)
      for (i in seq_len(specParams$n_pages)) {
        fn_i <- fs::path(subDest, paste0(basename(tools::file_path_sans_ext(soundFile)), "_", i), ext = "png")
        page_png_int(specParams, i, fn_i, res = 300)
        if (verbose) message("Static spectrogram saved at: ", fn_i)
      }
    }

    # plot first page
    if (plot) {
      draw_page_int(specParams, 1)

      if (verbose && specParams$n_pages > 1)
        message("For segmented spectrogram, only segment 1 shown")
    }

    return(specParams)
  }

# paged spectrogram engine (dynaspec() + paged())
paged_spectro_int <- function(wave,
                          ...,
                          file.name = NULL,
                          fps = 30,
                          highlight.col = "#4B0C6BFF",
                          highlight.alpha = .6,
                          lcol = "white",
                          delete.temp = TRUE,
                          speeds = c(1, 0.5, 0.25, 0.125),
                          keep.pitch = FALSE,
                          buttons = "time",
                          download.name = "sound",
                          play = TRUE,
                          parallel = 1,
                          verbose = TRUE)
{
  xmin <- ymin <- xmax <- ymax <- NULL
  #This ^^ suppresses note about "no visible binding for global variable ‘xmax’"

  # other arguments are passed to prep_paged_int()
  prep_args <- list(...)

  # 'wave' can be a sound or the output of prep_paged_spectro()
  is_params <- is.list(wave) && !inherits(wave, "Wave")

  # check arguments
  check_results <- .check_arguments(
    fun = "paged_spectro",
    args = list(wave = if (!is_params) wave, specParams = if (is_params) wave, file.name = file.name, fps = fps, highlight.col = highlight.col, highlight.alpha = highlight.alpha, lcol = lcol, delete.temp = delete.temp, speeds = speeds, keep.pitch = keep.pitch, buttons = buttons, play = play, parallel = parallel, verbose = verbose)
  )

  # report errors
  .report_assertions(check_results)

  # internal names
  framerate <- fps
  highlightCol <- highlight.col
  highlightAlpha <- highlight.alpha
  cursorCol <- lcol
  delete_temp_files <- delete.temp

  # web page instead of video
  html <- !is.null(file.name) && grepl("\\.html?$", file.name, ignore.case = TRUE)

  if (!html) {
    # stop if ffmpeg is not found
    if (!have_ffmpeg_exec2())
      stop2("'ffmpeg' could not be found. It must be installed (and available in the system path) to create videos (see https://github.com/maRce10/dynaSpec#ffmpeg)")
  }

  # prepare spectrograms of sounds
  if (is_params) {
    if (length(prep_args) > 0)
      warning2("arguments in '...' are ignored when 'wave' is the output of prep_paged_spectro()")
    specParams <- wave
  } else {
    # name output files after the Wave object
    if (inherits(wave, "Wave") && is.null(prep_args$png.name))
      prep_args$png.name <- make.names(paste(deparse(substitute(wave)), collapse = ""))
    # PNGs go to the folder of the output file (unless 'dest.path' is supplied)
    if (is.null(prep_args$dest.path) && !is.null(file.name))
      prep_args$dest.path <- dirname(file.name)
    specParams <- do.call(prep_paged_int, c(list(wave = wave, plot = FALSE, verbose = verbose), prep_args))
  }

  # output folder and base name (sans extension); defaults to the folder and name used in prep_paged_spectro()
  if (!is.null(file.name)) {
    destFolder <- dirname(file.name)
    iName0 <- tools::file_path_sans_ext(basename(file.name))
  } else {
    destFolder <- specParams$destFolder
    iName0 <- tools::file_path_sans_ext(basename(specParams$outFilename))
  }

  vidName <- fs::path(destFolder, paste0(iName0, if (html) ".html" else ".mp4"))

  # ffmpeg cannot handle single quotes in file names
  if (grepl("'", iName0))
    stop2("the video file name contains a single quote ('), which ffmpeg cannot handle. Please remove it: ", iName0)

  if (grepl("'", destFolder))
    stop2("the destination folder contains a single quote ('), which ffmpeg cannot handle. Please remove it: ", destFolder)

  # create destination folder if needed
  if (!dir.exists(destFolder)) {
    dir.create(destFolder, recursive = TRUE)
    if (verbose) message("Created destination folder: ", destFolder)
  }

  # uniquely named temporary folder (so no user folder is ever deleted)
  temp_folder <- tempfile(pattern = "dynaSpec_temp_", tmpdir = destFolder)
  dir.create(temp_folder)
  if (delete_temp_files)
    on.exit(unlink(temp_folder, recursive = TRUE), add = TRUE)
  else if (verbose) message("Temporary files saved at: ", temp_folder)

  n_pages <- specParams$n_pages
  page_png <- function(i, res) fs::path(temp_folder, paste0(iName0, "_", i, "_", res, ".png"))

  # web page: page images (twice the resolution of videos) and highlight box extents
  if (html) {
    if (verbose) message("Rendering ", n_pages, " page(s) ...")
    pages <- lapply(seq_len(n_pages), function(i) {
      file <- page_png(i, "html")
      ext <- page_png_int(specParams, i, file, res = 1000 / specParams$specHeight)
      list(ext = ext, uri = base64_uri_int(file, "image/png"))
    })

    page_html <- paged_html_int(pages = pages, wave = specParams$newWav, highlightCol = highlightCol, highlightAlpha = highlightAlpha,
                                cursorCol = cursorCol, speeds = speeds, keep.pitch = keep.pitch, buttons = buttons,
                                download.name = download.name, captions = specParams$layers$captions,
                                res = 1000 / specParams$specHeight, width = 1000 / specParams$specHeight * specParams$specWidth,
                                theme = if (is.null(specParams$theme)) list() else specParams$theme)

    if (verbose) message("Web page saved at: ", vidName)

    return(output_html_int(page_html, file.name = vidName, play = play))
  }

  # video: each page is drawn once (500 pixels high) and ffmpeg moves the highlight box and cursor over it
  render_page <- function(i) {
    png_file <- page_png(i, "video")
    ext <- page_png_int(specParams, i, png_file, res = 500 / specParams$specHeight)
    paged_video_int(png_file = png_file, wave = specParams$segWavs[[i]], ext = ext, file.name = fs::path(temp_folder, paste0(iName0, "_", i, "_.mp4")),
                    highlightCol = highlightCol, highlightAlpha = highlightAlpha, cursorCol = cursorCol, fps = framerate,
                    captions = specParams$layers$captions, res = 500 / specParams$specHeight)
  }

  # render pages (with progress bar)
  segmented <- n_pages > 1
  .pbapply_dyn(X = seq_len(n_pages), cl = min(parallel, n_pages), pbar = verbose, title = "Rendering video pages", FUN = render_page)

  # combine pages
  segVids <- fs::path(temp_folder, paste0(iName0, "_", seq_len(n_pages), "_.mp4"))
  if (segmented) {
    writeLines(paste0("file '", segVids, "'"), fs::path(temp_folder, "mp4Segments.txt"))
    run_ffmpeg_int(paste0('-f concat -safe 0 -i "', fs::path(temp_folder, "mp4Segments.txt"), '" -codec copy -y "', vidName, '"'), "concatenating video segments")
  } else
    file.copy(segVids, vidName, overwrite = TRUE)

  if (verbose) message("Video saved at: ", vidName)

  if (play)
    open_file_int(vidName)

  invisible(vidName)
}

### dynaspec objects (layered syntax) -----------------------------------------------------------------------

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

# keep the selections of the sound shown (tables with a 'sound.files' column, e.g. warbleR selection tables)
sound_file_selections_int <- function(sel, x) {
  X <- sel$X
  if (!"sound.files" %in% names(X) || !is.null(sel$sound.file) || length(unique(X$sound.files)) == 1)
    return(sel)
  if (!is.character(x$wave))
    stop2("the selection table has selections of several sound files; choose one with 'sound.file' in annotations()")
  keep <- X$sound.files == basename(x$wave)
  if (!any(keep))
    warning2("no selections of sound file '", basename(x$wave), "' in the selection table")
  sel$X <- X[keep, , drop = FALSE]
  sel
}

# shift times of selections and captions (e.g. when sounds are cropped)
shift_layers_int <- function(layers, d) {
  layers$selections <- lapply(layers$selections, function(sel) { sel$X$start <- sel$X$start + d; sel$X$end <- sel$X$end + d; sel })
  layers$captions <- lapply(layers$captions, function(a) { a$start <- a$start + d; a$end <- a$end + d; a })
  layers
}

# spectrogram arguments for seewave-based engines (static and scrolling): 'amp.trans' only applies to paged ones
spectro_args_int <- function(sp) {
  if (!is.null(sp$amp.trans))
    warning2("'amp.trans' is only used by paged spectrograms")
  sp$amp.trans <- NULL
  sp
}

# settings of the theme used by other layers when not supplied there: spectrogram palette and background, colors of
# panels, play line and highlight box, and font family of captions
theme_layers_int <- function(x) {
  th <- x$theme
  if (length(th) == 0) return(x)
  if (!is.null(x$spectrogram))
    for (i in intersect(c("pal", "colbg"), names(th)))
      if (is.null(x$spectrogram[[i]])) x$spectrogram[[i]] <- th[[i]]
  x$panels <- lapply(x$panels, function(p) { if (is.null(p$col)) p$col <- th$col.wave; p })
  if (!is.null(x$spectrum) && is.null(x$spectrum$col)) x$spectrum$col <- th$col.wave
  if (!is.null(th$col.line) && is.null(x$style$args$lcol)) x$style$args$lcol <- th$col.line
  if (identical(x$style$type, "scrolling"))
    for (i in c("lty", "lwd"))
      if (!is.null(th[[paste0(i, ".line")]]) && is.null(x$style$args[[i]])) x$style$args[[i]] <- th[[paste0(i, ".line")]]
  # default buttons of rounded (seek bar) and square (volume) player controls
  if (!is.null(th$bar.style) && th$bar.style != "standard" && is.null(x$player$buttons))
    x$player$buttons <- if (th$bar.style == "rounded") c("progress", "time") else c("volume", "time")
  if (!is.null(th$col.highlight) && identical(x$style$type, "paged") && is.null(x$style$args$highlight.col))
    x$style$args$highlight.col <- th$col.highlight
  if (!is.null(th$family)) x$captions <- lapply(x$captions, function(cp) { cp$family <- th$family; cp })
  x
}

# color of the lowest amplitude of a palette (default background of spectrograms)
low_col_int <- function(pal, n = 30) pal_fun_int(pal)(max(2, n))[1]

# color palette as a function (palettes can also be the name of a 'viridis' palette or two colors for a gradient)
pal_fun_int <- function(pal) {
  if (is.function(pal) || is.null(pal)) return(pal)
  function(n) paged_palette_int(pal, n)
}

# static spectrograms
save_static_int <- function(x, wave, layers, out, ext, play) {
  sp <- spectro_args_int(x$spectrogram)
  if (!is.null(sp$pal)) sp$pal <- pal_fun_int(sp$pal)
  panels <- side_panels_int(x)
  args <- c(
    list(wave = wave, file.name = out, flim = x$flim, play = play, layers = layers,
         spectro = !is.null(x$spectrogram), osc = length(panels) > 0, panels = panels,
         spectrum = if (!is.null(x$spectrum)) utils::modifyList(x$spectrum, list(col = panel_col_int(x$spectrum, "spectrum"))),
         theme = minimal_axis_int(x$theme, "static")),
    if (!is.null(x$theme$cex.lab)) list(cex.lab = 1.2 * x$theme$cex.lab),
    sp, x$style$args, x$player, list(download.name = x$name)
  )
  if (!is.null(x$overview))
    warning2("overview() is only shown in scrolling spectrograms")
  if ("zoom" %in% x$player$buttons)
    warning2("the zoom slider is only available in scrolling spectrograms")
  do.call(static_spectro_int, args, quote = TRUE)
}

# minimal axes (scale bars) are only available in scrolling spectrograms
minimal_axis_int <- function(theme, style) {
  if (identical(theme$axis.type, "minimal")) {
    warning2("axis.type = \"minimal\" is only available in scrolling spectrograms; standard axes are used")
    theme$axis.type <- "standard"
  }
  theme
}

# oscillogram and envelope panels of static spectrograms (in the order they were added)
side_panels_int <- function(x) {
  lapply(x$panels, function(p) list(type = p$type, position = p$position, col = panel_col_int(p), height = if (is.null(p$height)) 0.5 else p$height))
}

# color of oscillogram, envelope and spectrum panels (supplied, from the theme or the default)
panel_col_int <- function(p, type = p$type) {
  if (!is.null(p$col)) p$col else if (identical(type, "osc")) "black" else "#31688E"
}

# scrolling spectrograms
save_scrolling_int <- function(x, wave, layers, out, play, verbose) {
  if (is.null(x$spectrogram))
    stop2("scrolling spectrograms need a spectrogram: add spectrogram() (e.g. dynaspec(wave) + spectrogram() + scrolling())")
  sp <- spectro_args_int(x$spectrogram)
  if (!is.null(sp$pal)) sp$pal <- pal_fun_int(sp$pal)
  st <- x$style$args

  # one lower panel: oscillogram, amplitude envelope or overview (lower spectrogram of the whole sound)
  lower <- c(vapply(x$panels, `[[`, character(1), "type"), if (!is.null(x$overview)) "overview")
  if (length(lower) > 1)
    warning2("scrolling spectrograms show a single lower panel; only the first one added is shown (", lower[1], ")")
  if (!is.null(x$spectrum))
    warning2("spectrum() is only shown in static spectrograms")
  st$lower.spectro <- length(lower) > 0 && lower[1] == "overview"
  osc <- if (length(lower) > 0 && lower[1] != "overview") x$panels[[1]]
  if (!is.null(x$overview) && st$lower.spectro && !is.null(x$overview$height))
    st$height.prop <- c(1, x$overview$height)

  args <- c(
    list(wave = wave, file.name = out, flim = x$flim, play = play, verbose = verbose, layers = list(selections = layers$selections),
         captions = layers$captions),
    if (!is.null(osc)) c(list(osc = TRUE, colwave = panel_col_int(osc), osc.type = osc$type), if (!is.null(osc$height)) list(height.prop = c(1, osc$height))),
    list(theme = x$theme), if (!is.null(x$theme$axis.type)) list(axis.type = x$theme$axis.type),
    sp, st, x$player, list(download.name = x$name)
  )
  do.call(scrolling_spectro_int, args, quote = TRUE)
}

# paged spectrograms (style arguments are split between the preparation of the pages and the engine)
save_paged_int <- function(x, wave, layers, out, ext, play, verbose) {
  if (is.null(x$spectrogram))
    stop2("paged spectrograms need a spectrogram: add spectrogram() (e.g. dynaspec(wave) + spectrogram() + paged())")
  unused <- c(if (length(x$panels) > 0) "oscillogram() and envelope()", if (!is.null(x$spectrum)) "spectrum()", if (!is.null(x$overview)) "overview()")
  if (length(unused) > 0)
    warning2(paste(unused, collapse = ", "), " are not shown in paged spectrograms")
  if ("zoom" %in% x$player$buttons)
    warning2("the zoom slider is only available in scrolling spectrograms")
  st <- x$style$args
  engine_names <- c("fps", "highlight.col", "highlight.alpha", "lcol", "delete.temp", "parallel")
  engine_args <- c(st[intersect(names(st), engine_names)], x$player, list(download.name = x$name))
  prep_args <- st[setdiff(names(st), engine_names)]

  # spectrogram settings ('collevels' sets the amplitude range and number of color bands)
  sp <- x$spectrogram
  if (!is.null(sp$collevels)) {
    prep_args$min.db <- min(sp$collevels)
    prep_args$col.bins <- length(sp$collevels) - 1
    sp$collevels <- NULL
  }
  prep_args <- c(prep_args, sp)
  if (!is.null(x$flim)) prep_args$flim <- x$flim

  # whole sound (cropping is done by dynaspec())
  prep_args$crop <- FALSE
  prep_args$png.name <- paste0(x$name, ".png")
  prep_args$sound.name <- x$name
  prep_args$layers <- layers
  th <- minimal_axis_int(x$theme, "paged")
  prep_args$theme <- th
  if (!is.null(th$axis.type)) prep_args$axis.type <- th$axis.type

  # pages as png files
  if (ext == "png") {
    pp <- do.call(prep_paged_int, c(list(wave = wave, dest.path = dirname(out), plot = FALSE, verbose = verbose), prep_args))
    files <- if (pp$n_pages == 1) out else paste0(tools::file_path_sans_ext(out), "_", seq_len(pp$n_pages), ".png")
    for (i in seq_len(pp$n_pages)) page_png_int(pp, i, files[i], res = 300)
    if (verbose) message("Page(s) saved at: ", paste(files, collapse = ", "))
    if (play) for (f in files) open_file_int(normalizePath(f))
    return(invisible(files))
  }

  do.call(paged_spectro_int, c(list(wave = wave, file.name = out, play = play, verbose = verbose), engine_args, prep_args), quote = TRUE)
}

