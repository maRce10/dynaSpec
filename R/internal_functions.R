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

  for (i in c("fix.time", "play", "ggspectro", "lower.spectro", "derivative", "osc", "save.png", "plot.legend", "bg.flood", "delete.temp", "plot", "verbose", "keep.pitch", "spectro")) {
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
    checkmate::assert_choice(args$axis.type, choices = if (fun == "prep_paged_spectro") c("standard", "none") else c("standard", "minimal", "none"), add = check_collection, .var.name = "axis.type")
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

  ### static_spectro() and sound_buttons() arguments -------------------------

  if (!is.null(args$html.file.name)) {
    checkmate::assert_string(args$html.file.name, pattern = if (fun == "static_spectro") "\\.(html?|mp4|png)$" else "\\.html?$", ignore.case = TRUE, add = check_collection, .var.name = "file.name")
  }

  if (!is.null(args$speeds)) {
    checkmate::assert_numeric(args$speeds, lower = 0.0625, upper = 16, min.len = 1, any.missing = FALSE, unique = TRUE, add = check_collection, .var.name = "speeds")
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

### HTML players (static_spectro(), sound_buttons(), and web pages of scrolling_spectro() and paged_spectro()) ---

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
oscillo_player_int <- function(wave, xlim, colwave, n, axisX = FALSE, cex.lab = 1, offset = 0, max.amp = max(abs(wave@left), 1e-12)) {
  amp <- wave@left / max.amp
  tm <- offset + (seq_along(amp) - 1) / wave@samp.rate

  # small vertical margin so peaks do not touch the box
  graphics::plot(NA, xlim = xlim, ylim = c(-1.05, 1.05), xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "o")
  graphics::mtext("Amp.", side = 2, line = 0.5, cex = cex.lab, las = 1)

  if (axisX)
    time_axis_int(to_x = identity, xlim = xlim, cex.lab = cex.lab)

  if (length(amp) > 4 * n) {
    bins <- cut(tm, breaks = n, labels = FALSE)
    mins <- vapply(split(amp, bins), min, numeric(1))
    maxs <- vapply(split(amp, bins), max, numeric(1))
    x <- vapply(split(tm, bins), mean, numeric(1))
    graphics::segments(x0 = x, y0 = mins, y1 = maxs, col = colwave)
  } else
    graphics::lines(tm, amp, col = colwave)
}

# time axis; 'to_x' converts times to x axis coordinates
time_axis_int <- function(to_x, xlim, cex.lab) {
  # ticks slightly outside the plotted range are kept (the spectrogram starts and ends half a window inside the sound)
  tol <- diff(xlim) * 0.01
  at <- pretty(xlim)
  at <- at[at >= xlim[1] - tol & at <= xlim[2] + tol]
  # axis() drops ticks outside the plotting region, so those are moved to its edge (less than a pixel away)
  usr <- graphics::par("usr")
  graphics::axis(1, at = pmin(pmax(to_x(at), usr[1]), usr[2]), labels = at)
  graphics::mtext("Time (s)", side = 1, line = 3, cex = cex.lab)
}

# web page of scrolling_spectro(): a frame with the parts that do not move (axes, play line, lower spectrogram),
# with the full spectrogram (and oscillogram) below it, moved by the web page in sync with the sound
scrolling_html_int <- function(frame_file, spectro_file, osc_file, lower, plt, height.prop, wave, wave_sil, t.display, buffer,
                               step_time, step_time_low, fps, loop, fix.time, axis, width, res, annotation, annotation.args,
                               speeds, keep.pitch, lcol) {
  # plotting region (proportions of the frame from the top); the upper part has the scrolling spectrogram
  split <- plt[3] + (plt[4] - plt[3]) * height.prop[2] / height.prop[1]
  dur_wave <- seewave::duration(wave)

  # sound with the silence added at the start (as in videos)
  if (buffer > 0)
    wave <- seewave::pastew(wave2 = tuneR::silence(duration = buffer, samp.rate = wave@samp.rate, xunit = "time"), wave1 = wave, f = wave@samp.rate, output = "Wave")
  snd <- sound_data_int(wave)

  # annotations: one label at a time, fading in and out
  ann <- NULL
  if (!is.null(annotation)) {
    a <- annotation.args
    ann <- list(
      labels = as.character(a$labels), start = a$start + buffer, end = a$end + buffer,
      fading = if (is.null(a$fading)) 0 else a$fading,
      x = plt[1] + (plt[2] - plt[1]) * a$x[1], y = 1 - (plt[3] + (plt[4] - plt[3]) * a$y[1]),
      col = col2hex_int(if (is.null(a$col)) "black" else a$col[1]), cex = if (is.null(a$cex)) 1 else a$cex[1],
      font = if (is.null(a$font)) 1 else a$font[1], pos = if (is.null(a$pos)) 0 else a$pos[1]
    )
  }

  tick_step <- diff(pretty(c(0, t.display)))[1]

  cfg <- list(
    duration = seewave::duration(wave), durSil = seewave::duration(wave_sil), durWave = dur_wave, tDisplay = t.display,
    buffer = buffer, k = step_time * fps, kl = step_time_low * fps / t.display, loop = loop > 1, lower = lower,
    left = plt[1], right = plt[2], upTop = 1 - plt[4], upBottom = 1 - split, lowTop = 1 - split, lowBottom = 1 - plt[3],
    movingAxis = axis && !fix.time, tickStep = tick_step, tickDigits = max(0, -floor(log10(tick_step) + 1e-9)),
    width = width, res = res, ann = ann
  )

  paste0(
    '<div class="dynaspec-scroll" data-speeds="', paste(speeds, collapse = ","), '" data-keep-pitch="', tolower(keep.pitch),
    '" data-color="', col2hex_int(lcol), '" data-text-color="', contrast_col_int(lcol), '">\n',
    '<script type="application/json">', json_int(cfg), '</script>\n',
    '<img class="ds-frame" src="', base64_uri_int(frame_file, "image/png"), '" alt="Scrolling spectrogram">\n',
    '<img class="ds-strip" src="', base64_uri_int(spectro_file, "image/png"), '" alt="">\n',
    if (!is.null(osc_file)) paste0('<img class="ds-strip ds-osc" src="', base64_uri_int(osc_file, "image/png"), '" alt="">\n'),
    '<audio controls preload="auto"', if (loop > 1) " loop", ' src="', snd$uri, '"></audio>\n',
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
paged_html_int <- function(pages, wave, highlightCol, highlightAlpha, cursorCol, speeds, keep.pitch) {
  ext <- do.call(rbind, lapply(pages, `[[`, "ext"))
  snd <- sound_data_int(wave)
  n_pages <- length(pages)

  paste0(
    '<div class="dynaspec-paged" data-duration="', seewave::duration(wave),
    '" data-rows="', paste(apply(round(ext, 5), 1, paste, collapse = ","), collapse = ";"),
    '" data-highlight="', col2hex_int(highlightCol), '" data-alpha="', highlightAlpha, '" data-cursor="', col2hex_int(cursorCol),
    '" data-speeds="', paste(speeds, collapse = ","), '" data-keep-pitch="', tolower(keep.pitch),
    '" data-color="', col2hex_int(highlightCol), '" data-text-color="', contrast_col_int(highlightCol), '">\n',
    paste0('<img src="', vapply(pages, `[[`, character(1), "uri"), '" alt="Spectrogram, page ', seq_len(n_pages), ' of ', n_pages, '">', collapse = "\n"),
    '\n<audio controls preload="auto" src="', snd$uri, '"></audio>\n',
    '</div>\n',
    player_js_int()
  )
}

# video of one page of a paged spectrogram: the translucent highlight box (from the cursor to the end of the
# page) and the cursor move over the page image along the sound ('ext': time range and box extents)
paged_video_int <- function(png_file, wave, ext, file.name, highlightCol, highlightAlpha, cursorCol, fps) {
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
    "[v2][2]overlay=x='", x0 - lw / 2, "+", speed, "*t':y=", y0, ",pad=ceil(iw/2)*2:ceil(ih/2)*2,format=yuv420p[v]"
  )

  run_ffmpeg_int(
    paste0(
      '-y -loop 1 -framerate ', fps, ' -i "', png_file, '"',
      ' -f lavfi -i "color=c=', sub("#", "0x", col2hex_int(highlightCol)), ':s=', x1 - x0, 'x', bh, ':r=', fps, '"',
      ' -f lavfi -i "color=c=', sub("#", "0x", col2hex_int(cursorCol)), ':s=', lw, 'x', bh, ':r=', fps, '"',
      ' -i "', wav_file, '" -filter_complex "', filter, '" -map "[v]" -map 3:a',
      ' -c:v libx264 -tune stillimage -r ', fps, ' -c:a aac -b:a 192k -ac ', if (wave@stereo) 2 else 1, ' -t ', dur,
      ' "', file.name, '"'
    ),
    "creating the video"
  )

  invisible(file.name)
}

# video of a static_spectro() figure with the play line moving along the sound
# 'ext' has one row per segment (t0, t1, and left, right, top and bottom as proportions of the figure)
playhead_video_int <- function(png_file, wave, ext, file.name, lcol, fps, width, height) {
  wav_file <- tempfile(fileext = ".wav")
  on.exit(unlink(wav_file), add = TRUE)
  tuneR::writeWave(if (wave@pcm) wave else tuneR::normalize(wave, unit = "16"), filename = wav_file)

  dur <- utils::tail(ext[, "t1"], 1)
  lw <- max(2, round(width / 500))
  col <- sub("#", "0x", col2hex_int(lcol))

  # one line (color source) per row, shown while that row plays. The overlay filter evaluates the
  # position expressions on every frame (unlike drawbox in ffmpeg < 5)
  n_rows <- nrow(ext)
  lines <- vapply(seq_len(n_rows), function(k) {
    e <- ext[k, ]
    line_h <- round((e[["bottom"]] - e[["top"]]) * height)
    x0 <- e[["left"]] * width - lw / 2
    dx <- (e[["right"]] - e[["left"]]) * width / (e[["t1"]] - e[["t0"]])
    enable <- if (k < n_rows) sprintf("gte(t,%f)*lt(t,%f)", e[["t0"]], e[["t1"]]) else sprintf("gte(t,%f)", e[["t0"]])
    sprintf("[v%d][%d]overlay=x='%f+%f*(t-%f)':y=%d:enable='%s'[v%d]", k - 1, k + 1, x0, dx, e[["t0"]], round(e[["top"]] * height), enable, k)
  }, character(1))

  # video dimensions must be even
  filter <- paste(c("[0]pad=ceil(iw/2)*2:ceil(ih/2)*2:color=white[v0]", lines), collapse = ";")

  sources <- vapply(seq_len(n_rows), function(k) {
    sprintf('-f lavfi -i "color=c=%s:s=%dx%d:r=%s"', col, lw, max(1, round((ext[k, "bottom"] - ext[k, "top"]) * height)), fps)
  }, character(1))

  run_ffmpeg_int(
    paste0(
      '-y -loop 1 -framerate ', fps, ' -i "', png_file, '" -i "', wav_file, '" ', paste(sources, collapse = " "),
      ' -filter_complex "', filter, '" -map "[v', n_rows, ']" -map 1:a',
      ' -c:v libx264 -tune stillimage -pix_fmt yuv420p -r ', fps,
      # explicit number of channels (the aac encoder rejects the channel layout of mono files written by tuneR)
      ' -c:a aac -b:a 192k -ac ', if (wave@stereo) 2 else 1, ' -t ', dur,
      ' -movflags +faststart "', file.name, '"'
    ),
    "creating the video"
  )

  invisible(file.name)
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
col2hex_int <- function(col) {
  rgb <- grDevices::col2rgb(col)
  grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
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
      ".dynaspec-player audio,.dynaspec-sounds audio,.dynaspec-paged audio{display:none}" +
      ".ds-box{position:absolute;pointer-events:none}" +
      // reset image styles of the hosting page (e.g. backgrounds, borders or padding added by R Markdown themes)
      ".dynaspec-player img,.dynaspec-paged img,.dynaspec-scroll img{background:transparent!important;border:0!important;padding:0!important;border-radius:0!important;box-shadow:none!important;margin:0!important}" +
      ".dynaspec-scroll audio{display:none}" +
      ".dynaspec-scroll .ds-wrap{position:relative;cursor:pointer;background:#fff;overflow:hidden}" +
      ".dynaspec-scroll img.ds-frame{position:relative;z-index:2;display:block;width:100%;height:auto;margin:0}" +
      ".ds-vp{position:absolute;overflow:hidden;z-index:1}" +
      ".ds-vp img.ds-strip{position:absolute;top:0;height:100%;max-width:none;margin:0;display:block}" +
      ".ds-over{position:absolute;z-index:3;pointer-events:none}" +
      ".ds-text{position:absolute;z-index:4;pointer-events:none;white-space:nowrap;font-family:Helvetica,Arial,sans-serif;line-height:1}" +
      ".ds-bar .ds-page{font-variant-numeric:tabular-nums;color:#555}" +
      ".ds-head{position:absolute;width:2px;margin-left:-1px;pointer-events:none}" +
      ".ds-bar{display:flex;flex-wrap:wrap;gap:.4em;align-items:center;margin:.4em 0 1.2em;font-size:.9em}" +
      ".ds-bar button{border:1px solid #bbb;background:#fff;color:#222;border-radius:4px;padding:.15em .7em;line-height:1.5;cursor:pointer}" +
      ".ds-bar .ds-play{min-width:5.5em;font-weight:600}" +
      ".ds-bar .ds-time{font-variant-numeric:tabular-nums;color:#555;margin-left:.3em}" +
      ".ds-bar .ds-sep{color:#999;margin:0 .2em}" +
      ".ds-bar .ds-pitch{margin-left:.6em}";
    document.head.appendChild(css);

    var setup = function (a, keep) {
      a.preservesPitch = a.mozPreservesPitch = a.webkitPreservesPitch = keep;
      a.addEventListener("play", function () {
        all.forEach(function (o) { if (o !== a) o.pause(); });
      });
      all.push(a);
    };

    var paintBtn = function (b, on, color) {
      b.style.background = on ? color[0] : "";
      b.style.borderColor = on ? color[0] : "";
      b.style.color = on ? color[1] : "";
      // outline so active buttons are visible even with colors similar to the page background
      b.style.boxShadow = on ? "inset 0 0 0 1px rgba(0,0,0,0.35)" : "";
    };

    // speed buttons and keep pitch toggle; returns a function giving the current speed
    var speedGroup = function (bar, speeds, color, keep, audios, onChange) {
      var rate = speeds[0], btns = [];
      if (speeds.length < 2) return function () { return rate; };
      var sep = document.createElement("span");
      sep.className = "ds-sep";
      sep.textContent = "speed";
      bar.appendChild(sep);
      var paint = function () {
        btns.forEach(function (b) { paintBtn(b, b.dsRate === rate, color); });
      };
      speeds.forEach(function (s) {
        var b = document.createElement("button");
        b.type = "button";
        b.dsRate = s;
        b.textContent = label(s);
        b.title = s === 1 ? "real time" : s < 1 ? "slowed down" : "sped up";
        b.addEventListener("click", function () { rate = s; paint(); onChange(rate); });
        btns.push(b);
        bar.appendChild(b);
      });
      paint();

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

      var raf, fmt = function (t) { return dur < 5 ? t.toFixed(3) : t.toFixed(1); };
      var draw = function () {
        var t = Math.min(a.currentTime, dur), r = rowAt(t);
        head.style.left = 100 * (r.left + (r.right - r.left) * (t - r.t0) / (r.t1 - r.t0)) + "%";
        head.style.top = 100 * r.top + "%";
        head.style.height = 100 * (r.bottom - r.top) + "%";
        time.textContent = fmt(t) + " / " + fmt(dur) + " s";
        if (!a.paused) raf = requestAnimationFrame(draw);
      };
      draw();
      a.addEventListener("play", function () { play.textContent = "\\u275A\\u275A pause"; draw(); });
      a.addEventListener("pause", function () { play.textContent = "\\u25B6 play"; cancelAnimationFrame(raf); draw(); });
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

      var raf, shown = 0, fmt = function (t) { return dur < 5 ? t.toFixed(3) : t.toFixed(1); };
      var draw = function () {
        var t = Math.min(a.currentTime, dur), k = pageAt(t), p = pages[k];
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
      var go = function (k) {
        a.currentTime = pages[Math.max(0, Math.min(pages.length - 1, k))].t0;
        draw();
      };
      prev.addEventListener("click", function () { go(pageAt(a.currentTime) - 1); });
      next.addEventListener("click", function () { go(pageAt(a.currentTime) + 1); });
      a.addEventListener("play", function () { play.textContent = "\\u275A\\u275A pause"; draw(); });
      a.addEventListener("pause", function () { play.textContent = "\\u25B6 play"; cancelAnimationFrame(raf); draw(); });
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
      frame.parentNode.insertBefore(wrap, frame);
      wrap.appendChild(frame);

      // scrolling strips (viewports clip the part shown)
      var viewport = function (strip, top, bottom) {
        var vp = document.createElement("div");
        vp.className = "ds-vp";
        place(vp, c.left, c.right, top, bottom);
        vp.appendChild(strip);
        strip.style.width = pct(c.durSil / c.tDisplay);
        wrap.appendChild(vp);
        return strip;
      };
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
        grays = [over("rgba(190,190,190,0.42)"), over("rgba(190,190,190,0.42)")];
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
        tick.style.background = "#000";
        wrap.appendChild(el);
        wrap.appendChild(tick);
        return {text: el, tick: tick};
      };

      // annotations
      var ann = null;
      if (c.ann) {
        ann = document.createElement("div");
        ann.className = "ds-text";
        ann.style.color = c.ann.col;
        ann.style.fontWeight = c.ann.font === 2 || c.ann.font === 4 ? "bold" : "normal";
        ann.style.fontStyle = c.ann.font === 3 || c.ann.font === 4 ? "italic" : "normal";
        ann.style.left = pct(c.ann.x);
        ann.style.top = pct(c.ann.y);
        ann.style.transform = ["translate(-50%,-50%)", "translate(-50%,0.3em)", "translate(calc(-100% - 0.5em),-50%)", "translate(-50%,calc(-100% - 0.3em))", "translate(0.5em,-50%)"][c.ann.pos] || "translate(-50%,-50%)";
        wrap.appendChild(ann);
        ["labels", "start", "end"].forEach(function (k) { if (!Array.isArray(c.ann[k])) c.ann[k] = [c.ann[k]]; });
      }

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

      var dur = c.duration, raf, fmt = function (t) { return dur < 5 ? t.toFixed(3) : t.toFixed(1); };
      var draw = function () {
        var t = Math.min(a.currentTime, dur);
        // text size follows the size of the figure (12 points at the resolution of the frame)
        var px = 12 * c.res / 72 * wrap.clientWidth / c.width;

        // start of the window shown (time of the sound with the half windows added at both ends)
        var s = Math.min(Math.max(0, t * c.k - c.buffer), c.durSil - c.tDisplay);
        moving.forEach(function (strip) { strip.style.left = pct(-s / c.tDisplay); });

        if (c.lower === "spectro") {
          var w = c.tDisplay / c.durWave, mid = Math.max(0, t * c.kl - c.buffer / c.durWave), x0 = mid - w / 2, x1 = mid + w / 2;
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
          var first = Math.ceil(s / c.tickStep - 1e-9), n = Math.floor((s + c.tDisplay) / c.tickStep + 1e-9) - first + 1;
          while (ticks.length < n) ticks.push(label());
          ticks.forEach(function (tk, i) {
            var on = i < n, v = (first + i) * c.tickStep, x = c.left + (c.right - c.left) * (v - s) / c.tDisplay;
            tk.text.style.display = tk.tick.style.display = on ? "block" : "none";
            if (!on) return;
            tk.text.textContent = v.toFixed(c.tickDigits);
            tk.text.style.fontSize = px + "px";
            tk.text.style.left = pct(x);
            // labels and ticks placed as in R axes (measured on frames drawn by R)
            tk.text.style.top = "calc(" + pct(c.lowBottom) + " + " + 1.35 * px + "px)";
            tk.tick.style.left = pct(x);
            tk.tick.style.top = pct(c.lowBottom);
            tk.tick.style.height = 0.6 * px + "px";
          });
        }

        if (ann) {
          var op = 0, txt = "";
          c.ann.labels.forEach(function (l, i) {
            var t0 = c.ann.start[i], t1 = c.ann.end[i], f = Math.max(c.ann.fading, 1e-3);
            if (t >= t0 && t <= t1) { var o = Math.min(1, (t - t0) / f, (t1 - t) / f); if (o > op) { op = o; txt = l; } }
          });
          ann.textContent = txt;
          ann.style.opacity = op;
          ann.style.fontSize = c.ann.cex * px + "px";
        }

        time.textContent = fmt(t) + " / " + fmt(dur) + " s";
        if (!a.paused) raf = requestAnimationFrame(draw);
      };
      draw();
      window.addEventListener("resize", draw);
      a.addEventListener("play", function () { play.textContent = "\\u275A\\u275A pause"; draw(); });
      a.addEventListener("pause", function () { play.textContent = "\\u25B6 play"; cancelAnimationFrame(raf); draw(); });
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
          var s = Math.min(Math.max(0, a.currentTime * c.k - c.buffer), c.durSil - c.tDisplay);
          orig = s + f * c.tDisplay - c.tDisplay / 2;
        }
        a.currentTime = Math.max(0, Math.min(dur - 0.001, (Math.max(0, orig) + c.buffer) / c.k));
        a.play();
      });
    };

    var initSounds = function (div) {
      var speeds = div.dataset.speeds.split(",").map(Number), getRate;
      var keep = div.dataset.keepPitch === "true";
      var bar = document.createElement("div");
      bar.className = "ds-bar";
      div.querySelectorAll("audio").forEach(function (a) {
        setup(a, keep);
        var b = document.createElement("button");
        b.type = "button";
        b.textContent = "\\u25B6 " + a.title;
        b.addEventListener("click", function () {
          a.currentTime = 0;
          a.playbackRate = getRate();
          a.play();
        });
        bar.appendChild(b);
      });
      getRate = speedGroup(bar, speeds, [div.dataset.color, div.dataset.textColor], keep, Array.prototype.slice.call(div.querySelectorAll("audio")), function () {});
      div.appendChild(bar);
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
      document.querySelectorAll(".dynaspec-sounds:not([data-ready])").forEach(function (div) {
        div.setAttribute("data-ready", "");
        initSounds(div);
      });
    };
  }
  window.dynaSpecPlayers();
})();
</script>'
}

### scrolling_spectro() helpers -------------------------------------------------------------------------

#internal dynaSpec function called by scrolling_spectro. Fades in and out annotated text. Not to be called by users.
# x is a vector with alpha (transparesence values)
# start and end are the position of the fading in and out
# fading is the length of the fading period, text is the annotation text
fading_text_dynaspec_int <- function(x, start, end, alpha = 1, fading, labels){
  
  if (fading == 0) fading <- 1
  
  #fading-in part
  fi <- seq(0, alpha, length.out = fading)
  
  # fade out 
  fo <- rev(fi)

  # modify x
  x[start:end] <- alpha
  x[start:(start + fading -1)] <- fi
  x[(end - fading + 1):(end)] <- fo
  
  names(x)[start:end] <- labels
  
  return(x)
}

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

    # only downsample (upsampling is not supported)
    if (resampleRate < wav@samp.rate) {
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
  txt_col <- if (!sp$autoFontCol) sp$fontAndAxisCol else if (flood) contrast_col_int(sp$bg) else "black"
  has_title <- !is.null(sp$title) && nzchar(sp$title)

  op <- graphics::par(
    bg = if (flood) sp$bg else "white",
    mar = c(if (axes) 4.2 else 0, if (axes) 5.4 else 0, if (has_title) 3 else if (axes) 1.2 else 0, if (sp$plotLegend) 7 else if (axes) 1.2 else 0),
    col.axis = txt_col, col.lab = txt_col, col.main = txt_col, fg = txt_col, las = 1
  )
  on.exit(graphics::par(op), add = TRUE)

  # axis ranges expanded 5% on each side (as in previous versions)
  graphics::plot.new()
  graphics::plot.window(xlim = range_i + c(-1, 1) * 0.05 * diff(range_i), ylim = sp$yLim + c(-1, 1) * 0.05 * diff(sp$yLim), xaxs = "i", yaxs = "i")
  usr <- graphics::par("usr")
  graphics::rect(usr[1], usr[3], usr[2], usr[4], col = sp$bg, border = NA)

  if (length(d$time) > 1 && length(d$freq) > 1)
    graphics::.filled.contour(x = d$time, y = d$freq, z = t(d$amp), levels = levels, col = cols)

  # selections and annotations
  draw_layers_int(sp$layers, panel = "spectro", ylim = usr[3:4])

  if (axes) {
    graphics::axis(1, cex.axis = 16 / 12, tcl = -0.3, mgp = c(3, 0.6, 0), col = "gray20", col.axis = txt_col)
    graphics::axis(2, cex.axis = 16 / 12, tcl = -0.3, mgp = c(3, 0.6, 0), col = "gray20", col.axis = txt_col)
    graphics::title(xlab = "Time (s)", ylab = "Frequency (kHz)", cex.lab = 20 / 12, font.lab = 2, line = 2.7)
    graphics::box(col = "gray20")
  }

  if (has_title)
    graphics::title(main = sp$title, cex.main = 20 / 12, font.main = 2, adj = 0, line = 1)

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

# draw selections (boxes) and annotations (labels) of a dynaspec object in the current plot. 'to_x' converts
# times (s) of the sound to x coordinates of the plot. 'panel' is "spectro" (selections use their frequency
# range if available, and labels and annotations are drawn) or "osc" (selections span the whole panel)
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
    graphics::rect(to_x(X$start), bottom, to_x(X$end), top, col = grDevices::adjustcolor(cols, sel$alpha),
                   border = if (is.null(sel$border)) NA else sel$border, lwd = sel$lwd)

    # selection labels at the top of each box
    if (panel == "spectro" && !is.null(sel$labels) && sel$labels %in% names(X))
      graphics::text(to_x((X$start + X$end) / 2), top - 0.04 * diff(ylim), labels = X[[sel$labels]], adj = c(0.5, 1),
                     col = cols, cex = sel$cex, font = 2)
  }

  # annotations are drawn at the center of their time range (scrolling spectrograms fade them in and out instead)
  if (panel == "spectro")
    for (ann in layers$annotations)
      graphics::text(to_x((ann$start + ann$end) / 2), ylim[1] + ann$y * diff(ylim), labels = ann$labels,
                     col = ann$col, cex = ann$cex, font = ann$font)

  invisible(NULL)
}

### engines (static, scrolling and paged spectrograms) -------------------------------------------------------

# static spectrogram engine (dynaspec() + static()): figure with a play line moving over it. 'layers' are the
# selections and annotations of the dynaspec object
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
                           colbg = "white",
                           res = 100,
                           width = 1000,
                           height = 500,
                           rows = 1,
                           lcol = "#c0392b",
                           speeds = c(1, 0.5, 0.25, 0.125),
                           keep.pitch = FALSE,
                           alt = NULL,
                           fps = 30,
                           play = TRUE,
                           layers = NULL,
                           ...)
{
  # check arguments
  check_results <- .check_arguments(
    fun = "static_spectro",
    args = list(wave = wave, html.file.name = file.name, wl = wl, ovlp = ovlp, flim = flim, pal = pal, collevels = collevels, spectro = spectro, osc = osc, osc.position = osc.position, cex.lab = cex.lab, height.prop = height.prop, colwave = colwave, colbg = colbg, res = res, width = width, height = height, rows = rows, lcol = lcol, speeds = speeds, keep.pitch = keep.pitch, alt = alt, fps = fps, play = play)
  )

  # report errors
  .report_assertions(check_results)

  dots <- list(...)
  if (any(names(dots) %in% c("scale", "osc", "axisX", "axisY", "cexlab", "tlim", "tlab", "norm", "dBref")))
    stop2("'scale', 'osc', 'axisX', 'axisY', 'cexlab', 'tlim', 'tlab', 'norm' and 'dBref' cannot be supplied through '...' (use 'cex.lab' for the size of axis titles)")

  video <- !is.null(file.name) && grepl("\\.mp4$", file.name, ignore.case = TRUE)
  if (video && !have_ffmpeg_exec2())
    stop2("'ffmpeg' could not be found. It must be installed (and available in the system path) to create videos (see https://github.com/maRce10/dynaSpec#ffmpeg)")

  if (isFALSE(spectro) && isFALSE(osc))
    stop2("at least one of 'spectro' and 'osc' must be TRUE")

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

  below <- osc && osc.position == "below"

  # margins (in lines) of the spectrogram and oscillogram panels. The bottom panel of the last row has the time
  # axis; the other rows have a small gap instead
  spec_mar <- function(axis) c(if (below) 0.2 else if (axis) 4 else 0.5, 4, if (osc && !below) 0.2 else 1, 1)
  osc_mar <- function(axis) if (!spectro) c(if (axis) 4 else 0.5, 4, 1, 1) else c(if (below) { if (axis) 4 else 0.5 } else 0, 4, if (below) 0.2 else 1, 1)

  # panel heights (in inches) that keep 'height.prop' between plotting regions, with the same plotting
  # regions in all rows ('height' is the height of a row with time axis)
  lines_in <- 0.2 # height of a line of text in the png device (12 points * 1.2)
  panel_mar <- function(axis) c(if (spectro) sum(spec_mar(axis)[c(1, 3)]), if (osc) sum(osc_mar(axis)[c(1, 3)])) * lines_in
  plot_in <- height / res - sum(panel_mar(TRUE))
  if (plot_in <= 0)
    stop2("'height' is too small to fit the figure (increase 'height' or decrease 'res')")
  panel_heights <- function(axis) plot_in * (if (spectro && osc) height.prop / sum(height.prop) else 1) + panel_mar(axis)
  fig_height <- round(res * ((rows - 1) * sum(panel_heights(FALSE)) + sum(panel_heights(TRUE))))

  # maximum image size of the png device
  if (fig_height > 32767)
    stop2("the figure is too tall (decrease 'height' or 'rows')")

  # one row per segment; within a row the spectrogram is plotted first (its time range is used for the oscillogram)
  np <- spectro + osc
  order_row <- if (np == 2 && !below) 2:1 else seq_len(np)

  grDevices::png(filename = png_file, width = width, height = fig_height, res = res, bg = colbg)
  dev <- grDevices::dev.cur()
  ext <- tryCatch({
    graphics::layout(matrix(unlist(lapply(seq_len(rows) - 1, function(k) k * np + order_row)), ncol = 1),
                     heights = unlist(lapply(seq_len(rows), function(k) panel_heights(k == rows)[order_row])))

    # layout() shrinks text when there are 3 or more panels; keep the same size as with a single row
    graphics::par(cex = 1)

    # reference for dB and amplitude (so all rows share the same scale)
    if (spectro)
      max_amp <- max(seewave::spectro(wav, f = f, wl = wl, ovlp = ovlp, plot = FALSE, norm = FALSE, dB = NULL,
                                      wn = if (is.null(dots$wn)) "hanning" else dots$wn, zp = if (is.null(dots$zp)) 0 else dots$zp)$amp)
    max_wave <- max(abs(wav@left), 1e-12)

    ndc_y <- function(y) 1 - graphics::grconvertY(y, "user", "ndc")

    t(vapply(seq_len(rows), function(k) {
      seg <- wav[(row_lims[k] + 1):row_lims[k + 1]]
      t0 <- row_lims[k] / f
      t1 <- row_lims[k + 1] / f
      axis <- k == rows

      if (spectro) {
        graphics::par(mar = spec_mar(axis), bg = colbg)
        suppressMessages(seewave::spectro(wave = seg, f = f, wl = wl, ovlp = ovlp, flim = flim, palette = pal, collevels = collevels, scale = FALSE, osc = FALSE, grid = FALSE, colbg = colbg, axisX = FALSE, tlab = "", cexlab = cex.lab, norm = FALSE, dBref = max_amp, ...))

        # seewave::spectro() spreads the analysis windows evenly from 0 to the duration of the sound, but the
        # center of the first window is at wl / 2 and that of the last one is wl / 2 before the end. Times are
        # converted between the spectrogram x axis and the sound so the play line matches the sound
        n_seg <- length(seg@left)
        last_step <- utils::tail(seq(1, n_seg + 1 - wl, wl - (ovlp * wl / 100)), 1) - 1
        to_x <- function(t) (t - t0 - wl / (2 * f)) * n_seg / last_step
        to_time <- function(x) t0 + wl / (2 * f) + x * last_step / n_seg

        usr <- graphics::par("usr")

        # selections and annotations
        draw_layers_int(layers, to_x = to_x, panel = "spectro", ylim = usr[3:4])

        # time axis (only if the spectrogram is the bottom panel)
        if (!below && axis)
          time_axis_int(to_x = to_x, xlim = to_time(usr[1:2]), cex.lab = cex.lab)

        ext <- c(t0 = t0, t1 = t1, left = graphics::grconvertX(to_x(t0), "user", "ndc"), right = graphics::grconvertX(to_x(t1), "user", "ndc"),
                 top = ndc_y(usr[4]), bottom = ndc_y(usr[3]))
      }

      if (osc) {
        graphics::par(mar = osc_mar(axis), bg = colbg)
        oscillo_player_int(seg, xlim = if (spectro) to_time(usr[1:2]) else c(t0, t1), colwave = colwave, n = width, axisX = (below || !spectro) && axis, cex.lab = cex.lab, offset = t0, max.amp = max_wave)
        osc_usr <- graphics::par("usr")
        draw_layers_int(layers, to_x = identity, panel = "osc", ylim = osc_usr[3:4])

        if (!spectro)
          ext <- c(t0 = t0, t1 = t1, left = graphics::grconvertX(t0, "user", "ndc"), right = graphics::grconvertX(t1, "user", "ndc"),
                   top = ndc_y(osc_usr[4]), bottom = ndc_y(osc_usr[3])) else
          if (below) ext["bottom"] <- ndc_y(osc_usr[3]) else ext["top"] <- ndc_y(osc_usr[4])
      }
      ext
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
    playhead_video_int(png_file = png_file, wave = snd$wave, ext = ext, file.name = file.name, lcol = lcol, fps = fps, width = width, height = fig_height)
    if (play)
      open_file_int(normalizePath(file.name))
    return(invisible(file.name))
  }

  # player html
  html <- paste0(
    '<div class="dynaspec-player" data-duration="', dur,
    '" data-rows="', paste(apply(round(ext, 5), 1, paste, collapse = ","), collapse = ";"),
    '" data-speeds="', paste(speeds, collapse = ","), '" data-keep-pitch="', tolower(keep.pitch), '" data-color="', col2hex_int(lcol), '" data-text-color="', contrast_col_int(lcol), '">\n',
    '<img src="', base64_uri_int(png_file, "image/png"), '" alt="', html_escape_int(if (is.null(alt)) paste0(if (!spectro) "Oscillogram" else if (osc) "Oscillogram and spectrogram" else "Spectrogram", " of a ", round(dur, 2), " s sound") else alt), '">\n',
    '<audio controls preload="auto" src="', snd$uri, '"></audio>\n',
    '</div>\n',
    player_js_int()
  )

  output_html_int(html, file.name = file.name, play = play)
}

# scrolling spectrogram engine (dynaspec() + scrolling()): videos and web pages of spectrograms scrolling along a
# play line. 'layers' are the selections of the dynaspec object (annotations are passed in 'annotation.call')
scrolling_spectro_int <- function(wave, file.name = "scroll.spectro.mp4", hop.size = 11.6, wl = NULL, ovlp = 70, flim = NULL, pal = seewave::reverse.gray.colors.1, speed = 1, fps = 50, t.display = 1.5, fix.time = TRUE, res = 70, width = 700, height = 400, parallel = 1, verbose = TRUE, play = TRUE, loop = 1, lcol = "#07889B99", lty = 2, lwd = 2, axis.type = "standard", buffer = 1, ggspectro = FALSE, lower.spectro = TRUE, height.prop = c(5, 1), derivative = FALSE, osc = FALSE, colwave = "black", colbg = "white", spectro.call = NULL, annotation.call = NULL, speeds = c(1, 0.5, 0.25, 0.125), keep.pitch = FALSE, layers = NULL, ...)
{

  # check arguments
  check_results <- .check_arguments(
    fun = "scrolling_spectro",
    args = list(wave = wave, file.name = file.name, hop.size = hop.size, wl = wl, ovlp = ovlp, flim = flim, pal = pal, speed = speed, fps = fps, t.display = t.display, fix.time = fix.time, res = res, width = width, height = height, parallel = parallel, verbose = verbose, play = play, loop = loop, lcol = lcol, lty = lty, lwd = lwd, axis.type = axis.type, buffer = buffer, ggspectro = ggspectro, lower.spectro = lower.spectro, height.prop = height.prop, derivative = derivative, osc = osc, colwave = colwave, colbg = colbg, spectro.call = spectro.call, annotation.call = annotation.call, speeds = speeds, keep.pitch = keep.pitch)
  )

  # report errors
  .report_assertions(check_results)

  # other arguments in '...' are passed to seewave::spectro()
  spectro_args <- list(...)

  # no grid lines (as in static spectrograms) unless requested
  if (is.null(spectro_args$grid)) spectro_args$grid <- FALSE

  # web page instead of video
  html <- grepl("\\.html?$", file.name, ignore.case = TRUE)

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

  # annotation arguments (evaluated) for web pages
  annotation_args <- if (!is.null(annotation.call)) lapply(as.list(annotation.call)[-1], eval)

  # create a color transparency vector for labeling annotations
  if (!is.null(annotation.call)){

  # create vector
  ann_alpha <- rep(0, frames)

    # loop over labels
    # a sound at time 's' reaches the play line at frame (s + buffer) * fps + 1
    for (w in seq_len(length(annotation.call$labels)))
    ann_alpha <- fading_text_dynaspec_int(x = ann_alpha, start = round((annotation.call$start[w] + buffer) * fps) + 1, end = round((annotation.call$end[w] + buffer) * fps) + 1, fading = if (!is.null(annotation.call$fading)) round(annotation.call$fading * fps) else 1, labels = annotation.call$labels[w])

    # remove fading from call
     annotation.call$fading <- NULL

     # remove start and end
    annotation.call$start <- NULL
    annotation.call$end <- NULL

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

  wdt <- width * seewave::duration(wave_sil) / t.display
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

  # selections (times shifted by the half window added at the start; also repeated at both ends when looping)
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
    oscillo_dynaspec_int(wave = wave_sil, f = wave_sil@samp.rate, xaxt = "n", yaxt = "n", colwave = colwave, bg = colbg)
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
  # web pages, which add the scrolling spectrogram, window rectangles, annotations and moving time axis).
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
    grDevices::png(file, res = res, width = width, height = height, bg = if (html_frame) "transparent" else "white")

    # set regular margins
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

    # plot anything at specific time and freq
    # (time axis labels move along when time is not fixed, so they are added by the web page in html frames)
    graphics::plot(0,0, type = "n", xlim = if(fix.time) c(0, t.display) else tlim, ylim = flim, xlab = if (axis.type == "standard") "Time (s)" else "", ylab = if (axis.type == "standard") "Frequency (kHz)" else "", xaxs = "i", yaxs = "i", bty = "o", xaxt = if (axis.type == "standard" && (fix.time || !html_frame)) "s" else "n", yaxt = "n")

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
    graphics::rect(xleft = if (tlim_low[2] + usr[1] > usr[2] & loop > 1) tlim_low[2] + 2 * usr[1] - usr[2] else 0, ybottom = flim[1], ytop = flim[1] + (flim[2] - flim[1]) * height.prop[2] / height.prop[1], xright = tlim_low[1] + usr[1], col = grDevices::adjustcolor("gray", 0.42), border = NA)

    # right gray rectangle
    if (fix.time)
    graphics::rect(xleft = tlim_low[2] + usr[1], ybottom = flim[1], ytop = flim[1] + (flim[2] - flim[1]) * height.prop[2] / height.prop[1], xright = if(loop > 1 & tlim_low[1] - usr[1] <= 0) usr[2] - (white_window - (tlim_low[2] - usr[1])) else usr[2], col = grDevices::adjustcolor("gray", 0.42), border = NA) else # different time limits when looping
      graphics::rect(xleft = tlim_low[2] + usr[1], ybottom = flim[1], ytop = flim[1] + (flim[2] - flim[1]) * height.prop[2] / height.prop[1], xright = if(loop > 1 & tlim_low[1] - usr[1] <= 0) {usr[2] - (white_window - (tlim_low[2] - usr[1]))
        + if (tlim_low[1] - usr[1] <= 0) tlim_low[2] + usr[1] + (seewave::duration(wave_sil) - white_window) / 2 else 0
      }  else usr[2], col = grDevices::adjustcolor("gray", 0.42), border = NA)

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

      graphics::axis(side = 2, at = labs, labels = labs)
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
    if (lower.spectro | osc)
      graphics::abline(h = org.flim[1])

    # annotations
    if (!is.null(annotation.call) && !html_frame){

      # fix x and y to values relative to usr
      annotation.call$x <- ((usr[2] - usr[1]) * annotation.call$x) + usr[1]
      annotation.call$y <- ((usr[4] - usr[3]) * annotation.call$y) + usr[3]

      if (is.null(annotation.call$col)) annotation.call$col <- "black"

      # overwrite col in call
      annotation.call$col <- grDevices::adjustcolor(annotation.call$col, alpha.f = ann_alpha[x])

      # overwrite label in call

      annotation.call$labels <- names(ann_alpha)[x]
      # evaluate call
        eval(annotation.call)
      }

    # reprint box
    graphics::box()

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
      axis = axis.type == "standard", width = width, res = res, annotation = annotation.call,
      annotation.args = annotation_args, speeds = unique(c(speed, speeds)), keep.pitch = keep.pitch, lcol = lcol
    )

    return(output_html_int(page, file.name = file.name, play = play))
  }

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
    on.exit(parallel::stopCluster(cl), add = TRUE)
  } else cl <- parallel

  # turn progress bar on or off
  pbo <- pbapply::pboptions(type = if (verbose) "timer" else "none")
  on.exit(pbapply::pboptions(pbo), add = TRUE)

  #loop to create image files
  out <- pbapply::pblapply(X = seq_len(frames), cl = if (identical(cl, 1)) NULL else cl, FUN = function(x){

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
    # (selections and annotations) of dynaspec objects
    wave_name <- if (!is.null(dots$sound.name)) dots$sound.name else if (!missing(wave) && inherits(wave, "Wave")) make.names(paste(deparse(substitute(wave)), collapse = "")) else NULL
    layers <- dots$layers
    dots$sound.name <- dots$layers <- NULL

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
      layers = layers
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
    args = list(wave = if (!is_params) wave, specParams = if (is_params) wave, file.name = file.name, fps = fps, highlight.col = highlight.col, highlight.alpha = highlight.alpha, lcol = lcol, delete.temp = delete.temp, speeds = speeds, keep.pitch = keep.pitch, play = play, parallel = parallel, verbose = verbose)
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
                                cursorCol = cursorCol, speeds = speeds, keep.pitch = keep.pitch)

    if (verbose) message("Web page saved at: ", vidName)

    return(output_html_int(page_html, file.name = vidName, play = play))
  }

  # video: each page is drawn once (500 pixels high) and ffmpeg moves the highlight box and cursor over it
  render_page <- function(i) {
    png_file <- page_png(i, "video")
    ext <- page_png_int(specParams, i, png_file, res = 500 / specParams$specHeight)
    paged_video_int(png_file = png_file, wave = specParams$segWavs[[i]], ext = ext, file.name = fs::path(temp_folder, paste0(iName0, "_", i, "_.mp4")),
                    highlightCol = highlightCol, highlightAlpha = highlightAlpha, cursorCol = cursorCol, fps = framerate)
  }

  # set clusters for windows OS
  segmented <- n_pages > 1
  if (Sys.info()[1] == "Windows" & parallel > 1 & segmented) {
    cl <- parallel::makePSOCKcluster(min(parallel, n_pages))
    on.exit(parallel::stopCluster(cl), add = TRUE)
  } else cl <- if (parallel > 1) parallel else NULL

  # turn progress bar on or off
  pbo <- pbapply::pboptions(type = if (verbose & segmented) "timer" else "none")
  on.exit(pbapply::pboptions(pbo), add = TRUE)

  if (verbose) message("Rendering ", n_pages, " page(s)", if (parallel > 1 & segmented) paste0(" using ", min(parallel, n_pages), " cores"), " ...")

  pbapply::pblapply(X = seq_len(n_pages), cl = if (segmented) cl else NULL, FUN = render_page)

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
new_dynaspec_int <- function(wave, name, flim = NULL, crop = NULL) {
  structure(
    list(wave = wave, name = name, flim = flim, crop = crop,
         spectrogram = list(), oscillogram = NULL, selections = list(), annotations = list(), player = list(),
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

# shift times of selections and annotations (e.g. when sounds are cropped)
shift_layers_int <- function(layers, d) {
  layers$selections <- lapply(layers$selections, function(sel) { sel$X$start <- sel$X$start + d; sel$X$end <- sel$X$end + d; sel })
  layers$annotations <- lapply(layers$annotations, function(a) { a$start <- a$start + d; a$end <- a$end + d; a })
  layers
}

# spectrogram arguments for seewave-based engines (static and scrolling): 'amp.trans' only applies to paged ones
spectro_args_int <- function(sp) {
  if (!is.null(sp$amp.trans))
    warning2("'amp.trans' is only used by paged spectrograms")
  sp$amp.trans <- NULL
  sp
}

# static spectrograms
save_static_int <- function(x, wave, layers, out, ext, play) {
  sp <- spectro_args_int(x$spectrogram)
  osc <- x$oscillogram
  args <- c(
    list(wave = wave, file.name = out, flim = x$flim, play = play, layers = layers,
         spectro = TRUE, osc = !is.null(osc)),
    if (!is.null(osc)) c(list(osc.position = osc$position, colwave = osc$col), if (!is.null(osc$height)) list(height.prop = c(1, osc$height))),
    sp, x$style$args, x$player
  )
  do.call(static_spectro_int, args, quote = TRUE)
}

# scrolling spectrograms (annotations are passed as 'annotation.call')
save_scrolling_int <- function(x, wave, layers, out, play, verbose) {
  sp <- spectro_args_int(x$spectrogram)
  osc <- x$oscillogram
  st <- x$style$args
  if (!is.null(st$overview)) { st$lower.spectro <- st$overview; st$overview <- NULL }

  ann <- layers$annotations
  if (length(ann) > 0 && is.null(st$annotation.call))
    st$annotation.call <- as.call(c(as.name("text"), list(
      x = ann[[1]]$x, y = ann[[1]]$y, labels = unlist(lapply(ann, `[[`, "labels")),
      start = unlist(lapply(ann, `[[`, "start")), end = unlist(lapply(ann, `[[`, "end")),
      fading = ann[[1]]$fading, col = ann[[1]]$col, cex = ann[[1]]$cex, font = ann[[1]]$font)))

  args <- c(
    list(wave = wave, file.name = out, flim = x$flim, play = play, verbose = verbose, layers = list(selections = layers$selections)),
    if (!is.null(osc)) c(list(osc = TRUE, colwave = osc$col), if (!is.null(osc$height)) list(height.prop = c(1, osc$height))),
    sp, st, x$player
  )
  do.call(scrolling_spectro_int, args, quote = TRUE)
}

# paged spectrograms (style arguments are split between the preparation of the pages and the engine)
save_paged_int <- function(x, wave, layers, out, ext, play, verbose) {
  st <- x$style$args
  engine_names <- c("fps", "highlight.col", "highlight.alpha", "lcol", "delete.temp", "parallel")
  engine_args <- c(st[intersect(names(st), engine_names)], x$player)
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

