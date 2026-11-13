# general helpers: messages, progress bars, ffmpeg and sound files

# default window length of spectrograms (in ms): 512 samples at 44.1 kHz
wl_ms_default <- 512 / 44.1

# bandpass filter of ds_filter() ('bp' in kHz; 0 and Inf, or limits at or above the Nyquist frequency, leave that side
# open). seewave::ffilter() can drop some samples at the end, which are added back as silence (the sound is not shifted)
filter_wave_int <- function(w, bp) {
  f <- w@samp.rate
  nyquist <- f / 2000
  if (bp[1] >= nyquist)
    stop2("the lower limit of ds_filter(bp) (", bp[1], " kHz) must be lower than the Nyquist frequency of the sound (", round(nyquist, 2), " kHz)")
  from <- if (bp[1] > 0) bp[1] * 1000
  to <- if (is.finite(bp[2]) && bp[2] < nyquist) bp[2] * 1000
  if (is.null(from) && is.null(to)) return(w)
  n <- length(w@left)
  one <- function(channel) {
    y <- seewave::ffilter(w, f = f, channel = channel, from = from, to = to, bandpass = TRUE, wl = wl_from_ms_int(f, wl_ms_default), output = "matrix")
    y <- as.numeric(y)
    if (length(y) < n) y <- c(y, numeric(n - length(y)))
    y[seq_len(n)]
  }
  left <- one(1)
  right <- if (w@stereo) one(2)
  # (16-bit integer samples, scaled to the original peak amplitude)
  peak <- max(abs(c(left, right)), 1e-12)
  target <- min(32767, max(abs(c(w@left, if (w@stereo) w@right)), 1) * if (w@pcm) 1 else 32767)
  scale <- function(y) round(y / peak * target)
  if (w@stereo) tuneR::Wave(left = scale(left), right = scale(right), samp.rate = f, bit = 16) else tuneR::Wave(left = scale(left), samp.rate = f, bit = 16)
}

# window length (in samples, even) of 'ms' milliseconds at sampling rate 'rate' (Hz)
wl_from_ms_int <- function(rate, ms) {
  wl <- max(2, round(rate * ms / 1000))
  wl + wl %% 2
}

# window length (in samples) from 'wl.ms' (which takes precedence), 'wl', or the default duration
resolve_wl_int <- function(rate, wl = NULL, wl.ms = NULL) {
  if (!is.null(wl.ms)) wl_from_ms_int(rate, wl.ms) else if (!is.null(wl)) wl else wl_from_ms_int(rate, wl_ms_default)
}

# internal functions not to be called by users

# messages to users, as cli alerts (as in the package 'suwo'): "success" (e.g. files saved), "info" or "warning"
message_dyn_int <- function(..., type = "info") {
  text <- paste0(...)
  alert <- switch(type, success = cli::cli_alert_success, warning = cli::cli_alert_warning, cli::cli_alert_info)
  alert("{text}")
}


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
  # (-nostdin: ffmpeg otherwise reads the standard input, e.g. the rest of an R script read from it, as in R CMD check)
  out <- suppressWarnings(system2(ffmpeg_exec2(), args = paste("-nostdin -hide_banner -loglevel error", args), stdout = TRUE, stderr = TRUE))
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


# read a sound: a 'Wave' object or a path to a .wav or .mp3 file. Returns the Wave object ('wave'), the file it
# was read from ('file', NULL for Wave objects) and a name to use for output files ('name')
read_sound_int <- function(x, name = NULL, verbose = FALSE) {
  if (inherits(x, "Wave"))
    return(list(wave = x, file = NULL, name = if (is.null(name)) "sound" else name))

  ext <- tolower(tools::file_ext(x))
  if (verbose && ext == "mp3") message_dyn_int("Converting mp3 to wav")

  list(
    wave = if (ext == "mp3") tuneR::readMP3(x) else tuneR::readWave(x),
    file = x,
    name = tools::file_path_sans_ext(basename(x))
  )
}


# sampling rate of a sound ('Wave' object or .wav/.mp3 file; only the header of .wav files is read); NA if unknown
samp_rate_int <- function(wave) {
  tryCatch({
    if (inherits(wave, "Wave")) wave@samp.rate
    else if (grepl("\\.wav$", wave, ignore.case = TRUE)) tuneR::readWave(wave, header = TRUE)$sample.rate
    else tuneR::readMP3(wave)@samp.rate
  }, error = function(e) NA)
}
