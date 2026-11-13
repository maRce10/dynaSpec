# ds_compare(): two or more sounds shown one on top of the other and played in sequence

#' Compare sounds with dynamic spectrograms
#'
#' `ds_compare()` shows two or more sounds one on top of the other and plays them in sequence, from top to bottom. Layers are added with `+`, as in [dynaspec()], and are applied to all sounds.
#' @param ... The sounds (at least two), in the order they are shown and played: objects of class 'Wave', paths to '.wav' or '.mp3' files, or [dynaspec()] objects, or a single list of them. Use [dynaspec()] objects for settings or layers of a single sound (e.g. its own [ds_annotations()], [ds_captions()] or time range).
#' @param flim Numeric vector of length 2 with the frequency limits (in kHz) of all sounds, as in [dynaspec()]. Default is `NULL`: the limits set in [dynaspec()] objects; limits set for only some sounds are used for all of them, if they are the same. Sounds with different sampling rates are shown up to the lowest Nyquist frequency, so all use the same frequency axis (unless they have different limits set in [dynaspec()] objects).
#' @param tlim Numeric vector of length 1 or 2 with the time range (in s) of all sounds, as in [dynaspec()]. Default is `NULL` (the whole sounds, or the time range set in [dynaspec()] objects).
#' @param gap Numeric vector of length 1 with the silence (in s) between consecutive sounds. Default is 0.5.
#' @param same.scale Logical: use the same time scale for all sounds, so a second takes the same width in all of them? Static spectrograms leave empty room to the right of shorter sounds, and paged spectrograms without 't.display' use the duration of the longest sound as the duration of their pages (scrolling spectrograms and paged spectrograms with 't.display' always have the same scale). `FALSE` fits each sound to the width of the figure. Default is `TRUE`.
#' @return An object of class 'dynaspec_compare' (also of class 'dynaspec'), which is shown and saved as any [dynaspec()] object, with layers added with `+`.
#' @details All styles, panels, themes and output formats of [dynaspec()] objects can be used, and all sounds must use the same style:
#' - Web pages have a single set of controls below the sounds. Speed, pitch, volume and zoom apply to all sounds, the time readout shows the number of the sound being played and its time, the seek bar spans all sounds, the loop button repeats the sequence and the download button saves all sounds (browsers may ask for permission to download several files). Clicking on a spectrogram plays that sound from that point, and then the following ones (the sequence ends after the last sound).
#' - Videos ('.mp4', requires 'ffmpeg') stack the videos of the sounds: each sound keeps its first frame until it is played, and its last frame afterwards.
#' - Still images ('.png') stack the images of the sounds (paged spectrograms: one image per page, stacking the pages with the same number).
#'
#' Annotations and captions added to a comparison are applied to all sounds, with times referring to each sound. To add them to a single sound, add them to its [dynaspec()] object. Layers added to a comparison replace (or are combined with) those of the [dynaspec()] objects, as in any [dynaspec()] object.
#'
#' Amplitudes are scaled within each sound (as in [dynaspec()]), so the colors of the spectrograms and the height of the oscillograms do not compare the amplitude of the sounds.
#' @seealso [dynaspec()], [save_dynaspec()]
#' @export
#' @examples
#' data(female_barn_swallow, male_barn_swallow)
#'
#' x <- ds_compare(female_barn_swallow, male_barn_swallow, flim = c(0, 10)) +
#'   ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))
#'
#' save_dynaspec(x, file.path(tempdir(), "swallows.html"), play = FALSE)
#'
#' \dontrun{
#' # show it
#' x
#'
#' # other panels, styles and themes, as in dynaspec()
#' x + ds_oscillogram() + ds_theme_mako()
#' x + ds_scrolling(t.display = 1)
#' x + ds_paged(t.display = 1) + ds_player(buttons = c("progress", "time", "loop"))
#'
#' # more than two sounds
#' data(scythebill)
#' ds_compare(female_barn_swallow, male_barn_swallow, scythebill) +
#'   ds_spectrogram(wl.ms = 23)
#'
#' # video (requires ffmpeg)
#' save_dynaspec(x, file.path(tempdir(), "swallows.mp4"))
#'
#' # layers of a single sound
#' st <- data.frame(start = 0.5, end = 1, labels = "A")
#' a <- dynaspec(female_barn_swallow) + ds_annotations(st, labels = "labels")
#' ds_compare(a, male_barn_swallow, flim = c(0, 10), gap = 1) + ds_spectrogram(wl.ms = 23)
#' }
ds_compare <- function(..., flim = NULL, tlim = NULL, gap = 0.5, same.scale = TRUE) {
  check_results <- checkmate::makeAssertCollection()
  if (!is.null(flim))
    checkmate::assert_numeric(flim, len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_results, .var.name = "flim")
  if (!is.null(tlim))
    checkmate::assert_numeric(tlim, min.len = 1, max.len = 2, lower = 0, any.missing = FALSE, sorted = TRUE, unique = TRUE, add = check_results, .var.name = "tlim")
  checkmate::assert_number(gap, lower = 0, finite = TRUE, add = check_results, .var.name = "gap")
  checkmate::assert_flag(same.scale, add = check_results, .var.name = "same.scale")
  .report_assertions(check_results)

  # names of the sounds: names of the arguments or of the objects
  input <- list(...)
  labels <- vapply(as.list(substitute(list(...)))[-1], function(e) make.names(paste(deparse(e), collapse = "")), character(1))
  if (length(input) == 1 && is.list(input[[1]]) && !inherits(input[[1]], "dynaspec")) {
    input <- input[[1]]
    labels <- paste0("sound", seq_along(input))
  }
  if (!is.null(names(input))) labels <- ifelse(nzchar(names(input)), names(input), labels)
  if (length(input) < 2)
    stop2("ds_compare() needs at least two sounds")

  sounds <- unname(Map(compare_sound_int, input, labels))
  if (!is.null(tlim))
    sounds <- lapply(sounds, function(s) { s$tlim <- if (length(tlim) == 1) c(0, tlim) else tlim; s })
  sounds <- compare_flim_int(sounds, flim)

  structure(
    list(sounds = sounds, name = paste(vapply(sounds, `[[`, character(1), "name"), collapse = "_vs_"), gap = gap, same.scale = same.scale, style = sounds[[1]]$style),
    class = c("dynaspec_compare", "dynaspec")
  )
}


# sound of a comparison as a dynaspec object
compare_sound_int <- function(s, name) {
  if (inherits(s, "dynaspec_compare"))
    stop2("comparisons cannot be used as sounds of ds_compare()")
  if (inherits(s, "dynaspec")) return(s)
  d <- dynaspec(s)
  if (!is.character(s)) d$name <- name
  d
}


# can the sounds of a comparison share a y axis title? Spectrograms with standard axes, each with a single panel of the
# same type (e.g. only spectrograms; scrolling spectrograms without lower panel), stacked without gaps (no videos, no
# rounded player controls)
compare_ylab_int <- function(sounds) {
  kinds <- vapply(sounds, function(s) {
    th <- global_theme_int(s)$theme
    type <- s$style$type
    if (!is.null(s$video.layer) || identical(th$bar.style, "rounded") || identical(s$spectrum$side, "left") ||
        !theme_int(th, "axis.type", if (identical(type, "paged")) "none" else "standard") %in% "standard") return(NA_character_)
    if (identical(type, "paged")) return("spectro")
    if (identical(type, "scrolling")) return(if (length(s$panels) == 0 && is.null(s$overview)) "spectro" else NA_character_)
    if (!is.null(s$spectrogram) && length(s$panels) == 0) return("spectro")
    if (is.null(s$spectrogram) && length(s$panels) == 1) return(s$panels[[1]]$type)
    NA_character_
  }, character(1))
  !anyNA(kinds) && length(unique(kinds)) == 1
}

# frequency limits of a comparison: all sounds use the same frequency axis, unless they have different limits of their own
compare_flim_int <- function(sounds, flim) {
  nyquist <- vapply(sounds, function(s) samp_rate_int(s$wave) / 2000, numeric(1))
  if (!is.null(flim) && flim[1] >= min(nyquist))
    stop2("'flim' lower limit must be lower than the lowest Nyquist frequency of the sounds (", round(min(nyquist), 2), " kHz)")
  if (!is.null(flim) && is.finite(flim[2]) && flim[2] > min(nyquist))
    warning2("the upper limit of 'flim' (", flim[2], " kHz) is higher than the Nyquist frequency of the sound (", round(min(nyquist), 2),
             " kHz, half its sampling rate); spectrograms are shown up to ", round(min(nyquist), 2), " kHz")
  own <- unique(Filter(Negate(is.null), lapply(sounds, `[[`, "flim")))
  if (is.null(flim)) {
    # (different limits of their own, or the style default when the sounds have the same sampling rate)
    if (length(own) > 1 || (length(own) == 0 && length(unique(nyquist)) == 1)) return(sounds)
    flim <- if (length(own) == 1) own[[1]] else c(0, min(nyquist))
  }
  # (limits of a single sound can be above the Nyquist frequency of another one)
  if (flim[1] >= min(nyquist))
    stop2("'flim' lower limit must be lower than the lowest Nyquist frequency of the sounds (", round(min(nyquist), 2), " kHz)")
  flim[2] <- min(flim[2], min(nyquist))
  lapply(sounds, function(s) { s$flim <- flim; s })
}


# duration of the part of the sound shown (after 'tlim')
compare_dur_int <- function(s) {
  d <- seewave::duration(read_sound_int(s$wave)$wave)
  if (is.null(s$tlim)) d else min(s$tlim[2], d) - s$tlim[1]
}


# add silence at the end of a sound up to a given duration
pad_wave_int <- function(w, to) {
  n <- round(to * w@samp.rate) - length(w@left)
  if (n <= 0) return(w)
  w@left <- c(w@left, vector(typeof(w@left), n))
  if (w@stereo) w@right <- c(w@right, vector(typeof(w@right), n))
  w
}


# separation (in pixels) between stacked figures: none when figures have margins with axes, so they do not touch
compare_band_int <- function(s) {
  # (paged spectrograms have no axes by default)
  default <- if (identical(s$style$type, "paged")) "none" else "standard"
  if (theme_int(global_theme_int(s)$theme, "axis.type", default) %in% c("standard", "compact")) 0 else 8
}


# background of the separation between stacked images and videos (color of the figure, or white if transparent)
compare_bg_int <- function(s) {
  bg <- theme_int(global_theme_int(s)$theme, "bg", "white")
  rgb <- grDevices::col2rgb(bg, alpha = TRUE)
  if (rgb[4] == 0) "#FFFFFF" else grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
}


# comparison of sounds: each sound is rendered as any dynaspec object, and the outputs are stacked
save_compare_int <- function(x, out, ext, play, verbose) {
  sounds <- x$sounds
  if (length(unique(vapply(sounds, function(s) s$style$type, character(1)))) > 1)
    stop2("all sounds of a comparison must use the same style (add the style to the comparison, e.g. ds_compare(x, y) + ds_scrolling())")
  durs <- vapply(sounds, compare_dur_int, numeric(1))
  sounds <- lapply(sounds, function(s) { s$compare <- TRUE; s })
  # (only the last sound has a time axis title)
  for (i in seq_len(length(sounds) - 1)) sounds[[i]]$theme$time.title <- FALSE

  # same time scale
  if (isTRUE(x$same.scale)) {
    type <- sounds[[1]]$style$type
    sounds <- lapply(sounds, function(s) {
      if (type == "static") s$pad.to <- max(durs)
      if (type == "paged" && (is.null(s$style$args$t.display) || !is.finite(s$style$args$t.display))) s$style$args$t.display <- max(durs)
      s
    })
  }

  render <- function(s, file) {
    # the code of the web pages is combined below, not embedded when knitting (and temporary files are not reported)
    op <- options(knitr.in.progress = FALSE, dynaSpec.report = FALSE)
    on.exit(options(op), add = TRUE)
    save_dynaspec(s, file, play = FALSE, verbose = verbose)
  }

  # a single y axis title for all sounds (spectrograms with a single type of panel): the figures are measured
  # (web pages, quick) and each one draws the part of the title that falls on it, centered on the panels of all sounds
  if (compare_ylab_int(sounds)) {
    lay <- lapply(sounds, function(s) attr(render(s, NULL), "layout"))
    h <- vapply(lay, `[[`, numeric(1), "height")
    offset <- c(0, cumsum(h))[seq_along(h)]
    n <- length(sounds)
    center <- (offset[1] + lay[[1]]$top * h[1] + offset[n] + lay[[n]]$bottom * h[n]) / 2
    for (i in seq_len(n)) sounds[[i]]$theme$ylab.center <- (center - offset[i]) / h[i]
  }

  res <- switch(ext,
    html = compare_html_int(lapply(sounds, render, file = NULL), durs, x$gap, out, play, band = compare_band_int(sounds[[1]])),
    mp4 = compare_video_int(sounds, render, durs, x, out),
    png = compare_png_int(sounds, render, out, verbose)
  )
  if (ext != "html" && play) for (f in res) open_file_int(normalizePath(f))
  if (ext == "html") res else invisible(res)
}


# web page of a comparison: the players of all sounds (their javascript goes after them, as it sets the sequence)
compare_html_int <- function(players, durs, gap, out, play, band = 0) {
  js <- player_js_int()
  html <- paste0(
    '<div class="dynaspec-compare" data-gap="', gap, '" data-durations="', paste(durs, collapse = ","), '"',
    if (band > 0) paste0(' style="--ds-cmp-gap:', band, 'px"'), '>\n',
    paste(vapply(players, function(p) sub(js, "", p, fixed = TRUE), character(1)), collapse = ""),
    '</div>\n',
    js
  )
  output_html_int(html, file.name = out, play = play)
}


# video of a comparison: each sound keeps its first frame until it is played, and its last frame afterwards
compare_video_int <- function(sounds, render, durs, x, out) {
  n <- length(sounds)
  files <- replicate(n, tempfile(fileext = ".mp4"))
  on.exit(unlink(files), add = TRUE)
  for (i in seq_len(n)) render(sounds[[i]], files[i])
  info <- lapply(files, video_info_int)

  # (static and paged videos can include silence at the end, which is not played)
  d <- if (sounds[[1]]$style$type == "scrolling") vapply(info, `[[`, numeric(1), "duration") else durs
  g <- x$gap
  w <- info[[1]]$width
  w <- w + w %% 2
  bg <- paste0("0x", substring(compare_bg_int(sounds[[1]]), 2))
  band <- compare_band_int(sounds[[1]])
  num <- function(v) format(v, digits = 10, scientific = FALSE)
  chains <- vapply(seq_len(n), function(i) {
    before <- sum(d[seq_len(i - 1)]) + (i - 1) * g
    after <- sum(d[-seq_len(i)]) + (n - i) * g
    tpad <- c(if (before > 0) paste0("start_mode=clone:start_duration=", num(before)), if (after > 0) paste0("stop_mode=clone:stop_duration=", num(after)))
    # (a band of the figure background separates the videos)
    sprintf("[%d:v]trim=end=%s,setpts=PTS-STARTPTS,tpad=%s,scale=%d:-2,setsar=1%s[v%d]", i - 1, num(d[i]), paste(tpad, collapse = ":"), w,
            if (i < n && band > 0) sprintf(",pad=iw:ih+%d:0:0:color=%s", band, bg) else "", i)
  }, character(1))
  audio <- vapply(seq_len(n), function(i) {
    sprintf("[%d:a]atrim=end=%s,asetpts=PTS-STARTPTS,aformat=sample_rates=44100:channel_layouts=stereo%s[a%d]", i - 1, num(d[i]),
            if (i < n && g > 0) paste0(",apad=pad_dur=", num(g)) else "", i)
  }, character(1))
  filter <- paste(c(
    chains, paste0(paste0("[v", seq_len(n), "]", collapse = ""), "vstack=inputs=", n, "[v]"),
    audio, paste0(paste0("[a", seq_len(n), "]", collapse = ""), "concat=n=", n, ":v=0:a=1[a]")
  ), collapse = ";")
  run_ffmpeg_int(paste("-y", paste("-i", shQuote(files), collapse = " "), "-filter_complex", shQuote(filter),
                       "-map [v] -map [a] -c:v libx264 -pix_fmt yuv420p -c:a aac", shQuote(out)), "stacking the videos of the sounds")
  out
}


# images of a comparison: images of the sounds stacked (paged spectrograms: pages with the same number)
compare_png_int <- function(sounds, render, out, verbose) {
  dir <- tempfile("compare_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  files <- lapply(seq_along(sounds), function(i) render(sounds[[i]], file.path(dir, paste0("sound", i, ".png"))))
  n <- max(lengths(files))
  res <- if (n == 1) out else paste0(tools::file_path_sans_ext(out), "_", seq_len(n), ".png")
  bg <- grDevices::col2rgb(compare_bg_int(sounds[[1]])) / 255
  for (i in seq_len(n)) {
    img <- lapply(files, function(f) if (i <= length(f)) png::readPNG(f[i]))
    png::writePNG(stack_images_int(img, bg, band = compare_band_int(sounds[[1]])), res[i])
  }
  res
}


# stack images (arrays of png::readPNG()) with a band of the background color between them; missing images (NULL) are
# filled with the background color
stack_images_int <- function(img, bg, band = 8) {
  present <- Filter(Negate(is.null), img)
  w <- max(vapply(present, function(m) dim(m)[2], numeric(1)))
  h <- max(vapply(present, function(m) dim(m)[1], numeric(1)))
  blank <- function(h, w) {
    m <- array(1, c(h, w, 4))
    for (k in 1:3) m[, , k] <- bg[k]
    m
  }
  rgba <- function(m) {
    if (is.null(m)) return(blank(h, w))
    if (length(dim(m)) == 2) m <- array(m, c(dim(m), 1))
    if (dim(m)[3] < 3) m <- array(c(m[, , 1], m[, , 1], m[, , 1], if (dim(m)[3] == 2) m[, , 2] else rep(1, length(m[, , 1]))), c(dim(m)[1:2], 4))
    if (dim(m)[3] == 3) m <- array(c(m, rep(1, prod(dim(m)[1:2]))), c(dim(m)[1:2], 4))
    if (dim(m)[2] < w) {
      out <- blank(dim(m)[1], w)
      out[, seq_len(dim(m)[2]), ] <- m
      m <- out
    }
    m
  }
  parts <- lapply(img, rgba)
  heights <- vapply(parts, function(m) dim(m)[1], numeric(1))
  res <- blank(sum(heights) + band * (length(parts) - 1), w)
  top <- 0
  for (i in seq_along(parts)) {
    res[top + seq_len(heights[i]), , ] <- parts[[i]]
    top <- top + heights[i] + band
  }
  res
}
