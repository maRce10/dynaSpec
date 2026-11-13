# static spectrogram engine (figure with a moving play line) and its videos

# video of a static_spectro() figure with the play line moving along the sound
# 'ext' has one row per segment (t0, t1, and left, right, top and bottom as proportions of the figure)
# dashes of a line type, as lengths (in line widths) of dashes and gaps: NULL for solid lines and 0 for blank ones
lty_dash_int <- function(lty) {
  if (is.null(lty)) return(NULL)
  names <- c(blank = "0", solid = "1", dashed = "44", dotted = "13", dotdash = "1343", longdash = "73", twodash = "2262")
  code <- if (is.numeric(lty)) names[[as.integer(lty) + 1]] else names[[lty]]
  if (code == "0") return(0)
  if (code == "1") return(NULL)
  as.numeric(strsplit(code, "")[[1]])
}


# image of a vertical line (height x width pixels; array for png::writePNG()) with the color, transparency and dashes
# of a play line
line_image_int <- function(h, w, col, dash = NULL) {
  rgba <- grDevices::col2rgb(col, alpha = TRUE)[, 1] / 255
  on <- rep(TRUE, h)
  if (length(dash) > 1) {
    pattern <- rep(rep(c(TRUE, FALSE), length.out = length(dash)), times = pmax(1, round(dash * w)))
    on <- rep_len(pattern, h)
  }
  img <- array(0, c(h, w, 4))
  for (k in 1:3) img[, , k] <- rgba[k]
  img[, , 4] <- on * rgba[4]
  img
}


playhead_video_int <- function(png_file, wave, ext, file.name, lcol, fps, width, height, line = TRUE, captions = NULL, res = 100, lwd = NULL, dash = NULL) {
  wav_file <- tempfile(fileext = ".wav")
  on.exit(unlink(wav_file), add = TRUE)
  tuneR::writeWave(if (wave@pcm) wave else tuneR::normalize(wave, unit = "16"), filename = wav_file)

  dur <- utils::tail(ext[, "t1"], 1)
  lw <- if (is.null(lwd)) max(2, round(width / 500)) else max(1, round(lwd))

  # one line per row (an image with the color, transparency and dashes of the line), shown while that row plays (no
  # lines when 'line' is FALSE). The overlay filter evaluates the position expressions on every frame (unlike drawbox in
  # ffmpeg < 5)
  n_rows <- if (line) nrow(ext) else 0
  lines <- vapply(seq_len(n_rows), function(k) {
    e <- ext[k, ]
    x0 <- e[["left"]] * width - lw / 2
    dx <- (e[["right"]] - e[["left"]]) * width / (e[["t1"]] - e[["t0"]])
    enable <- if (k < n_rows) sprintf("gte(t,%f)*lt(t,%f)", e[["t0"]], e[["t1"]]) else sprintf("gte(t,%f)", e[["t0"]])
    sprintf("[v%d][%d]overlay=x='%f+%f*(t-%f)':y=%d:enable='%s'[v%d]", k - 1, k + 1, x0, dx, e[["t0"]], round(e[["top"]] * height), enable, k)
  }, character(1))

  line_files <- vapply(seq_len(n_rows), function(k) {
    f <- tempfile(fileext = ".png")
    png::writePNG(line_image_int(max(1, round((ext[k, "bottom"] - ext[k, "top"]) * height)), lw, lcol, dash), f)
    f
  }, character(1))
  on.exit(unlink(line_files), add = TRUE)
  sources <- vapply(line_files, function(f) sprintf('-loop 1 -framerate %s -i "%s"', fps, f), character(1))

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


# static spectrogram engine (dynaspec() + ds_static()): figure with a play line moving over it. 'layers' are the
# selections of the dynaspec object
static_spectro_int <- function(wave,
                           file.name = NULL,
                           wl = NULL,
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
                           lty = 1,
                           lwd = NULL,
                           wl.ms = NULL,
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
                           audio = NULL,
                           ...)
{
  # check arguments
  check_results <- .check_arguments(
    fun = "static_spectro",
    args = list(wave = wave, html.file.name = file.name, wl = wl, ovlp = ovlp, flim = flim, pal = pal, collevels = collevels, spectro = spectro, osc = osc, osc.position = osc.position, cex.lab = cex.lab, height.prop = height.prop, colwave = colwave, colbg = colbg, res = res, width = width, height = height, rows = rows, lcol = lcol, lty = lty, lwd = lwd, speeds = speeds, keep.pitch = keep.pitch, buttons = buttons, alt = alt, fps = fps, play = play, play.line = play.line)
  )

  # report errors
  .report_assertions(check_results)

  # spectrogram background: color of the lowest amplitude of the palette, unless supplied (independent of the figure
  # background, ds_theme(bg), which is white by default)
  fig_bg_default <- "white"
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

  # play line: width in pixels of the figure ('lwd' as in R, 1/96 inch; default 2 pixels, or 1/500 of the width in videos)
  # and pattern of dashes (lengths in line widths); blank lines are not shown
  dash <- lty_dash_int(lty)
  if (identical(dash, 0)) play.line <- FALSE
  lwd_px <- if (!is.null(lwd)) lwd * res / 96

  # read sound and audio data to embed
  snd <- sound_data_int(wave)
  # ('audio': sound embedded in web pages when it is not the one in the figure, see save_sound_int())
  if (!is.null(audio)) snd$uri <- sound_data_int(audio, read = FALSE)$uri
  wav <- tuneR::mono(snd$wave, which = "left")
  dur <- seewave::duration(wav)
  # (window length: 'wl.ms', 'wl', or 11.61 ms)
  wl <- resolve_wl_int(wav@samp.rate, wl, wl.ms)

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
  # (the 'Amp.' labels of oscillograms and envelopes next to a spectrogram need more room, so its axis title does not
  # overlap them)
  amp_labels <- spectro && any(vapply(panels, `[[`, character(1), "type") %in% c("osc", "env"))
  # (axis titles at a fixed distance from the axis labels, and margins that fit them, see title_margin_int())
  # ('Amp.' labels are about 1.75 lines wide at size 1, from line 0.5)
  ylab_line <- if (amp_labels) max(0.8 + 1.75 * cex.lab, 1.4 + cex_axis) else 1.4 + cex_axis
  lab_lines <- if (inside) 0 else if (compact) 0.6 + 1.6 * cex_axis else title_margin_int(ylab_line, cex.lab)
  # (with a power spectrum on the left side, the frequency axis is drawn on the left of the spectrum instead)
  spec_left <- !is.null(spectrum) && spectrum$side == "left"
  # (the time axis title is placed at line 1.8 + cex.lab, and the 'Amp.' label of spectra above the top panel)
  # (vertical time labels, las = 2 or 3, take more room)
  bottom_lines <- if (inside) 0.45 + cex_axis else if (compact) 0 else
    xlab_margin_int(1 + 0.9 * cex_axis, cex.lab) + x_lab_extra_int(theme_int(theme, "las", 1), c(pretty(c(0, dur)), pretty(c(0, dur / rows))), cex_axis)
  # (no top margin under a video, see ds_video(), unless it has time labels or the label of the power spectrum)
  top_lines <- if (compact) 0.4 + 1.1 * cex_axis else if (!is.null(spectrum) && spec_slot == 1) max(1, 0.1 + 0.68 * cex.lab * theme_int(theme, "cex.title", 1)) else if (inside || isFALSE(theme$top.margin)) 0 else 1
  flush <- inside || compact
  # (the gap between the spectrogram and the power spectrum, 'spec_gap' lines on each side, is as wide as the gap
  # between stacked panels)
  spec_gap <- 0.1
  # (rows other than the last one get time labels without axis title)
  row_lines <- if (inside) 0.45 + cex_axis else 1.05 + cex_axis + x_lab_extra_int(theme_int(theme, "las", 1), pretty(c(0, dur)), cex_axis)
  # (no time axis title in sounds of comparisons other than the last one: the last row is as the others, see below)
  time_title <- !isFALSE(theme$time.title)
  # a single y axis title for all rows when each row has a single panel (e.g. only spectrograms), centered on the panels
  # of all rows; comparisons of sounds give its position ('ylab.center', from the top of the figure, as a proportion of
  # its height), so the parts drawn in each figure make a single title when they are stacked
  ylab_combined <- axes && !inside && !compact && !spec_left && n_stack == 1 && (rows > 1 || !is.null(theme$ylab.center))
  # (rows other than the first one have a smaller top margin)
  slot_mar <- function(i, axis, first = TRUE) c(if (i < n_stack) 0.2 else if (axes && !compact) (if (axis) bottom_lines else row_lines) else if (axis && axes) bottom_lines else if (flush) 0 else 0.5, if (spec_left) spec_gap else if (axes) lab_lines else if (inside) 0 else 1, if (i == 1) (if (first || compact) top_lines else min(top_lines, 0.4)) else 0, if (is.null(spectrum) || spec_left) (if (flush) 0 else 0.1) else spec_gap)

  # slot heights (in inches) that keep relative heights between plotting regions, with the same plotting
  # regions in all rows ('height' is the height of a row with time axis)
  # (height of a line of text in the png device: 0.2 inches on Linux and macOS, but it depends on the device and fonts,
  # e.g. 0.192 inches on Windows, so it is measured)
  lines_in <- png_line_in_int(res, theme)
  slot_marg <- function(axis, first = TRUE) vapply(seq_len(n_stack), function(i) sum(slot_mar(i, axis, first)[c(1, 3)]), numeric(1)) * lines_in
  plot_in <- height / res - sum(slot_marg(TRUE))
  if (plot_in <= 0)
    stop2("'height' is too small to fit the figure (increase 'height' or decrease 'res')")
  # (without time axis title the figure is shorter, and the plotting regions are as high as with it)
  if (!time_title && !compact) bottom_lines <- row_lines
  slot_heights <- function(axis, first = TRUE) plot_in * rel / sum(rel) + slot_marg(axis, first)
  fig_height <- round(res * sum(vapply(seq_len(rows), function(k) sum(slot_heights(k == rows, k == 1)), numeric(1))))

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
  png_device_int(filename = png_file, width = width, height = fig_height, res = res, bg = fig_bg)
  dev <- grDevices::dev.cur()
  theme_par_int(theme)
  ext <- tryCatch({
    graphics::layout(lay, heights = unlist(lapply(seq_len(rows), function(k) slot_heights(k == rows, k == 1))), widths = widths)

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

    e <- t(vapply(seq_len(rows), function(k) {
      seg <- wav[(row_lims[k] + 1):row_lims[k + 1]]
      t0 <- row_lims[k] / f
      t1 <- row_lims[k + 1] / f
      axis <- k == rows
      # (theme of the row: no time axis title except in the last row)
      th_row <- if (axis && time_title) theme else utils::modifyList(theme, list(xlab = ""))
      tops <- bottoms <- numeric(n_stack)

      for (i in plot_order) {
        p <- stack[[i]]
        bottom_slot <- i == n_stack
        graphics::par(mar = slot_mar(i, axis, k == 1), bg = fig_bg, font.lab = theme_int(theme, "font.lab", 1))

        if (p$type == "spectro") {
          col_lab <- theme_int(theme, "col.lab", "black")
          spg <- spectro_int(seg, f = f, wl = wl, ovlp = ovlp, flim = flim, pal = pal, collevels = collevels, colbg = colbg, dBref = max_amp,
                      axisY = axes && (!spec_left || inside), flab = if (ylab_combined) "" else theme_int(theme, "ylab", "Frequency (kHz)"), cex.lab = cex.lab,
                      cex.axis = theme_int(theme, "cex.axis", 1), col.lab = col_lab, theme = theme, ylab.line = ylab_line, ...)

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

          # time axis (if the spectrogram is the bottom panel; axis title only in the last row)
          if (bottom_slot && axes && !compact)
            do.call(time_axis_int, c(list(to_x = to_x, xlim = to_time(usr[1:2]), cex.lab = cex.lab, inside = inside), th_row[intersect(names(th_row), c("xlab", "font.lab", "col.lab", "cex.axis", "col.axis", "las"))]))

          left <- graphics::grconvertX(to_x(t0), "user", "ndc")
          right <- graphics::grconvertX(to_x(t1), "user", "ndc")
          xlim <- to_time(usr[1:2])

          # compact time axis above the top panel of each row
          if (compact && axes && i == 1)
            compact_row_axis_int(to_x, xlim, theme, cex_axis)

          # power spectrum next to the spectrogram (same frequency axis)
          if (!is.null(spectrum)) {
            graphics::par(mar = c(slot_mar(i, axis, k == 1)[c(1, 3)], if (spec_left) c(if (axes && !inside) lab_lines else if (flush) 0 else 1, spec_gap) else c(spec_gap, if (flush) 0 else 0.1))[c(1, 3, 2, 4)])
            spectrum_player_int(spectra[[k]], ylim = usr[3:4], side = spectrum$side, col = spectrum$col, max.amp = max_spec, label = k == 1 && !compact, cex.lab = cex.lab, axis = spec_left && axes && !inside, theme = theme)
          }
        } else {
          if (!spectro) xlim <- c(t0, t1)
          if (p$type == "env")
            envelope_player_int(seg, xlim = xlim, col = p$col, axisX = bottom_slot && axes && !compact, cex.lab = cex.lab, offset = t0, max.env = max_env, theme = c(if (axes && !flush) th_row else c(th_row, list(hide = TRUE)), list(ylab.hide = ylab_combined)))
          else
            oscillo_player_int(seg, xlim = xlim, colwave = p$col, n = width, axisX = bottom_slot && axes && !compact, cex.lab = cex.lab, offset = t0, max.amp = max_wave, empty = p$type == "empty", theme = c(if (axes && !flush) th_row else c(th_row, list(hide = TRUE)), list(ylab.hide = ylab_combined)))
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
    ext <- e

    # single y axis title (drawn as R draws axis titles, in a plotting region with the left margin of the panels)
    if (ylab_combined) {
      yc <- if (!is.null(theme$ylab.center)) theme$ylab.center else mean(c(ext[1, "top"], ext[rows, "bottom"]))
      graphics::par(fig = c(0, 1, 0, 1), mar = c(0, lab_lines, 0, 0), new = TRUE)
      graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
      col_lab <- theme_int(theme, "col.lab", "black")
      if (stack[[1]]$type == "spectro")
        graphics::mtext(theme_int(theme, "ylab", "Frequency (kHz)"), side = 2, line = ylab_line, at = 1 - yc,
                        # (titles of comparisons span several figures, so they are not made smaller to fit this one)
                        cex = if (is.null(theme$ylab.center)) fit_ylab_cex_int(theme_int(theme, "ylab", "Frequency (kHz)"), cex.lab, 1 - yc, theme_int(theme, "font.lab", 1)) else cex.lab, col = col_lab, font = theme_int(theme, "font.lab", 1), las = 0)
      else if (stack[[1]]$type != "empty")
        graphics::mtext("Amp.", side = 2, line = 0.5, at = 1 - yc, cex = cex.lab, las = 1, col = col_lab, font = theme_int(theme, "font.lab", 1))
    }
    ext
  }, finally = grDevices::dev.off(dev))

  # still image
  if (!is.null(file.name) && grepl("\\.png$", file.name, ignore.case = TRUE)) {
    file.copy(png_file, file.name, overwrite = TRUE)
    if (play) open_file_int(normalizePath(file.name))
    return(invisible(file.name))
  }

  # video of the figure with the play line
  if (video) {
    playhead_video_int(png_file = png_file, wave = snd$wave, ext = ext, file.name = file.name, lcol = lcol, fps = fps, width = width, height = fig_height, line = play.line, captions = layers$captions, res = res,
                       lwd = lwd_px, dash = dash)
    if (play)
      open_file_int(normalizePath(file.name))
    return(invisible(file.name))
  }

  # player html
  html <- paste0(
    '<div class="dynaspec-player" data-duration="', dur,
    '" data-rows="', paste(apply(round(ext, 5), 1, paste, collapse = ","), collapse = ";"),
    '"', player_attrs_int(speeds, keep.pitch, buttons, download.name, theme), ' data-line="', tolower(play.line), '"',
    if (!is.null(lwd_px)) paste0(' data-lwd="', signif(lwd_px, 4), '"'), if (length(dash) > 1) paste0(' data-dash="', paste(dash, collapse = ","), '"'),
    ' data-color="', col2hex_int(lcol), '" data-text-color="', contrast_col_int(lcol), '">\n',
    '<img srcset="', base64_uri_int(png_file, "image/png"), ' 1x" alt="', html_escape_int(if (is.null(alt)) paste0(if (empty) "Time axis" else panel_names_int(spectro, panels, spectrum), " of a ", round(dur, 2), " s sound") else alt), '">\n',
    '<audio controls preload="auto" src="', snd$uri, '"></audio>\n',
    captions_json_int(layers$captions, res = res, width = width),
    '</div>\n',
    player_js_int()
  )

  res <- output_html_int(html, file.name = file.name, play = play)
  # (figure height and vertical extent of the panels, from the top as proportions of the height; see save_compare_int())
  attr(res, "layout") <- list(height = fig_height, top = ext[1, "top"], bottom = ext[rows, "bottom"])
  res
}
