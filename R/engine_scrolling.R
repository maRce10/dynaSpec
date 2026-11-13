# scrolling spectrogram engine (spectrogram moving along a fixed play line)

# web page of scrolling_spectro(): a frame with the parts that do not move (axes, play line, lower spectrogram),
# with the full spectrogram (and oscillogram) below it, moved by the web page in sync with the sound
# (embedded images use 'srcset' instead of 'src': R Markdown pages that are not self-contained, such as pkgdown
# articles, pass every <img src> through utils::URLdecode(), which takes minutes for data URIs of several MB)
scrolling_html_int <- function(frame_file, spectro_file, osc_file, lower, plt, height.prop, wave, wave_sil, t.display, buffer,
                               step_time, step_time_low, fps, loop, fix.time, axis, zoom = FALSE, inside = FALSE, compact = FALSE, width, res, captions = NULL,
                               speeds, keep.pitch, buttons = "time", download.name = "sound", lcol, theme = list(), still.ends = FALSE, lty = 2, lwd = 2) {
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
    colAxis = col2hex_int(axis_col_int(theme)), xVert = axis && !inside && !compact && isTRUE(theme_int(theme, "las", 0) %in% 2:3),
    cexAxis = theme_int(theme, "cex.axis", 1),
    dim = col2hex_int(grDevices::adjustcolor(theme_int(theme, "col.highlight", "gray"), 0.42), alpha = TRUE),
    # still ends: the play line is moved by the page (color with transparency, width in pixels of the frame and dashes, in
    # line widths)
    stillEnds = still.ends, lineColor = col2hex_int(lcol, alpha = TRUE), lineWidth = if (identical(lty_dash_int(lty), 0)) 0 else max(1, lwd * res / 96),
    lineDash = if (length(lty_dash_int(lty)) > 1) lty_dash_int(lty) else 0
  )

  paste0(
    '<div class="dynaspec-scroll"', player_attrs_int(speeds, keep.pitch, buttons, download.name, theme),
    ' data-color="', col2hex_int(lcol), '" data-text-color="', contrast_col_int(lcol), '">\n',
    '<script type="application/json">', json_int(cfg), '</script>\n',
    '<img class="ds-frame" srcset="', base64_uri_int(frame_file, "image/png"), ' 1x" alt="Scrolling spectrogram">\n',
    '<img class="ds-strip" srcset="', base64_uri_int(spectro_file, "image/png"), ' 1x" alt="">\n',
    if (!is.null(osc_file)) paste0('<img class="ds-strip ds-osc" srcset="', base64_uri_int(osc_file, "image/png"), ' 1x" alt="">\n'),
    '<audio controls preload="auto"', if (loop > 1) " loop", ' src="', snd$uri, '"></audio>\n',
    captions_json_int(captions, res = res, width = width, offset = buffer),
    '</div>\n',
    player_js_int()
  )
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


# scrolling spectrogram engine (dynaspec() + ds_scrolling()): videos and web pages of spectrograms scrolling along a
# play line. 'layers' are the selections and 'captions' the captions of the dynaspec object
scrolling_spectro_int <- function(wave, file.name = "scroll.spectro.mp4", hop.size = wl_ms_default, wl = NULL, ovlp = 70, flim = NULL, pal = seewave::reverse.gray.colors.1, speed = 1, fps = 50, t.display = 1.5, fix.time = TRUE, res = 70, width = 700, height = 400, parallel = 1, verbose = TRUE, play = TRUE, loop = 1, lcol = "#07889B99", lty = 2, lwd = 2, axis.type = "standard", buffer = 0, ggspectro = FALSE, lower.spectro = TRUE, height.prop = c(5, 1), derivative = FALSE, osc = FALSE, colwave = "black", colbg = NULL, spectro.call = NULL, annotation.call = NULL, speeds = c(1, 0.5, 0.25, 0.125), keep.pitch = FALSE, buttons = "time", download.name = "sound", layers = NULL, osc.type = "osc", theme = list(), captions = NULL, still.ends = TRUE, ...)
{

  # check arguments
  check_results <- .check_arguments(
    fun = "scrolling_spectro",
    args = list(wave = wave, file.name = file.name, still.ends = still.ends, hop.size = hop.size, wl = wl, ovlp = ovlp, flim = flim, pal = pal, speed = speed, fps = fps, t.display = t.display, fix.time = fix.time, res = res, width = width, height = height, parallel = parallel, verbose = verbose, play = play, loop = loop, lcol = lcol, lty = lty, lwd = lwd, axis.type = axis.type, buffer = buffer, ggspectro = ggspectro, lower.spectro = lower.spectro, height.prop = height.prop, derivative = derivative, osc = osc, colwave = colwave, colbg = colbg, spectro.call = spectro.call, annotation.call = annotation.call, speeds = speeds, keep.pitch = keep.pitch, buttons = buttons)
  )

  # report errors
  .report_assertions(check_results)

  # still ends: the window shown stays within the sound (the spectrogram is still at the start and the end, and the play
  # line moves from the left edge to the center and from the center to the right edge); looping sounds scroll continuously
  still_ends <- isTRUE(still.ends) && loop == 1

  # no time axis title in sounds of comparisons other than the last one: frames are shorter by the room of the title, so
  # the spectrogram is as high as with it
  time_title <- !(isFALSE(theme$time.title) && axis.type == "standard")
  # (axis titles at a fixed distance from the axis labels, and margins that fit them, see title_margin_int())
  title_line <- 1.4 + theme_int(theme, "cex.axis", 1)
  xlab_line <- xlab_line_int(1 + 0.9 * theme_int(theme, "cex.axis", 1), theme_int(theme, "cex.lab", 1))
  bottom_title <- xlab_margin_int(1 + 0.9 * theme_int(theme, "cex.axis", 1), theme_int(theme, "cex.lab", 1))
  if (!time_title) height <- height - round((bottom_title - 2.2) * png_line_in_int(res, theme) * res)

  # other arguments in '...' are passed to seewave::spectro()
  spectro_args <- list(...)

  # spectrogram background: color of the lowest amplitude of the palette, unless supplied
  if (is.null(colbg)) colbg <- low_col_int(pal, if (is.null(spectro_args$collevels)) 30 else length(spectro_args$collevels) - 1)

  # no grid lines (as in static spectrograms) unless requested
  # grid lines (ds_spectrogram(grid = TRUE) or ds_theme(grid = TRUE))
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

  # frequency limits within the Nyquist frequency (as in static spectrograms)
  nyquist <- wave@samp.rate / 2000
  if (!is.null(flim) && flim[1] >= nyquist)
    stop2("'flim' lower limit must be lower than the Nyquist frequency (", nyquist, " kHz)")
  if (!is.null(flim))
    flim[2] <- min(flim[2], nyquist)

  # flim for spectro() (NULL = full range; an upper limit at the Nyquist frequency makes seewave index past the last
  # frequency bin, so it is kept one bin below)
  spectro_flim <- flim
  if (!is.null(spectro_flim))
    spectro_flim[2] <- min(spectro_flim[2], nyquist - wave@samp.rate / wl / 1000)

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
  png_device_int(filename = file.path(temp_dir, "temp.full.spectro.png"), height = height, width = wdt, res = res)

  # set plot margins for spectrogram
  graphics::par(mar = rep(0, 4))

  # sound for the spectrogram: half a window more at each end, see padded_wave_int()
  wave_pad <- padded_wave_int(wave, wave_sil, length(add_sgm_strt@left), length(add_sgm_end@left), wl, loop > 1)
  dur_sil <- seewave::duration(wave_sil)
  # (overlap adjusted so a whole number of steps between windows spans the sound exactly; less than a step of difference)
  n_steps <- max(1, round(length(wave_sil@left) / (wl * (1 - ovlp / 100))))
  ovlp_pad <- 100 * (1 - length(wave_sil@left) / n_steps / wl)

  # plot spectro (times are then those of 'wave_sil', the time axis of the image)
  if (is.null(spectro.call)) {
  # (seewave::spectro() prints a note with cat() when drawing takes more than 10 s, which is not shown)
  invisible(utils::capture.output(suppressMessages(do.call(seewave::spectro, c(list(wave = wave_pad, f = wave_pad@samp.rate, wl = wl, ovlp = ovlp_pad, axisX = FALSE, axisY = FALSE, scale = FALSE, flim = spectro_flim, palette = pal, osc = FALSE, colbg = colbg), spectro_args)))))
  graphics::par(usr = c(0, dur_sil, graphics::par("usr")[3:4]))
  } else {

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
    spg <- do.call(seewave::spectro, c(list(wave = wave_pad, f = wave_pad@samp.rate, wl = wl, ovlp = ovlp_pad, flim = spectro_flim, plot = FALSE), spc_args))
    spg$time <- spg$time * dur_sil / seewave::duration(wave_pad)
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
  png_device_int(filename = file.path(temp_dir, "temp.full.spectro.png"), height = height, width = wdt, res = res)

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
    png_device_int(filename = file.path(temp_dir, "temp.full.oscillo.png"), height = height, width = wdt, res = res)

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
    # time of the frame (time of the sound, which is also the start of the window in the strip, as the strip starts with
    # half a window of silence) and center of the window shown (within the sound with still ends)
    t_frame <- max(0, (x - 1) * step_time - buffer)
    center <- if (still_ends) min(max(t_frame, t.display / 2), seewave::duration(wave) - t.display / 2) else t_frame
    # (position of the play line in the window, as a proportion of its width)
    line_at <- 0.5 + (t_frame - center) / t.display
    tlim <- c(center, center + t.display)

    # pixel limt
    img.x.lim <- round(tlim * px.per.s, 0)

    # tlim_low are limits for white rectangle in lower spectrogram (centered on the window shown)
    tlim_low <- rep(center * t.display / seewave::duration(wave), 2)

    tlim_low <- tlim_low + c(white_window * -1, white_window) / 2

    # start png device (tiff files from R are not readable by recent ffmpeg versions)
    png_device_int(file, res = res, width = width, height = height, bg = if (html_frame) "transparent" else theme_int(theme, "bg", "white"))

    # axis colors, fonts and sizes (ds_theme())
    col_lab <- theme_int(theme, "col.lab", "black")
    # (parts of the overview outside the window shown are dimmed)
    dim_col <- grDevices::adjustcolor(theme_int(theme, "col.highlight", "gray"), 0.42)
    col_axis <- axis_col_int(theme)
    x_las <- theme_int(theme, "las", 0)
    # (room taken by vertical time labels, las = 2 or 3)
    x_extra <- x_lab_extra_int(x_las, pretty(c(0, max(t.display, seewave::duration(wave)))), theme_int(theme, "cex.axis", 1))
    graphics::par(col.axis = col_axis, col.lab = col_lab, fg = col_lab, font.lab = theme_int(theme, "font.lab", 1),
                  cex.lab = theme_int(theme, "cex.lab", 1), cex.axis = theme_int(theme, "cex.axis", 1))
    theme_par_int(theme)

    # set regular margins
    if (axis.type == "compact")
      graphics::par(mar = c(0, 0.6 + 1.6 * theme_int(theme, "cex.axis", 1), 0.4 + 1.1 * theme_int(theme, "cex.axis", 1), 0)) else
    if (axis.type == "inside")
      graphics::par(mar = c(0.45 + theme_int(theme, "cex.axis", 1), 0, 0, 0)) else
    if (axis.type != "standard")
      graphics::par(mar =  rep(0, 4)) else
    graphics::par(mar =  c((if (time_title) bottom_title else 2.2) + x_extra, title_margin_int(title_line, theme_int(theme, "cex.lab", 1)), if (isFALSE(theme$top.margin)) 0 else 1, 0) + 0.1)

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
    graphics::plot(0,0, type = "n", xlim = if(fix.time) c(0, t.display) else tlim, ylim = flim, xlab = "", ylab = "", xaxs = "i", yaxs = "i", bty = "n", xaxt = if (axis.type == "standard" && fix.time && frame_time) "s" else "n", yaxt = "n", las = x_las)
    # time axis title (below vertical time labels)
    if (axis.type == "standard" && time_title)
      graphics::title(xlab = theme_int(theme, "xlab", "Time (s)"), line = xlab_line + x_extra)
    # frequency axis title (in comparisons of sounds, at the position shared by all of them: 'ylab.center', from the top of
    # the figure as a proportion of its height)
    if (axis.type == "standard") {
      ylab <- theme_int(theme, "ylab", "Frequency (kHz)")
      if (is.null(theme$ylab.center)) graphics::title(ylab = ylab, line = title_line) else
        graphics::mtext(ylab, side = 2, line = title_line, at = graphics::grconvertY(1 - theme$ylab.center, "ndc", "user"), cex = graphics::par("cex.lab"),
                        col = graphics::par("col.lab"), font = graphics::par("font.lab"), las = 0)
    }

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

    # add play line (with still ends, web pages move it, so it is not drawn in their frame)
    if (!(still_ends && html_frame))
      graphics::abline(v = usr[1] + (usr[2] - usr[1]) * line_at, lty = lty, col = lcol, lwd = lwd)

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
        graphics::axis(1, at = t_at + half, labels = t_at, las = x_las)
    }

    # compact axes (time labels move along in html frames when time is not fixed)
    if (axis.type == "compact") {
      compact_freq_axis_int(org.flim, col = col_lab, cex = theme_int(theme, "cex.axis", 1), bottom = org.flim[1], col.text = col_axis)
      if (frame_time) {
        compact_time_axis_int(t_at + half, t_at, col = col_lab, cex = theme_int(theme, "cex.axis", 1), col.text = col_axis)
      }
    }

    # labels inside the figure (time labels move along in html frames when time is not fixed)
    if (axis.type == "inside") {
      inside_freq_axis_int(org.flim, col = col_lab, cex = theme_int(theme, "cex.axis", 1), col.text = col_axis)
      if (frame_time) {
        inside_time_axis_int(t_at + half, t_at, col = col_lab, cex = theme_int(theme, "cex.axis", 1), col.text = col_axis)
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
      # (exact steps: 'frames', rounded to whole frames for videos, would move the strips at a slightly different
      # speed than the sound, noticeably in short sounds)
      step_time = (seewave::duration(wave_sil) - t.display) / (seewave::duration(wave) * fps),
      step_time_low = t.display / (seewave::duration(wave) * fps), fps = fps, loop = loop, fix.time = fix.time,
      axis = axis.type %in% c("standard", "inside", "compact"), zoom = zoom, inside = axis.type == "inside", compact = axis.type == "compact", width = width, res = res, captions = captions,
      speeds = speeds, keep.pitch = keep.pitch, buttons = buttons, download.name = download.name,
      lcol = lcol, theme = theme, still.ends = still_ends, lty = lty, lwd = lwd
    )

    res_html <- output_html_int(page, file.name = file.name, play = play)
    # (figure height and vertical extent of the plotting region, from the top as proportions of the height; see
    # save_compare_int())
    attr(res_html, "layout") <- list(height = height, top = 1 - plt[4], bottom = 1 - plt[3])
    return(res_html)
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


# sound used for the spectrogram of scrolling spectrograms. seewave::spectro() draws its first window (centered half
# a window after the start of the sound) at time 0 and its last one at the end of the sound, which stretches the time
# axis by the length of a window. With half a window more at each end, the image spans exactly 'wave_sil': frames fall
# at their true times, so the spectrogram matches the audio and the oscillogram, and when looping its end matches its
# start. The sound continues as it does when looping (the added segments have 'n_start' and 'n_end' samples); otherwise
# the silence at its ends is extended
padded_wave_int <- function(wave, wave_sil, n_start, n_end, wl, loop) {
  half <- wl %/% 2
  n <- length(wave@left)
  sil <- wave_sil@left
  before <- if (loop) wave@left[(seq(n - n_start - half, length.out = half)) %% n + 1] else rep(sil[1], half)
  after <- if (loop) wave@left[(seq(n_end, length.out = half)) %% n + 1] else rep(sil[length(sil)], half)
  wave_sil@left <- c(before, sil, after)
  wave_sil@right <- numeric(0)
  wave_sil@stereo <- FALSE
  wave_sil
}
