# paged spectrogram engine (pages with a highlight box revealing the part played)

# web page of paged_spectro(): one image per page ('pages': list of page images (data uris) and the extents of
# their highlight boxes)
paged_html_int <- function(pages, wave, highlightCol, highlightAlpha, cursorCol, speeds, keep.pitch, buttons = "time", download.name = "sound",
                           captions = NULL, res = 100, width = 1000, theme = list(), audio = NULL) {
  ext <- do.call(rbind, lapply(pages, `[[`, "ext"))
  # ('audio': sound embedded when it is not the one in the figure, see save_sound_int())
  snd <- sound_data_int(if (is.null(audio)) wave else audio, read = FALSE)
  n_pages <- length(pages)

  paste0(
    '<div class="dynaspec-paged" data-duration="', seewave::duration(wave),
    '" data-rows="', paste(apply(round(ext, 5), 1, paste, collapse = ","), collapse = ";"),
    '" data-highlight="', col2hex_int(highlightCol), '" data-alpha="', highlightAlpha, '" data-cursor="', col2hex_int(cursorCol),
    '"', player_attrs_int(speeds, keep.pitch, buttons, download.name, theme),
    ' data-color="', col2hex_int(highlightCol), '" data-text-color="', contrast_col_int(highlightCol), '">\n',
    paste0('<img srcset="', vapply(pages, `[[`, character(1), "uri"), ' 1x" alt="Spectrogram, page ', seq_len(n_pages), ' of ', n_pages, '">', collapse = "\n"),
    '\n<audio controls preload="auto" src="', snd$uri, '"></audio>\n',
    captions_json_int(captions, res = res, width = width),
    '</div>\n',
    player_js_int()
  )
}


# video of one page of a paged spectrogram: the translucent highlight box (from the cursor to the end of the
# page) and the cursor move over the page image along the sound ('ext': time range and box extents)
paged_video_int <- function(png_file, wave, ext, file.name, highlightCol, highlightAlpha, cursorCol, fps, captions = NULL, res = 100, audio = TRUE) {
  wav_file <- tempfile(fileext = ".wav")
  on.exit(unlink(wav_file), add = TRUE)
  tuneR::writeWave(if (wave@pcm) wave else tuneR::normalize(wave, unit = "16"), filename = wav_file)

  dims <- dim(png::readPNG(png_file))
  W <- dims[2]
  H <- dims[1]
  dur <- ext[["t1"]] - ext[["t0"]]
  # (frames of the page counted from the start of the sound, so rounding to whole frames does not add up over pages)
  n_frames <- max(1, round(ext[["t1"]] * fps) - round(ext[["t0"]] * fps))
  x0 <- round(ext[["left"]] * W)
  x1 <- round(ext[["right"]] * W)
  y0 <- round(ext[["top"]] * H)
  bh <- max(1, round((ext[["bottom"]] - ext[["top"]]) * H))
  lw <- max(2, round(W / 400))
  speed <- (x1 - x0) / dur

  # the box moves right as the sound plays; the part of the frame right of the page end (if any) is put back on top
  filter <- paste0(
    if (x1 < W) "[0]split=2[a][b];" else "[0]null[a];",
    "[1]format=rgba,colorchannelmixer=aa=", highlightAlpha, "[box];",
    "[a][box]overlay=x='", x0, "+", speed, "*t':y=", y0, if (x1 < W) "[v1];" else "[v2];",
    if (x1 < W) paste0("[b]crop=", W - x1, ":", H, ":", x1, ":0[r];[v1][r]overlay=x=", x1, ":y=0[v2];"),
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
      ' -i "', wav_file, '" ', paste(caps$inputs, collapse = " "), ' -filter_complex "', filter, '" -map "[v]"', if (audio) ' -map 3:a',
      ' -c:v libx264 -tune stillimage -r ', fps, if (audio) paste0(' -c:a aac -b:a 192k -ac ', if (wave@stereo) 2 else 1, ' -t ', dur) else paste0(' -an -frames:v ', n_frames),
      ' "', file.name, '"'
    ),
    "creating the video"
  )

  invisible(file.name)
}


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
      if (verbose) message_dyn_int("Crop longer than file duration (", round(fileDur0, 2), " s), crop end set to file duration")
    }
  } else{
    #If crop not supplied, default crop to duration, unless >10 sec, warning user
    if (fileDur0 > 10) {
      crop = c(0, 10)
      if (verbose) message_dyn_int("Cropping to 1st 10 sec due to slow processing of large WAV files. To override, set crop = FALSE or crop = c(startInSec, stopInSec)")
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
      if (verbose) message_dyn_int("xLim set to 5 sec by default; define to override")
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
  
  # (a tiny excess, as a fraction of a sample from the silence added above, does not make another page)
  segLens <- seq(0, ceiling(wavDur / xLim[2] - 1e-3) * xLim[2], xLim[2])
  indx <- 1:(length(segLens) - 1)
  segWavs <-
    lapply(indx, function(i)
      seewave::cutw(
        wav,
        from = segLens[i],
        to = min(segLens[i + 1], wavDur),
        output = "Wave",
        bit = wav0@bit
      ))
  n_pages = length(segWavs)
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
paged_spec_data_int <- function(wav, resampleRate, yLim, segLens, wl, ovlp, wn, verbose = TRUE, wl.ms = NULL, ...) {
  if (!is.null(resampleRate)) {
    # keep the Nyquist frequency at or above the upper frequency limit
    if (resampleRate < yLim[2] * 2000) {
      resampleRate <- yLim[2] * 2000
      if (verbose) message_dyn_int("resample.rate increased to ", resampleRate, " Hz so the spectrogram reaches the upper frequency limit (", yLim[2], " kHz)")
    }

    # only downsample (upsampling is not supported), and only when it reduces the sound noticeably
    # (tuneR::downsample() does not filter, so small rate changes add artifacts without saving time)
    if (resampleRate < 0.9 * wav@samp.rate) {
      if (verbose) message_dyn_int("Original wav (", wav@samp.rate, " samples/sec) data points: ", length(wav@left))
      wav <- tuneR::downsample(wav, samp.rate = resampleRate)
      if (verbose) message_dyn_int("Downsampled wav (", wav@samp.rate, " samples/sec) data points: ", length(wav@left))
    }
  }
  # (window length at the sampling rate of the spectrogram: 'wl.ms', 'wl', or 11.61 ms)
  wl <- resolve_wl_int(wav@samp.rate, wl, wl.ms)


  sp <- seewave::spectro(wav, plot = FALSE, ovlp = ovlp, wl = wl, wn = wn, ...)

  # one more frequency bin and time frame beyond each limit, so the spectrogram reaches the edges of the figure
  # (they are clipped when drawing)
  around <- function(x, from, to) {
    keep <- which(x >= from & x <= to)
    if (length(keep) == 0) keep <- which.min(abs(x - (from + to) / 2))
    seq(max(1, min(keep) - 1), min(length(x), max(keep) + 1))
  }
  in_flim <- around(sp$freq, yLim[1], yLim[2])
  freq <- sp$freq[in_flim]
  amp <- sp$amp[in_flim, , drop = FALSE]
  # (the last frequency bin of spectrograms is one bin below the Nyquist frequency)
  if (max(freq) < yLim[2]) {
    freq <- c(freq, yLim[2])
    amp <- rbind(amp, amp[nrow(amp), ])
  }

  n_pages <- length(segLens) - 1
  lapply(seq_len(n_pages), function(i) {
    in_page <- around(sp$time, segLens[i], segLens[i + 1])
    list(time = sp$time[in_page], freq = freq, amp = amp[, in_page, drop = FALSE])
  })
}


# colors of a palette (viridis palette name, two or more colors for a gradient, or palette function)
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
  # color of axis labels, and room taken by vertical time labels (las = 2 or 3)
  axis_col <- theme_int(th, "col.axis", txt_col)
  x_extra <- x_lab_extra_int(theme_int(th, "las", 1), pretty(range_i), cex_axis)
  # (no time axis title in sounds of comparisons other than the last one, see paged_bottom_int())
  time_title <- !isFALSE(th$time.title)

  op <- graphics::par(
    bg = fig_bg,
    mar = if (inside) c(0.45 + cex_axis, 0, if (has_title) 3 else 0, if (sp$plotLegend) 7 else 0) else
      if (compact) c(0.3, 0.6 + 1.6 * cex_axis, 0.4 + 1.1 * cex_axis + if (has_title) 2.4 else 0, if (sp$plotLegend) 7 else 0.1) else
      c(if (axes) paged_bottom_int(th, time_title) + x_extra else 0, if (axes) title_margin_int(paged_title_line_int(th), 20 / 12 * lab_cex) else 0, if (has_title) 3 else if (axes && !isFALSE(th$top.margin)) 1.2 else 0, if (sp$plotLegend) 7 else if (axes) 0.1 else 0),
    col.axis = txt_col, col.lab = txt_col, col.main = theme_int(th, "col.title", txt_col), fg = txt_col, las = theme_int(th, "las", 1),
    family = theme_int(th, "family", "sans"), tcl = tcl
  )
  on.exit(graphics::par(op), add = TRUE)
  box_col <- theme_int(th, "col.box", "gray20")

  # the spectrogram fills the plotting region
  graphics::plot.new()
  graphics::plot.window(xlim = range_i, ylim = sp$yLim, xaxs = "i", yaxs = "i")
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
    compact_freq_axis_int(sp$yLim, col = txt_col, cex = cex_axis, col.text = axis_col)
    at <- pretty(range_i)
    at <- at[at >= usr[1] & at <= usr[2]]
    compact_time_axis_int(at, at, col = txt_col, cex = cex_axis, band = 0.4 + 1.1 * cex_axis, col.text = axis_col)
    if (!isFALSE(th$box)) graphics::box(col = box_col)
  } else if (inside) {
    inside_freq_axis_int(sp$yLim, col = txt_col, cex = cex_axis, col.text = axis_col)
    at <- pretty(range_i)
    at <- at[at >= usr[1] & at <= usr[2]]
    inside_time_axis_int(at, at, col = txt_col, cex = cex_axis, col.text = axis_col)
    if (!isFALSE(th$box)) graphics::box(col = box_col)
  } else if (axes) {
    graphics::axis(1, cex.axis = cex_axis, tcl = tcl, mgp = c(3, 0.6, 0), col = box_col, col.axis = axis_col)
    graphics::axis(2, cex.axis = cex_axis, tcl = tcl, mgp = c(3, 0.6, 0), col = box_col, col.axis = axis_col)
    font_lab <- theme_int(th, "font.lab", 2)
    cex_lab <- 20 / 12 * lab_cex
    if (time_title)
      graphics::title(xlab = theme_int(th, "xlab", "Time (s)"), cex.lab = cex_lab, font.lab = font_lab, line = xlab_line_int(0.6 + 0.9 * cex_axis, cex_lab) + x_extra)
    # the frequency axis title is shrunk if it is longer than the figure height
    ylab <- theme_int(th, "ylab", "Frequency (kHz)")
    # (it is centered on the plotting region, so it can extend twice the distance to the closest figure edge)
    plt <- graphics::par("plt")
    room <- 2 * min(mean(plt[3:4]), 1 - mean(plt[3:4])) * graphics::par("din")[2]
    cex_y <- min(cex_lab, cex_lab * 0.95 * room / graphics::strwidth(ylab, units = "inches", cex = cex_lab, font = font_lab))
    # (in comparisons of sounds, at the position shared by all of them: 'ylab.center', from the top of the figure as a
    # proportion of its height)
    if (is.null(th$ylab.center)) graphics::title(ylab = ylab, cex.lab = cex_y, font.lab = font_lab, line = paged_title_line_int(th)) else
      graphics::mtext(ylab, side = 2, line = paged_title_line_int(th), at = graphics::grconvertY(1 - th$ylab.center, "ndc", "user"), cex = cex_lab, font = font_lab, col = txt_col, las = 0)
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


# bottom margin (in lines, without the room taken by vertical labels) of pages with standard axes, with or without time
# axis title
paged_bottom_int <- function(th, title = TRUE) {
  if (title) xlab_margin_int(0.6 + 0.9 * 16 / 12 * theme_int(th, "cex.axis", 1), 20 / 12 * theme_int(th, "cex.lab", 1)) else 1.3 + 16 / 12 * theme_int(th, "cex.axis", 1)
}

# line of the axis titles of pages: a fixed distance from the axis labels (16 points at cex.axis 1)
paged_title_line_int <- function(th) 0.6 + 1.2 * 16 / 12 * theme_int(th, "cex.axis", 1)


# draw page i of a paged spectrogram in a png file, 'spec.width' x 'spec.height' inches at resolution 'res'
page_png_int <- function(sp, i, file, res) {
  png_device_int(file, width = sp$specWidth, height = sp$specHeight, units = "in", res = res, bg = "white")
  dev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dev))
  draw_page_int(sp, i)
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
           wl = NULL,
           wl.ms = NULL,
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
      if (verbose) message_dyn_int("Created destination folder: ", destFolder)
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

    # frequency limits within the Nyquist frequency (as in other styles)
    nyquist <- wav0@samp.rate / 2000
    if (yLim[1] >= nyquist)
      stop2("'flim' lower limit must be lower than the Nyquist frequency (", nyquist, " kHz)")
    yLim[2] <- min(yLim[2], nyquist)

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
      verbose = verbose,
      wl.ms = wl.ms
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
        if (verbose) message_dyn_int("Static spectrogram saved at: ", fn_i, type = "success")
      }
    }

    # plot first page
    if (plot) {
      draw_page_int(specParams, 1)

      if (verbose && specParams$n_pages > 1)
        message_dyn_int("For segmented spectrogram, only segment 1 shown")
    }

    return(specParams)
  }


# paged spectrogram engine (dynaspec() + ds_paged())
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
                          verbose = TRUE,
                          audio = NULL,
                          video.res = NULL)
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
    if (verbose) message_dyn_int("Created destination folder: ", destFolder)
  }

  # uniquely named temporary folder (so no user folder is ever deleted)
  temp_folder <- tempfile(pattern = "dynaSpec_temp_", tmpdir = destFolder)
  dir.create(temp_folder)
  if (delete_temp_files)
    on.exit(unlink(temp_folder, recursive = TRUE), add = TRUE)
  else if (verbose) message_dyn_int("Temporary files saved at: ", temp_folder)

  n_pages <- specParams$n_pages
  page_png <- function(i, res) fs::path(temp_folder, paste0(iName0, "_", i, "_", res, ".png"))

  # web page: page images (twice the resolution of videos) and highlight box extents
  if (html) {
    pages <- .pbapply_dyn(X = seq_len(n_pages), pbar = verbose, title = "Rendering pages", FUN = function(i) {
      file <- page_png(i, "html")
      ext <- page_png_int(specParams, i, file, res = 1000 / specParams$specHeight)
      list(ext = ext, uri = base64_uri_int(file, "image/png"))
    })

    page_html <- paged_html_int(pages = pages, wave = specParams$newWav, highlightCol = highlightCol, highlightAlpha = highlightAlpha,
                                cursorCol = cursorCol, speeds = speeds, keep.pitch = keep.pitch, buttons = buttons,
                                download.name = download.name, captions = specParams$layers$captions,
                                res = 1000 / specParams$specHeight, width = 1000 / specParams$specHeight * specParams$specWidth,
                                theme = if (is.null(specParams$theme)) list() else specParams$theme, audio = audio)


    res_html <- output_html_int(page_html, file.name = vidName, play = play)
    # (height of the pages at a width of 1000 pixels, and vertical extent of the spectrogram, from the top as proportions
    # of the height; see save_compare_int())
    attr(res_html, "layout") <- list(height = 1000 * specParams$specHeight / specParams$specWidth, top = pages[[1]]$ext[["top"]], bottom = pages[[1]]$ext[["bottom"]])
    return(res_html)
  }

  # (resolution of video pages: 'video.res', or pages 500 pixels high)
  video_res <- if (is.null(video.res)) 500 / specParams$specHeight else video.res

  # video: each page is drawn once (500 pixels high) and ffmpeg moves the highlight box and cursor over it
  render_page <- function(i) {
    png_file <- page_png(i, "video")
    ext <- page_png_int(specParams, i, png_file, res = video_res)
    paged_video_int(png_file = png_file, wave = specParams$segWavs[[i]], ext = ext, file.name = fs::path(temp_folder, paste0(iName0, "_", i, "_.mp4")),
                    highlightCol = highlightCol, highlightAlpha = highlightAlpha, cursorCol = cursorCol, fps = framerate,
                    captions = specParams$layers$captions, res = video_res, audio = n_pages == 1)
  }

  # render pages (with progress bar)
  segmented <- n_pages > 1
  .pbapply_dyn(X = seq_len(n_pages), cl = min(parallel, n_pages), pbar = verbose, title = "Rendering video pages", FUN = render_page)

  # combine pages
  segVids <- fs::path(temp_folder, paste0(iName0, "_", seq_len(n_pages), "_.mp4"))
  if (segmented) {
    # (pages have no sound: the sound of each page encoded separately starts a few ms late, which adds up over the pages,
    # so the whole sound is added once)
    writeLines(paste0("file '", segVids, "'"), fs::path(temp_folder, "mp4Segments.txt"))
    pages_file <- fs::path(temp_folder, "pages_only.mp4")
    run_ffmpeg_int(paste0('-f concat -safe 0 -i "', fs::path(temp_folder, "mp4Segments.txt"), '" -codec copy -y "', pages_file, '"'), "concatenating video segments")
    sound <- Reduce(tuneR::bind, specParams$segWavs)
    wav_file <- fs::path(temp_folder, "pages_sound.wav")
    tuneR::writeWave(if (sound@pcm) sound else tuneR::normalize(sound, unit = "16"), filename = wav_file)
    run_ffmpeg_int(paste0('-y -i "', pages_file, '" -i "', wav_file, '" -map 0:v -map 1:a -c:v copy -c:a aac -b:a 192k -ac ', if (sound@stereo) 2 else 1,
                          ' -movflags +faststart "', vidName, '"'), "adding the sound")
  } else
    file.copy(segVids, vidName, overwrite = TRUE)


  if (play)
    open_file_int(vidName)

  invisible(vidName)
}
