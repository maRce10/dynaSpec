# drawing helpers shared by the engines: panels, axes and png devices

# oscillogram downsampled to min/max amplitude per pixel column
oscillo_player_int <- function(wave, xlim, colwave, n, axisX = FALSE, cex.lab = 1, offset = 0, max.amp = max(abs(wave@left), 1e-12), empty = FALSE, theme = list()) {
  amp <- wave@left / max.amp
  tm <- offset + (seq_along(amp) - 1) / wave@samp.rate

  # small vertical margin so peaks do not touch the box
  panel_frame_int(xlim, c(-1.05, 1.05), theme)
  on.exit(theme_box_int(theme), add = TRUE)
  if (!empty && !isTRUE(theme$hide) && !isTRUE(theme$ylab.hide))
    graphics::mtext("Amp.", side = 2, line = 0.5, cex = cex.lab, las = 1, xpd = NA, font = theme_int(theme, "font.lab", 1), col = theme_int(theme, "col.lab", "black"))

  if (axisX)
    do.call(time_axis_int, c(list(to_x = identity, xlim = xlim, cex.lab = cex.lab, inside = identical(theme$axis.type, "inside")), theme[intersect(names(theme), c("xlab", "font.lab", "col.lab", "cex.axis", "col.axis", "las"))]))

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


# lines of text below the time axis taken by vertical time labels (las = 2 or 3) beyond those taken by horizontal
# labels (approximate width of the longest label, in lines), to move the axis title and widen the margin
x_lab_extra_int <- function(las, labels, cex = 1) {
  if (!isTRUE(las %in% 2:3) || length(labels) == 0) return(0)
  max(0, max(nchar(as.character(labels))) * 0.5 * cex - 0.6)
}


# margin (in lines) that fits a y axis title of size 'cex' placed at 'line' (rotated titles grow outwards from the
# line; they are about 0.85 lines high at size 1), so larger titles widen the margin instead of being cut at the edge
title_margin_int <- function(line, cex) line + 0.85 * cex + 0.15

# line of a time axis title of size 'cex' below labels that end at line 'labels': titles below the plot grow upwards from
# their baseline (about one line below 'line'), so larger titles are moved down to keep a fixed gap above them
xlab_line_int <- function(labels, cex) labels - 1 + 0.85 * cex + 0.3

# bottom margin (in lines) that fits that title (and the parts of its letters below the baseline)
xlab_margin_int <- function(labels, cex) xlab_line_int(labels, cex) + 1 + 0.2 * cex

# size of a y axis title that fits along the height of the figure (titles are centered on 'center', from the bottom of
# the device as a proportion of its height, and are made smaller when longer than twice the distance to its closest edge)
fit_ylab_cex_int <- function(label, cex, center, font = 1) {
  if (!nzchar(label)) return(cex)
  room <- 2 * min(center, 1 - center) * graphics::par("din")[2]
  min(cex, cex * 0.95 * room / graphics::strwidth(label, units = "inches", cex = cex, font = font))
}

# png device used for all figures: on Windows the cairo-based device (when available) instead of the default Windows GDI
# device, which differs in line heights and fills (e.g. figure corners); elsewhere the default device (cairo on Linux;
# quartz on macOS, where the cairo device may not write files, e.g. without XQuartz)
png_device_int <- function(...) {
  if (.Platform$OS.type == "windows" && isTRUE(capabilities("cairo"))) grDevices::png(..., type = "cairo") else grDevices::png(...)
}


# height (in inches) of a line of text (margin line) in a png device at resolution 'res' with the font family of the theme
png_line_in_int <- function(res, theme = list()) {
  old <- grDevices::dev.cur()
  tmp <- tempfile(fileext = ".png")
  png_device_int(tmp, width = 100, height = 100, res = res)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
    if (old > 1) grDevices::dev.set(old)
  })
  theme_par_int(theme)
  graphics::par("csi")
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
  if (!isTRUE(theme$hide) && !isTRUE(theme$ylab.hide))
    graphics::mtext("Amp.", side = 2, line = 0.5, cex = cex.lab, las = 1, xpd = NA, font = theme_int(theme, "font.lab", 1), col = theme_int(theme, "col.lab", "black"))
  if (axisX)
    do.call(time_axis_int, c(list(to_x = identity, xlim = xlim, cex.lab = cex.lab, inside = identical(theme$axis.type, "inside")), theme[intersect(names(theme), c("xlab", "font.lab", "col.lab", "cex.axis", "col.axis", "las"))]))
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
    compact_freq_axis_int(ylim, col = col_lab, cex = theme_int(theme, "cex.axis", 1), col.text = axis_col_int(theme))
  else if (axis) {
    # as the frequency axis of the spectrogram
    graphics::axis(2, las = theme_int(theme, "las", 1), col = col_lab, col.axis = axis_col_int(theme), cex.axis = theme_int(theme, "cex.axis", 1))
    graphics::title(ylab = theme_int(theme, "ylab", "Frequency (kHz)"), cex.lab = cex.lab, col.lab = col_lab, font.lab = theme_int(theme, "font.lab", 1))
  }
  theme_box_int(theme)
  if (label)
    graphics::mtext("Amp.", side = 3, line = 0.1, cex = 0.8 * cex.lab * theme_int(theme, "cex.title", 1), col = theme_int(theme, "col.title", col_lab))
}


# spectrogram panel of static spectrograms: computed with seewave::spectro() and drawn in the current panel
# (seewave::spectro() plots set the background of the whole page to 'colbg', covering other panels and the figure
# background). As in seewave, analysis windows are spread evenly from 0 to the duration of the sound. Other
# arguments are passed to seewave::spectro()
spectro_int <- function(wave, f, wl, ovlp, flim, pal, collevels, colbg = "white", dBref = NULL, axisY = TRUE,
                        flab = "Frequency (kHz)", cex.lab = 1, cex.axis = 1, col.lab = "black", theme = list(), ylab.line = 3, ...) {
  # (an upper limit at the Nyquist frequency makes seewave index past the last frequency bin, so it is kept one bin below)
  if (!is.null(flim))
    flim[2] <- min(flim[2], (f / 2 - f / wl) / 1000)
  sp <- seewave::spectro(wave, f = f, wl = wl, ovlp = ovlp, flim = flim, norm = FALSE, dBref = dBref, plot = FALSE, ...)
  graphics::plot.new()
  graphics::plot.window(xlim = range(sp$time), ylim = range(sp$freq), xaxs = "i", yaxs = "i")
  usr <- graphics::par("usr")
  graphics::rect(usr[1], usr[3], usr[2], usr[4], col = colbg, border = NA)
  graphics::.filled.contour(sp$time, sp$freq, t(sp$amp), levels = collevels, col = pal(length(collevels) - 1))
  theme_grid_int(theme)
  theme_box_int(theme, col.lab)
  col_axis <- axis_col_int(theme, col.lab)
  if (axisY && identical(theme$axis.type, "inside"))
    inside_freq_axis_int(range(sp$freq), col = col.lab, cex = cex.axis, col.text = col_axis)
  else if (axisY && identical(theme$axis.type, "compact"))
    compact_freq_axis_int(range(sp$freq), col = col.lab, cex = cex.axis, col.text = col_axis)
  else if (axisY) {
    graphics::axis(2, las = theme_int(theme, "las", 1), col = col.lab, col.axis = col_axis, cex.axis = cex.axis)
    if (nzchar(flab)) graphics::mtext(flab, side = 2, line = ylab.line, col = col.lab, font = graphics::par("font.lab"), las = 0,
                                      cex = fit_ylab_cex_int(flab, cex.lab, mean(graphics::grconvertY(graphics::par("usr")[3:4], "user", "ndc")), graphics::par("font.lab")))
  }
  invisible(sp)
}


# time axis; 'to_x' converts times to x axis coordinates
time_axis_int <- function(to_x, xlim, cex.lab, xlab = "Time (s)", font.lab = 1, col.lab = "black", cex.axis = 1, inside = FALSE,
                          col.axis = col.lab, las = 0) {
  # ticks slightly outside the plotted range are kept (the spectrogram starts and ends half a window inside the sound)
  tol <- diff(xlim) * 0.01
  at <- pretty(xlim)
  at <- at[at >= xlim[1] - tol & at <= xlim[2] + tol]
  # axis() drops ticks outside the plotting region, so those are moved to its edge (less than a pixel away)
  usr <- graphics::par("usr")
  if (inside) {
    inside_time_axis_int(pmin(pmax(to_x(at), usr[1]), usr[2]), at, col = col.lab, cex = cex.axis, col.text = col.axis)
    return(invisible(NULL))
  }
  # (labels that would be cut at the edges of the figure are not shown, but their ticks are)
  x <- pmin(pmax(to_x(at), usr[1]), usr[2])
  half <- if (isTRUE(las %in% 2:3)) graphics::strheight("0", cex = cex.axis) / 2 * diff(usr[1:2]) / diff(usr[3:4]) * graphics::par("pin")[2] / graphics::par("pin")[1] else
    graphics::strwidth(as.character(at), cex = cex.axis) / 2
  fits <- x - half >= graphics::grconvertX(0, "ndc", "user") & x + half <= graphics::grconvertX(1, "ndc", "user")
  graphics::axis(1, at = x, labels = ifelse(fits, as.character(at), ""), col = col.lab, col.axis = col.axis, cex.axis = cex.axis, las = las)
  # (the title moves down when time labels are vertical)
  graphics::mtext(xlab, side = 1, line = xlab_line_int(1 + 0.9 * cex.axis, cex.lab) + x_lab_extra_int(las, at, cex.axis), cex = cex.lab, font = font.lab, col = col.lab)
}


# time axis of axis.type = "inside": no axis line or title, labels with units (e.g. "2s") close to short ticks.
# Labels too close to the figure edges are dropped (ticks are kept)
inside_time_axis_int <- function(x, at, col = "black", cex = 1, col.text = col) {
  usr <- graphics::par("usr")
  graphics::axis(1, at = x, labels = FALSE, lwd = 0, lwd.ticks = 1, col = col)
  half <- graphics::strwidth(paste0(at, "s"), cex = cex) / 2
  room <- graphics::grconvertX(c(0, 1), "ndc", "user")
  keep <- x - half >= room[1] & x + half <= room[2]
  if (any(keep))
    graphics::axis(1, at = x[keep], labels = paste0(at[keep], "s"), lwd = 0, tick = FALSE, col.axis = col.text, cex.axis = cex,
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
compact_freq_axis_int <- function(ylim, col = "black", cex = 1, bottom = graphics::par("usr")[3], col.text = col) {
  lw <- graphics::grconvertX(1, "lines", "user") - graphics::grconvertX(0, "lines", "user")
  x0 <- graphics::grconvertX(0, "nfc", "user") + 0.3 * lw
  sh <- graphics::strheight("8", cex = cex)
  at <- pretty(ylim, n = 6)
  at <- at[at > bottom + 2.6 * sh & at <= ylim[2]]
  graphics::segments(x0, at, x0 + 0.9 * lw, at, col = col, xpd = NA)
  graphics::text(x0, at - 0.35 * sh, labels = at, adj = c(0, 1), col = col.text, cex = cex, xpd = NA)
  graphics::text(x0, bottom + 0.35 * sh, labels = "kHz", adj = c(0, 0), col = col.text, cex = cex, xpd = NA)
}


# compact time axis above the current panel of a static spectrogram ('to_x' converts times to x axis coordinates)
compact_row_axis_int <- function(to_x, xlim, theme, cex) {
  usr <- graphics::par("usr")
  # (ticks slightly outside the plotted range are kept at its edge, as in time_axis_int())
  tol <- diff(xlim) * 0.01
  at <- pretty(xlim)
  at <- at[at >= xlim[1] - tol & at <= xlim[2] + tol]
  compact_time_axis_int(pmin(pmax(to_x(at), usr[1]), usr[2]), at, col = theme_int(theme, "col.lab", "black"), cex = cex, col.text = axis_col_int(theme))
}


compact_time_axis_int <- function(x, at, col = "black", cex = 1, band = NULL, col.text = col) {
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
    graphics::text(x[keep] + pad, (usr[4] + top) / 2, labels = labs[keep], adj = c(0, 0.5), col = col.text, cex = cex, xpd = NA)
}


# frequency labels of axis.type = "inside": labels with units (e.g. "8k") followed by a short tick, inside the
# left side of the spectrogram (labels closer than half a line of text to the top or bottom are dropped)
inside_freq_axis_int <- function(ylim, col = "black", cex = 1, col.text = col) {
  usr <- graphics::par("usr")
  at <- pretty(ylim, n = 6)
  pad <- graphics::strheight("8", cex = cex)
  at <- at[at >= ylim[1] + pad & at <= ylim[2] - pad]
  if (length(at) == 0) return(invisible(NULL))
  labs <- paste0(at, "k")
  ch <- graphics::strwidth("0", cex = cex)
  x_lab <- usr[1] + 0.8 * ch + max(graphics::strwidth(labs, cex = cex))
  tick <- max(0, -graphics::par("tcl")) * (graphics::grconvertX(1, "lines", "user") - graphics::grconvertX(0, "lines", "user"))
  graphics::text(x_lab, at, labels = labs, adj = c(1, 0.5), col = col.text, cex = cex, xpd = NA)
  graphics::segments(x_lab + 0.4 * ch, at, x_lab + 0.4 * ch + tick, at, col = col)
}
