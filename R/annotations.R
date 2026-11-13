# annotations (boxes, lines, frequency contours) and captions, and how they are drawn

#' Annotation layer of dynamic spectrograms
#'
#' `ds_annotations()` highlights parts of the sound in a [dynaspec()] object with translucent boxes (e.g. calls, songs or the parts of a duet), optionally with borders and labels, or with horizontal lines above the sounds (e.g. to show which individual sings each part of a duet).
#' @param X Data frame with the selections: columns 'start' and 'end' (in s) and, optionally, 'bottom.freq' and 'top.freq' (in kHz) to restrict boxes to a frequency range. Selection tables from 'warbleR' (or tables from 'ohun' or 'Rraven') can be used directly (see details). Extended selection tables are not supported.
#' @param sound.file Name of the sound file whose selections are shown, when 'X' has a 'sound.files' column with several sound files. Default is `NULL`: when the sound of [dynaspec()] is a file, the selections of that file are shown (matching the file name); otherwise 'X' must contain a single sound file.
#' @param col Color of the boxes, or the name of a column of 'X' with the color of each box. Default is `NULL`: 'col.annotation' of [ds_theme()], or "#E69F00".
#' @param alpha Opacity of the boxes or lines (0-1). Default is 0.3 (1 for lines).
#' @param border Color of the box borders, or the name of a column of 'X' with the border color of each box. Default is `NULL` (no borders).
#' @param lwd Width of the box borders, or of the lines when `type = "line"`. Default is 1 (8 for lines).
#' @param type Character string: "box" (translucent boxes), "spectrogram" (the spectrogram is colored within each selection, with a gradient from the spectrogram background to the color of the selection, as [warbleR::color_spectro()]; only borders and labels are drawn on top) or "line" (a horizontal line from the start to the end of each selection at the frequency set by 'line.freq', drawn on spectrograms only; labels are shown above the lines). Default is "box".
#' @param line.freq Frequency (in kHz) at which lines are drawn when `type = "line"`, or the name of a column of 'X' with the frequency of each line (e.g. a different height for each individual of a duet). Default is `NULL`: near the top of the spectrogram (90% of the frequency range shown).
#' @param lty Line type of the box borders (or of the lines), as in [graphics::par()]: an integer (0-6) or a name ("solid", "dashed", "dotted", "dotdash", "longdash" or "twodash"). Default is 1 (solid).
#' @param labels Name of a column of 'X' with labels to show at the top of each box (or above each line). Default is `NULL` (no labels).
#' @param cex Size of the labels. Default is 1.
#' @param freq Logical to control if the frequency range of selections is used (when 'X' has 'bottom.freq' and 'top.freq' columns). If `FALSE` boxes span the whole frequency range. Default is `TRUE`.
#' @param contour Frequency contours to draw over the spectrogram within each selection. Default is `NULL` (no contours). It can be:
#' - "dominant" or "fundamental": contours measured on the sound with [seewave::dfreq()] or [seewave::fund()], within the frequency range of the selection ('bottom.freq' and 'top.freq' columns, if 'freq' is `TRUE`) or the frequency limits of [dynaspec()].
#' - A list with one element per selection (`NULL` for no contour): a numeric vector of frequencies (in kHz) evenly spaced from the start to the end of the selection, or a data frame or matrix with the time (in s, as in 'start' and 'end') and frequency (in kHz) of each point, in that order or in columns named 'time' and 'freq'.
#' - A data frame or matrix with one row per selection and the frequencies (in kHz) evenly spaced from its start to its end in numeric columns, as the output of [warbleR::freq_ts()] (columns 'sound.files' and 'selec' are ignored).
#'
#' `NA` frequencies leave gaps in the contour.
#' @param contour.col Color of the contours, or the name of a column of 'X' with the color of each contour. Default is `NULL` (the color of the selection, opaque).
#' @param contour.lwd Width of the contours. Default is 2.
#' @param contour.args List with the settings used to measure contours: 'wl.ms' (window length, in ms; takes precedence over 'wl') or 'wl' (in samples); default is the window of [ds_spectrogram()], or 11.61 ms, 'ovlp' (percent overlap; default 90), 'threshold' (amplitude threshold, as a percentage, below which no frequency is measured; default 5) and 'smooth' (number of consecutive values of the running median and mean used to smooth contours; default 5, and 1 for no smoothing).
#' @return A layer to be added to a [dynaspec()] object with `+`. Several annotation layers can be added.
#' @details Tables with a 'sound.files' column (e.g. 'warbleR' selection tables) can contain selections of several sound files; only those of the sound shown are used (see 'sound.file').
#'
#' Contours are only drawn on spectrograms. To show contours without boxes use `alpha = 0`. Dominant frequency contours jump between frequencies when another frequency becomes the loudest one (e.g. fast trills), and fundamental frequency contours are coarse for high-pitched sounds at low sampling rates (they are measured in steps of the sampling period).
#' @seealso [dynaspec()], [ds_captions()]
#' @export
#' @examples
#' data(scythebill)
#'
#' # note types
#' st <- data.frame(start = c(0.34, 1.13, 1.44, 1.75), end = c(1.03, 1.36, 1.68, 2.28),
#'                  labels = c("A", "B", "B", "C"),
#'                  colors = c("#E69F00", "#56B4E9", "#56B4E9", "#009E73"))
#'
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
#'   ds_annotations(st, col = "colors", labels = "labels")
#'
#' # dashed borders of the same color as the boxes
#' x <- x + ds_annotations(st, col = "colors", alpha = 0.1, border = "colors", lwd = 2, lty = "dashed")
#'
#' # spectrogram colored within the selections
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
#'   ds_annotations(st, col = "colors", type = "spectrogram")
#'
#' # lines above the notes (one height per note type)
#' st$line.freq <- c(4.6, 4.9, 4.9, 5.2)
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
#'   ds_annotations(st, col = "colors", type = "line", line.freq = "line.freq", labels = "labels")
#'
#' # frequency contours
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
#'   ds_annotations(st, col = "colors", labels = "labels", contour = "dominant",
#'                  contour.col = "#E69F00")
ds_annotations <- function(X, col = NULL, alpha = 0.3, type = "box", border = NULL, lwd = 1, lty = 1, labels = NULL, cex = 1, freq = TRUE, sound.file = NULL, line.freq = NULL,
                           contour = NULL, contour.col = NULL, contour.lwd = 2, contour.args = list()) {
  check_results <- checkmate::makeAssertCollection()
  if (inherits(X, "extended_selection_table"))
    check_results$push("'X': Extended selection tables are not supported; use a selection table (e.g. warbleR::selection_table()) or a data frame")
  checkmate::assert_data_frame(X, min.rows = 1, add = check_results, .var.name = "X")
  if (is.data.frame(X)) {
    checkmate::assert_names(names(X), must.include = c("start", "end"), add = check_results, .var.name = "X")
    if (!is.null(labels)) checkmate::assert_choice(labels, names(X), add = check_results, .var.name = "labels")
  }
  checkmate::assert_number(alpha, lower = 0, upper = 1, add = check_results, .var.name = "alpha")
  checkmate::assert_choice(type, c("box", "spectrogram", "line"), add = check_results, .var.name = "type")
  if (!is.null(line.freq) && !(is.data.frame(X) && checkmate::test_string(line.freq) && line.freq %in% names(X) && is.numeric(X[[line.freq]])) &&
      !checkmate::test_number(line.freq, lower = 0, finite = TRUE))
    check_results$push("'line.freq': Must be a frequency in kHz or the name of a numeric column of 'X'")
  checkmate::assert_flag(freq, add = check_results, .var.name = "freq")
  is_col <- function(y) all(vapply(y, function(z) !inherits(try(grDevices::col2rgb(z), silent = TRUE), "try-error"), logical(1)))
  if (is.data.frame(X) && !is.null(col) && !(length(col) == 1 && col %in% names(X)) && !is_col(col))
    check_results$push("'col': Must be a valid color or the name of a column of 'X'")
  if (is.data.frame(X) && !is.null(border) && !(length(border) == 1 && border %in% names(X)) && !is_col(border))
    check_results$push("'border': Must be a valid color or the name of a column of 'X'")
  if (is.data.frame(X) && !is.null(contour.col) && !(length(contour.col) == 1 && contour.col %in% names(X)) && !is_col(contour.col))
    check_results$push("'contour.col': Must be a valid color or the name of a column of 'X'")
  checkmate::assert_number(contour.lwd, lower = 0, add = check_results, .var.name = "contour.lwd")
  checkmate::assert_list(contour.args, names = "unique", add = check_results, .var.name = "contour.args")
  if (length(contour.args) > 0 && !all(names(contour.args) %in% c("wl", "wl.ms", "ovlp", "threshold", "smooth")))
    check_results$push("'contour.args': Names must be 'wl', 'wl.ms', 'ovlp', 'threshold' or 'smooth'")
  checkmate::assert_number(lwd, lower = 0, add = check_results, .var.name = "lwd")
  if (!(checkmate::test_choice(lty, 0:6) || checkmate::test_choice(lty, c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))))
    check_results$push("'lty': Must be an integer between 0 and 6 or one of 'blank', 'solid', 'dashed', 'dotted', 'dotdash', 'longdash' or 'twodash'")
  if (!is.null(sound.file)) {
    checkmate::assert_string(sound.file, add = check_results, .var.name = "sound.file")
    if (is.data.frame(X) && !"sound.files" %in% names(X))
      check_results$push("'sound.file': 'X' has no 'sound.files' column")
    else if (is.data.frame(X) && checkmate::test_string(sound.file) && !sound.file %in% X$sound.files)
      check_results$push(sprintf("'sound.file': '%s' is not in the 'sound.files' column of 'X'", sound.file))
  }
  .report_assertions(check_results)
  # plain data frame (e.g. from warbleR selection tables; attributes are not needed)
  X <- data.frame(lapply(as.list(X), function(v) { attributes(v) <- NULL; v }), stringsAsFactors = FALSE)
  # (each selection keeps the number of its contour when selections are filtered or repeated)
  contour <- contour_input_int(contour, X)
  X$.contour <- seq_len(nrow(X))
  if (!is.null(sound.file)) X <- X[X$sound.files == sound.file, , drop = FALSE]
  # (lines are thick and opaque unless 'lwd' and 'alpha' are supplied)
  if (type == "line" && missing(lwd)) lwd <- 8
  if (type == "line" && missing(alpha)) alpha <- 1
  layer_int("annotations", list(X = X, col = col, alpha = alpha, type = type, border = border, lwd = lwd, lty = lty, labels = labels, cex = cex, freq = freq, sound.file = sound.file, line.freq = line.freq,
                                  contour = contour, contour.col = contour.col, contour.lwd = contour.lwd, contour.args = contour.args))
}


#' Caption layer of dynamic spectrograms
#'
#' `ds_captions()` adds text to a [dynaspec()] object at a fixed position of the figure, shown (fading in and out) during a time range of the sound, like subtitles. They are shown in the same way in all styles, in web pages and videos. To label parts of the spectrogram use the 'labels' argument of [ds_annotations()] instead.
#' @param labels Character vector with the captions.
#' @param start,end Numeric vectors with the start and end times (in s) of each caption.
#' @param x,y Position of the center of the captions as proportions of the figure width and height (0-1, from the left and bottom). Defaults are 0.5 and 0.9.
#' @param fading Duration (in s) of the fade-in and fade-out of captions. Default is 0.
#' @param col,cex,font Color, size and font (1 = plain, 2 = bold, 3 = italic, 4 = bold italic) of the captions. Defaults are "black", 1 and 1.
#' @return A layer to be added to a [dynaspec()] object with `+`. Several caption layers can be added (e.g. at different positions).
#' @details Each caption layer shows one caption at a time; when the time ranges of its captions overlap the most visible one is shown.
#' @seealso [dynaspec()], [ds_annotations()]
#' @export
#' @examples
#' data(scythebill)
#'
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
#'   ds_captions(c("long note", "arched notes", "trill"), start = c(0.34, 1.13, 1.75),
#'               end = c(1.03, 1.68, 2.28), fading = 0.2)
ds_captions <- function(labels, start, end, x = 0.5, y = 0.9, fading = 0, col = "black", cex = 1, font = 1) {
  check_results <- checkmate::makeAssertCollection()
  checkmate::assert_character(labels, min.len = 1, any.missing = FALSE, add = check_results, .var.name = "labels")
  checkmate::assert_numeric(start, len = length(labels), lower = 0, any.missing = FALSE, add = check_results, .var.name = "start")
  checkmate::assert_numeric(end, len = length(labels), lower = 0, any.missing = FALSE, add = check_results, .var.name = "end")
  checkmate::assert_number(x, lower = 0, upper = 1, add = check_results, .var.name = "x")
  checkmate::assert_number(y, lower = 0, upper = 1, add = check_results, .var.name = "y")
  checkmate::assert_number(fading, lower = 0, add = check_results, .var.name = "fading")
  checkmate::assert_number(cex, lower = 0.01, add = check_results, .var.name = "cex")
  checkmate::assert_choice(font, 1:4, add = check_results, .var.name = "font")
  if (inherits(try(grDevices::col2rgb(col), silent = TRUE), "try-error"))
    check_results$push("'col': Must be a valid color")
  .report_assertions(check_results)
  layer_int("captions", list(labels = labels, start = start, end = end, x = x, y = y, fading = fading, col = col, cex = cex, font = font))
}


# captions (ds_captions() layers) of web pages: settings read by the player script. 'res' and 'width' are those of the
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
      png_device_int(png_file, width = width, height = height, res = res, bg = "transparent")
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
    cols <- annotation_cols_int(sel)
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


# colors of the boxes of an annotation layer (a color, a column of the selection table, or the default)
annotation_cols_int <- function(sel) {
  X <- sel$X
  col <- if (is.null(sel$col)) "#E69F00" else sel$col
  if (length(col) == 1 && col %in% names(X)) X[[col]] else rep_len(col, nrow(X))
}


# frequency contours of a layer of annotations (in the coordinates of the spectrogram; times are relative to the start of
# each selection, so they follow the selections when these are shifted or repeated)
draw_contours_int <- function(sel, to_x) {
  if (!is.list(sel$contour)) return(invisible(NULL))
  X <- sel$X
  cols <- annotation_cols_int(sel)
  cols <- grDevices::rgb(t(grDevices::col2rgb(cols)), maxColorValue = 255)
  if (!is.null(sel$contour.col))
    cols <- if (length(sel$contour.col) == 1 && sel$contour.col %in% names(X)) X[[sel$contour.col]] else rep_len(sel$contour.col, nrow(X))
  for (i in seq_len(nrow(X))) {
    ct <- sel$contour[[X$.contour[i]]]
    if (!is.null(ct))
      graphics::lines(to_x(X$start[i] + ct$time), ct$freq, col = cols[i], lwd = sel$contour.lwd)
  }
  invisible(NULL)
}


draw_layers_int <- function(layers, to_x = identity, panel = "spectro", ylim = graphics::par("usr")[3:4]) {
  if (is.null(layers))
    return(invisible(NULL))

  for (sel in layers$selections) {
    X <- sel$X

    # lines above the sounds (spectrograms only), with labels above them
    if (identical(sel$type, "line")) {
      if (panel != "spectro") next
      cols <- annotation_cols_int(sel)
      y <- if (is.null(sel$line.freq)) rep(ylim[1] + 0.9 * diff(ylim), nrow(X)) else if (is.character(sel$line.freq)) X[[sel$line.freq]] else rep(sel$line.freq, nrow(X))
      graphics::segments(to_x(X$start), y, to_x(X$end), y, col = grDevices::adjustcolor(cols, sel$alpha), lwd = sel$lwd,
                         lty = if (is.null(sel$lty)) 1 else sel$lty, lend = 1)
      if (!is.null(sel$labels) && sel$labels %in% names(X))
        graphics::text(to_x((X$start + X$end) / 2), y + graphics::strheight("M", cex = sel$cex) * 0.4 + sel$lwd / 96 / graphics::par("pin")[2] * diff(graphics::par("usr")[3:4]) / 2,
                       labels = X[[sel$labels]], adj = c(0.5, 0), col = cols, cex = sel$cex, font = 2, xpd = NA)
      draw_contours_int(sel, to_x)
      next
    }

    has_freq <- panel == "spectro" && sel$freq && all(c("bottom.freq", "top.freq") %in% names(X))
    # boxes are clipped to the frequency range shown (so labels at the top of the boxes are visible)
    bottom <- if (has_freq) pmax(X$bottom.freq, ylim[1]) else rep(ylim[1], nrow(X))
    top <- if (has_freq) pmin(X$top.freq, ylim[2]) else rep(ylim[2], nrow(X))
    cols <- annotation_cols_int(sel)
    borders <- if (is.null(sel$border)) NA else if (length(sel$border) == 1 && sel$border %in% names(X)) X[[sel$border]] else sel$border
    # (type = "spectrogram": the spectrogram is colored instead, see color_annotations_int(); only borders are drawn)
    fill <- if (identical(sel$type, "spectrogram")) NA else grDevices::adjustcolor(cols, sel$alpha)
    graphics::rect(to_x(X$start), bottom, to_x(X$end), top, col = fill,
                   border = borders, lwd = sel$lwd, lty = if (is.null(sel$lty)) 1 else sel$lty)

    # frequency contours (spectrograms only)
    if (panel == "spectro") draw_contours_int(sel, to_x)

    # selection labels at the top of each box
    if (panel %in% c("spectro", "empty") && !is.null(sel$labels) && sel$labels %in% names(X))
      graphics::text(to_x((X$start + X$end) / 2), top - 0.04 * diff(ylim), labels = X[[sel$labels]], adj = c(0.5, 1),
                     col = cols, cex = sel$cex, font = 2)
  }

  invisible(NULL)
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


# contours supplied to ds_annotations() as a list with one element per selection: time (relative to the start of the
# selection) and frequency of each point ("dominant" and "fundamental" are measured when saving, see measure_contours_int())
contour_input_int <- function(contour, X) {
  if (is.null(contour)) return(NULL)
  if (is.character(contour)) {
    if (length(contour) != 1 || !contour %in% c("dominant", "fundamental"))
      stop2("'contour': Must be \"dominant\", \"fundamental\", a list with one element per selection, or a data frame or matrix with one row per selection")
    return(contour)
  }
  n <- nrow(X)
  even <- function(y, i) {
    y <- as.numeric(y)
    data.frame(time = if (length(y) > 1) seq(0, X$end[i] - X$start[i], length.out = length(y)) else (X$end[i] - X$start[i]) / 2, freq = y)
  }
  # a row per selection (e.g. warbleR::freq_ts())
  if (is.data.frame(contour) || is.matrix(contour)) {
    m <- as.data.frame(contour)
    m <- m[setdiff(names(m), c("sound.files", "selec"))]
    if (nrow(m) != n || ncol(m) == 0 || !all(vapply(m, is.numeric, logical(1))))
      stop2("'contour': A data frame or matrix must have one row per selection and numeric columns with the frequencies")
    return(lapply(seq_len(n), function(i) even(unlist(m[i, ]), i)))
  }
  if (!is.list(contour) || length(contour) != n)
    stop2("'contour': Must be \"dominant\", \"fundamental\", a list with one element per selection, or a data frame or matrix with one row per selection")
  lapply(seq_len(n), function(i) {
    y <- contour[[i]]
    if (is.null(y)) return(NULL)
    if (is.numeric(y) && is.null(dim(y))) return(even(y, i))
    if (!(is.data.frame(y) || is.matrix(y)) || ncol(y) < 2)
      stop2("'contour': Each element must be NULL, a numeric vector of frequencies, or a data frame or matrix with times and frequencies")
    y <- as.data.frame(y)
    pts <- if (all(c("time", "freq") %in% names(y))) y[c("time", "freq")] else stats::setNames(y[1:2], c("time", "freq"))
    if (!all(vapply(pts, is.numeric, logical(1))))
      stop2("'contour': Times and frequencies must be numeric")
    pts$time <- pts$time - X$start[i]
    pts
  })
}


# measure the contours of a layer of annotations on the sound ("dominant" and "fundamental")
measure_contours_int <- function(sel, x) {
  if (!is.character(sel$contour)) return(sel)
  w <- tuneR::mono(read_sound_int(x$wave)$wave, which = "left")
  f <- w@samp.rate
  X <- sel$X
  has_freq <- isTRUE(sel$freq) && all(c("bottom.freq", "top.freq") %in% names(X))
  res <- vector("list", max(X$.contour, 0))
  for (i in seq_len(nrow(X))) {
    band <- if (has_freq) c(X$bottom.freq[i], X$top.freq[i]) else if (!is.null(x$flim)) x$flim else c(0, f / 2000)
    # (window: 'wl.ms' or 'wl' of contour.args, or else of ds_spectrogram(), or 11.61 ms)
    ca <- sel$contour.args
    wl <- if (!is.null(ca$wl.ms) || !is.null(ca[["wl"]])) resolve_wl_int(f, ca[["wl"]], ca$wl.ms) else resolve_wl_int(f, x$spectrogram[["wl"]], x$spectrogram$wl.ms)
    ca$wl.ms <- NULL
    res[[X$.contour[i]]] <- measure_contour_int(w, X$start[i], X$end[i], band, sel$contour, utils::modifyList(ca, list(wl = wl)))
  }
  sel$contour <- res
  sel
}


# contour of a selection: time from the start of the selection (at the center of each window) and frequency
measure_contour_int <- function(w, start, end, band, type, args) {
  f <- w@samp.rate
  from <- max(start, 0)
  to <- min(end, seewave::duration(w))
  if (to <= from) return(NULL)
  seg <- w[(round(from * f) + 1):max(round(from * f) + 1, round(to * f))]
  n <- length(seg@left)
  wl <- if (is.null(args[["wl"]])) wl_from_ms_int(f, wl_ms_default) else args[["wl"]]
  # (windows must fit in the selection, as in seewave)
  wl <- min(wl, 2 * floor((n - 1) / 2))
  if (wl < 8) return(NULL)
  ovlp <- if (is.null(args$ovlp)) 90 else args$ovlp
  threshold <- if (is.null(args$threshold)) 5 else args$threshold
  smooth <- if (is.null(args$smooth)) 5 else args$smooth
  band <- c(band[1], min(band[2], (f / 2 - f / wl) / 1000))
  m <- if (type == "dominant")
    seewave::dfreq(seg, f = f, wl = wl, ovlp = ovlp, threshold = threshold, bandpass = band * 1000, plot = FALSE)
  else
    seewave::fund(seg, f = f, wl = wl, ovlp = ovlp, threshold = threshold, fmax = band[2] * 1000, plot = FALSE)
  y <- as.numeric(m[, 2])
  y[y < band[1] | y > band[2] | y == 0] <- NA
  # (seewave spreads the windows evenly from 0 to the duration of the sound, but the center of the first window is at
  # wl / 2 and windows are 'wl * (1 - ovlp / 100)' samples apart)
  data.frame(time = from - start + (wl / 2 + (seq_along(y) - 1) * wl * (1 - ovlp / 100)) / f, freq = smooth_contour_int(y, smooth))
}


# smooth the consecutive frequencies of a contour (gaps are kept): a running median removes isolated jumps, also at the
# ends, and a running mean removes the steps of the frequency bins
smooth_contour_int <- function(y, k) {
  k <- 2 * floor(k / 2) + 1
  if (k < 3) return(y)
  runs <- rle(is.na(y))
  ends <- cumsum(runs$lengths)
  h <- (k - 1) / 2
  for (i in which(!runs$values & runs$lengths >= k)) {
    at <- (ends[i] - runs$lengths[i] + 1):ends[i]
    v <- as.numeric(stats::runmed(y[at], k, endrule = "constant"))
    y[at] <- as.numeric(stats::filter(c(rep(v[1], h), v, rep(v[length(v)], h)), rep(1 / k, k), sides = 2))[h + seq_along(at)]
  }
  y
}


# keep the selections of the sound shown (tables with a 'sound.files' column, e.g. warbleR selection tables)
sound_file_selections_int <- function(sel, x) {
  X <- sel$X
  if (!"sound.files" %in% names(X) || !is.null(sel$sound.file) || length(unique(X$sound.files)) == 1)
    return(sel)
  if (!is.character(x$wave))
    stop2("the selection table has selections of several sound files; choose one with 'sound.file' in ds_annotations()")
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
