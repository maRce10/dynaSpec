# styles (static, scrolling and paged spectrograms) and how each one is saved

#' Styles of dynamic spectrograms
#'
#' Set the style of a [dynaspec()] object:
#' - `ds_scrolling()`: the spectrogram scrolls from right to left past the play line, which stays at the center (it only moves at the start and the end, while the spectrogram is still, so the window shown is always full of sound; see 'still.ends').
#' - `ds_paged()`: the spectrogram is split into pages, and a translucent box reveals the part of each page being played.
#' - `ds_static()`: a play line moves over the whole spectrogram (and oscillogram), which can be split in several rows (default style).
#'
#' Only the arguments supplied are used; other settings take the defaults shown.
#' @param t.display Time (in s) shown at once: the time window of scrolling spectrograms (default 1.5) or the duration of each page of paged spectrograms (default: the whole sound, up to 5 s).
#' @param still.ends Logical: keep the window shown within the sound in scrolling spectrograms? With `TRUE` the spectrogram is still at the start, while the play line moves from the left edge to the center, then scrolls with the line at the center, and is still again at the end, while the line moves to the right edge (so the window never shows the empty room before the start or after the end of the sound). With `FALSE` the line stays at the center and the sound enters from the center and leaves at the center. Looping spectrograms (`loop > 1`) always scroll continuously. Default is `TRUE`.
#' @param fix.time Logical: does the time axis stay fixed (`TRUE`, default) or move along with the scrolling spectrogram?
#' @param buffer Time (in s) before the scrolling spectrogram starts moving. Default is 0.
#' @param loop Number of times the sound is repeated in scrolling videos (web pages loop until paused when > 1). Default is 1.
#' @param speed Playback speed of videos of scrolling spectrograms (e.g. 0.5 for half speed). Web pages start at the first speed of [ds_player()] ('speeds'), as in all styles. Default is 1.
#' @param width,height,res Size (in pixels) and resolution of the figures: frames of scrolling spectrograms (defaults 700, 400 and 70), pages of paged spectrograms (defaults 1350, 450 and 150; web pages show them at a higher resolution), and each row of static spectrograms (defaults 1000, 500 and 100).
#' @param fps Frames per second of videos. Defaults are 50 (scrolling) and 30 (paged and static).
#' @param parallel Number of cores used to render videos of scrolling and paged spectrograms. Default is 1.
#' @param ... Other arguments of the style:
#' - `ds_static()`: none.
#' - `ds_scrolling()`: 'ggspectro' (use [seewave::ggspectro()]; default `FALSE`), 'derivative' (spectral derivatives; requires 'imager'; default `FALSE`) and 'spectro.call' (a call to a spectrogram function replacing the default one, made with [base::call()]).
#' - `ds_paged()`: 'resample.rate' (sampling rate in Hz used for the spectrogram; default 15000), 'amp.thresh' (amplitude threshold, as a percentage; default 0), 'dest.path' (folder for temporary files) and 'delete.temp' (delete temporary files; default `TRUE`).
#' @param title Title of paged spectrograms.
#' @param highlight.alpha Opacity of the highlight box of paged spectrograms (default 0.6).
#' @param legend Logical: show the amplitude color scale of paged spectrograms? Default is `FALSE`.
#' @param rows Number of rows in which static spectrograms are split (each row shows a consecutive segment of the same duration). Default is 1.
#' @param alt Alternative text of static spectrograms in web pages (for screen readers).
#' @param play.line Logical: show the line moving along the sound in static spectrograms? With `FALSE` web pages show the figure without the moving line (clicking on it still plays the sound from that point) and videos show the figure with the sound. Default is `TRUE`.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details Only the arguments supplied are used: the defaults shown are those of each style, which apply when an argument is not supplied (adding the style again keeps the arguments supplied before and changes those supplied again).
#'
#' The play line (its color, type and width) and the color of the highlight box of paged spectrograms are set with [ds_theme()] ('col.line', 'lty.line', 'lwd.line' and 'col.highlight'), for all styles.
#' @seealso [dynaspec()], [save_dynaspec()], [ds_theme()]
#' @name styles
#' @examples
#' data(scythebill)
#'
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1))
#'
#' \dontrun{
#' data(antshrike)
#' y <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1))
#' y + ds_scrolling(t.display = 2, axis.type = "minimal")
#' x + ds_paged(t.display = 1, axis.type = "standard")
#' x + ds_oscillogram() + ds_static(rows = 2, height = 350)
#' x + ds_theme(col.line = "red", lty.line = "dashed", lwd.line = 3)
#' }
NULL


# arguments of styles that were removed or renamed, and what replaces them
replaced_style_args_int <- function(args) {
  replaced <- c(lcol = "ds_theme(col.line)", lty = "ds_theme(lty.line)", lwd = "ds_theme(lwd.line)", highlight.col = "ds_theme(col.highlight)",
                filter = "ds_filter(bp) (a bandpass filter, for all styles)", height.prop = "the 'height' of ds_oscillogram(), ds_envelope() or ds_overview()",
                hop.size = "ds_spectrogram(wl.ms)", spec.width = "'width', 'height' (in pixels) and 'res'", spec.height = "'width', 'height' (in pixels) and 'res'",
                plot.legend = "'legend'")
  old <- intersect(names(args), names(replaced))
  if (length(old) > 0)
    stop2(paste0("'", old, "' is no longer used: use ", replaced[old], collapse = "; "))
  args
}

#' @rdname styles
#' @export
ds_scrolling <- function(t.display = 1.5, still.ends = TRUE, fix.time = TRUE, buffer = 0, loop = 1, speed = 1, width = 700, height = 400, res = 70,
                         fps = 50, parallel = 1, ...) {
  style_int("scrolling", replaced_style_args_int(provided_args_int(match.call(), parent.frame())))
}


#' @rdname styles
#' @export
ds_paged <- function(t.display = NULL, title = NULL, highlight.alpha = 0.6, legend = FALSE, width = 1350, height = 450, res = 150, fps = 30,
                     parallel = 1, ...) {
  style_int("paged", replaced_style_args_int(provided_args_int(match.call(), parent.frame())))
}


#' @rdname styles
#' @export
ds_static <- function(rows = 1, width = 1000, height = 500, res = 100, alt = NULL, fps = 30, play.line = TRUE, ...) {
  args <- replaced_style_args_int(provided_args_int(match.call(), parent.frame()))
  other <- setdiff(names(args), c("rows", "width", "height", "res", "alt", "fps", "play.line"))
  if (length(other) > 0)
    stop2("unused argument(s) in ds_static(): ", paste(other, collapse = ", "))
  style_int("static", args)
}


# spectrogram arguments for seewave-based engines (static and scrolling): 'amp.trans' only applies to paged ones
spectro_args_int <- function(sp) {
  # (the engines call the spectrogram background 'colbg', as seewave)
  if (!is.null(sp$col.bg)) { sp$colbg <- sp$col.bg; sp$col.bg <- NULL }
  # ('wl.ms' takes precedence over 'wl')
  if (!is.null(sp$wl.ms)) sp[["wl"]] <- NULL
  if (!is.null(sp$amp.trans))
    warning2("'amp.trans' is only used by paged spectrograms")
  sp$amp.trans <- NULL
  sp
}


# color of the lowest amplitude of a palette (default background of spectrograms)
low_col_int <- function(pal, n = 30) pal_fun_int(pal)(max(2, n))[1]


# color palette as a function (palettes can also be the name of a 'viridis' palette or two or more colors for a gradient)
pal_fun_int <- function(pal) {
  if (is.function(pal) || is.null(pal)) return(pal)
  function(n) paged_palette_int(pal, n)
}


# static spectrograms
save_static_int <- function(x, wave, layers, out, ext, play, audio = NULL) {
  sp <- spectro_args_int(x$spectrogram)
  if (!is.null(sp$pal)) sp$pal <- pal_fun_int(sp$pal)
  panels <- side_panels_int(x)
  args <- c(
    list(wave = wave, file.name = out, flim = x$flim, play = play, layers = layers,
         spectro = !is.null(x$spectrogram), osc = length(panels) > 0, panels = panels,
         spectrum = if (!is.null(x$spectrum)) utils::modifyList(x$spectrum, list(col = panel_col_int(x$spectrum, "spectrum"))),
         theme = minimal_axis_int(x$theme, "static"), audio = audio),
    if (!is.null(x$theme$cex.lab)) list(cex.lab = 1.2 * x$theme$cex.lab),
    sp, x$style$args, x$player, list(download.name = x$name)
  )
  if (!is.null(x$overview))
    warning2("ds_overview() is only shown in scrolling spectrograms")
  if ("zoom" %in% x$player$buttons)
    warning2("the zoom slider is only available in scrolling spectrograms")
  do.call(static_spectro_int, args, quote = TRUE)
}


# oscillogram and envelope panels of static spectrograms (in the order they were added)
side_panels_int <- function(x) {
  lapply(x$panels, function(p) list(type = p$type, position = p$position, col = panel_col_int(p), height = if (is.null(p$height)) 0.5 else p$height))
}


# color of oscillogram, envelope and spectrum panels (supplied, from the theme or the default)
panel_col_int <- function(p, type = p$type) {
  if (!is.null(p$col)) p$col else "black"
}


# scrolling spectrograms
save_scrolling_int <- function(x, wave, layers, out, play, verbose) {
  if (is.null(x$spectrogram))
    stop2("scrolling spectrograms need a spectrogram: add ds_spectrogram() (e.g. dynaspec(wave) + ds_spectrogram() + ds_scrolling())")
  sp <- spectro_args_int(x$spectrogram)
  if (!is.null(sp$pal)) sp$pal <- pal_fun_int(sp$pal)
  st <- x$style$args
  # (window length in milliseconds, 'wl.ms', which takes precedence over 'wl': the engine's 'hop.size', used when 'wl' is
  # not supplied, 11.61 ms by default)
  if (!is.null(sp$wl.ms)) { st$hop.size <- sp$wl.ms; sp[["wl"]] <- NULL }
  sp$wl.ms <- NULL

  # one lower panel: oscillogram, amplitude envelope or overview (lower spectrogram of the whole sound)
  lower <- c(vapply(x$panels, `[[`, character(1), "type"), if (!is.null(x$overview)) "overview")
  if (length(lower) > 1)
    warning2("scrolling spectrograms show a single lower panel; only the first one added is shown (", lower[1], ")")
  if (!is.null(x$spectrum))
    warning2("ds_spectrum() is only shown in static spectrograms")
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
save_paged_int <- function(x, wave, layers, out, ext, play, verbose, audio = NULL) {
  if (is.null(x$spectrogram))
    stop2("paged spectrograms need a spectrogram: add ds_spectrogram() (e.g. dynaspec(wave) + ds_spectrogram() + ds_paged())")
  unused <- c(if (length(x$panels) > 0) "ds_oscillogram() and ds_envelope()", if (!is.null(x$spectrum)) "ds_spectrum()", if (!is.null(x$overview)) "ds_overview()")
  if (length(unused) > 0)
    warning2(paste(unused, collapse = ", "), " are not shown in paged spectrograms")
  if ("zoom" %in% x$player$buttons)
    warning2("the zoom slider is only available in scrolling spectrograms")
  st <- x$style$args
  # (size of the pages in pixels and resolution: pages are 'width' / 'res' x 'height' / 'res' inches, and videos and
  # images are drawn at 'res')
  page_res <- if (is.null(st$res)) 150 else st$res
  page_in <- c(if (is.null(st$width)) 1350 else st$width, if (is.null(st$height)) 450 else st$height) / page_res
  st$width <- st$height <- st$res <- NULL
  st$spec.width <- page_in[1]
  st$spec.height <- page_in[2]
  st$video.res <- page_res
  if (!is.null(st$legend)) { st$plot.legend <- st$legend; st$legend <- NULL }
  engine_names <- c("fps", "highlight.col", "highlight.alpha", "lcol", "delete.temp", "parallel", "video.res")
  engine_args <- c(st[intersect(names(st), engine_names)], x$player, list(download.name = x$name, audio = audio))
  prep_args <- st[setdiff(names(st), engine_names)]

  # spectrogram settings ('collevels' sets the amplitude range and number of color bands)
  sp <- x$spectrogram
  if (!is.null(sp$wl.ms)) sp[["wl"]] <- NULL
  if (!is.null(sp$col.bg)) { sp$colbg <- sp$col.bg; sp$col.bg <- NULL }
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
  # (pages without time axis title are shorter, so their spectrogram is as high as with the title)
  if (isFALSE(th$time.title) && theme_int(th, "axis.type", "standard") == "standard") {
    h <- if (is.null(prep_args$spec.height)) 3 else prep_args$spec.height
    prep_args$spec.height <- h - (paged_bottom_int(th, TRUE) - paged_bottom_int(th, FALSE)) * png_line_in_int(100, th)
  }
  if (!is.null(th$axis.type)) prep_args$axis.type <- th$axis.type

  # pages as png files
  if (ext == "png") {
    pp <- do.call(prep_paged_int, c(list(wave = wave, dest.path = dirname(out), plot = FALSE, verbose = verbose), prep_args))
    files <- if (pp$n_pages == 1) out else paste0(tools::file_path_sans_ext(out), "_", seq_len(pp$n_pages), ".png")
    for (i in seq_len(pp$n_pages)) page_png_int(pp, i, files[i], res = page_res)
    if (play) for (f in files) open_file_int(normalizePath(f))
    return(invisible(files))
  }

  do.call(paged_spectro_int, c(list(wave = wave, file.name = out, play = play, verbose = verbose), engine_args, prep_args), quote = TRUE)
}
