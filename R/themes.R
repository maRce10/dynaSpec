# themes: ds_theme(), preset themes, the theme of the session and theme helpers

#' Theme of dynamic spectrograms
#'
#' `ds_theme()` sets the look of a [dynaspec()] object (axes, text, panels, colors and player controls), in the same way for all styles. Adding it again changes the settings supplied (other settings are kept). Preset themes (e.g. `ds_theme_dark()`) set several settings at once.
#' @param xlab,ylab Titles of the time and frequency axes. Defaults are "Time (s)" and "Frequency (kHz)".
#' @param cex.lab,cex.axis Size of axis titles and axis labels, relative to the style default (e.g. 1.5 is 50% larger). Defaults are 1 (1.2 for axis titles of static spectrograms).
#' @param font.lab Font of axis titles: 1 (plain), 2 (bold), 3 (italic) or 4 (bold italic). Defaults are 1 (static and scrolling) and 2 (paged).
#' @param col.lab Color of axes, axis labels and titles. Default is black (paged spectrograms: picked to contrast with the background).
#' @param col.axis Color of the axis labels (tick labels), when they should differ from the axis titles and lines. Default is 'col.lab'.
#' @param axis.type Character string: "standard" (axes with labels and titles), "inside" (no axis titles; frequency labels inside the spectrogram, e.g. "8k", and time labels below the lowest panel, e.g. "2s"), "compact" (no axis titles; frequency labels below short ticks in a narrow column on the left, with the unit at the bottom, and time labels in minutes and seconds, e.g. "0:02", above the figure), "minimal" (1 s and 1 kHz scale bars; scrolling spectrograms only) or "none" (no axes). Defaults are "standard" (static and scrolling) and "none" (paged).
#' @param bg Background color of the figure around the panels ("transparent" for a transparent background). Default is white. The backgrounds of the panels are set separately: 'col.bg' (spectrograms, see [ds_spectrogram()]) and 'col.panel' (other panels).
#' @param family Font family of all text, including captions and annotation labels: "sans", "serif", "mono" or the name of a font installed in the system. Default is "sans".
#' @param las Orientation of the axis labels (tick labels), as in [graphics::par()]: 0 (parallel to the axis), 1 (horizontal), 2 (perpendicular to the axis) or 3 (vertical). Applies to both axes (e.g. 2 makes time labels vertical and frequency labels horizontal) with standard axes ('axis.type'). Defaults are 1 (static and paged: horizontal labels) and 0 (scrolling: vertical frequency labels).
#' @param tick.length Length of axis ticks, in lines of text (0 hides ticks). Defaults are 0.5 (static and scrolling) and 0.3 (paged).
#' @param box Logical argument to control whether boxes are drawn around panels. Default is `TRUE`.
#' @param col.box Color of the boxes around panels. Default is 'col.lab'.
#' @param grid Logical argument to control whether horizontal grid lines are drawn at the frequency axis ticks of spectrograms. Default is `FALSE`.
#' @param col.grid Color of grid lines. Default is a translucent gray.
#' @param col.panel Background color of oscillogram, envelope and power spectrum panels. Default is 'bg' (the figure background shows through).
#' @param col.title,cex.title Color and size of titles (the title of paged spectrograms and the 'Amp.' label of power spectra). Defaults are 'col.lab' and 1.
#' @param pal,col.bg Color palette and background of spectrograms (see [ds_spectrogram()]). Used when not supplied in [ds_spectrogram()].
#' @param col.wave Color of oscillograms, envelopes and power spectra. Used when not supplied in [ds_oscillogram()], [ds_envelope()] or [ds_spectrum()].
#' @param col.line Color of the play line (and of the cursor of paged spectrograms and the active player buttons). Defaults are "#07889B99" (scrolling), "white" (paged) and `viridis::mako(10)[9]` (static).
#' @param lty.line,lwd.line Type and width of the play line of scrolling and static spectrograms, as in [graphics::par()]: 'lty.line' is an integer (0-6) or a name ("solid", "dashed", "dotted", "dotdash", "longdash", "twodash" or "blank"), and 'lwd.line' a width in which 1 is 1/96 inch (scaled with the resolution). Defaults are 2 and 2 (scrolling), and a solid line 2 pixels wide (static).
#' @param col.highlight Color of the highlight box of paged spectrograms (default "#4B0C6BFF") and of the translucent boxes dimming the parts of the [ds_overview()] outside the window shown in scrolling spectrograms. Default is gray in overviews.
#' @param col.annotation Color of the boxes of [ds_annotations()]. Used when 'col' is not supplied in [ds_annotations()].
#' @param col.bar,bg.bar Color of the text and background of the player controls of web pages (play button, sliders, ...). Defaults are dark gray and transparent (the page background).
#' @param bar.style Style of the player controls of web pages: "standard" (rectangular buttons), "rounded" (round play button, rounded controls, a seek bar and a frame with rounded corners around the figure; 'bg.bar' is used as the background of the whole player) or "square" (flat bar with a square play button and sliders in the color of the play line). With "rounded" [ds_player()] shows the seek bar and the time by default (`buttons = c("progress", "time")`), and with "square" the volume control and the time (`buttons = c("volume", "time")`). Default is "standard".
#' @param ... Settings of `ds_theme()` that replace those of the preset theme.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details Only the settings supplied are used: settings left as `NULL` take the defaults of each style (given for each setting above). Settings supplied in other layers (e.g. the palette in [ds_spectrogram()]) take precedence over those of the theme.
#'
#' Preset themes are complete themes: adding one replaces previous themes (use `ds_theme()` afterwards to change some of its settings, or supply them in the preset):
#' \itemize{
#'   \item `ds_theme_dark()`: black figure background, white text, 'mako' palette (the spectrogram background is its darkest color, as in all presets).
#'   \item `ds_theme_mako()`: light theme with the 'mako' palette (dark blue background and light sounds) and 'mako' colors for panels, annotations, play line and player controls.
#'   \item `ds_theme_presentation()`: large bold text for slides and projectors.
#'   \item `ds_theme_paper()`: black and white with serif fonts, for printed figures.
#'   \item `ds_theme_minimal()`: no boxes, light gray axes and short ticks.
#'   \item `ds_theme_transparent()`: transparent background, to blend in slides or web pages of any color.
#'   \item `ds_theme_void()`: no axes, titles or boxes (only the sound).
#'   \item `ds_theme_macaulay()`: inspired by the player of the 'Macaulay Library': gray spectrogram, compact axes (time in minutes and seconds above the figure) and a black player bar with a square play button. Best with [ds_scrolling()] and an [ds_overview()] (e.g. `ds_scrolling(t.display = 1.5, fix.time = FALSE)`).
#'   \item `ds_theme_xenocanto()`: inspired by the player of the 'Xeno-canto' web site: gray spectrogram, labels inside the figure, solid play line and rounded player controls with a seek bar. Best with [ds_scrolling()] and an [ds_oscillogram()] (e.g. `ds_scrolling(t.display = 1.5, fix.time = FALSE)`).
#' }
#' @seealso [ds_set_theme()] to set a theme for all spectrograms, [dynaspec()], [ds_spectrogram()]
#' @export
#' @examples
#' data(scythebill)
#'
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) +
#'   ds_theme(xlab = "Time (s)", ylab = "Frequency (kHz)", cex.lab = 1.4, font.lab = 2)
#'
#' # preset theme with larger axis titles
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) + ds_oscillogram() +
#'   ds_theme_dark(cex.lab = 1.4)
#'
#' # Macaulay Library look
#' data(antshrike)
#' x <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1)) +
#'   ds_overview() + ds_theme_macaulay() + ds_scrolling(t.display = 3, fix.time = FALSE)
#'
#' # Xeno-canto look (and its color version)
#' x <- dynaspec(antshrike, flim = c(0.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 23, collevels = seq(-50, 0, 1)) + ds_oscillogram(height = 0.15) +
#'   ds_theme_xenocanto() + ds_scrolling(t.display = 3, fix.time = FALSE)
#' x <- x + ds_theme_xenocanto(pal = viridis::inferno, col.bg = "black", bg = "black",
#'   col.panel = "black", col.wave = "white", col.lab = "white", col.line = "white")
ds_theme <- function(xlab = NULL, ylab = NULL, cex.lab = NULL, cex.axis = NULL, font.lab = NULL, col.lab = NULL, col.axis = NULL, axis.type = NULL,
                     bg = NULL, family = NULL, las = NULL, tick.length = NULL, box = NULL, col.box = NULL, grid = NULL, col.grid = NULL, col.panel = NULL,
                     col.title = NULL, cex.title = NULL, pal = NULL, col.bg = NULL, col.wave = NULL, col.line = NULL, lty.line = NULL, lwd.line = NULL,
                     col.highlight = NULL, col.annotation = NULL, col.bar = NULL, bg.bar = NULL, bar.style = NULL) {
  args <- provided_args_int(match.call(), parent.frame())
  layer_int("theme", check_theme_int(args))
}


#' @rdname ds_theme
#' @export
ds_theme_dark <- function(...) theme_preset_int(list(
  bg = "black", col.lab = "white", col.box = "gray60", col.panel = "black", pal = viridis::mako,
  col.wave = "#A0DFB9", col.line = "white", col.highlight = "#3E356B", col.grid = "#FFFFFF40", col.bar = "white", bg.bar = "black"
), ...)


#' @rdname ds_theme
#' @export
ds_theme_presentation <- function(...) theme_preset_int(list(
  cex.lab = 1.6, cex.axis = 1.4, font.lab = 2, cex.title = 1.5, tick.length = 0.6
), ...)


#' @rdname ds_theme
#' @export
ds_theme_paper <- function(...) theme_preset_int(list(
  family = "serif", col.lab = "black", pal = seewave::reverse.gray.colors.1, col.wave = "black",
  col.panel = "white", bg = "white", col.line = "black", las = 1, tick.length = 0.3, cex.axis = 0.9
), ...)


#' @rdname ds_theme
#' @export
ds_theme_minimal <- function(...) theme_preset_int(list(
  box = FALSE, col.lab = "gray30", tick.length = 0.25, las = 1, grid = TRUE, col.grid = "#00000014"
), ...)


#' @rdname ds_theme
#' @export
ds_theme_transparent <- function(...) theme_preset_int(list(
  bg = "transparent", col.panel = "transparent", bg.bar = "transparent"
), ...)


#' @rdname ds_theme
#' @export
ds_theme_void <- function(...) theme_preset_int(list(
  axis.type = "none", box = FALSE
), ...)


#' @rdname ds_theme
#' @export
ds_theme_macaulay <- function(...) theme_preset_int(list(
  axis.type = "compact", pal = c("#EBEBEB", "#111111"), bg = "white", col.panel = "#EBEBEB",
  col.wave = "#333333", col.lab = "#555555", col.box = "#9E9E9E", cex.axis = 0.96, cex.lab = 1.2, col.line = "#3D7299", lty.line = 1,
  lwd.line = 2, col.highlight = "#333333", bar.style = "square", col.bar = "white", bg.bar = "black"
), ...)


#' @rdname ds_theme
#' @export
ds_theme_xenocanto <- function(...) theme_preset_int(list(
  axis.type = "inside", box = FALSE, pal = seewave::reverse.gray.colors.1, bg = "white", col.panel = "white",
  col.wave = "black", col.lab = "#333333", cex.axis = 0.85, tick.length = 0.3, col.line = "#333333", lty.line = 1, lwd.line = 2,
  col.highlight = "#F0F0F0", bar.style = "rounded", col.bar = "#333333", bg.bar = "#F0F0F0"
), ...)


#' @rdname ds_theme
#' @export
ds_theme_mako <- function(...) theme_preset_int(list(
  pal = viridis::mako, col.lab = "#28192F", col.box = "#3B2F5E", col.panel = "white", bg = "white",
  col.wave = "#40498E", col.line = "#38AAAC", col.highlight = "#28192F", col.annotation = "#366A9F", col.grid = "#3B2F5E33",
  col.bar = "#28192F", bg.bar = "#DEF5E5"
), ...)


#' Set the theme of all dynamic spectrograms
#'
#' `ds_set_theme()` sets a theme used by all [dynaspec()] objects created or shown afterwards in the R session (e.g. in all the code chunks of an R Markdown or Quarto document), as [ggplot2::theme_set()]. `ds_get_theme()` returns the current theme.
#' @param theme A theme: [ds_theme()] or a preset theme (e.g. [ds_theme_dark()]). `NULL` removes the theme (the defaults of each style are used).
#' @return `ds_set_theme()` returns the previous theme invisibly (so it can be restored); `ds_get_theme()` returns the current theme (`NULL` if none was set).
#' @details Themes added to a [dynaspec()] object with `+` take precedence: [ds_theme()] changes some settings of the global theme, while preset themes (e.g. `+ ds_theme_paper()`) are complete themes that replace it.
#' @seealso [ds_theme()]
#' @export
#' @examples
#' data(scythebill)
#'
#' old <- ds_set_theme(ds_theme_dark(cex.lab = 1.3))
#'
#' # uses the dark theme
#' x <- dynaspec(scythebill, flim = c(1.5, 5.5)) +
#'   ds_spectrogram(wl.ms = 18, collevels = seq(-50, 0, 1)) + ds_oscillogram()
#'
#' # dark theme with other axis titles
#' x <- x + ds_theme(xlab = "Time of the song (s)")
#'
#' # restore the previous theme
#' ds_set_theme(old)
ds_set_theme <- function(theme = NULL) {
  if (!is.null(theme) && !(inherits(theme, "dynaspec_layer") && identical(theme$type, "theme")))
    stop2("'theme' must be a theme: ds_theme() or a preset theme (e.g. ds_theme_dark()), or NULL")
  old <- .dynaspec_env$theme
  .dynaspec_env$theme <- theme
  invisible(old)
}


#' @rdname ds_set_theme
#' @export
ds_get_theme <- function() .dynaspec_env$theme


# theme setting (supplied in ds_theme()) or its default
theme_int <- function(theme, name, default) if (is.null(theme[[name]])) default else theme[[name]]


# color of the axis labels (tick labels): 'col.axis' of the theme, or the color of the axes
axis_col_int <- function(theme, default = "black") theme_int(theme, "col.axis", theme_int(theme, "col.lab", default))


# check the settings of ds_theme() (only those supplied)
check_theme_int <- function(args) {
  check_results <- .check_arguments("dynatheme", args[intersect(names(args), c("cex.lab", "axis.type", "bg"))])
  for (i in intersect(names(args), c("xlab", "ylab", "family")))
    checkmate::assert_string(args[[i]], add = check_results, .var.name = i)
  for (i in intersect(names(args), c("cex.axis", "cex.title")))
    checkmate::assert_number(args[[i]], lower = 0.01, add = check_results, .var.name = i)
  if (!is.null(args$tick.length)) checkmate::assert_number(args$tick.length, lower = 0, add = check_results, .var.name = "tick.length")
  if (!is.null(args$font.lab)) checkmate::assert_choice(args$font.lab, 1:4, add = check_results, .var.name = "font.lab")
  if (!is.null(args$las)) checkmate::assert_choice(args$las, 0:3, add = check_results, .var.name = "las")
  if (!is.null(args$lwd.line)) checkmate::assert_number(args$lwd.line, lower = 0.01, add = check_results, .var.name = "lwd.line")
  if (!is.null(args$lty.line)) {
    lty_names <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    if (length(args$lty.line) != 1 || !(args$lty.line %in% c(0:6, lty_names)))
      check_results$push(paste0("'lty.line': Must be an integer between 0 and 6 or one of {'", paste(lty_names, collapse = "','"), "'}"))
  }
  if (!is.null(args$bar.style)) checkmate::assert_choice(args$bar.style, c("standard", "rounded", "square"), add = check_results, .var.name = "bar.style")
  for (i in intersect(names(args), c("box", "grid")))
    checkmate::assert_flag(args[[i]], add = check_results, .var.name = i)
  for (i in intersect(names(args), c("col.lab", "col.axis", "bg", "col.box", "col.grid", "col.panel", "col.title", "col.bg", "col.wave", "col.annotation", "col.line", "col.highlight", "col.bar", "bg.bar")))
    if (!checkmate::test_string(args[[i]]) || inherits(try(grDevices::col2rgb(args[[i]]), silent = TRUE), "try-error"))
      check_results$push(paste0("'", i, "': Must be a valid color"))
  if (!is.null(args$pal) && !is.function(args$pal) && !checkmate::test_character(args$pal, min.len = 1, any.missing = FALSE))
    check_results$push("'pal': Must be a color palette function, the name of a 'viridis' palette or two or more colors")
  else if (is.character(args$pal) && length(args$pal) == 1 && !args$pal %in% c("viridis", "magma", "plasma", "inferno", "cividis", "rocket", "mako", "turbo"))
    check_results$push("'pal': Must be a color palette function, the name of a 'viridis' palette or two or more colors")
  else if (is.character(args$pal) && length(args$pal) > 1 && inherits(try(grDevices::col2rgb(args$pal), silent = TRUE), "try-error"))
    check_results$push("'pal': Must be a color palette function, the name of a 'viridis' palette or two or more colors")
  .report_assertions(check_results)
  args
}


# settings of the package during the R session (theme set with ds_set_theme())
.dynaspec_env <- new.env(parent = emptyenv())


# theme of a dynaspec object on top of the theme set with ds_set_theme() (unless a preset theme, which is
# complete, was added to the object)
global_theme_int <- function(x) {
  g <- .dynaspec_env$theme
  if (is.null(g) || isTRUE(x$theme_complete))
    return(x)
  x$theme <- utils::modifyList(g$args, if (is.null(x$theme)) list() else x$theme)
  x
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


# settings of the theme used by other layers when not supplied there: spectrogram palette and background, colors of
# panels, play line and highlight box, and font family of captions
theme_layers_int <- function(x) {
  th <- x$theme
  if (length(th) == 0) return(x)
  if (!is.null(x$spectrogram))
    for (i in intersect(c("pal", "col.bg"), names(th)))
      if (is.null(x$spectrogram[[i]])) x$spectrogram[[i]] <- th[[i]]
  x$panels <- lapply(x$panels, function(p) { if (is.null(p$col)) p$col <- th$col.wave; p })
  x$selections <- lapply(x$selections, function(sel) { if (is.null(sel$col)) sel$col <- th$col.annotation; sel })
  if (!is.null(x$spectrum) && is.null(x$spectrum$col)) x$spectrum$col <- th$col.wave
  if (!is.null(th$col.line) && is.null(x$style$args$lcol)) x$style$args$lcol <- th$col.line
  if (x$style$type %in% c("scrolling", "static"))
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


# minimal axes (scale bars) are only available in scrolling spectrograms
minimal_axis_int <- function(theme, style) {
  if (identical(theme$axis.type, "minimal")) {
    warning2("axis.type = \"minimal\" is only available in scrolling spectrograms; standard axes are used")
    theme$axis.type <- "standard"
  }
  theme
}
