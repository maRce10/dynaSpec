#' Defunct functions in dynaSpec
#'
#' These functions of dynaSpec 1.x were removed in version 2.0.0, which creates dynamic spectrograms with layers
#' (see [dynaspec()] and `vignette("dynaSpec")`). Calling them gives an error showing the new syntax:
#' \itemize{
#'   \item `scrolling_spectro()`: use `dynaspec(wave) + ds_spectrogram() + ds_scrolling()`.
#'   \item `prep_static_ggspectro()` and `prepStaticSpec()`: use `dynaspec(wave) + ds_spectrogram() + ds_paged()` (save
#'   the pages with `save_dynaspec(x, "pages.png")`).
#'   \item `paged_spectro()` and `pagedSpec()`: use `dynaspec(wave) + ds_spectrogram() + ds_paged()` (save the video with
#'   `save_dynaspec(x, "paged.mp4")`).
#' }
#' @param ... Ignored.
#' @return None (an error is raised).
#' @name dynaSpec-defunct
#' @keywords internal
NULL

# error for functions of dynaSpec 1.x, showing the code that replaces them
defunct_int <- function(old, new, save) {
  # (same condition class as .Defunct(), reported for the call of the removed function)
  msg <- paste0(
    "'", old, "()' was removed in dynaSpec 2.0.0. Dynamic spectrograms are now built with layers, e.g.:\n",
    "  x <- ", new, "\n",
    "  save_dynaspec(x, \"", save, "\")\n",
    "See vignette(\"dynaSpec\")"
  )
  stop(structure(class = c("defunctError", "error", "condition"), list(message = msg, call = sys.call(-1))))
}

#' @rdname dynaSpec-defunct
#' @export
scrolling_spectro <- function(...) defunct_int("scrolling_spectro", "dynaspec(wave) + ds_spectrogram() + ds_scrolling()", "scrolling.mp4")

#' @rdname dynaSpec-defunct
#' @export
prep_static_ggspectro <- function(...) defunct_int("prep_static_ggspectro", "dynaspec(wave) + ds_spectrogram() + ds_paged()", "pages.png")

#' @rdname dynaSpec-defunct
#' @export
prepStaticSpec <- function(...) defunct_int("prepStaticSpec", "dynaspec(wave) + ds_spectrogram() + ds_paged()", "pages.png")

#' @rdname dynaSpec-defunct
#' @export
paged_spectro <- function(...) defunct_int("paged_spectro", "dynaspec(wave) + ds_spectrogram() + ds_paged()", "paged.mp4")

#' @rdname dynaSpec-defunct
#' @export
pagedSpec <- function(...) defunct_int("pagedSpec", "dynaspec(wave) + ds_spectrogram() + ds_paged()", "paged.mp4")
