#' Create HTML sound buttons
#'
#' `sound_buttons` creates a row of HTML buttons, each one playing a sound, along with buttons to change the playback speed.
#' @param waves The sounds (one button per sound): an object of class 'Wave', the path to a '.wav' or '.mp3' file, or a list (or vector) of those.
#' @param labels Character vector with the labels of the buttons. Default is the names of 'waves' (if supplied) or "Sound 1", "Sound 2", etc.
#' @param file.name Character string with the name of an HTML file to save the buttons as a standalone web page. Must include the .html extension. Default is `NULL` (no file is saved).
#' @param lcol Color of the active speed button. Default is "#c0392b".
#' @param speeds Numeric vector with the playback speeds offered as buttons (between 0.0625 and 16). The first value is the initial speed. Use a single value to hide speed buttons. Default is `c(1, 0.5, 0.25, 0.125)`.
#' @param keep.pitch Logical argument to control if the pitch is kept when playback speed changes (see [player()]). Default is `FALSE`.
#' @param play Logical argument to control if the buttons are shown after they are created (see [player()]). Default is `TRUE`.
#' @return Invisibly returns a character string (of class 'dynaSpec_html') with the HTML code of the buttons, which can be pasted in any web page. When called inside an R Markdown or Quarto document rendered to HTML, the buttons are embedded in the document. If 'file.name' is supplied the buttons are also saved as a standalone web page.
#' @details Sounds are embedded in the HTML code (base64 encoded), so no other files are needed. See [static()] for details.
#' @seealso [static()]
#' @export
#' @name sound_buttons
#' @examples
#' data(canyon_wren)
#'
#' wrens <- list(
#'   "First song" = seewave::cutw(canyon_wren, from = 0, to = 2, output = "Wave"),
#'   "Second song" = seewave::cutw(canyon_wren, from = 2, to = 4, output = "Wave")
#' )
#'
#' sound_buttons(wrens, file.name = file.path(tempdir(), "wrens.html"), play = FALSE)
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#' @references
#' Araya-Salas M & Wilkins M R. (2020). *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.

sound_buttons <- function(waves,
                          labels = NULL,
                          file.name = NULL,
                          lcol = "#c0392b",
                          speeds = c(1, 0.5, 0.25, 0.125),
                          keep.pitch = FALSE,
                          play = TRUE)
{
  if (inherits(waves, "Wave") || is.character(waves))
    waves <- if (inherits(waves, "Wave")) list(waves) else as.list(waves)

  if (is.null(labels))
    labels <- if (!is.null(names(waves)) && all(nzchar(names(waves)))) names(waves) else paste("Sound", seq_along(waves))

  # check arguments
  check_results <- .check_arguments(
    fun = "sound_buttons",
    args = list(html.file.name = file.name, lcol = lcol, speeds = speeds, keep.pitch = keep.pitch, play = play)
  )

  if (!checkmate::test_list(waves, min.len = 1))
    check_results$push("'waves': Must be a 'Wave' object, a sound file path or a list of those")
  else for (i in seq_along(waves))
    if (!isTRUE(res <- check_wave_int(waves[[i]])))
      check_results$push(sprintf("'waves' (sound %i): %s", i, res))

  if (!checkmate::test_character(labels, len = length(waves), any.missing = FALSE))
    check_results$push(sprintf("'labels': Must be a character vector with one label per sound (%i)", length(waves)))

  # report errors
  .report_assertions(check_results)

  uris <- vapply(waves, function(x) sound_data_int(x, read = FALSE)$uri, character(1))

  html <- paste0(
    '<div class="dynaspec-sounds" data-speeds="', paste(speeds, collapse = ","), '" data-keep-pitch="', tolower(keep.pitch), '" data-color="', col2hex_int(lcol), '" data-text-color="', contrast_col_int(lcol), '">\n',
    paste0('<audio controls preload="auto" src="', uris, '" title="', html_escape_int(labels), '"></audio>', collapse = "\n"),
    '\n</div>\n',
    player_js_int()
  )

  output_html_int(html, file.name = file.name, play = play)
}
