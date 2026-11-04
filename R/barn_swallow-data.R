#' Acoustic recordings of *Hirundo rustica* (barn swallow) songs
#'
#' Songs of a female and a male barn swallow recorded by Matthew R. Wilkins (the example sound files of dynaSpec 1.x).
#'
#' @format Wave objects ([tuneR::Wave]): mono, 22.05 kHz sampling rate, 16 bits; `female_barn_swallow` lasts 1.93 s and
#'   `male_barn_swallow` 4.75 s.
#'
#' @usage data(female_barn_swallow)
#' @name barn_swallow
#' @aliases female_barn_swallow male_barn_swallow
#' @examples
#' data(female_barn_swallow)
#'
#' x <- dynaspec(female_barn_swallow, flim = c(0, 10)) +
#'   ds_spectrogram(wl = 300, collevels = seq(-50, 0, 1))
"female_barn_swallow"

#' @rdname barn_swallow
"male_barn_swallow"
