test_that("prep_paged_spectro returns spectrogram parameters", {
  skip_on_cran()
  null_device()

  dest <- file.path(tempdir(), "prep_test")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- suppressMessages(prep_paged(example_wavs()[1], dest.path = dest, axis.type = "standard", bg.flood = TRUE))

  expect_type(p, "list")
  expect_true(all(c("destFolder", "outFilename", "xLim", "yLim", "bg", "spec", "newWav", "segWavs") %in% names(p)))
  # spectrogram data of each page
  expect_named(p$spec[[1]], c("time", "freq", "amp"))
  expect_equal(dim(p$spec[[1]]$amp), c(length(p$spec[[1]]$freq), length(p$spec[[1]]$time)))
  expect_s4_class(p$newWav, "Wave")
  expect_equal(p$n_pages, length(p$segWavs))
  expect_equal(p$outFilename, "femaleBarnSwallow.png")
  expect_true(dir.exists(dest))

  # spectrogram reaches upper frequency limit despite default resampleRate
  expect_gt(max(p$spec[[1]]$freq), 9.5)
})

test_that("prep_paged_spectro segments long files and saves PNGs", {
  skip_on_cran()
  null_device()

  dest <- file.path(tempdir(), "prep_test_seg")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- suppressMessages(prep_paged(example_wavs()[2], dest.path = dest, t.display = 2, save.png = TRUE))

  expect_equal(p$n_pages, length(p$spec))
  expect_gt(p$n_pages, 1)

  pngs <- list.files(dest, pattern = "png$", recursive = TRUE)
  expect_length(pngs, p$n_pages)
})

test_that("prep_paged_spectro resolves 'wd' and relative destFolder to working directory", {
  skip_on_cran()
  null_device()

  wd <- file.path(tempdir(), "prep_test_wd")
  dir.create(wd, showWarnings = FALSE)
  owd <- setwd(wd)
  on.exit({ setwd(owd); unlink(wd, recursive = TRUE) }, add = TRUE)

  p <- suppressMessages(prep_paged(example_wavs()[1], dest.path = "wd"))
  expect_equal(normalizePath(p$destFolder), normalizePath(wd))

  p <- suppressMessages(prep_paged(example_wavs()[1], dest.path = "out/specs"))
  expect_equal(normalizePath(p$destFolder), normalizePath(file.path(wd, "out", "specs")))
  expect_true(dir.exists(file.path(wd, "out", "specs")))
})

test_that("prep_paged_spectro is silent with verbose = FALSE and plot = FALSE", {
  skip_on_cran()
  null_device()

  dest <- file.path(tempdir(), "prep_test_quiet")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  # segmented file (would otherwise print several messages)
  expect_silent(p <- prep_paged(example_wavs()[2], dest.path = dest, t.display = 2, verbose = FALSE, plot = FALSE))
  expect_gt(p$n_pages, 1)
})

test_that("prep_paged_spectro accepts custom 2-color palettes", {
  skip_on_cran()
  null_device()

  dest <- file.path(tempdir(), "prep_test_pal")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- suppressMessages(prep_paged(example_wavs()[1], dest.path = dest, pal = c("white", "black")))
  expect_false(p$isViridis)
  expect_equal(p$bg, "white")
  expect_equal(p$fontAndAxisCol, "#000000")
})

test_that("prep_paged_spectro takes Wave objects", {
  null_device()
  dest <- file.path(tempdir(), "prep_wave_test")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  wren <- short_wren(1)
  p <- prep_paged(wren, dest.path = dest, save.png = TRUE, plot = FALSE, verbose = FALSE)
  expect_equal(p$outFilename, "wren.png")
  expect_true(file.exists(file.path(dest, "wren_static_specs", "wren_1.png")))

})

test_that("prep_paged_spectro takes palette functions", {
  null_device()
  dest <- file.path(tempdir(), "pal_fun_test")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- prep_paged(example_wavs()[1], dest.path = dest, pal = viridis::mako, plot = FALSE, verbose = FALSE)
  expect_true(is.function(p$colPal))
})
