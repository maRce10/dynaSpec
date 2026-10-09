test_that("prep_static_ggspectro returns spectrogram parameters", {
  skip_on_cran()
  null_device()

  dest <- file.path(tempdir(), "prep_test")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- suppressMessages(prep_static_ggspectro(example_wavs()[1], destFolder = dest, onlyPlotSpec = FALSE, bgFlood = TRUE))

  expect_type(p, "list")
  expect_true(all(c("destFolder", "outFilename", "xLim", "yLim", "bg", "spec", "newWav", "segWavs") %in% names(p)))
  expect_s3_class(p$spec[[1]], "ggplot")
  expect_s4_class(p$newWav, "Wave")
  expect_equal(p$n_pages, length(p$segWavs))
  expect_equal(p$outFilename, "femaleBarnSwallow.png")
  expect_true(dir.exists(dest))

  # spectrogram reaches upper frequency limit despite default resampleRate
  expect_gt(max(p$spec[[1]]$data$freq), 9.5)
})

test_that("prep_static_ggspectro segments long files and saves PNGs", {
  skip_on_cran()
  null_device()

  dest <- file.path(tempdir(), "prep_test_seg")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- suppressMessages(prep_static_ggspectro(example_wavs()[2], destFolder = dest, xLim = 2, savePNG = TRUE))

  expect_equal(p$n_pages, length(p$spec))
  expect_gt(p$n_pages, 1)

  pngs <- list.files(dest, pattern = "png$", recursive = TRUE)
  expect_length(pngs, p$n_pages)
})

test_that("prep_static_ggspectro resolves 'wd' and relative destFolder to working directory", {
  skip_on_cran()
  null_device()

  wd <- file.path(tempdir(), "prep_test_wd")
  dir.create(wd, showWarnings = FALSE)
  owd <- setwd(wd)
  on.exit({ setwd(owd); unlink(wd, recursive = TRUE) }, add = TRUE)

  p <- suppressMessages(prep_static_ggspectro(example_wavs()[1], destFolder = "wd"))
  expect_equal(normalizePath(p$destFolder), normalizePath(wd))

  p <- suppressMessages(prep_static_ggspectro(example_wavs()[1], destFolder = "out/specs"))
  expect_equal(normalizePath(p$destFolder), normalizePath(file.path(wd, "out", "specs")))
  expect_true(dir.exists(file.path(wd, "out", "specs")))
})

test_that("prep_static_ggspectro accepts custom 2-color palettes", {
  skip_on_cran()
  null_device()

  dest <- file.path(tempdir(), "prep_test_pal")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- suppressMessages(prep_static_ggspectro(example_wavs()[1], destFolder = dest, colPal = c("white", "black")))
  expect_false(p$isViridis)
  expect_equal(p$bg, "white")
  expect_equal(p$fontAndAxisCol, "#000000")
})
