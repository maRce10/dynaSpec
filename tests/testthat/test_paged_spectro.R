test_that("paged_spectro creates single and multi-page videos", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()
  skip_if_not_installed("av")

  dest <- file.path(tempdir(), "paged_test")
  dir.create(dest, showWarnings = FALSE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  # keep it fast: small spectrograms and only 2 s of sound
  p <- suppressMessages(prep_static_ggspectro(example_wavs()[2], destFolder = dest, crop = 2, xLim = 1, specWidth = 4, specHeight = 1.5))
  expect_equal(p$n_pages, 2)

  vid <- suppressMessages(paged_spectro(p, vidName = "multi", play = FALSE))

  expect_equal(basename(vid), "multi.mp4")
  expect_true(file.exists(vid))
  expect_gt(file.size(vid), 0)

  # temporary folder removed, other files untouched
  expect_equal(sort(list.files(dest)), "multi.mp4")
})

test_that("paged_spectro keeps temporary files when requested", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()
  skip_if_not_installed("av")

  dest <- file.path(tempdir(), "paged_test_tmp")
  dir.create(dest, showWarnings = FALSE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- suppressMessages(prep_static_ggspectro(example_wavs()[1], destFolder = dest, crop = 1, xLim = 1, specWidth = 4, specHeight = 1.5))

  vid <- suppressMessages(paged_spectro(p, play = FALSE, delete_temp_files = FALSE))

  expect_true(file.exists(vid))
  expect_length(list.files(dest, pattern = "^dynaSpec_temp_"), 1)
})
