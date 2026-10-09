test_that("paged_spectro creates multi-page videos in parallel", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()
  skip_if_not_installed("av")

  dest <- file.path(tempdir(), "paged_test")
  dir.create(dest, showWarnings = FALSE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  # keep it fast: small spectrograms, only 2 s of sound and a low frame rate
  p <- prep_static_ggspectro(example_wavs()[2], destFolder = dest, crop = 2, xLim = 1, specWidth = 4, specHeight = 1.5, verbose = FALSE)
  expect_equal(p$n_pages, 2)

  vid <- paged_spectro(p, vidName = "multi", play = FALSE, parallel = 2, framerate = 10, verbose = FALSE)

  expect_equal(basename(vid), "multi.mp4")
  expect_true(file.exists(vid))
  expect_gt(file.size(vid), 0)

  # temporary folder removed, other files untouched
  expect_equal(sort(list.files(dest)), "multi.mp4")
})

test_that("paged_spectro creates missing folders and keeps temporary files when requested", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()
  skip_if_not_installed("av")

  dest <- file.path(tempdir(), "paged_test_tmp")
  dir.create(dest, showWarnings = FALSE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- prep_static_ggspectro(example_wavs()[1], destFolder = dest, crop = 1, xLim = 1, specWidth = 4, specHeight = 1.5, verbose = FALSE)

  new_dest <- file.path(dest, "new", "videos")

  msgs <- capture_messages(
    vid <- paged_spectro(p, destFolder = new_dest, play = FALSE, delete_temp_files = FALSE, framerate = 10)
  )
  expect_match(msgs, "Created destination folder", all = FALSE)
  expect_match(msgs, "Video saved at", all = FALSE)

  expect_true(dir.exists(new_dest))
  expect_true(file.exists(vid))
  expect_length(list.files(new_dest, pattern = "^dynaSpec_temp_"), 1)
})

test_that("paged_spectro fails clearly without ffmpeg", {
  skip_if_not_installed("av")
  local_mocked_bindings(have_ffmpeg_exec2 = function() FALSE)

  # minimal valid specParams (validated before ffmpeg is looked for)
  sp <- setNames(vector("list", 14), c("destFolder", "outFilename", "xLim", "yLim", "bg", "spec", "newWav", "segWavs", "specWidth", "specHeight", "onlyPlotSpec", "bgFlood", "autoFontCol", "fontAndAxisCol"))

  expect_error(paged_spectro(sp, play = FALSE), "'ffmpeg' could not be found", fixed = TRUE)
})
