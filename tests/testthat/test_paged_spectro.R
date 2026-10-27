test_that("paged_spectro creates multi-page videos in parallel", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()

  dest <- file.path(tempdir(), "paged_test")
  dir.create(dest, showWarnings = FALSE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  # keep it fast: small spectrograms, only 2 s of sound and a low frame rate
  p <- prep_paged(example_wavs()[2], dest.path = dest, crop = 2, t.display = 1, spec.width = 4, spec.height = 1.5, verbose = FALSE)
  expect_equal(p$n_pages, 2)

  vid <- paged_engine(p, file.name = file.path(dest, "multi.mp4"), play = FALSE, parallel = 2, fps = 10, verbose = FALSE)

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

  dest <- file.path(tempdir(), "paged_test_tmp")
  dir.create(dest, showWarnings = FALSE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- prep_paged(example_wavs()[1], dest.path = dest, crop = 1, t.display = 1, spec.width = 4, spec.height = 1.5, verbose = FALSE)

  new_dest <- file.path(dest, "new", "videos")

  msgs <- capture_messages(
    vid <- paged_engine(p, file.name = file.path(new_dest, "video.mp4"), play = FALSE, delete.temp = FALSE, fps = 10)
  )
  expect_match(msgs, "Created destination folder", all = FALSE)
  expect_match(msgs, "Video saved at", all = FALSE)

  expect_true(dir.exists(new_dest))
  expect_true(file.exists(vid))
  expect_length(list.files(new_dest, pattern = "^dynaSpec_temp_"), 1)
})

test_that("paged_spectro fails clearly without ffmpeg", {
  local_mocked_bindings(have_ffmpeg_exec2 = function() FALSE)

  # minimal valid specParams (validated before ffmpeg is looked for)
  sp <- setNames(vector("list", 14), c("destFolder", "outFilename", "xLim", "yLim", "bg", "spec", "newWav", "segWavs", "specWidth", "specHeight", "onlyPlotSpec", "bgFlood", "autoFontCol", "fontAndAxisCol"))

  expect_error(paged_engine(sp, play = FALSE), "'ffmpeg' could not be found", fixed = TRUE)
})

test_that("paged_spectro takes sounds directly", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()

  dest <- file.path(tempdir(), "paged_direct_test")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  # sound file with settings for prep_paged_spectro() in '...'
  vid <- paged_engine(example_wavs()[2], dest.path = dest, crop = 1, spec.width = 4, spec.height = 1.5, fps = 5, play = FALSE, verbose = FALSE)
  expect_equal(basename(vid), "maleBarnSwallow.mp4")
  expect_true(file.exists(vid))

  # Wave object (video named after the object)
  hb <- short_sound(1)
  vid <- paged_engine(hb, dest.path = dest, spec.width = 4, spec.height = 1.5, fps = 5, play = FALSE, verbose = FALSE)
  expect_equal(basename(vid), "hb.mp4")

  # arguments for the preparation of pages are ignored with prepared pages
  p <- prep_paged(hb, dest.path = dest, spec.width = 4, spec.height = 1.5, plot = FALSE, verbose = FALSE)
  expect_warning(paged_engine(p, file.name = file.path(dest, "dots.mp4"), fps = 5, play = FALSE, verbose = FALSE, crop = 1), "ignored")
})

test_that("paged_spectro creates web pages", {
  null_device()
  dest <- file.path(tempdir(), "paged_html_test")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  p <- prep_paged(example_wavs()[2], dest.path = dest, crop = 2, t.display = 1, spec.width = 4, spec.height = 1.5, axis.type = "standard", plot = FALSE, verbose = FALSE)
  res <- paged_engine(p, file.name = file.path(dest, "pages.html"), play = FALSE, verbose = FALSE)

  expect_s3_class(res, "dynaSpec_html")
  expect_true(file.exists(file.path(dest, "pages.html")))
  # no temporary files left
  expect_equal(list.files(dest), "pages.html")

  # one image and one set of extents per page, within the image
  expect_equal(lengths(regmatches(res, gregexpr("<img ", res))), 2)
  ext <- player_rows(res)
  expect_equal(nrow(ext), 2)
  expect_equal(ext$t0, c(0, 1))
  expect_true(all(ext$left > 0 & ext$left < ext$right & ext$right < 1))
  expect_true(all(ext$top > 0 & ext$top < ext$bottom & ext$bottom < 1))
  expect_match(res, 'data-highlight="#4B0C6B"', fixed = TRUE)

  # embedded when knitting
  withr::local_options(knitr.in.progress = TRUE)
  expect_s3_class(paged_engine(p, file.name = file.path(dest, "pages.html"), play = FALSE, verbose = FALSE), "knit_asis")
})

