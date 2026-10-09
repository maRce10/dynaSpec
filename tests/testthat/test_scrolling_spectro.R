test_that("scrolling_spectro creates a video", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()

  out <- file.path(tempdir(), "scroll test.mp4")
  on.exit(unlink(out), add = TRUE)

  res <- scrolling_spectro(short_wren(), file.name = out, wl = 300, t.display = 1, fps = 10, width = 300, height = 200, flim = c(1, 9), play = FALSE, pb = FALSE)

  expect_equal(res, out)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)

  # no frames left behind
  expect_length(list.files(tempdir(), pattern = "\\.temp\\.img\\.png$"), 0)
})

test_that("scrolling_spectro works with default flim (full frequency range)", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()

  out <- file.path(tempdir(), "scroll_flim.mp4")
  on.exit(unlink(out), add = TRUE)

  scrolling_spectro(short_wren(), file.name = out, wl = 300, t.display = 1, fps = 10, width = 300, height = 200, play = FALSE, pb = FALSE)

  expect_true(file.exists(out))
})

test_that("scrolling_spectro works with loop, oscillogram, annotations and minimal axes", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()

  out <- file.path(tempdir(), "scroll_loop.mp4")
  on.exit(unlink(out), add = TRUE)

  ann <- call("text", x = 0.3, y = 0.85, labels = "wren", start = 0.2, end = 1.5, fading = 0.2)

  expect_silent(
    scrolling_spectro(short_wren(), file.name = out, wl = 300, t.display = 1, fps = 10, width = 300, height = 200, flim = c(1, 9), loop = 2, osc = TRUE, annotation.call = ann, axis.type = "minimal", speed = 0.5, play = FALSE, pb = FALSE)
  )

  expect_true(file.exists(out))
})

test_that("scrolling_spectro adjusts selection tables in spectro.call", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()

  out <- file.path(tempdir(), "scroll_call.mp4")
  on.exit(unlink(out), add = TRUE)

  # capture the selection table that reaches the spectrogram function
  got_X <- NULL
  spectro_fun <- function(wave, X) {
    got_X <<- X
    seewave::spectro(wave, scale = FALSE, axisX = FALSE, axisY = FALSE)
  }

  X <- data.frame(start = c(0.1, 1.7), end = c(0.5, 1.95))

  # embed the function itself in the call (calls are evaluated inside scrolling_spectro)
  sp_cl <- as.call(list(spectro_fun, X = X))

  scrolling_spectro(short_wren(), file.name = out, t.display = 0.6, fps = 10, width = 300, height = 200, loop = 2, spectro.call = sp_cl, play = FALSE, pb = FALSE)

  # selections near the end repeated at the start, near the start repeated at
  # the end, and everything shifted by t.display / 2
  expect_equal(got_X$start, c(0, 0.4, 2, 2.4), tolerance = 1e-6)
  expect_equal(got_X$end, c(0.25, 0.8, 2.25, 2.8), tolerance = 1e-6)
})

test_that("scrolling_spectro warns about mutually exclusive arguments only when supplied", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()

  out <- file.path(tempdir(), "scroll_warn.mp4")
  on.exit(unlink(out), add = TRUE)

  expect_warning(
    scrolling_spectro(short_wren(), file.name = out, wl = 300, t.display = 1, fps = 10, width = 300, height = 200, osc = TRUE, lower.spectro = TRUE, play = FALSE, pb = FALSE),
    "mutually exclusive"
  )
})
