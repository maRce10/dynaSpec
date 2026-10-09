## processSound() -------------------------------------------------------------

wv <- tuneR::sine(440, duration = 6 * 8000, samp.rate = 8000, bit = 16, pcm = TRUE)

test_that("processSound keeps crop start and pads to full pages", {
  # 5 s (1-6) padded to 3 pages of 2 s
  p <- suppressMessages(dynaSpec:::processSound(wv, filter = NULL, ampThresh = 0, crop = c(1, 6), xLim = 2))
  expect_equal(p$wavDur, 6)
  expect_equal(p$n_pages, 3)
  expect_length(p$segWavs, 3)
})

test_that("processSound sets crop end to file duration", {
  expect_message(
    p <- dynaSpec:::processSound(wv, filter = NULL, ampThresh = 0, crop = c(2, 9), xLim = 2),
    "crop end set to file duration"
  )

  # silent when verbose = FALSE
  expect_silent(dynaSpec:::processSound(wv, filter = NULL, ampThresh = 0, crop = c(2, 9), xLim = 2, verbose = FALSE))
  expect_equal(p$wavDur, 4)
})

test_that("processSound crop = FALSE keeps whole file", {
  p <- suppressMessages(dynaSpec:::processSound(wv, filter = NULL, ampThresh = 0, crop = FALSE, xLim = 2))
  expect_equal(p$wavDur, 6)
  expect_equal(p$xLim, c(0, 2))
})

test_that("processSound defaults xLim to 5 s and Inf to cropped duration", {
  p <- suppressMessages(dynaSpec:::processSound(wv, filter = NULL, ampThresh = 0, crop = NULL, xLim = NULL))
  expect_equal(p$xLim, c(0, 5))

  p <- suppressMessages(dynaSpec:::processSound(wv, filter = NULL, ampThresh = 0, crop = NULL, xLim = Inf))
  expect_equal(p$xLim, c(0, 6))
  expect_equal(p$n_pages, 1)
})

## helpers --------------------------------------------------------------------

test_that("is.url only detects URLs", {
  expect_equal(
    dynaSpec:::is.url(c("C:/data/wwwrecord.wav", "https://x.org/a.wav", "http://x.org/a.wav", "www.x.org/a.wav", "my.www.file.wav")),
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )
})

test_that("contrast_col_int picks black or white", {
  expect_equal(dynaSpec:::contrast_col_int("white"), "#000000")
  expect_equal(dynaSpec:::contrast_col_int("black"), "#ffffff")
  expect_equal(dynaSpec:::contrast_col_int(viridis::inferno(1)), "#ffffff")
})

test_that("fading_text_dynaspec_int fades in and out", {
  x <- dynaSpec:::fading_text_dynaspec_int(rep(0, 20), start = 5, end = 15, fading = 3, labels = "a")
  expect_equal(unname(x[5:7]), c(0, 0.5, 1))
  expect_equal(unname(x[13:15]), c(1, 0.5, 0))
  expect_equal(unname(x[c(1:4, 16:20)]), rep(0, 9))
  expect_true(all(names(x)[5:15] == "a"))
})

test_that("ffmpeg helpers fail gracefully", {
  local_mocked_bindings(ffmpeg_exec2 = function(quote = FALSE) stop("Could not find ffmpeg"))
  expect_false(dynaSpec:::have_ffmpeg_exec2())
})

test_that("run_ffmpeg_int reports ffmpeg errors", {
  skip_if_no_ffmpeg()
  expect_error(
    dynaSpec:::run_ffmpeg_int(paste("-i", shQuote(file.path(tempdir(), "does_not_exist.wav")), "-y", shQuote(file.path(tempdir(), "out.wav"))), "testing"),
    "ffmpeg failed when testing",
    fixed = TRUE
  )
})
