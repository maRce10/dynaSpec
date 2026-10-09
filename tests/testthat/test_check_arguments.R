## scrolling_spectro() -------------------------------------------------------

test_that("scrolling_spectro reports all invalid arguments at once", {
  err <- expect_error(
    scrolling_spectro(canyon_wren, ovlp = 120, axis.type = "fancy", lty = 9, play = "yes"),
    "4 input argument(s) failed",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "'ovlp'", fixed = TRUE)
  expect_match(conditionMessage(err), "'axis.type'", fixed = TRUE)
  expect_match(conditionMessage(err), "'lty'", fixed = TRUE)
  expect_match(conditionMessage(err), "'play'", fixed = TRUE)
})

test_that("scrolling_spectro checks wave, file name, palette and colors", {
  expect_error(scrolling_spectro("not a wave"), "'wave'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, file.name = "out.avi"), "'file.name'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, pal = "gray"), "'pal'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, lcol = "notacolor"), "'lcol'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, colbg = c("red", "blue")), "'colbg'", fixed = TRUE)
})

test_that("scrolling_spectro checks numeric arguments", {
  expect_error(scrolling_spectro(canyon_wren, loop = 0), "'loop'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, parallel = 1.5), "'parallel'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, speed = 0), "'speed'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, hop.size = -1), "'hop.size'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, buffer = -1), "'buffer'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, flim = c(9, 1)), "'flim'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, height.prop = c(1, 5)), "'height.prop'", fixed = TRUE)
  expect_error(scrolling_spectro(canyon_wren, t.display = 100), "'t.display'", fixed = TRUE)
})

test_that("scrolling_spectro checks calls", {
  expect_error(scrolling_spectro(canyon_wren, spectro.call = "spectro"), "'spectro.call'", fixed = TRUE)

  # missing start/end
  expect_error(
    scrolling_spectro(canyon_wren, annotation.call = call("text", x = 0.5, y = 0.5, labels = "a")),
    "Must include argument(s) 'start', 'end'",
    fixed = TRUE
  )

  # one start/end per label
  expect_error(
    scrolling_spectro(canyon_wren, annotation.call = call("text", x = 0.5, y = 0.5, labels = c("a", "b"), start = 1, end = 2)),
    "one value per label",
    fixed = TRUE
  )
})

test_that("valid scrolling_spectro arguments pass the checks", {
  # the only failure left should be about ffmpeg
  null_device()
  local_mocked_bindings(have_ffmpeg_exec2 = function() FALSE)
  expect_error(
    scrolling_spectro(canyon_wren, wl = 300, t.display = 1.7, pal = viridis::viridis, flim = c(1, 9), lty = "dotted", file.name = "x.MP4", axis.type = "minimal"),
    "'ffmpeg' could not be found",
    fixed = TRUE
  )
})

## prep_static_ggspectro() ---------------------------------------------------

test_that("prep_static_ggspectro reports invalid arguments", {
  err <- expect_error(
    prep_static_ggspectro("missing_file.wav", colPal = "rainbow", crop = c(5, 2), min_dB = 10, wn = "kaiser", ampThresh = 150, savePNG = NA),
    "7 input argument(s) failed",
    fixed = TRUE
  )
  for (i in c("'soundFile'", "'colPal'", "'crop'", "'min_dB'", "'wn'", "'ampThresh'", "'savePNG'"))
    expect_match(conditionMessage(err), i, fixed = TRUE)
})

test_that("prep_static_ggspectro checks custom palettes and limits", {
  f <- example_wavs()[1]
  expect_error(prep_static_ggspectro(f, colPal = c("white", "nocolor")), "'colPal'", fixed = TRUE)
  expect_error(prep_static_ggspectro(f, yLim = 0), "'yLim'", fixed = TRUE)
  expect_error(prep_static_ggspectro(f, crop = 0), "'crop'", fixed = TRUE)
  expect_error(prep_static_ggspectro(f, filter = c(2, 1)), "'filter'", fixed = TRUE)
  expect_error(prep_static_ggspectro(f, wl = 1), "'wl'", fixed = TRUE)
})

## paged_spectro() -----------------------------------------------------------

test_that("paged_spectro reports invalid arguments", {
  err <- expect_error(
    paged_spectro(list(a = 1), highlightAlpha = 2, cursorCol = "zzz", framerate = 2.5),
    "4 input argument(s) failed",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Must be the output of prep_static_ggspectro()", fixed = TRUE)
  expect_match(conditionMessage(err), "'highlightAlpha'", fixed = TRUE)
  expect_match(conditionMessage(err), "'cursorCol'", fixed = TRUE)
  expect_match(conditionMessage(err), "'framerate'", fixed = TRUE)
})

## internal function ---------------------------------------------------------

test_that(".check_arguments ignores NULL arguments and returns a collection", {
  res <- dynaSpec:::.check_arguments(fun = "scrolling_spectro", args = list(wl = NULL, flim = NULL))
  expect_s3_class(res, "AssertCollection")
  expect_true(res$isEmpty())
  expect_true(dynaSpec:::.report_assertions(res))
})

test_that(".check_arguments does not partially match argument names", {
  # 'res' must not match 'resampleRate'
  res <- dynaSpec:::.check_arguments(fun = "prep_static_ggspectro", args = list(resampleRate = 15000.5))
  expect_true(res$isEmpty())
})
