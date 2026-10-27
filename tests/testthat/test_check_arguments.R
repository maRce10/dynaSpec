## scrolling_spectro() -------------------------------------------------------

test_that("scrolling_spectro reports all invalid arguments at once", {
  err <- expect_error(
    scrolling_engine(scaly_breasted_hummingbird, ovlp = 120, axis.type = "fancy", lty = 9, play = "yes"),
    "4 input argument(s) failed",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "'ovlp'", fixed = TRUE)
  expect_match(conditionMessage(err), "'axis.type'", fixed = TRUE)
  expect_match(conditionMessage(err), "'lty'", fixed = TRUE)
  expect_match(conditionMessage(err), "'play'", fixed = TRUE)
})

test_that("scrolling_spectro checks wave, file name, palette and colors", {
  expect_error(scrolling_engine("not a wave"), "'wave'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, file.name = "out.avi"), "'file.name'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, pal = "gray"), "'pal'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, lcol = "notacolor"), "'lcol'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, colbg = c("red", "blue")), "'colbg'", fixed = TRUE)
})

test_that("scrolling_spectro checks numeric arguments", {
  expect_error(scrolling_engine(scaly_breasted_hummingbird, loop = 0), "'loop'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, parallel = 1.5), "'parallel'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, speed = 0), "'speed'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, hop.size = -1), "'hop.size'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, buffer = -1), "'buffer'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, flim = c(9, 1)), "'flim'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, height.prop = c(1, 5)), "'height.prop'", fixed = TRUE)
  expect_error(scrolling_engine(scaly_breasted_hummingbird, t.display = 100), "'t.display'", fixed = TRUE)
})

test_that("scrolling_spectro checks calls", {
  expect_error(scrolling_engine(scaly_breasted_hummingbird, spectro.call = "spectro"), "'spectro.call'", fixed = TRUE)

  # missing start/end
  expect_error(
    scrolling_engine(scaly_breasted_hummingbird, annotation.call = call("text", x = 0.5, y = 0.5, labels = "a")),
    "Must include argument(s) 'start', 'end'",
    fixed = TRUE
  )

  # one start/end per label
  expect_error(
    scrolling_engine(scaly_breasted_hummingbird, annotation.call = call("text", x = 0.5, y = 0.5, labels = c("a", "b"), start = 1, end = 2)),
    "one value per label",
    fixed = TRUE
  )
})

test_that("valid scrolling_spectro arguments pass the checks", {
  # the only failure left should be about ffmpeg
  null_device()
  local_mocked_bindings(have_ffmpeg_exec2 = function() FALSE)
  expect_error(
    scrolling_engine(scaly_breasted_hummingbird, wl = 300, t.display = 1.7, pal = viridis::viridis, flim = c(1, 9), lty = "dotted", file.name = "x.MP4", axis.type = "minimal"),
    "'ffmpeg' could not be found",
    fixed = TRUE
  )
})

## prep_paged_spectro() ---------------------------------------------------

test_that("prep_paged_spectro reports invalid arguments", {
  err <- expect_error(
    prep_paged("missing_file.wav", pal = "rainbow", crop = c(5, 2), min.db = 10, wn = "kaiser", amp.thresh = 150, save.png = NA),
    "7 input argument(s) failed",
    fixed = TRUE
  )
  for (i in c("'wave'", "'pal'", "'crop'", "'min.db'", "'wn'", "'amp.thresh'", "'save.png'"))
    expect_match(conditionMessage(err), i, fixed = TRUE)
})

test_that("prep_paged_spectro checks custom palettes and limits", {
  f <- example_wavs()[1]
  expect_error(prep_paged(f, pal = c("white", "nocolor")), "'pal'", fixed = TRUE)
  expect_error(prep_paged(f, flim = 0), "'flim'", fixed = TRUE)
  expect_error(prep_paged(f, crop = 0), "'crop'", fixed = TRUE)
  expect_error(prep_paged(f, filter = c(2, 1)), "'filter'", fixed = TRUE)
  expect_error(prep_paged(f, wl = 1), "'wl'", fixed = TRUE)
})

## paged_spectro() -----------------------------------------------------------

test_that("new verbose, plot and parallel arguments are checked", {
  f <- example_wavs()[1]
  expect_error(prep_paged(f, plot = "yes"), "'plot'", fixed = TRUE)
  expect_error(prep_paged(f, verbose = 1), "'verbose'", fixed = TRUE)
  expect_error(paged_engine(list(a = 1), parallel = 0), "'parallel'", fixed = TRUE)
  expect_error(paged_engine(list(a = 1), verbose = NA), "'verbose'", fixed = TRUE)
})

test_that("paged_spectro reports invalid arguments", {
  err <- expect_error(
    paged_engine(list(a = 1), highlight.alpha = 2, lcol = "zzz", fps = -1),
    "4 input argument(s) failed",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Must be the output of prep_paged_spectro()", fixed = TRUE)
  expect_match(conditionMessage(err), "'highlight.alpha'", fixed = TRUE)
  expect_match(conditionMessage(err), "'lcol'", fixed = TRUE)
  expect_match(conditionMessage(err), "'fps'", fixed = TRUE)
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
  res <- dynaSpec:::.check_arguments(fun = "prep_paged_spectro", args = list(resample.rate = 15000.5))
  expect_true(res$isEmpty())
})
