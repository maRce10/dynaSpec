test_that("dynaspec objects collect layers", {
  st <- data.frame(start = c(0.2, 1), end = c(0.6, 1.5), labels = c("a", "b"))
  x <- dynaspec(short_wren(), flim = c(1, 10)) +
    spectrogram(wl = 300) + spectrogram(ovlp = 50) +
    oscillogram(position = "above") +
    selections(st, labels = "labels") + selections(st[1, ]) +
    annotations("wren", start = 0, end = 2) +
    player(speeds = c(1, 0.5)) +
    scrolling(t.display = 1)

  expect_s3_class(x, "dynaspec")
  # spectrogram arguments are combined, selections and annotations added
  expect_equal(x$spectrogram, list(wl = 300, ovlp = 50))
  expect_equal(x$oscillogram$position, "above")
  expect_length(x$selections, 2)
  expect_length(x$annotations, 1)
  expect_equal(x$style$type, "scrolling")
  expect_equal(x$style$args, list(t.display = 1))
  # the last style replaces previous ones
  expect_equal((x + paged())$style$type, "paged")
  expect_named(x$player, c("speeds", "keep.pitch"))

  # objects and layers print a short description when not interactive
  expect_output(print(x), "scrolling spectrogram")
  expect_output(print(spectrogram()), "spectrogram")
})

test_that("dynaspec checks its arguments", {
  expect_error(dynaspec("no_file.wav"), "does not exist")
  expect_error(dynaspec("https://xeno-canto.org/490771"), "does not exist")
  expect_error(dynaspec(short_wren(), flim = c(5, 1)), "flim")
  expect_error(dynaspec(short_wren()) + 1, "only dynaspec layers")
  expect_error(selections(data.frame(a = 1)), "start")
  expect_error(selections(data.frame(start = 1, end = 2), col = "zzz"), "'col'")
  expect_error(annotations(c("a", "b"), start = 1, end = 2), "start")
  expect_error(oscillogram(position = "left"), "position")
  expect_error(player(speeds = 0.01), "speeds")
  expect_error(save_dynaspec(dynaspec(short_wren()), "x.wav"), "file.name")
})

test_that("static spectrograms are saved as web pages and images", {
  st <- data.frame(start = 0.2, end = 0.6, colors = "#56B4E9")
  x <- dynaspec(short_wren(), flim = c(1, 10)) + spectrogram(wl = 300) + oscillogram() + selections(st, col = "colors") + static(width = 400, height = 250)

  res <- save_dynaspec(x, play = FALSE, verbose = FALSE)
  expect_s3_class(res, "dynaSpec_html")
  expect_match(res, 'class="dynaspec-player"', fixed = TRUE)

  png_file <- file.path(tempdir(), "static_test.png")
  on.exit(unlink(png_file), add = TRUE)
  save_dynaspec(x, png_file, play = FALSE, verbose = FALSE)
  expect_true(file.exists(png_file))

  # embedded when knitting
  withr::local_options(knitr.in.progress = TRUE)
  expect_s3_class(knitr::knit_print(x), "knit_asis")
})

test_that("cropping shifts selections and annotations", {
  x <- dynaspec(short_wren(), crop = c(0.5, 1.5)) + spectrogram(wl = 300) +
    selections(data.frame(start = 1, end = 1.2)) + static(width = 300, height = 200)
  res <- save_dynaspec(x, play = FALSE, verbose = FALSE)
  expect_equal(player_rows(res)$t1, 1)
})

test_that("scrolling and paged spectrograms are saved as web pages", {
  x <- dynaspec(short_wren(), flim = c(1, 10)) + spectrogram(wl = 300) +
    annotations("wren", start = 0.2, end = 1.5, fading = 0.1)

  res <- save_dynaspec(x + scrolling(t.display = 1, width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(res, 'class="dynaspec-scroll"', fixed = TRUE)
  expect_match(res, '"labels":"wren"', fixed = TRUE)

  res <- save_dynaspec(x + paged(t.display = 1, spec.width = 4, spec.height = 1.5), play = FALSE, verbose = FALSE)
  expect_match(res, 'class="dynaspec-paged"', fixed = TRUE)
  expect_equal(nrow(player_rows(res)), 2)

  # pages as images
  out <- file.path(tempdir(), "pages_test.png")
  files <- save_dynaspec(x + paged(t.display = 1, spec.width = 4, spec.height = 1.5), out, play = FALSE, verbose = FALSE)
  on.exit(unlink(files), add = TRUE)
  expect_equal(basename(files), c("pages_test_1.png", "pages_test_2.png"))
  expect_error(save_dynaspec(x + scrolling(), out, play = FALSE), "only available")
})

test_that("videos are created for all styles", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()

  x <- dynaspec(short_wren(1)) + spectrogram(wl = 300)
  out <- file.path(tempdir(), c("v_static.mp4", "v_paged.mp4", "v_scroll.mp4"))
  on.exit(unlink(out), add = TRUE)
  save_dynaspec(x + static(width = 300, height = 200), out[1], play = FALSE, verbose = FALSE)
  save_dynaspec(x + paged(spec.width = 4, spec.height = 1.5), out[2], play = FALSE, verbose = FALSE)
  save_dynaspec(x + scrolling(t.display = 0.5, width = 300, height = 200, fps = 10), out[3], play = FALSE, verbose = FALSE)
  expect_true(all(file.exists(out)))
})
