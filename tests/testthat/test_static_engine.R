test_that("static_spectro creates a self-contained HTML player", {
  out <- file.path(tempdir(), "player.html")
  on.exit(unlink(out), add = TRUE)

  res <- static_engine(short_wren(), file.name = out, wl = 300, flim = c(1, 10), width = 400, height = 250, play = FALSE)

  expect_s3_class(res, "dynaSpec_html")
  expect_true(file.exists(out))
  expect_match(res, 'class="dynaspec-player"', fixed = TRUE)
  expect_match(res, "data:image/png;base64,", fixed = TRUE)
  expect_match(res, "data:audio/wav;base64,", fixed = TRUE)
  expect_match(res, 'data-duration="2"', fixed = TRUE)

  # panel extents are proportions of the figure
  ext <- player_rows(res)
  expect_equal(nrow(ext), 1)
  expect_true(ext$left > 0 && ext$left < 0.5)
})

test_that("static_spectro works with sound files and without oscillogram", {
  res <- static_engine(example_wavs()[1], osc = FALSE, width = 400, height = 250, speeds = 1)
  expect_match(res, 'data-speeds="1"', fixed = TRUE)
})

test_that("static_spectro is embedded when knitting", {
  withr::local_options(knitr.in.progress = TRUE)
  res <- static_engine(short_wren(), wl = 300, width = 300, height = 200)
  expect_s3_class(res, "knit_asis")
  expect_match(res, "```{=html}", fixed = TRUE)
})

test_that("static_spectro reports invalid arguments", {
  expect_error(static_engine(short_wren(), file.name = "x.mp3", speeds = 0.01, flim = c(5, 1)), "3 input argument")
  expect_error(static_engine(short_wren(), scale = TRUE), "cannot be supplied")
  expect_error(static_engine(short_wren(), flim = c(30, 40)), "Nyquist")
})

test_that("sound_buttons creates one button per sound", {
  res <- sound_buttons(list(a = short_wren(1), b = example_wavs()[1]))
  expect_match(res, 'class="dynaspec-sounds"', fixed = TRUE)
  expect_equal(lengths(regmatches(res, gregexpr("<audio", res))), 2)
  expect_match(res, 'title="b"', fixed = TRUE)
  expect_error(sound_buttons(list(short_wren()), labels = c("a", "b")), "labels")
  expect_error(sound_buttons("no_file.wav"), "does not exist")
})

test_that("base64 encoding is correct", {
  expect_equal(dynaSpec:::base64_enc_int(charToRaw("Man")), "TWFu")
  expect_equal(dynaSpec:::base64_enc_int(charToRaw("Ma")), "TWE=")
  expect_equal(dynaSpec:::base64_enc_int(charToRaw("M")), "TQ==")
})

test_that("players are previewed in the viewer", {
  viewed <- NULL
  withr::local_options(viewer = function(url) viewed <<- url)
  dynaSpec:::view_html_int(static_engine(short_wren(1), wl = 300, width = 300, height = 200, play = FALSE))
  expect_true(file.exists(viewed))
  expect_match(paste(readLines(viewed), collapse = "\n"), "dynaspec-player", fixed = TRUE)
})

test_that("keep.pitch is passed to the player", {
  expect_match(static_engine(short_wren(1), wl = 300, width = 300, height = 200, play = FALSE), 'data-keep-pitch="false"', fixed = TRUE)
  expect_match(sound_buttons(short_wren(1), keep.pitch = TRUE, play = FALSE), 'data-keep-pitch="true"', fixed = TRUE)
  expect_error(sound_buttons(short_wren(1), keep.pitch = "yes"), "keep.pitch")
})

test_that("oscillogram can be plotted above or below the spectrogram", {
  ext <- function(x, side) player_rows(x)[[side]]
  below <- static_engine(short_wren(1), wl = 300, width = 300, height = 200, play = FALSE)
  above <- static_engine(short_wren(1), wl = 300, width = 300, height = 200, osc.position = "above", play = FALSE)

  # play line spans both panels (from the top margin to the time axis margin) in both layouts
  expect_equal(ext(below, "top"), 0.1)
  expect_equal(ext(below, "bottom"), 0.6)
  expect_equal(ext(above, "top"), ext(below, "top"))
  expect_equal(ext(above, "bottom"), ext(below, "bottom"))
  expect_error(static_engine(short_wren(1), osc.position = "left"), "osc.position")
})

test_that("static_spectro can show only the oscillogram", {
  res <- static_engine(short_wren(1), spectro = FALSE, width = 300, height = 200, play = FALSE)
  expect_match(res, 'alt="Oscillogram of a 1 s sound"', fixed = TRUE)
  expect_error(static_engine(short_wren(1), spectro = FALSE, osc = FALSE), "at least one")
})

test_that("static_spectro splits sounds in rows", {
  res <- static_engine(short_wren(), wl = 300, width = 300, height = 200, rows = 3, play = FALSE)
  ext <- player_rows(res)

  expect_equal(nrow(ext), 3)
  # rows cover the whole sound, in order, from top to bottom
  expect_equal(ext$t0, c(0, ext$t1[1:2]))
  expect_equal(ext$t1[3], 2)
  expect_true(all(diff(ext$top) > 0))
  expect_true(all(ext$bottom[1:2] < ext$top[2:3]))
  # oscillogram only
  expect_equal(nrow(player_rows(static_engine(short_wren(), spectro = FALSE, width = 300, height = 200, rows = 2, play = FALSE))), 2)

  expect_error(static_engine(short_wren(), wl = 300, rows = 400, height = 50), "too short")
  expect_error(static_engine(short_wren(), wl = 300, rows = 100), "too tall")
  expect_error(static_engine(short_wren(), rows = 0), "rows")
  # image is 3 rows high (upper rows have no time axis: 3.5 lines shorter, 0.2 inches per line at 100 dpi)
  skip_if_not_installed("base64enc")
  img <- png::readPNG(base64enc::base64decode(sub('".*', "", sub('.*<img src="data:image/png;base64,', "", res))))
  expect_equal(dim(img)[1:2], c(600 - 2 * 70, 300))
})

test_that("static_spectro creates a video with the play line", {
  skip_on_cran()
  skip_if_no_ffmpeg()

  out <- file.path(tempdir(), "static spectro.mp4")
  frame <- tempfile(fileext = ".png")
  on.exit(unlink(c(out, frame)), add = TRUE)

  res <- static_engine(short_wren(), file.name = out, wl = 300, width = 400, height = 200, rows = 2, play = FALSE)
  expect_equal(res, out)
  expect_true(file.exists(out))

  # line position in a frame of the second row matches the html player
  ext <- player_rows(static_engine(short_wren(), wl = 300, width = 400, height = 200, rows = 2, play = FALSE))
  t <- 1.5
  dynaSpec:::run_ffmpeg_int(paste0('-y -ss ', t, ' -i "', out, '" -frames:v 1 "', frame, '"'), "extracting a frame")
  img <- png::readPNG(frame)
  red <- img[, , 1] > 0.6 & img[, , 2] < 0.35 & img[, , 3] < 0.35
  line_x <- mean(which(colSums(red) > 0))
  line_y <- range(which(rowSums(red) > 0))
  e <- ext[2, ]
  expect_equal(line_x, (e$left + (e$right - e$left) * (t - e$t0) / (e$t1 - e$t0)) * 400, tolerance = 2 / 400)
  # figure height (upper row has no time axis, so it is shorter than 'height'); the video may be padded by 1 pixel to an even size
  fig_h <- 2 * 200 - 70
  # (video compression blurs the colored ends of the line by a couple of pixels)
  expect_lt(abs(line_y[1] - e$top * fig_h), 6)
  expect_lt(abs(line_y[2] - e$bottom * fig_h), 6)
})
