test_that("dynaspec objects collect layers", {
  st <- data.frame(start = c(0.2, 1), end = c(0.6, 1.5), labels = c("a", "b"))
  x <- dynaspec(short_wren(), flim = c(1, 10)) +
    spectrogram(wl = 300) + spectrogram(ovlp = 50) +
    oscillogram(position = "above") +
    selections(st, labels = "labels") + selections(st[1, ]) +
    captions("wren", start = 0, end = 2) +
    player(speeds = c(1, 0.5)) +
    scrolling(t.display = 1)

  expect_s3_class(x, "dynaspec")
  # spectrogram arguments are combined, selections and annotations added
  expect_equal(x$spectrogram, list(wl = 300, ovlp = 50))
  expect_equal(x$panels[[1]]$position, "above")
  expect_length(x$selections, 2)
  expect_length(x$captions, 1)
  expect_equal(x$style$type, "scrolling")
  expect_equal(x$style$args, list(t.display = 1))
  # the last style replaces previous ones
  expect_equal((x + paged())$style$type, "paged")
  expect_named(x$player, c("speeds", "keep.pitch", "buttons"))

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
  expect_error(captions(c("a", "b"), start = 1, end = 2), "start")
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
    captions("wren", start = 0.2, end = 1.5, fading = 0.1)

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

test_that("static spectrograms can hide the play line", {
  x <- dynaspec(short_wren(1)) + spectrogram(wl = 300) + static(width = 300, height = 200, play.line = FALSE, lcol = "#c0392b")
  res <- save_dynaspec(x, play = FALSE, verbose = FALSE)
  expect_match(res, 'data-line="false"', fixed = TRUE)
  expect_match(save_dynaspec(x + static(play.line = TRUE), play = FALSE, verbose = FALSE), 'data-line="true"', fixed = TRUE)
  expect_error(save_dynaspec(x + static(play.line = "no"), play = FALSE), "play.line")

  # videos show the figure without the (red) line
  skip_on_cran()
  skip_if_no_ffmpeg()
  out <- file.path(tempdir(), "no_line.mp4")
  frame <- tempfile(fileext = ".png")
  on.exit(unlink(c(out, frame)), add = TRUE)
  save_dynaspec(x, out, play = FALSE, verbose = FALSE)
  dynaSpec:::run_ffmpeg_int(paste0('-y -ss 0.5 -i "', out, '" -frames:v 1 "', frame, '"'), "extracting a frame")
  img <- png::readPNG(frame)
  expect_equal(sum(img[, , 1] > 0.6 & img[, , 2] < 0.35 & img[, , 3] < 0.35), 0)
})

test_that("the default play line color of static spectrograms is mako(10)[9]", {
  res <- save_dynaspec(dynaspec(short_wren(1)) + spectrogram(wl = 300) + static(width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(res, 'data-color="#A0DFB9"', fixed = TRUE)
  # dark text on the light active buttons
  expect_match(res, 'data-text-color="#000000"', fixed = TRUE)
})

test_that("only the panels added as layers are shown", {
  wren <- dynaspec(short_wren(1))
  expect_null(wren$spectrogram)
  expect_equal((wren + spectrogram())$spectrogram, list())

  html <- function(x) save_dynaspec(x + static(width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(html(wren), 'alt="Time axis', fixed = TRUE)
  expect_match(html(wren + oscillogram()), 'alt="Oscillogram of', fixed = TRUE)
  expect_match(html(wren + spectrogram(wl = 300)), 'alt="Spectrogram of', fixed = TRUE)
  expect_match(html(wren + spectrogram(wl = 300) + oscillogram()), 'alt="Spectrogram and oscillogram of', fixed = TRUE)

  # selections and annotations can be drawn on empty panels
  sel <- selections(data.frame(start = 0.2, end = 0.5, labels = "a"), labels = "labels")
  expect_match(html(wren + sel + captions("x", start = 0, end = 1)), 'alt="Time axis', fixed = TRUE)

  # scrolling and paged spectrograms need a spectrogram
  expect_error(save_dynaspec(wren + scrolling(), play = FALSE), "need a spectrogram")
  expect_error(save_dynaspec(wren + oscillogram() + paged(), play = FALSE), "need a spectrogram")
})

test_that("envelope, spectrum and overview layers", {
  wren <- dynaspec(short_wren(1)) + spectrogram(wl = 300)
  html <- function(x) save_dynaspec(x + static(width = 400, height = 300), play = FALSE, verbose = FALSE)

  # envelope and oscillogram stacked; layers of the same kind replace each other
  x <- wren + oscillogram(position = "above") + envelope() + envelope(col = "red")
  expect_length(x$panels, 2)
  expect_equal(x$panels[[2]]$col, "red")
  expect_match(html(x), 'alt="Spectrogram, oscillogram and amplitude envelope of', fixed = TRUE)
  expect_match(html(dynaspec(short_wren(1)) + envelope()), 'alt="Amplitude envelope of', fixed = TRUE)

  # power spectrum on either side (play line only spans the spectrogram column)
  right <- player_rows(html(wren + spectrum(side = "right")))
  left <- player_rows(html(wren + spectrum(side = "left")))
  expect_lt(right$right, 0.9)
  expect_gt(left$left, 0.2)
  expect_match(html(wren + spectrum()), 'and power spectrum of', fixed = TRUE)

  # overview: scrolling only; one lower panel in scrolling spectrograms
  expect_warning(html(wren + overview()), "only shown in scrolling")
  sc <- function(x) save_dynaspec(x + scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(sc(wren + overview()), '"lower":"spectro"', fixed = TRUE)
  expect_match(sc(wren + envelope()), '"lower":"osc"', fixed = TRUE)
  expect_match(sc(wren), '"lower":"none"', fixed = TRUE)
  expect_warning(sc(wren + oscillogram() + overview()), "single lower panel")
  expect_warning(sc(wren + spectrum()), "only shown in static")
  expect_warning(save_dynaspec(wren + envelope() + paged(spec.width = 4, spec.height = 1.5), play = FALSE, verbose = FALSE), "not shown in paged")

  # no delay before scrolling starts by default
  expect_match(sc(wren), '"buffer":0', fixed = TRUE)

  expect_error(envelope(position = "left"), "position")
  expect_error(spectrum(side = "top"), "side")
})

test_that("dynatheme sets axis titles and look in all styles", {
  wren <- dynaspec(short_wren(1)) + spectrogram(wl = 300)
  x <- wren + dynatheme(xlab = "Tiempo (s)", cex.lab = 1.3) + dynatheme(col.lab = "gray30")
  # theme settings are combined
  expect_equal(x$theme, list(xlab = "Tiempo (s)", cex.lab = 1.3, col.lab = "gray30"))

  expect_s3_class(save_dynaspec(x + static(width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(x + dynatheme(axis.type = "none", bg = "black") + static(width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
  sc <- save_dynaspec(x + dynatheme(bg = "black") + scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(sc, '"bg":"#000000"', fixed = TRUE)
  expect_s3_class(save_dynaspec(x + paged(spec.width = 4, spec.height = 1.5), play = FALSE, verbose = FALSE), "dynaSpec_html")

  # minimal axes only in scrolling spectrograms
  expect_warning(save_dynaspec(x + dynatheme(axis.type = "minimal") + static(width = 300, height = 200), play = FALSE, verbose = FALSE), "only available in scrolling")

  expect_error(dynatheme(axis.type = "fancy"), "axis.type")
  expect_error(dynatheme(font.lab = 7), "font.lab")
  expect_error(dynatheme(col.lab = "zzz"), "col.lab")
})

test_that("captions are shown in all styles", {
  x <- dynaspec(short_wren(1)) + spectrogram(wl = 300) +
    captions(c("a", "b"), start = c(0.1, 0.5), end = c(0.4, 0.9), fading = 0.1) +
    captions("top", start = 0, end = 1, x = 0.2, y = 0.95, col = "red", font = 2)
  html <- function(y) save_dynaspec(y, play = FALSE, verbose = FALSE)
  for (st in list(static(width = 300, height = 200), scrolling(t.display = 0.5, width = 300, height = 200), paged(spec.width = 4, spec.height = 1.5))) {
    res <- html(x + st)
    expect_match(res, 'class="ds-captions"', fixed = TRUE)
    expect_match(res, '"labels":["a","b"]', fixed = TRUE)
    expect_match(res, '"x":0.2', fixed = TRUE)
  }
  # positions from the top in web pages
  expect_match(html(x + static(width = 300, height = 200)), '"y":0.05', fixed = TRUE)
  expect_error(captions("a", start = 0, end = 1, font = 5), "font")

  # videos
  skip_on_cran()
  skip_if_no_ffmpeg()
  out <- file.path(tempdir(), c("cap_static.mp4", "cap_paged.mp4"))
  on.exit(unlink(out), add = TRUE)
  save_dynaspec(x + static(width = 300, height = 200), out[1], play = FALSE, verbose = FALSE)
  save_dynaspec(x + paged(spec.width = 4, spec.height = 1.5), out[2], play = FALSE, verbose = FALSE)
  expect_true(all(file.exists(out)))
})

test_that("selections take warbleR selection tables", {
  skip_if_not_installed("warbleR")
  skip_if_not_installed("NatureSounds")
  e <- new.env()
  utils::data("lbh_selec_table", package = "warbleR", envir = e)
  utils::data("Phae.long1", "lbh.est", package = "NatureSounds", envir = e)
  st <- e$lbh_selec_table

  # sound file: selections of that file only
  f <- file.path(tempdir(), "Phae.long1.wav")
  tuneR::writeWave(e$Phae.long1, f)
  on.exit(unlink(f), add = TRUE)
  x <- dynaspec(f) + spectrogram(wl = 300) + selections(st) + static(width = 300, height = 200)
  res <- save_dynaspec(x, play = FALSE, verbose = FALSE)
  expect_s3_class(res, "dynaSpec_html")
  sel <- dynaSpec:::sound_file_selections_int(x$selections[[1]], x)
  expect_equal(unique(sel$X$sound.files), "Phae.long1.wav")
  expect_equal(nrow(sel$X), sum(st$sound.files == "Phae.long1.wav"))

  # Wave object: the sound file must be chosen
  w <- dynaspec(e$Phae.long1) + spectrogram(wl = 300) + static(width = 300, height = 200)
  expect_error(save_dynaspec(w + selections(st), play = FALSE), "sound.file")
  expect_s3_class(save_dynaspec(w + selections(st, sound.file = "Phae.long1.wav"), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_error(selections(st, sound.file = "nope.wav"), "not in the 'sound.files'")

  # extended selection tables are not supported
  expect_error(selections(e$lbh.est), "Extended selection tables are not supported")
})

test_that("player() adds buttons to web pages", {
  x <- dynaspec(short_wren(), flim = c(1, 10)) + spectrogram(wl = 300)
  b <- c("loop", "volume", "download", "fullscreen")

  # default: time readout only
  res <- save_dynaspec(x + static(width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(res, 'data-buttons="time"', fixed = TRUE)

  # all styles get the buttons and the name of the downloaded sound
  for (st in list(static(width = 300, height = 200), scrolling(t.display = 0.5, width = 300, height = 200), paged(spec.width = 4, spec.height = 1.5))) {
    res <- save_dynaspec(x + player(buttons = b) + st, play = FALSE, verbose = FALSE)
    expect_match(res, paste0('data-buttons="', paste(b, collapse = ","), '" data-name="'), fixed = TRUE)
  }

  expect_match(save_dynaspec(x + player(buttons = character()), play = FALSE, verbose = FALSE), 'data-buttons=""', fixed = TRUE)
  expect_error(player(buttons = "rewind"), "buttons")
})

test_that("the theme background is kept around spectrograms, also with a power spectrum on the left", {
  png_file <- file.path(tempdir(), "bg_test.png")
  on.exit(unlink(png_file), add = TRUE)
  x <- dynaspec(short_wren(), flim = c(1, 10)) + spectrogram(wl = 300) + oscillogram() + dynatheme(bg = "black", col.lab = "white") + static(width = 400, height = 250, rows = 2)
  for (y in list(x, x + spectrum(side = "left"))) {
    save_dynaspec(y, png_file, play = FALSE, verbose = FALSE)
    img <- png::readPNG(png_file)
    # corners are figure background
    expect_equal(c(img[1, 1, 1:3], img[nrow(img), ncol(img), 1:3]), rep(0, 6))
  }
})

test_that("dynatheme() settings and preset themes", {
  x <- dynaspec(short_wren(), flim = c(1, 10)) + spectrogram(wl = 300)

  # presets replace previous themes; dynatheme() afterwards changes some settings
  y <- x + dynatheme(xlab = "Tiempo") + dynatheme_dark(cex.lab = 1.4) + dynatheme(family = "serif")
  expect_null(y$theme$xlab)
  expect_equal(y$theme[c("bg", "cex.lab", "family")], list(bg = "black", cex.lab = 1.4, family = "serif"))

  # theme colors are used when not supplied in other layers
  z <- dynaSpec:::theme_layers_int(x + spectrogram(colbg = "white") + oscillogram() + envelope(col = "red") + static() + dynatheme_dark())
  expect_identical(z$spectrogram$pal, viridis::mako)
  expect_equal(z$spectrogram$colbg, "white")
  expect_equal(vapply(z$panels, `[[`, character(1), "col"), c("#A0DFB9", "red"))
  expect_equal(z$style$args$lcol, "white")

  # all presets in all styles
  for (th in list(dynatheme_dark(), dynatheme_presentation(), dynatheme_paper(), dynatheme_minimal(), dynatheme_transparent(), dynatheme_void())) {
    expect_s3_class(save_dynaspec(x + oscillogram() + spectrum(side = "left") + th + static(width = 400, height = 250), play = FALSE, verbose = FALSE), "dynaSpec_html")
    expect_s3_class(save_dynaspec(x + oscillogram() + th + scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
    expect_s3_class(save_dynaspec(x + th + paged(spec.width = 4, spec.height = 1.5), play = FALSE, verbose = FALSE), "dynaSpec_html")
  }

  # player controls and fonts of web pages
  res <- save_dynaspec(x + dynatheme(col.bar = "white", bg.bar = "black", family = "mono"), play = FALSE, verbose = FALSE)
  expect_match(res, 'data-bar-col="#FFFFFF"', fixed = TRUE)
  expect_match(res, 'data-bar-bg="#000000"', fixed = TRUE)
  expect_match(res, "data-family=", fixed = TRUE)

  # palettes can be names of 'viridis' palettes
  expect_s3_class(save_dynaspec(x + dynatheme(pal = "magma", grid = TRUE, box = FALSE, las = 0, tick.length = 0) + static(width = 400, height = 250), play = FALSE, verbose = FALSE), "dynaSpec_html")

  expect_error(dynatheme(las = 3), "las")
  expect_error(dynatheme(col.bar = "zzz"), "col.bar")
  expect_error(dynatheme_dark(box = "yes"), "box")
})
