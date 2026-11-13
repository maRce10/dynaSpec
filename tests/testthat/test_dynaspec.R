test_that("dynaspec objects collect layers", {
  st <- data.frame(start = c(0.2, 1), end = c(0.6, 1.5), labels = c("a", "b"))
  x <- dynaspec(short_sound(), flim = c(1, 5)) +
    ds_spectrogram(wl = 300) + ds_spectrogram(ovlp = 50) +
    ds_oscillogram(position = "above") +
    ds_annotations(st, labels = "labels") + ds_annotations(st[1, ]) +
    ds_captions("hb", start = 0, end = 2) +
    ds_player(speeds = c(1, 0.5)) +
    ds_scrolling(t.display = 1)

  expect_s3_class(x, "dynaspec")
  # spectrogram arguments are combined, selections and annotations added
  expect_equal(x$spectrogram, list(wl = 300, ovlp = 50))
  expect_equal(x$panels[[1]]$position, "above")
  expect_length(x$selections, 2)
  expect_length(x$captions, 1)
  expect_equal(x$style$type, "scrolling")
  expect_equal(x$style$args, list(t.display = 1))
  # the last style replaces previous ones
  expect_equal((x + ds_paged())$style$type, "paged")
  # (only the supplied ds_player() settings are stored; other settings depend on the style and theme)
  expect_named(x$player, "speeds")

  # objects and layers print a short description when not interactive
  expect_output(print(x), "scrolling spectrogram")
  expect_output(print(ds_spectrogram()), "spectrogram")
})

test_that("dynaspec checks its arguments", {
  expect_error(dynaspec("no_file.wav"), "does not exist")
  expect_error(dynaspec("https://xeno-canto.org/490771"), "does not exist")
  expect_error(dynaspec(short_sound(), flim = c(5, 1)), "flim")
  expect_error(dynaspec(short_sound()) + 1, "only dynaspec layers")
  expect_error(ds_annotations(data.frame(a = 1)), "start")
  expect_error(ds_annotations(data.frame(start = 1, end = 2), col = "zzz"), "'col'")
  expect_error(ds_captions(c("a", "b"), start = 1, end = 2), "start")
  expect_error(ds_oscillogram(position = "left"), "position")
  expect_error(ds_player(speeds = 0.01), "speeds")
  expect_error(save_dynaspec(dynaspec(short_sound()), "x.wav"), "file.name")
})

test_that("static spectrograms are saved as web pages and images", {
  st <- data.frame(start = 0.2, end = 0.6, colors = "#56B4E9")
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300) + ds_oscillogram() + ds_annotations(st, col = "colors") + ds_static(width = 400, height = 250)

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
  x <- dynaspec(short_sound(), tlim = c(0.5, 1.5)) + ds_spectrogram(wl = 300) +
    ds_annotations(data.frame(start = 1, end = 1.2)) + ds_static(width = 300, height = 200)
  res <- save_dynaspec(x, play = FALSE, verbose = FALSE)
  expect_equal(player_rows(res)$t1, 1, tolerance = 1e-3)
})

test_that("scrolling and paged spectrograms are saved as web pages", {
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300) +
    ds_captions("hb", start = 0.2, end = 1.5, fading = 0.1)

  res <- save_dynaspec(x + ds_scrolling(t.display = 1, width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(res, 'class="dynaspec-scroll"', fixed = TRUE)
  expect_match(res, '"labels":"hb"', fixed = TRUE)

  res <- save_dynaspec(x + ds_paged(t.display = 1, width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE)
  expect_match(res, 'class="dynaspec-paged"', fixed = TRUE)
  expect_equal(nrow(player_rows(res)), 2)

  # pages as images
  out <- file.path(tempdir(), "pages_test.png")
  files <- save_dynaspec(x + ds_paged(t.display = 1, width = 600, height = 225, res = 150), out, play = FALSE, verbose = FALSE)
  on.exit(unlink(files), add = TRUE)
  expect_equal(basename(files), c("pages_test_1.png", "pages_test_2.png"))
  expect_error(save_dynaspec(x + ds_scrolling(), out, play = FALSE), "only available")
})

test_that("videos are created for all styles", {
  skip_on_cran()
  null_device()
  skip_if_no_ffmpeg()

  x <- dynaspec(short_sound(1)) + ds_spectrogram(wl = 300)
  out <- file.path(tempdir(), c("v_static.mp4", "v_paged.mp4", "v_scroll.mp4"))
  on.exit(unlink(out), add = TRUE)
  save_dynaspec(x + ds_static(width = 300, height = 200), out[1], play = FALSE, verbose = FALSE)
  save_dynaspec(x + ds_paged(width = 600, height = 225, res = 150), out[2], play = FALSE, verbose = FALSE)
  save_dynaspec(x + ds_scrolling(t.display = 0.5, width = 300, height = 200, fps = 10), out[3], play = FALSE, verbose = FALSE)
  expect_true(all(file.exists(out)))
})

test_that("static spectrograms can hide the play line", {
  x <- dynaspec(short_sound(1)) + ds_spectrogram(wl = 300) + ds_static(width = 300, height = 200, play.line = FALSE) + ds_theme(col.line = "#c0392b")
  res <- save_dynaspec(x, play = FALSE, verbose = FALSE)
  expect_match(res, 'data-line="false"', fixed = TRUE)
  expect_match(save_dynaspec(x + ds_static(play.line = TRUE), play = FALSE, verbose = FALSE), 'data-line="true"', fixed = TRUE)
  expect_error(save_dynaspec(x + ds_static(play.line = "no"), play = FALSE), "play.line")

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
  res <- save_dynaspec(dynaspec(short_sound(1)) + ds_spectrogram(wl = 300) + ds_static(width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(res, 'data-color="#A0DFB9"', fixed = TRUE)
  # dark text on the light active buttons
  expect_match(res, 'data-text-color="#000000"', fixed = TRUE)
})

test_that("only the panels added as layers are shown", {
  hb <- dynaspec(short_sound(1))
  expect_null(hb$spectrogram)
  expect_equal((hb + ds_spectrogram())$spectrogram, list())

  html <- function(x) save_dynaspec(x + ds_static(width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(html(hb), 'alt="Time axis', fixed = TRUE)
  expect_match(html(hb + ds_oscillogram()), 'alt="Oscillogram of', fixed = TRUE)
  expect_match(html(hb + ds_spectrogram(wl = 300)), 'alt="Spectrogram of', fixed = TRUE)
  expect_match(html(hb + ds_spectrogram(wl = 300) + ds_oscillogram()), 'alt="Spectrogram and oscillogram of', fixed = TRUE)

  # selections and annotations can be drawn on empty panels
  sel <- ds_annotations(data.frame(start = 0.2, end = 0.5, labels = "a"), labels = "labels")
  expect_match(html(hb + sel + ds_captions("x", start = 0, end = 1)), 'alt="Time axis', fixed = TRUE)

  # scrolling and paged spectrograms need a spectrogram
  expect_error(save_dynaspec(hb + ds_scrolling(), play = FALSE), "need a spectrogram")
  expect_error(save_dynaspec(hb + ds_oscillogram() + ds_paged(), play = FALSE), "need a spectrogram")
})

test_that("envelope, spectrum and overview layers", {
  hb <- dynaspec(short_sound(1)) + ds_spectrogram(wl = 300)
  html <- function(x) save_dynaspec(x + ds_static(width = 400, height = 300), play = FALSE, verbose = FALSE)

  # envelope and oscillogram stacked; layers of the same kind replace each other
  x <- hb + ds_oscillogram(position = "above") + ds_envelope() + ds_envelope(col = "red")
  expect_length(x$panels, 2)
  expect_equal(x$panels[[2]]$col, "red")
  expect_match(html(x), 'alt="Spectrogram, oscillogram and amplitude envelope of', fixed = TRUE)
  expect_match(html(dynaspec(short_sound(1)) + ds_envelope()), 'alt="Amplitude envelope of', fixed = TRUE)

  # power spectrum on either side (play line only spans the spectrogram column)
  none <- player_rows(html(hb))
  right <- player_rows(html(hb + ds_spectrum(side = "right")))
  left <- player_rows(html(hb + ds_spectrum(side = "left")))
  expect_lt(right$right, none$right - 0.02)
  expect_gt(left$left, none$left + 0.02)
  expect_match(html(hb + ds_spectrum()), 'and power spectrum of', fixed = TRUE)

  # overview: scrolling only; one lower panel in scrolling spectrograms
  expect_warning(html(hb + ds_overview()), "only shown in scrolling")
  sc <- function(x) save_dynaspec(x + ds_scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(sc(hb + ds_overview()), '"lower":"spectro"', fixed = TRUE)
  expect_match(sc(hb + ds_envelope()), '"lower":"osc"', fixed = TRUE)
  expect_match(sc(hb), '"lower":"none"', fixed = TRUE)
  expect_warning(sc(hb + ds_oscillogram() + ds_overview()), "single lower panel")
  expect_warning(sc(hb + ds_spectrum()), "only shown in static")
  expect_warning(save_dynaspec(hb + ds_envelope() + ds_paged(width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE), "not shown in paged")

  # no delay before scrolling starts by default
  expect_match(sc(hb), '"buffer":0', fixed = TRUE)

  expect_error(ds_envelope(position = "left"), "position")
  expect_error(ds_spectrum(side = "top"), "side")
})

test_that("dynatheme sets axis titles and look in all styles", {
  hb <- dynaspec(short_sound(1)) + ds_spectrogram(wl = 300)
  x <- hb + ds_theme(xlab = "Time (s)", cex.lab = 1.3) + ds_theme(col.lab = "gray30")
  # theme settings are combined
  expect_equal(x$theme, list(xlab = "Time (s)", cex.lab = 1.3, col.lab = "gray30"))

  expect_s3_class(save_dynaspec(x + ds_static(width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(x + ds_theme(axis.type = "none", bg = "black") + ds_static(width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
  sc <- save_dynaspec(x + ds_theme(bg = "black") + ds_scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(sc, '"bg":"#000000"', fixed = TRUE)
  expect_s3_class(save_dynaspec(x + ds_paged(width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE), "dynaSpec_html")

  # minimal axes only in scrolling spectrograms
  expect_warning(save_dynaspec(x + ds_theme(axis.type = "minimal") + ds_static(width = 300, height = 200), play = FALSE, verbose = FALSE), "only available in scrolling")

  expect_error(ds_theme(axis.type = "fancy"), "axis.type")
  expect_error(ds_theme(font.lab = 7), "font.lab")
  expect_error(ds_theme(col.lab = "zzz"), "col.lab")
})

test_that("captions are shown in all styles", {
  x <- dynaspec(short_sound(1)) + ds_spectrogram(wl = 300) +
    ds_captions(c("a", "b"), start = c(0.1, 0.5), end = c(0.4, 0.9), fading = 0.1) +
    ds_captions("top", start = 0, end = 1, x = 0.2, y = 0.95, col = "red", font = 2)
  html <- function(y) save_dynaspec(y, play = FALSE, verbose = FALSE)
  for (st in list(ds_static(width = 300, height = 200), ds_scrolling(t.display = 0.5, width = 300, height = 200), ds_paged(width = 600, height = 225, res = 150))) {
    res <- html(x + st)
    expect_match(res, 'class="ds-captions"', fixed = TRUE)
    expect_match(res, '"labels":["a","b"]', fixed = TRUE)
    expect_match(res, '"x":0.2', fixed = TRUE)
  }
  # positions from the top in web pages
  expect_match(html(x + ds_static(width = 300, height = 200)), '"y":0.05', fixed = TRUE)
  expect_error(ds_captions("a", start = 0, end = 1, font = 5), "font")

  # videos
  skip_on_cran()
  skip_if_no_ffmpeg()
  out <- file.path(tempdir(), c("cap_static.mp4", "cap_paged.mp4"))
  on.exit(unlink(out), add = TRUE)
  save_dynaspec(x + ds_static(width = 300, height = 200), out[1], play = FALSE, verbose = FALSE)
  save_dynaspec(x + ds_paged(width = 600, height = 225, res = 150), out[2], play = FALSE, verbose = FALSE)
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
  x <- dynaspec(f) + ds_spectrogram(wl = 300) + ds_annotations(st) + ds_static(width = 300, height = 200)
  res <- save_dynaspec(x, play = FALSE, verbose = FALSE)
  expect_s3_class(res, "dynaSpec_html")
  sel <- dynaSpec:::sound_file_selections_int(x$selections[[1]], x)
  expect_equal(unique(sel$X$sound.files), "Phae.long1.wav")
  expect_equal(nrow(sel$X), sum(st$sound.files == "Phae.long1.wav"))

  # Wave object: the sound file must be chosen
  w <- dynaspec(e$Phae.long1) + ds_spectrogram(wl = 300) + ds_static(width = 300, height = 200)
  expect_error(save_dynaspec(w + ds_annotations(st), play = FALSE), "sound.file")
  expect_s3_class(save_dynaspec(w + ds_annotations(st, sound.file = "Phae.long1.wav"), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_error(ds_annotations(st, sound.file = "nope.wav"), "not in the 'sound.files'")

  # extended selection tables are not supported
  expect_error(ds_annotations(e$lbh.est), "Extended selection tables are not supported")
})

test_that("ds_player() adds buttons to web pages", {
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300)
  b <- c("loop", "volume", "download", "fullscreen", "progress")

  # default: time readout only
  res <- save_dynaspec(x + ds_static(width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(res, 'data-buttons="time"', fixed = TRUE)

  # all styles get the buttons and the name of the downloaded sound
  for (st in list(ds_static(width = 300, height = 200), ds_scrolling(t.display = 0.5, width = 300, height = 200), ds_paged(width = 600, height = 225, res = 150))) {
    res <- save_dynaspec(x + ds_player(buttons = b) + st, play = FALSE, verbose = FALSE)
    expect_match(res, paste0('data-buttons="', paste(b, collapse = ","), '" data-name="'), fixed = TRUE)
  }

  expect_match(save_dynaspec(x + ds_player(buttons = character()), play = FALSE, verbose = FALSE), 'data-buttons=""', fixed = TRUE)
  expect_error(ds_player(buttons = "rewind"), "buttons")
})

test_that("ds_player(buttons = 'zoom') stretches scrolling spectrograms", {
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300) + ds_overview()
  sc <- function(y) save_dynaspec(y + ds_scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE)
  plain <- sc(x)
  zoomed <- sc(x + ds_player(buttons = c("zoom", "time")))
  expect_match(zoomed, '"zoom":true', fixed = TRUE)
  # (time labels are added by the page, also when time is fixed)
  expect_match(zoomed, '"movingAxis":true', fixed = TRUE)
  expect_match(plain, '"movingAxis":false', fixed = TRUE)
  # strips are drawn at twice the resolution
  strip_width <- function(h) {
    uri <- regmatches(h, regexpr('class="ds-strip" srcset="data:image/png;base64,[^ "]+', h))
    dim(png::readPNG(base64enc::base64decode(sub(".*base64,", "", uri))))[2]
  }
  skip_if_not_installed("base64enc")
  expect_equal(strip_width(zoomed), 2 * strip_width(plain), tolerance = 0.01)
  expect_warning(save_dynaspec(dynaspec(short_sound()) + ds_spectrogram(wl = 300) + ds_player(buttons = "zoom") + ds_static(width = 300, height = 200), play = FALSE, verbose = FALSE), "only available in scrolling")
})

test_that("spectrogram, panel and figure backgrounds are independent", {
  png_file <- file.path(tempdir(), "bg_indep.png")
  on.exit(unlink(png_file), add = TRUE)
  x <- dynaspec(short_sound(), flim = c(1.5, 5.5)) + ds_spectrogram(wl = 200, col.bg = "black") + ds_oscillogram() +
    ds_static(width = 400, height = 250)
  save_dynaspec(x, png_file, play = FALSE, verbose = FALSE)
  img <- png::readPNG(png_file)
  # (figure corners stay white when only the spectrogram background is set)
  expect_equal(c(img[1, 1, 1:3], img[nrow(img), ncol(img), 1:3]), rep(1, 6))
  save_dynaspec(x + ds_theme(bg = "gray50", col.panel = "red"), png_file, play = FALSE, verbose = FALSE)
  img <- png::readPNG(png_file)
  # (compared with the color as drawn by the same png device: on macOS the device applies color management, e.g. gray50
  # is stored as 0.424 instead of 0.498)
  expect_equal(img[1, 1, 1:3], png_color("gray50"), tolerance = 0.02)
  # (some pixels of the oscillogram panel are red)
  expect_true(any(img[, , 1] > 0.35 & img[, , 1] > img[, , 2] + 0.2 & img[, , 1] > img[, , 3] + 0.2))
})

test_that("the theme background is kept around spectrograms, also with a power spectrum on the left", {
  png_file <- file.path(tempdir(), "bg_test.png")
  on.exit(unlink(png_file), add = TRUE)
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300) + ds_oscillogram() + ds_theme(bg = "black", col.lab = "white") + ds_static(width = 400, height = 250, rows = 2)
  for (y in list(x, x + ds_spectrum(side = "left"))) {
    save_dynaspec(y, png_file, play = FALSE, verbose = FALSE)
    img <- png::readPNG(png_file)
    # corners are figure background
    expect_equal(c(img[1, 1, 1:3], img[nrow(img), ncol(img), 1:3]), rep(0, 6))
  }
})

test_that("ds_theme() settings and preset themes", {
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300)

  # presets replace previous themes; ds_theme() afterwards changes some settings
  y <- x + ds_theme(xlab = "Time (s)") + ds_theme_dark(cex.lab = 1.4) + ds_theme(family = "serif")
  expect_null(y$theme$xlab)
  expect_equal(y$theme[c("bg", "cex.lab", "family")], list(bg = "black", cex.lab = 1.4, family = "serif"))

  # theme colors are used when not supplied in other layers
  z <- dynaSpec:::theme_layers_int(x + ds_spectrogram(col.bg = "white") + ds_oscillogram() + ds_envelope(col = "red") + ds_static() + ds_theme_dark())
  expect_identical(z$spectrogram$pal, viridis::mako)
  expect_equal(z$spectrogram$col.bg, "white")
  # (presets do not set the spectrogram background: it is the color of the lowest amplitude of their palette)
  for (th in list(ds_theme_dark(), ds_theme_paper(), ds_theme_minimal(), ds_theme_macaulay(), ds_theme_mako()))
    expect_null(th$args$col.bg)
  expect_null(dynaSpec:::theme_layers_int(x + ds_theme_dark())$spectrogram$col.bg)
  expect_equal(low_col_int(viridis::mako), viridis::mako(30)[1])
  expect_equal(vapply(z$panels, `[[`, character(1), "col"), c("#A0DFB9", "red"))
  expect_equal(z$style$args$lcol, "white")

  # all presets in all styles
  for (th in list(ds_theme_dark(), ds_theme_presentation(), ds_theme_paper(), ds_theme_minimal(), ds_theme_transparent(), ds_theme_void(), ds_theme_xenocanto(), ds_theme_macaulay())) {
    expect_s3_class(save_dynaspec(x + ds_oscillogram() + ds_spectrum(side = "left") + th + ds_static(width = 400, height = 250), play = FALSE, verbose = FALSE), "dynaSpec_html")
    expect_s3_class(save_dynaspec(x + ds_oscillogram() + th + ds_scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
    expect_s3_class(save_dynaspec(x + th + ds_paged(width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE), "dynaSpec_html")
  }

  # player controls and fonts of web pages
  res <- save_dynaspec(x + ds_theme(col.bar = "white", bg.bar = "black", family = "mono"), play = FALSE, verbose = FALSE)
  expect_match(res, 'data-bar-col="#FFFFFF"', fixed = TRUE)
  expect_match(res, 'data-bar-bg="#000000"', fixed = TRUE)
  expect_match(res, "data-family=", fixed = TRUE)

  # palettes can be names of 'viridis' palettes
  expect_s3_class(save_dynaspec(x + ds_theme(pal = "magma", grid = TRUE, box = FALSE, las = 0, tick.length = 0) + ds_static(width = 400, height = 250), play = FALSE, verbose = FALSE), "dynaSpec_html")

  # rounded player controls show the seek bar by default (unless buttons are supplied); play line of scrolling
  z <- dynaSpec:::theme_layers_int(x + ds_theme_xenocanto(lwd.line = 3) + ds_scrolling())
  expect_equal(z$player$buttons, c("progress", "time"))
  expect_equal(z$style$args[c("lty", "lwd")], list(lty = 1, lwd = 3))
  expect_equal(dynaSpec:::theme_layers_int(x + ds_theme_xenocanto() + ds_static())$style$args$lty, 1)
  expect_equal(dynaSpec:::theme_layers_int(x + ds_player(buttons = "loop") + ds_theme_xenocanto())$player$buttons, "loop")
  res <- save_dynaspec(x + ds_theme_xenocanto(), play = FALSE, verbose = FALSE)
  expect_match(res, 'data-bar-style="rounded"', fixed = TRUE)
  expect_match(res, 'data-buttons="progress,time"', fixed = TRUE)

  # labels inside the figure in all styles (also with a power spectrum, and with moving time labels)
  ins <- x + ds_oscillogram() + ds_theme(axis.type = "inside")
  expect_s3_class(save_dynaspec(ins + ds_spectrum(side = "left") + ds_static(width = 400, height = 250, rows = 2), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_match(save_dynaspec(ins + ds_scrolling(t.display = 0.5, fix.time = FALSE, width = 300, height = 200), play = FALSE, verbose = FALSE), '"unit":"s"', fixed = TRUE)
  expect_s3_class(save_dynaspec(x + ds_theme(axis.type = "inside") + ds_paged(width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE), "dynaSpec_html")

  # compact axes in all styles (time in minutes and seconds)
  cmp <- x + ds_oscillogram() + ds_theme(axis.type = "compact")
  expect_s3_class(save_dynaspec(cmp + ds_spectrum(side = "left") + ds_static(width = 400, height = 250, rows = 2), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_match(save_dynaspec(cmp + ds_scrolling(t.display = 0.5, fix.time = FALSE, width = 300, height = 200), play = FALSE, verbose = FALSE), '"top":true', fixed = TRUE)
  expect_s3_class(save_dynaspec(x + ds_theme(axis.type = "compact") + ds_paged(width = 600, height = 225, res = 150, title = "a"), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_equal(dynaSpec:::mmss_int(c(0, 1, 65)), c("0:00", "0:01", "1:05"))
  expect_equal(dynaSpec:::mmss_int(c(0.5, 2)), c("0:00.5", "0:02.0"))

  # square player controls show the volume control by default; overview dimmed with col.highlight
  expect_equal(dynaSpec:::theme_layers_int(x + ds_theme_macaulay())$player$buttons, c("volume", "time"))
  res <- save_dynaspec(x + ds_overview() + ds_theme_macaulay() + ds_scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(res, 'data-bar-style="square"', fixed = TRUE)
  expect_match(res, '"dim":"#3333336B"', fixed = TRUE)

  expect_error(ds_theme(axis.type = "outside"), "axis.type")
  expect_error(ds_theme(lty.line = "wavy"), "lty.line")
  expect_error(ds_theme(bar.style = "round"), "bar.style")
  expect_error(ds_theme(las = 5), "las")
  expect_error(ds_theme(col.bar = "zzz"), "col.bar")
  expect_error(ds_theme_dark(box = "yes"), "box")
})

test_that("ds_annotations() borders can be set by color, column, width and line type", {
  st <- data.frame(start = c(0.2, 0.6), end = c(0.4, 0.9), colors = c("#E69F00", "#56B4E9"))
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300) + ds_static(width = 300, height = 200)
  a <- ds_annotations(st, col = "colors", border = "colors", lwd = 2, lty = "dashed")
  expect_equal(a$args[c("border", "lwd", "lty")], list(border = "colors", lwd = 2, lty = "dashed"))
  expect_s3_class(save_dynaspec(x + a, play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(x + ds_annotations(st, border = "black", lty = 3), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_error(ds_annotations(st, lty = "wavy"), "'lty'")
  expect_error(ds_annotations(st, border = "zzz"), "'border'")
})

test_that("ds_annotations() can color the spectrogram within selections", {
  st <- data.frame(start = c(0.2, 0.6), end = c(0.4, 0.9), bottom.freq = 2, top.freq = 8, colors = c("#E69F00", "#56B4E9"))
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300) + ds_annotations(st, col = "colors", type = "spectrogram")
  png_file <- file.path(tempdir(), "colored_test.png")
  on.exit(unlink(png_file), add = TRUE)
  save_dynaspec(x + ds_static(width = 400, height = 250), png_file, play = FALSE, verbose = FALSE)
  img <- png::readPNG(png_file)
  # colored pixels (red and blue channels differ) only appear with type = "spectrogram"
  expect_gt(sum(abs(img[, , 1] - img[, , 3]) > 0.2), 0)
  expect_s3_class(save_dynaspec(x + ds_scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(x + ds_paged(width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_error(ds_annotations(st, type = "fill"), "type")
})

test_that("ds_set_theme() sets the theme of all dynaspec objects", {
  old <- ds_set_theme(ds_theme(col.bar = "white", cex.lab = 1.3))
  withr::defer(ds_set_theme(old))
  expect_equal(ds_get_theme()$args[c("col.bar", "cex.lab")], list(col.bar = "white", cex.lab = 1.3))

  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300) + ds_static(width = 300, height = 200)
  html <- function(y) save_dynaspec(y, play = FALSE, verbose = FALSE)
  expect_match(html(x), 'data-bar-col="#FFFFFF"', fixed = TRUE)
  # ds_theme() modifies the global theme, presets replace it
  expect_match(html(x + ds_theme(bg.bar = "black")), 'data-bar-col="#FFFFFF" data-bar-line="[^"]+" data-bar-bg="#000000"')
  expect_no_match(html(x + ds_theme_paper()), "data-bar-col", fixed = TRUE)
  th <- dynaSpec:::global_theme_int(x + ds_theme(xlab = "a"))$theme
  expect_equal(th[sort(names(th))], list(cex.lab = 1.3, col.bar = "white", xlab = "a"))

  # NULL removes it
  ds_set_theme(NULL)
  expect_null(ds_get_theme())
  expect_no_match(html(x), "data-bar-col", fixed = TRUE)
  expect_error(ds_set_theme(ds_spectrogram()), "must be a theme")
})

test_that("ds_theme_mako() and theme colors of annotations", {
  st <- data.frame(start = 0.2, end = 0.6)
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300) + ds_annotations(st) + ds_oscillogram()
  z <- dynaSpec:::theme_layers_int(x + ds_theme_mako())
  expect_equal(z$selections[[1]]$col, "#366A9F")
  expect_equal(z$panels[[1]]$col, "#40498E")
  # (low amplitudes dark, as in viridis::mako())
  expect_equal(z$spectrogram$pal(3), viridis::mako(3))
  # colors supplied in layers take precedence; without theme the default color is used
  expect_equal(dynaSpec:::theme_layers_int(x + ds_annotations(st, col = "red") + ds_theme(col.annotation = "blue"))$selections[[2]]$col, "red")
  expect_equal(dynaSpec:::annotation_cols_int(x$selections[[1]]), "#E69F00")
  for (st_l in list(ds_static(width = 400, height = 250), ds_scrolling(t.display = 0.5, width = 300, height = 200), ds_paged(width = 600, height = 225, res = 150)))
    expect_s3_class(suppressWarnings(save_dynaspec(x + ds_theme_mako() + st_l, play = FALSE, verbose = FALSE)), "dynaSpec_html")
  expect_error(ds_theme(col.annotation = "zzz"), "col.annotation")
})

test_that("ds_theme() sets the color and orientation of axis labels", {
  x <- dynaspec(short_sound(), flim = c(1, 5)) + ds_spectrogram(wl = 300) + ds_oscillogram() + ds_theme(las = 2, col.axis = "red")
  png_file <- file.path(tempdir(), "las_test.png")
  on.exit(unlink(png_file), add = TRUE)
  save_dynaspec(x + ds_static(width = 400, height = 250), png_file, play = FALSE, verbose = FALSE)
  img <- png::readPNG(png_file)
  # red tick labels
  expect_gt(sum(img[, , 1] > 0.8 & img[, , 2] < 0.3 & img[, , 3] < 0.3), 0)
  # vertical time labels are drawn in scrolling web pages too
  res <- save_dynaspec(x + ds_scrolling(t.display = 0.5, fix.time = FALSE, width = 300, height = 200), play = FALSE, verbose = FALSE)
  expect_match(res, '"xVert":true', fixed = TRUE)
  expect_match(res, '"colAxis":"#FF0000"', fixed = TRUE)
  expect_s3_class(suppressWarnings(save_dynaspec(x + ds_theme(axis.type = "standard", las = 3) + ds_paged(width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE)), "dynaSpec_html")
  # axis label sizes of the Macaulay preset
  expect_equal(ds_theme_macaulay()$args[c("cex.axis", "cex.lab")], list(cex.axis = 0.96, cex.lab = 1.2))
  expect_error(ds_theme(las = 4), "las")
  expect_error(ds_theme(col.axis = "zzz"), "col.axis")
})

test_that("functions of dynaSpec 1.x are defunct and show the new syntax", {
  expect_error(scrolling_spectro(scythebill), "ds_scrolling\\(\\)", class = "defunctError")
  expect_error(paged_spectro(), "ds_paged\\(\\)", class = "defunctError")
  expect_error(pagedSpec(), "removed in dynaSpec 2.0.0")
  expect_error(prep_static_ggspectro(scythebill), "pages.png")
  expect_error(prepStaticSpec(), "pages.png")
})

test_that("ds_annotations(type = 'line') draws lines above the sounds in all styles", {
  st <- data.frame(start = c(0.1, 0.8), end = c(0.6, 1.5), who = c("a", "b"), y = c(4, 5))
  x <- dynaspec(short_sound(), flim = c(1.5, 5.5)) + ds_spectrogram(wl = 200)
  ann <- ds_annotations(st, col = "red", type = "line", line.freq = "y", labels = "who")
  expect_equal(ann$args[c("lwd", "alpha")], list(lwd = 8, alpha = 1))
  expect_equal(ds_annotations(st, type = "line", lwd = 2, alpha = 0.5)$args[c("lwd", "alpha")], list(lwd = 2, alpha = 0.5))
  png_file <- file.path(tempdir(), "lines.png")
  on.exit(unlink(png_file), add = TRUE)
  save_dynaspec(x + ann + ds_oscillogram() + ds_static(width = 400, height = 250), png_file, play = FALSE, verbose = FALSE)
  img <- png::readPNG(png_file)
  expect_true(any(img[, , 1] > 0.35 & img[, , 1] > img[, , 2] + 0.2 & img[, , 1] > img[, , 3] + 0.2))
  expect_s3_class(save_dynaspec(x + ann + ds_scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(x + ann + ds_paged(width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_error(ds_annotations(st, type = "line", line.freq = "who"), "line.freq")
  expect_error(ds_annotations(st, type = "lines"), "type")
})

test_that("multi-row static spectrograms label the time of every row", {
  x <- dynaspec(short_sound(), flim = c(1.5, 5.5)) + ds_spectrogram(wl = 200)
  one <- player_rows(save_dynaspec(x + ds_static(width = 400, height = 200, rows = 2), play = FALSE, verbose = FALSE))
  # (rows are separated by the margin of the time labels, not only by a small gap)
  expect_gt(one$top[2] - one$bottom[1], 0.05)
})

test_that("dynaspec() warns when flim is higher than the Nyquist frequency", {
  expect_warning(x <- dynaspec(short_sound(), flim = c(1.5, 6.5)), "higher than the Nyquist frequency")
  expect_silent(dynaspec(short_sound(), flim = c(1.5, 5)))
  # spectrograms are shown up to the Nyquist frequency in all styles
  x <- x + ds_spectrogram(wl = 200)
  expect_s3_class(save_dynaspec(x + ds_static(width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(x + ds_scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(x + ds_paged(width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE), "dynaSpec_html")
})

test_that("Inf as upper limit of flim and tlim means the Nyquist frequency and the end of the sound", {
  sound <- short_sound()
  expect_silent(x <- dynaspec(sound, flim = c(0, Inf), tlim = Inf) + ds_spectrogram(wl = 200))
  expect_silent(y <- dynaspec(sound, flim = c(1, Inf), tlim = c(0.5, Inf)) + ds_spectrogram(wl = 200))
  for (obj in list(x, y)) {
    expect_no_warning(expect_s3_class(save_dynaspec(obj + ds_static(width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html"))
    expect_no_warning(expect_s3_class(save_dynaspec(obj + ds_scrolling(t.display = 0.5, width = 300, height = 200), play = FALSE, verbose = FALSE), "dynaSpec_html"))
    expect_no_warning(expect_s3_class(save_dynaspec(obj + ds_paged(t.display = 1, width = 600, height = 225, res = 150), play = FALSE, verbose = FALSE), "dynaSpec_html"))
  }
  # the lower limit must still be within the sound
  expect_error(dynaspec(sound, flim = c(Inf, Inf)), "flim")
  expect_error(save_dynaspec(dynaspec(sound, flim = c(8, Inf)) + ds_spectrogram(wl = 200) + ds_paged(), play = FALSE, verbose = FALSE), "Nyquist")
})

test_that("paged spectrograms fill the figure", {
  p <- prep_paged(short_sound(), crop = FALSE, t.display = 1, flim = c(0, Inf), wl = 200, plot = FALSE, verbose = FALSE, dest.path = tempdir())
  expect_equal(p$yLim, c(0, short_sound()@samp.rate / 2000))

  # without axes the page spans the whole figure
  file <- tempfile(fileext = ".png")
  ext <- dynaSpec:::page_png_int(p, 1, file, res = 50)
  expect_equal(unname(ext[c("left", "right", "top", "bottom")]), c(0, 1, 0, 1))

  # data reach the limits of the page and the frequency limits
  d <- p$spec[[1]]
  expect_lte(min(d$time), 0)
  expect_gte(max(d$time), 1)
  expect_gte(max(d$freq), p$yLim[2])
  expect_equal(dim(d$amp), c(length(d$freq), length(d$time)))

  # a tiny excess of sound does not make another page
  expect_equal(prep_paged(dynaSpec::scythebill, crop = FALSE, t.display = 1.5, wl = 200, plot = FALSE, verbose = FALSE, dest.path = tempdir())$n_pages, 3)
})

test_that("looping scrolling spectrograms match at the loop point", {
  # clicks at known times, also where the sound starts again
  f <- 22050
  y <- rep(0, 1.5 * f)
  for (t0 in c(0.06, 0.75, 1.44)) y[round(t0 * f) + 0:30] <- 20000 * sin(2 * pi * 4000 * (0:30) / f)
  w <- tuneR::Wave(left = round(y), samp.rate = f, bit = 16)
  t_d <- 0.5
  html <- save_dynaspec(dynaspec(w, flim = c(1, 8)) +
    ds_spectrogram(wl = 1024, ovlp = 90, collevels = seq(-40, 0, 2), pal = viridis::viridis, col.bg = "black") +
    ds_scrolling(t.display = t_d, width = 700, height = 200, res = 50, loop = 3), play = FALSE, verbose = FALSE)
  uris <- regmatches(html, gregexpr("data:image/png;base64,[A-Za-z0-9+/=]+", html))[[1]]
  strip <- png::readPNG(base64enc::base64decode(sub("^.*base64,", "", uris[2])))
  g <- strip[, , 1] + strip[, , 2] + strip[, , 3]
  px <- ncol(g) / (1.5 + t_d)
  n <- round(t_d * px)
  # best match between the image at the start of the sound and when it starts again
  lags <- -40:40
  cols <- 45:(n - 45)
  mse <- vapply(lags, function(l) mean((g[, cols] - g[, cols + round(1.5 * px) + l])^2), numeric(1))
  expect_lte(abs(lags[which.min(mse)]), 1)

  # clicks are at their true time in the image
  col_sum <- colSums(g)
  at <- function(t0) { r <- round((t0 + t_d / 2 - 0.05) * px):round((t0 + t_d / 2 + 0.05) * px); sum(r * col_sum[r]^4) / sum(col_sum[r]^4) / px - t_d / 2 }
  expect_equal(at(0.06), 0.06 + 15 / f, tolerance = 0.002 / 0.06)
  expect_equal(at(1.44), 1.44 + 15 / f, tolerance = 0.002 / 1.44)

  # strips of web pages move at the speed of the sound, even when it is a fraction of a video frame longer
  cfg <- save_dynaspec(dynaspec(seewave::cutw(w, from = 0, to = 0.1453, output = "Wave")) + ds_spectrogram(wl = 64) +
    ds_scrolling(t.display = 0.05, width = 300, height = 150, loop = 2), play = FALSE, verbose = FALSE)
  k <- as.numeric(sub('.*&quot;k&quot;:\\s*([0-9.e-]+).*|.*"k":\\s*([0-9.e-]+).*', "\\1\\2", cfg))
  expect_equal(k, 1, tolerance = 1e-3)
})

test_that("the player time waits for the audio and does not go back", {
  js <- dynaSpec:::player_js_int()
  expect_match(js, "if (ct === last) return wrap(t);", fixed = TRUE)
  expect_match(js, "t = Math.max(t, next + 0.15 * err)", fixed = TRUE)
  expect_false(grepl("Math.abs(err) > 0.15", js, fixed = TRUE))
})

test_that("ds_compare() stacks any number of sounds and applies layers to all", {
  a <- short_sound(0.8); b <- short_sound(1.2); d <- short_sound(1)
  x <- ds_compare(a, b, d, gap = 0.2) + ds_spectrogram(wl = 128) + ds_oscillogram()
  expect_s3_class(x, "dynaspec")
  expect_length(x$sounds, 3)
  expect_equal(x$name, "a_vs_b_vs_d")
  expect_true(all(vapply(x$sounds, function(s) s$spectrogram$wl == 128 && length(s$panels) == 1, logical(1))))

  # layers of dynaspec objects are kept, and a list of sounds with names can be used
  own <- ds_compare(dynaspec(a) + ds_captions("only here", 0, 0.5), b)
  expect_length(own$sounds[[1]]$captions, 1)
  expect_length(own$sounds[[2]]$captions, 0)
  expect_equal(ds_compare(list(one = a, two = b))$name, "one_vs_two")

  expect_error(ds_compare(a), "at least two sounds")
  expect_error(ds_compare(x, a), "cannot be used as sounds")
  expect_error(ds_compare(a, b, gap = -1), "gap")
  expect_error(save_dynaspec(ds_compare(dynaspec(a) + ds_paged(), dynaspec(b)) + ds_spectrogram(wl = 128), play = FALSE, verbose = FALSE), "same style")
})

test_that("ds_compare() uses the same frequency limits", {
  a <- short_sound(0.8)
  high <- tuneR::Wave(left = round(32000 * sin(2 * pi * 2000 * (1:22050) / 22050)), samp.rate = 22050, bit = 16)
  # (lowest Nyquist frequency when sampling rates differ, without warnings)
  expect_silent(x <- ds_compare(a, high))
  expect_equal(x$sounds[[2]]$flim, c(0, a@samp.rate / 2000))
  expect_equal(ds_compare(a, high, flim = c(1, Inf))$sounds[[1]]$flim, c(1, a@samp.rate / 2000))
  expect_warning(ds_compare(a, high, flim = c(1, 8)), "Nyquist")
  expect_error(ds_compare(a, high, flim = c(8, 9)), "Nyquist")
  # limits of a single sound are used for all; different ones are kept
  expect_equal(ds_compare(dynaspec(a, flim = c(1, 4)), high)$sounds[[2]]$flim, c(1, 4))
  y <- ds_compare(dynaspec(a, flim = c(1, 4)), dynaspec(high, flim = c(2, 5)))
  expect_equal(lapply(y$sounds, `[[`, "flim"), list(c(1, 4), c(2, 5)))
})

test_that("web pages of ds_compare() have all players, one script and the sounds themselves", {
  a <- short_sound(0.8); b <- short_sound(1.2); d <- short_sound(1)
  base <- ds_compare(a, b, d, gap = 0.2, flim = c(1, 5)) + ds_spectrogram(wl = 128)
  count <- function(pattern, x) lengths(regmatches(x, gregexpr(pattern, x, fixed = TRUE)))
  for (st in list(ds_static(width = 400, height = 200), ds_scrolling(t.display = 0.5, width = 300, height = 150), ds_paged(t.display = 0.5))) {
    html <- save_dynaspec(base + st, play = FALSE, verbose = FALSE)
    expect_s3_class(html, "dynaSpec_html")
    expect_match(html, 'class="dynaspec-compare" data-gap="0.2" data-durations="', fixed = TRUE)
    expect_equal(count("<audio", html), 3)
    expect_equal(count("<script>", html), 1)
    # (the script goes after the players)
    expect_gt(regexpr("<script>", html, fixed = TRUE), max(gregexpr("<audio", html, fixed = TRUE)[[1]]))
    # (the sounds are embedded as they are, also when the figures show silence after them)
    for (w in list(a, b, d)) expect_true(grepl(sound_data_int(w)$uri, html, fixed = TRUE))
  }
  # (the sequence is set in the script of the players)
  expect_match(player_js_int(), "initCompare", fixed = TRUE)
  file <- tempfile(fileext = ".html")
  save_dynaspec(base, file, play = FALSE, verbose = FALSE)
  expect_true(file.exists(file))
})

test_that("ds_compare() keeps the same time scale and stacks images", {
  a <- short_sound(0.8); b <- short_sound(1.6)
  base <- ds_compare(a, b, a) + ds_spectrogram(wl = 128) + ds_static(width = 400, height = 200)
  png_file <- tempfile(fileext = ".png")
  save_dynaspec(base, png_file, play = FALSE, verbose = FALSE)
  img <- png::readPNG(png_file)
  expect_equal(dim(img)[2], 400)
  # (only the last sound has a time axis title, and there is no band between figures with axes)
  expect_lt(dim(img)[1], 3 * 200)
  expect_gt(dim(img)[1], 3 * 150)
  expect_equal(seewave::duration(pad_wave_int(a, 1.6)), 1.6, tolerance = 1e-3)
  expect_identical(pad_wave_int(b, 1), b)

  # the part after the shorter sound is empty, unless each sound fits the figure
  ink <- function(m, rows) mean(m[rows, 300:370, 1:3] < 0.9)
  expect_lt(ink(img, 30:110), 0.001)
  save_dynaspec(ds_compare(a, b, same.scale = FALSE) + ds_spectrogram(wl = 128) + ds_static(width = 400, height = 200), png_file, play = FALSE, verbose = FALSE)
  expect_gt(ink(png::readPNG(png_file), 30:110), 0.001)

  # paged spectrograms: images per page, and pages as long as the longest sound by default
  files <- save_dynaspec(ds_compare(a, b) + ds_spectrogram(wl = 128) + ds_paged(t.display = 0.8), file.path(tempdir(), "cmp.png"), play = FALSE, verbose = FALSE)
  expect_length(files, 2)
  expect_true(all(file.exists(files)))
  expect_length(save_dynaspec(ds_compare(a, b) + ds_spectrogram(wl = 128) + ds_paged(), file.path(tempdir(), "cmp1.png"), play = FALSE, verbose = FALSE), 1)
  expect_error(save_dynaspec(ds_compare(a, b) + ds_spectrogram(wl = 128) + ds_scrolling(t.display = 0.5), png_file, play = FALSE, verbose = FALSE), "still images")
})

test_that("videos of ds_compare() play the sounds in sequence", {
  skip_if(!have_ffmpeg_exec2(), "ffmpeg not available")
  a <- short_sound(0.6); b <- short_sound(0.9); d <- short_sound(0.7)
  file <- tempfile(fileext = ".mp4")
  save_dynaspec(ds_compare(a, b, d, gap = 0.2, flim = c(1, 5)) + ds_spectrogram(wl = 128) + ds_static(width = 300, height = 150, fps = 10), file, play = FALSE, verbose = FALSE)
  info <- video_info_int(file)
  expect_equal(info$duration, 0.6 + 0.9 + 0.7 + 2 * 0.2, tolerance = 0.05)
  expect_equal(info$width, 300)
  # (only the last sound has a time axis title; no band between figures with axes)
  expect_lt(info$height, 3 * 150)
  expect_gt(info$height, 2 * 150)
})

test_that("frequency contours are measured and drawn at their time and frequency", {
  # sweep from 1 to 4 kHz in 2 s, after 1 s of silence
  f <- 22050
  tm <- seq_len(2 * f) / f
  sweep <- round(20000 * sin(2 * pi * (1000 * tm + 750 * tm^2)))
  w <- tuneR::Wave(left = c(rep(0L, f), sweep, rep(0L, f)), samp.rate = f, bit = 16)
  expected <- function(t) 1 + 1.5 * (t - 1)
  st <- data.frame(start = 1, end = 3)

  # measured: frequency at the time of each point, relative to the start of the selection
  m <- measure_contour_int(w, 1, 3, c(0, 11), "dominant", list(wl = 512))
  expect_equal(m$freq, expected(1 + m$time), tolerance = 0.03)
  expect_equal(min(m$time), 256 / f, tolerance = 1e-3)
  expect_equal(max(m$time), 2 - 256 / f, tolerance = 0.01)
  fund <- measure_contour_int(w, 1, 3, c(0, 5), "fundamental", list(wl = 512))
  expect_true(all(c("time", "freq") %in% names(fund)))
  # limits of the selection
  expect_true(all(measure_contour_int(w, 1, 3, c(2, 3), "dominant", list(wl = 512))$freq >= 2, na.rm = TRUE))
  expect_null(measure_contour_int(w, 1, 1.0002, c(0, 11), "dominant", list()))
  # gaps are kept when smoothing
  expect_equal(which(is.na(smooth_contour_int(c(1, 2, NA, 4, 5, 9, 7, 8), 3))), 3)
  # isolated jumps are removed, also at the ends, and steps are smoothed
  expect_equal(smooth_contour_int(c(9, 3, 3, 3, 3, 3, 3, 9), 3), rep(3, 8))
  expect_equal(smooth_contour_int(c(3, 3, 3, 3, 4, 4, 4, 4), 3), c(3, 3, 3, 10 / 3, 11 / 3, 4, 4, 4))
  expect_identical(smooth_contour_int(c(1, 5, 2), 1), c(1, 5, 2))

  # drawn at the right place of static spectrograms, also after cutting the sound
  red_y <- function(x, col_x) {
    file <- tempfile(fileext = ".png")
    save_dynaspec(x, file, play = FALSE, verbose = FALSE)
    img <- png::readPNG(file)
    red <- img[, col_x, 1] > 0.8 & img[, col_x, 2] < 0.3 & img[, col_x, 3] < 0.3
    list(y = mean(which(red)), rows = nrow(img), any = apply(img[, , 1] > 0.8 & img[, , 2] < 0.3 & img[, , 3] < 0.3, 2, any))
  }
  x <- dynaspec(w, flim = c(0, 5)) + ds_spectrogram(wl = 256, ovlp = 50) + ds_theme(axis.type = "none") + ds_static(width = 400, height = 300) +
    ds_annotations(st, alpha = 0, contour = "dominant", contour.col = "red")
  a <- red_y(x, 209)
  # (x = 209 is the middle of the sound, 2 s, or 2.5 kHz: without axes the plotting region goes from 1 line, 20 pixels,
  # to the right edge of the figure)
  expect_equal(1 - a$y / a$rows, 0.5, tolerance = 0.05)
  # (no contour before and after the selection)
  expect_true(!any(a$any[1:80]) && !any(a$any[320:400]))
  cut <- red_y(dynaspec(w, flim = c(0, 5), tlim = c(1, 3)) + ds_spectrogram(wl = 256, ovlp = 50) + ds_theme(axis.type = "none") + ds_static(width = 400, height = 300) +
    ds_annotations(st, alpha = 0, contour = "dominant", contour.col = "red"), 209)
  expect_equal(cut$y, a$y, tolerance = 0.05)

  # supplied contours: same place as the measured ones
  list_points <- dynaspec(w, flim = c(0, 5)) + ds_spectrogram(wl = 256, ovlp = 50) + ds_theme(axis.type = "none") + ds_static(width = 400, height = 300) +
    ds_annotations(st, alpha = 0, contour = list(data.frame(time = c(1, 3), freq = c(1, 4))), contour.col = "red")
  expect_equal(red_y(list_points, 209)$y, a$y, tolerance = 0.05)
  even <- dynaspec(w, flim = c(0, 5)) + ds_spectrogram(wl = 256, ovlp = 50) + ds_theme(axis.type = "none") + ds_static(width = 400, height = 300) +
    ds_annotations(st, alpha = 0, contour = data.frame(sound.files = "a", selec = 1, f1 = 1, f2 = 2.5, f3 = 4), contour.col = "red")
  expect_equal(red_y(even, 209)$y, a$y, tolerance = 0.05)
  # (numeric vector and missing contours)
  two <- data.frame(start = c(1, 2), end = c(1.5, 3))
  z <- ds_annotations(two, contour = list(NULL, c(1, 2)))
  expect_null(z$args$contour[[1]])
  expect_equal(z$args$contour[[2]]$time, c(0, 1))
})

test_that("contours work with all styles and selections of several sound files", {
  a <- short_sound(1.5)
  st <- data.frame(sound.files = c("a", "b", "b"), start = c(0.1, 0.2, 0.9), end = c(0.5, 0.7, 1.3))
  base <- dynaspec(a, flim = c(1, 5)) + ds_spectrogram(wl = 128) +
    ds_annotations(st, sound.file = "b", contour = list(c(9, 9), c(2, 3), c(3, 4)), contour.col = "red")
  sel <- measure_contours_int(sound_file_selections_int(base$selections[[1]], base), base)
  # (contours follow their selections when these are filtered)
  expect_equal(sel$X$.contour, 2:3)
  for (s in list(ds_static(width = 400, height = 200), ds_scrolling(t.display = 0.5, width = 300, height = 150), ds_scrolling(t.display = 0.5, width = 300, height = 150, loop = 2), ds_paged(t.display = 0.5)))
    expect_s3_class(save_dynaspec(base + s, play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(ds_compare(a, a, flim = c(1, 5)) + ds_spectrogram(wl = 128) + ds_annotations(data.frame(start = 0.2, end = 0.8), contour = "dominant"), play = FALSE, verbose = FALSE), "dynaSpec_html")
  # (contours on lines and colored spectrograms; only on spectrograms)
  for (ty in c("line", "spectrogram"))
    expect_s3_class(save_dynaspec(dynaspec(a) + ds_spectrogram(wl = 128) + ds_oscillogram() + ds_annotations(st[2:3, ], type = ty, contour = "dominant", sound.file = "b"), play = FALSE, verbose = FALSE), "dynaSpec_html")

  expect_error(ds_annotations(st, contour = "other"), "contour")
  expect_error(ds_annotations(st, contour = list(1)), "one element per selection")
  expect_error(ds_annotations(st, contour = data.frame(a = 1)), "one row per selection")
  expect_error(ds_annotations(st, contour = list(1, "a", 2)), "Each element")
  expect_error(ds_annotations(st, contour.args = list(foo = 1)), "contour.args")
  expect_error(ds_annotations(st, contour.col = "notacolor"), "contour.col")
})

test_that("sounds of comparisons other than the last one have no time axis title and spectrograms of the same height", {
  a <- short_sound(1)
  base <- dynaspec(a, flim = c(1, 5)) + ds_spectrogram(wl = 128)
  no_title <- function(x) { x$theme$time.title <- FALSE; x }

  # static: plotting regions (data-rows: top and bottom of the panels, as proportions of the figure) of the same height
  panel_px <- function(x) {
    html <- save_dynaspec(x, play = FALSE, verbose = FALSE)
    file <- tempfile(fileext = ".png")
    save_dynaspec(x, file, play = FALSE, verbose = FALSE)
    # (last row, which has the time axis)
    rows <- as.numeric(strsplit(utils::tail(strsplit(sub('.*data-rows="([^"]+)".*', "\\1", html), ";")[[1]], 1), ",")[[1]])
    h <- nrow(png::readPNG(file))
    c(fig = h, panel = (rows[6] - rows[5]) * h)
  }
  for (st in list(ds_static(width = 400, height = 300), ds_static(width = 400, height = 300, rows = 2))) {
    for (p in list(NULL, ds_oscillogram())) {
      x <- base + st
      if (!is.null(p)) x <- x + p
      with_title <- panel_px(x)
      without <- panel_px(no_title(x))
      expect_lt(without[["fig"]], with_title[["fig"]])
      expect_equal(without[["panel"]], with_title[["panel"]], tolerance = 2 / with_title[["panel"]])
    }
  }

  # scrolling: plotting region (as proportions of the frame) of the same height in pixels
  frame_px <- function(x) {
    html <- save_dynaspec(x, play = FALSE, verbose = FALSE)
    cfg <- regmatches(html, regexpr('<script type="application/json">[^<]+', html))
    num <- function(name) as.numeric(sub(paste0('.*"', name, '":([0-9.e-]+).*'), "\\1", cfg))
    img <- sub('.*<img class="ds-frame" srcset="data:image/png;base64,([^ ]+) 1x".*', "\\1", html)
    f <- tempfile(fileext = ".png")
    writeBin(base64_decode_test(img), f)
    h <- nrow(png::readPNG(f))
    c(fig = h, panel = (num("lowBottom") - num("upTop")) * h)
  }
  skip_if_not_installed("base64enc")
  base64_decode_test <- function(x) base64enc::base64decode(x)
  x <- base + ds_scrolling(t.display = 0.5, width = 300, height = 200)
  with_title <- frame_px(x)
  without <- frame_px(no_title(x))
  expect_lt(without[["fig"]], with_title[["fig"]])
  expect_equal(without[["panel"]], with_title[["panel"]], tolerance = 2 / with_title[["panel"]])

  # comparisons: the last sound keeps its title
  cmp <- ds_compare(a, a, a) + ds_spectrogram(wl = 128)
  html <- save_dynaspec(cmp + ds_static(width = 400, height = 300), play = FALSE, verbose = FALSE)
  heights <- vapply(regmatches(html, gregexpr('<img srcset="data:image/png;base64,[^ ]+', html))[[1]], function(u) {
    f <- tempfile(fileext = ".png")
    writeBin(base64enc::base64decode(sub('<img srcset="data:image/png;base64,', "", u)), f)
    nrow(png::readPNG(f))
  }, numeric(1))
  expect_equal(unname(heights[1]), unname(heights[2]))
  expect_gt(heights[3], heights[1])
  expect_false(grepl('style="--ds-cmp-gap', html, fixed = TRUE))
  # (figures without axes are separated)
  expect_match(save_dynaspec(cmp + ds_paged(t.display = 0.5), play = FALSE, verbose = FALSE), 'style="--ds-cmp-gap:8px"', fixed = TRUE)
})

test_that("static play lines take line type and width (also from themes)", {
  x <- dynaspec(short_sound(1), flim = c(1, 5)) + ds_spectrogram(wl = 128)
  attr_of <- function(html, name) { m <- regmatches(html, regexpr(paste0(name, '="[^"]*"'), html)); if (length(m)) sub('.*="([^"]*)"', "\\1", m) else NA }

  html <- save_dynaspec(x + ds_static(width = 400, height = 200), play = FALSE, verbose = FALSE)
  expect_true(is.na(attr_of(html, "data-lwd")) && is.na(attr_of(html, "data-dash")))
  html <- save_dynaspec(x + ds_static(width = 400, height = 200, res = 96) + ds_theme(lty.line = "dashed", lwd.line = 3), play = FALSE, verbose = FALSE)
  expect_equal(as.numeric(attr_of(html, "data-lwd")), 3)
  expect_equal(attr_of(html, "data-dash"), "4,4")
  expect_equal(attr_of(html, "data-line"), "true")
  # (blank lines are not shown; themes set the line when not supplied)
  expect_equal(attr_of(save_dynaspec(x + ds_theme(lty.line = 0), play = FALSE, verbose = FALSE), "data-line"), "false")
  html <- save_dynaspec(x + ds_theme(lty.line = "dotted", lwd.line = 2) + ds_static(width = 400, height = 200, res = 96), play = FALSE, verbose = FALSE)
  expect_equal(attr_of(html, "data-dash"), "1,3")
  expect_equal(as.numeric(attr_of(html, "data-lwd")), 2)
  expect_error(ds_theme(lty.line = "wavy"), "lty.line")
  # (the play line is set only in themes)
  expect_error(ds_static(lty = 2), "lty.line")
  expect_error(ds_scrolling(lcol = "red"), "col.line")
  expect_error(ds_paged(highlight.col = "red", lcol = "blue"), "col.highlight")
  expect_error(ds_static(foo = 1), "unused")

  # line patterns and images
  expect_null(lty_dash_int(1))
  expect_equal(lty_dash_int("blank"), 0)
  expect_equal(lty_dash_int(4), c(1, 3, 4, 3))
  img <- line_image_int(16, 2, "#FF000080", c(4, 4))
  expect_equal(dim(img), c(16, 2, 4))
  expect_equal(img[, 1, 4] > 0, rep(rep(c(TRUE, FALSE), each = 8), length.out = 16))
  expect_equal(max(img[, , 4]), 128 / 255)
  expect_true(all(img[, , 1] == 1))

  # videos: the line has the width supplied
  skip_if(!have_ffmpeg_exec2(), "ffmpeg not available")
  file <- tempfile(fileext = ".mp4")
  save_dynaspec(x + ds_theme(axis.type = "none") + ds_static(width = 400, height = 200, res = 96, fps = 10) + ds_theme(col.line = "red", lwd.line = 6), file, play = FALSE, verbose = FALSE)
  frame <- tempfile(fileext = ".png")
  system2(ffmpeg_exec2(), c("-hide_banner", "-loglevel", "error", "-y", "-ss", "0.5", "-i", shQuote(file), "-frames:v", "1", shQuote(frame)))
  m <- png::readPNG(frame)
  red <- which(m[100, , 1] > 0.7 & m[100, , 2] < 0.4 & m[100, , 3] < 0.4)
  expect_equal(length(red), 6, tolerance = 2 / 6)
})

# test video (needs ffmpeg with lavfi and libx264): white flash from 2 to 2.2 s and a 2 kHz beep at the same time, with the
# sound track starting 0.3 s after the video (as in some cameras)
make_test_video <- function(ext = "mp4") {
  file <- tempfile(fileext = paste0(".", ext))
  args <- paste("-hide_banner -loglevel error -y -f lavfi -i",
                shQuote("color=black:s=320x180:r=25:d=6,drawbox=x=0:y=0:w=iw:h=ih:color=white:t=fill:enable='between(t,2,2.2)'"),
                "-itsoffset 0.3 -f lavfi -i", shQuote("aevalsrc='if(between(t,1.7,1.9),0.8*sin(2*PI*2000*t),0.02*sin(2*PI*500*t))':s=22050:d=5.7"),
                "-map 0:v -map 1:a -c:v libx264 -pix_fmt yuv420p -c:a aac", shQuote(file))
  system2(ffmpeg_exec2(), args, stdout = FALSE, stderr = FALSE)
  if (file.exists(file)) file else NULL
}
# time (s) of the first loud sound and of the first bright frame (top or bottom quarter of the frames) of a video
beep_time <- function(w) { a <- stats::filter(abs(w@left), rep(1 / 200, 200)); which(a > 0.3 * max(a, na.rm = TRUE))[1] / w@samp.rate }
flash_time <- function(file, region = "top") {
  d <- tempfile(); dir.create(d)
  system2(ffmpeg_exec2(), c("-hide_banner -loglevel error -y -i", shQuote(file), "-vf fps=50", file.path(d, "f%05d.png")))
  br <- vapply(sort(list.files(d, full.names = TRUE)), function(f) {
    m <- png::readPNG(f); h <- dim(m)[1]
    mean(m[if (region == "top") 1:floor(h / 4) else floor(3 * h / 4):h, , 1])
  }, numeric(1))
  (which(br > 0.5)[1] - 1) / 50
}
video_sound <- function(file) {
  wav <- tempfile(fileext = ".wav")
  system2(ffmpeg_exec2(), c("-hide_banner -loglevel error -y -i", shQuote(file), "-vn -c:a pcm_s16le", shQuote(wav)))
  tuneR::readWave(wav)
}

test_that("dynaspec() reads the sound of videos, in step with their frames", {
  skip_if(!have_ffmpeg_exec2(), "ffmpeg not available")
  vid <- make_test_video()
  skip_if(is.null(vid), "ffmpeg cannot create test videos")
  x <- dynaspec(vid, flim = c(0, 5))
  expect_s4_class(x$wave, "Wave")
  expect_equal(x$video.file, normalizePath(vid))
  expect_equal(x$name, tools::file_path_sans_ext(basename(vid)))
  # (the sound track starts 0.3 s late; the beep is at 2 s of the video)
  expect_equal(beep_time(x$wave), 2, tolerance = 0.01 / 2)
  expect_equal(seewave::duration(x$wave), 6, tolerance = 0.05 / 6)

  expect_s3_class(ds_video(), "dynaspec_layer")
  expect_error(ds_video("left"), "position")
  expect_error(dynaspec(sub("\\.mp4$", ".mkv", vid)), "does\\s+not\\s+exist")
  # (videos without sound; ds_video() without a video)
  silent <- tempfile(fileext = ".mp4")
  system2(ffmpeg_exec2(), c("-hide_banner -loglevel error -y -f lavfi -i color=black:s=64x64:r=10:d=1 -c:v libx264", shQuote(silent)))
  expect_error(dynaspec(silent), "no sound track")
  expect_error(save_dynaspec(dynaspec(short_sound(1)) + ds_spectrogram(wl = 128) + ds_video(), play = FALSE, verbose = FALSE), "needs a video")
  expect_warning(save_dynaspec(x + ds_spectrogram(wl = 256) + ds_video() + ds_static(width = 300, height = 150), tempfile(fileext = ".png"), play = FALSE, verbose = FALSE), "still images")
})

test_that("videos with ds_video() keep the video in step with the sound in all styles", {
  skip_if(!have_ffmpeg_exec2(), "ffmpeg not available")
  vid <- make_test_video()
  skip_if(is.null(vid), "ffmpeg cannot create test videos")
  x <- dynaspec(vid, flim = c(0, 5), tlim = c(1, 4)) + ds_spectrogram(wl = 256)
  check <- function(layers, region = "top", expected = 1) {
    out <- tempfile(fileext = ".mp4")
    save_dynaspec(Reduce(`+`, layers, x), out, play = FALSE, verbose = FALSE)
    beep <- beep_time(video_sound(out))
    flash <- flash_time(out, region)
    expect_equal(beep, expected, tolerance = 0.03 / expected)
    # (within one frame)
    expect_lt(abs(flash - beep), 0.05)
    video_info_int(out)
  }
  info <- check(list(ds_video(), ds_static(width = 320, height = 150)))
  expect_equal(info$width, 320)
  expect_equal(info$height, 180 + 150)
  check(list(ds_video(position = "below"), ds_static(width = 320, height = 150)), region = "bottom")
  check(list(ds_video(), ds_paged(t.display = 0.5, fps = 25)))
  check(list(ds_video(), ds_scrolling(t.display = 0.5, width = 320, height = 150, fps = 25)))
  # (slowed down, with buffer: the beep is at (0.5 + 1) / 0.5 s)
  check(list(ds_video(), ds_scrolling(t.display = 0.5, width = 320, height = 150, fps = 25, speed = 0.5, buffer = 0.5)), expected = 3)
})

test_that("web pages with ds_video() embed the video in the player", {
  skip_if(!have_ffmpeg_exec2(), "ffmpeg not available")
  vid <- make_test_video()
  skip_if(is.null(vid), "ffmpeg cannot create test videos")
  x <- dynaspec(vid, flim = c(0, 5)) + ds_spectrogram(wl = 256)
  for (st in list(ds_static(width = 300, height = 150), ds_scrolling(t.display = 0.5, width = 300, height = 150, buffer = 0.2), ds_paged(t.display = 1))) {
    html <- save_dynaspec(x + ds_video(position = "below") + st, play = FALSE, verbose = FALSE)
    expect_match(html, '<video class="ds-video" muted playsinline preload="auto" src="data:video/mp4;base64,', fixed = TRUE)
    expect_match(html, 'data-video="below"', fixed = TRUE)
    expect_match(html, paste0('data-video-offset="', if (st$style == "scrolling") 0.2 else 0, '"'), fixed = TRUE)
  }
  # (without ds_video() the video is only used for its sound)
  expect_false(grepl("<video", save_dynaspec(x, play = FALSE, verbose = FALSE), fixed = TRUE))
  expect_match(player_js_int(), "videoSync", fixed = TRUE)
  # comparisons of videos
  expect_s3_class(save_dynaspec(ds_compare(x, x) + ds_video(), play = FALSE, verbose = FALSE), "dynaSpec_html")
})

test_that("paged videos keep the sound in step with the pages", {
  skip_if(!have_ffmpeg_exec2(), "ffmpeg not available")
  vid <- make_test_video()
  skip_if(is.null(vid), "ffmpeg cannot create test videos")
  x <- dynaspec(vid, flim = c(0, 5), tlim = c(1, 4)) + ds_spectrogram(wl = 256)
  out <- tempfile(fileext = ".mp4")
  save_dynaspec(x + ds_paged(t.display = 0.25, fps = 25), out, play = FALSE, verbose = FALSE)
  # (12 pages: the sound used to be about 23 ms later at each page)
  expect_equal(beep_time(video_sound(out)), 1, tolerance = 0.01)
  expect_equal(video_info_int(out)$duration, 3, tolerance = 0.02 / 3)
  # (relative file names)
  withr::with_dir(tempdir(), expect_true(file.exists(save_dynaspec(x + ds_paged(t.display = 1.5, fps = 10), "paged_relative.mp4", play = FALSE, verbose = FALSE))))
})

test_that("messages are cli alerts and saved files are reported once", {
  x <- dynaspec(short_sound(1), flim = c(1, 5)) + ds_spectrogram(wl = 128)
  file <- tempfile(fileext = ".png")
  msgs <- capture_messages(save_dynaspec(x + ds_static(width = 300, height = 150), file, play = FALSE))
  expect_length(msgs, 1)
  expect_match(msgs, "File saved at", fixed = TRUE)
  expect_match(msgs, basename(file), fixed = TRUE)
  expect_silent(save_dynaspec(x + ds_static(width = 300, height = 150), file, play = FALSE, verbose = FALSE))
  # (no message for web pages without file; several files of paged images and comparisons are reported together)
  expect_length(capture_messages(save_dynaspec(x, play = FALSE)), 0)
  msgs <- capture_messages(save_dynaspec(x + ds_paged(t.display = 0.5), file.path(tempdir(), "pages.png"), play = FALSE))
  expect_match(paste(msgs, collapse = ""), "Files saved at", fixed = TRUE)
  expect_false(any(grepl("Segmented", msgs)))
  msgs <- capture_messages(save_dynaspec(ds_compare(short_sound(1), short_sound(1)) + ds_spectrogram(wl = 128), file, play = FALSE))
  expect_length(grep("saved at", msgs), 1)
})

test_that("ds_video() sets the height of videos and figures go right below them", {
  expect_error(ds_video(height = 0), "height")
  expect_error(ds_video(height = "a"), "height")
  expect_equal(ds_video(height = 0.5)$args, list(position = "above", height = 0.5))

  # figures without top margin (static spectrograms: the panels start at the top of the figure)
  x <- dynaspec(short_sound(1), flim = c(1, 5)) + ds_spectrogram(wl = 128) + ds_static(width = 300, height = 150)
  top_of <- function(x) as.numeric(strsplit(sub('.*data-rows="([^"]+)".*', "\\1", save_dynaspec(x, play = FALSE, verbose = FALSE)), ",")[[1]][5])
  expect_gt(top_of(x), 0.02)
  y <- x
  y$theme$top.margin <- FALSE
  expect_equal(top_of(y), 0, tolerance = 0.005)

  skip_if(!have_ffmpeg_exec2(), "ffmpeg not available")
  vid <- make_test_video()
  skip_if(is.null(vid), "ffmpeg cannot create test videos")
  v <- dynaspec(vid, flim = c(0, 5)) + ds_spectrogram(wl = 256) + ds_static(width = 320, height = 150)
  out <- tempfile(fileext = ".mp4")
  save_dynaspec(v + ds_video(height = 0.6), out, play = FALSE, verbose = FALSE)
  info <- video_info_int(out)
  expect_equal(info$width, 320)
  # (video 320 x 180 pixels, figure 180 / 0.6 = 300 pixels high)
  expect_equal(info$height, 180 + 300, tolerance = 4 / 480)
  # (the video takes the width of the figure, 320 x 180 pixels, and the figure is as high as the video divided by 'height')
  save_dynaspec(v + ds_video(height = 1.5), out, play = FALSE, verbose = FALSE)
  expect_equal(video_info_int(out)$height, 180 + 120, tolerance = 4 / 300)
  expect_equal(video_figure_size_int(v + ds_video(height = 0.5))$style$args$height, 360)
  expect_equal(video_figure_size_int(v + ds_video(height = 1) + ds_scrolling(width = 640))$style$args$height, 360)
  expect_equal(video_figure_size_int(v + ds_video(height = 2) + ds_paged(width = 1200))$style$args$height, round(1200 * 180 / 320 / 2))
  expect_error(save_dynaspec(v + ds_video(height = 3), out, play = FALSE, verbose = FALSE), "too little room")
  html <- save_dynaspec(v + ds_video(position = "below", height = 0.6), play = FALSE, verbose = FALSE)
  expect_false(grepl("data-video-height", html, fixed = TRUE))
  # (no top margin under a video above the figure only)
  expect_gt(as.numeric(strsplit(sub('.*data-rows="([^"]+)".*', "\\1", html), ",")[[1]][5]), 0.02)
  html <- save_dynaspec(v + ds_video(), play = FALSE, verbose = FALSE)
  expect_equal(as.numeric(strsplit(sub('.*data-rows="([^"]+)".*', "\\1", html), ",")[[1]][5]), 0, tolerance = 0.005)
})

test_that("palettes can be two or more colors in all styles and themes", {
  x <- dynaspec(short_sound(1), flim = c(1, 5)) + ds_spectrogram(wl = 128, pal = c("white", "gold", "red", "black"))
  for (st in list(ds_static(width = 300, height = 150), ds_scrolling(t.display = 0.5, width = 300, height = 150), ds_paged(t.display = 0.5)))
    expect_s3_class(save_dynaspec(x + st, play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(dynaspec(short_sound(1)) + ds_spectrogram(wl = 128) + ds_theme(pal = c("white", "#21908C", "black")), play = FALSE, verbose = FALSE), "dynaSpec_html")
  # (gradient through all the colors, from low to high amplitude)
  cols <- pal_fun_int(c("white", "red", "black"))(3)
  expect_equal(toupper(cols), c("#FFFFFF", "#FF0000", "#000000"))
  expect_equal(low_col_int(c("white", "red", "black")), "#FFFFFF")
  expect_error(ds_theme(pal = c("white", "notacolor")), "two or more colors")
  expect_error(ds_theme(pal = "notapalette"), "two or more colors")
  expect_error(save_dynaspec(x + ds_spectrogram(pal = c("white", "nope")) + ds_paged(), play = FALSE, verbose = FALSE), "valid color")
})

test_that("a single y axis title is drawn for rows and sounds with a single type of panel", {
  # (groups of rows with dark pixels in the column of the y axis title, at the left edge of the figure)
  title_groups <- function(file) {
    m <- png::readPNG(file)
    dark <- apply(m[, 1:22, 1:3, drop = FALSE] < 0.5, 1, any)
    # (gaps between the letters and words of a title are part of it)
    r <- rle(dark)
    r$values[!r$values & r$lengths <= 15 & c(FALSE, r$values[-length(r$values)]) & c(r$values[-1], FALSE)] <- TRUE
    r <- rle(inverse.rle(r))
    list(n = sum(r$values & r$lengths > 3), rows = which(dark), height = nrow(m))
  }
  png_of <- function(x) { f <- tempfile(fileext = ".png"); save_dynaspec(x, f, play = FALSE, verbose = FALSE); f }
  x <- dynaspec(short_sound(2), flim = c(1, 5)) + ds_spectrogram(wl = 128)
  # rows: one title (spectrograms only, or oscillograms only) centered on the panels of all rows
  rows2 <- x + ds_static(rows = 2, width = 400, height = 200)
  one <- title_groups(png_of(rows2))
  expect_equal(one$n, 1)
  lay <- attr(save_dynaspec(rows2, play = FALSE, verbose = FALSE), "layout")
  expect_equal(mean(range(one$rows)) / one$height, mean(c(lay$top, lay$bottom)), tolerance = 0.03)
  expect_equal(title_groups(png_of(dynaspec(short_sound(2)) + ds_oscillogram() + ds_static(rows = 3, width = 400, height = 150)))$n, 1)
  # (several types of panels: one title per panel)
  expect_gt(title_groups(png_of(x + ds_oscillogram() + ds_static(rows = 2, width = 400, height = 300)))$n, 1)

  # comparisons: a single title across the stacked figures (it crosses the boundary between them)
  cmp <- ds_compare(short_sound(1), short_sound(1.5)) + ds_spectrogram(wl = 128) + ds_static(width = 400, height = 200)
  expect_true(compare_ylab_int(cmp$sounds))
  f <- png_of(cmp)
  g <- title_groups(f)
  expect_equal(g$n, 1)
  first_height <- attr(save_dynaspec(cmp$sounds[[1]] + ds_theme(), play = FALSE, verbose = FALSE), "layout")$height
  expect_true(min(g$rows) < first_height && max(g$rows) > first_height)
  # (not for other styles, mixed panels or videos)
  expect_false(compare_ylab_int((cmp + ds_oscillogram())$sounds))
  expect_false(compare_ylab_int((cmp + ds_scrolling(t.display = 0.5) + ds_overview())$sounds))
  # (scrolling spectrograms without lower panel, and paged spectrograms with standard axes)
  expect_true(compare_ylab_int((cmp + ds_scrolling(t.display = 0.5))$sounds))
  expect_false(compare_ylab_int((cmp + ds_paged(t.display = 0.5))$sounds))
  expect_true(compare_ylab_int((cmp + ds_paged(t.display = 0.5) + ds_theme(axis.type = "standard"))$sounds))
  html <- save_dynaspec(cmp + ds_scrolling(t.display = 0.5, width = 400, height = 200), play = FALSE, verbose = FALSE)
  expect_s3_class(html, "dynaSpec_html")
  expect_false(compare_ylab_int((cmp + ds_theme(axis.type = "inside"))$sounds))
})

test_that("axis titles fit in the figure at any size", {
  # (larger titles widen the margins: the y axis title is not cut at the left edge, and the time axis title stays below
  # the labels)
  expect_gt(title_margin_int(2.4, 2.4), title_margin_int(2.4, 1.2))
  expect_gt(xlab_line_int(1.9, 2.4), xlab_line_int(1.9, 1.2))
  expect_equal(xlab_margin_int(1.9, 1.2) - xlab_line_int(1.9, 1.2), 1 + 0.2 * 1.2)
  for (cx in c(1, 2.5)) {
    x <- dynaspec(short_sound(1), flim = c(1, 5)) + ds_spectrogram(wl = 128) + ds_theme(cex.lab = cx)
    f <- tempfile(fileext = ".png")
    save_dynaspec(x + ds_static(width = 400, height = 250), f, play = FALSE, verbose = FALSE)
    m <- png::readPNG(f)
    # (no dark pixels of the title at the left edge of the figure)
    expect_false(any(m[, 1, 1:3] < 0.5))
  }
  # (long titles are made smaller to fit the height of the figure; titles shared by comparisons are not)
  png_device_int(tempfile(fileext = ".png"), width = 400, height = 100, res = 100)
  graphics::par(mar = rep(0, 4))
  graphics::plot.new()
  expect_lt(fit_ylab_cex_int("Frequency (kHz)", 3, 0.5), 3)
  expect_equal(fit_ylab_cex_int("kHz", 1, 0.5), 1)
  grDevices::dev.off()
})

test_that("the window length can be set in milliseconds (wl.ms)", {
  expect_equal(wl_from_ms_int(11025, 20), 220)
  expect_equal(wl_from_ms_int(44100, 20), 882)
  expect_equal(wl_from_ms_int(22050, 1) %% 2, 0)
  expect_error(ds_spectrogram(wl.ms = -1), "wl.ms")
  png_of <- function(x) { f <- tempfile(fileext = ".png"); save_dynaspec(x + ds_static(width = 300, height = 150), f, play = FALSE, verbose = FALSE); png::readPNG(f) }
  a <- short_sound(1)
  # (the same figure as with the window length in samples at the sampling rate of the sound; 'wl.ms' takes precedence;
  # default: 512 samples at 44.1 kHz)
  expect_identical(png_of(dynaspec(a) + ds_spectrogram(wl.ms = 20)), png_of(dynaspec(a) + ds_spectrogram(wl = wl_from_ms_int(a@samp.rate, 20))))
  expect_identical(png_of(dynaspec(a) + ds_spectrogram(wl = 128, wl.ms = 20)), png_of(dynaspec(a) + ds_spectrogram(wl.ms = 20)))
  expect_identical(png_of(dynaspec(a) + ds_spectrogram()), png_of(dynaspec(a) + ds_spectrogram(wl.ms = 512 / 44.1)))
  expect_equal(resolve_wl_int(44100), 512)
  expect_equal(resolve_wl_int(11025, wl = 300), 300)
  expect_equal(resolve_wl_int(11025, wl = 300, wl.ms = 20), 220)
  expect_error(ds_annotations(data.frame(start = 0, end = 1), contour.args = list(wl.ms = 10)), NA)
  expect_false(identical(png_of(dynaspec(a) + ds_spectrogram(wl.ms = 5)), png_of(dynaspec(a) + ds_spectrogram(wl.ms = 40))))
  # (scrolling and paged spectrograms, and comparisons of sounds with different sampling rates)
  for (st in list(ds_scrolling(t.display = 0.5, width = 300, height = 150), ds_paged(t.display = 0.5)))
    expect_s3_class(save_dynaspec(dynaspec(a) + ds_spectrogram(wl.ms = 15) + st, play = FALSE, verbose = FALSE), "dynaSpec_html")
  b <- tuneR::Wave(left = round(32000 * sin(2 * pi * 2000 * (1:44100) / 44100)), samp.rate = 44100, bit = 16)
  expect_s3_class(save_dynaspec(ds_compare(a, b) + ds_spectrogram(wl.ms = 15), play = FALSE, verbose = FALSE), "dynaSpec_html")
})

test_that("ds_filter() filters the sound that is shown and heard", {
  expect_s3_class(ds_filter(c(1, 3)), "dynaspec_layer")
  expect_error(ds_filter(c(3, 1)), "bp")
  expect_error(ds_filter(c(0, Inf)), "does not filter")
  expect_error(ds_filter(1), "bp")
  # tones at 0.5 and 3 kHz, the first one removed by a high-pass filter
  f <- 22050
  t <- seq_len(f) / f
  w <- tuneR::Wave(left = round(10000 * sin(2 * pi * 500 * t) + 10000 * sin(2 * pi * 3000 * t)), samp.rate = f, bit = 16)
  power <- function(x, lo, hi) { s <- seewave::meanspec(x, plot = FALSE); sum(s[s[, 1] > lo & s[, 1] < hi, 2]) }
  hp <- filter_wave_int(w, c(1.5, Inf))
  expect_equal(length(hp@left), length(w@left))
  expect_lt(power(hp, 0.3, 0.7) / power(hp, 2.8, 3.2), 0.05)
  lp <- filter_wave_int(w, c(0, 1.5))
  expect_lt(power(lp, 2.8, 3.2) / power(lp, 0.3, 0.7), 0.05)
  # (limits beyond the Nyquist frequency are not applied; lower limits beyond it give an error)
  expect_identical(filter_wave_int(w, c(0, 20)), w)
  expect_error(filter_wave_int(w, c(12, 13)), "Nyquist")
  # (no shift in time: a click stays in place)
  x <- numeric(f); x[5000] <- 20000
  click <- filter_wave_int(tuneR::Wave(left = x, samp.rate = f, bit = 16), c(1, 5))
  expect_equal(which.max(abs(click@left)), 5000)
  # stereo sounds keep both channels
  expect_true(filter_wave_int(tuneR::stereo(w, w), c(1.5, Inf))@stereo)
  # the sound embedded in web pages is the filtered one
  x <- dynaspec(w) + ds_spectrogram(wl.ms = 10) + ds_filter(c(1.5, Inf))
  html <- save_dynaspec(x, play = FALSE, verbose = FALSE)
  expect_true(grepl(sound_data_int(hp)$uri, html, fixed = TRUE))
  for (st in list(ds_scrolling(t.display = 0.5, width = 300, height = 150), ds_paged(t.display = 0.5)))
    expect_s3_class(save_dynaspec(x + st, play = FALSE, verbose = FALSE), "dynaSpec_html")
  expect_s3_class(save_dynaspec(ds_compare(w, w) + ds_filter(c(1.5, Inf)), play = FALSE, verbose = FALSE), "dynaSpec_html")
})

test_that("removed and renamed arguments point to their replacements", {
  expect_error(ds_paged(filter = c(0, 1)), "ds_filter")
  expect_error(ds_scrolling(height.prop = c(5, 1)), "height")
  expect_error(ds_scrolling(hop.size = 10), "wl.ms")
  expect_error(ds_paged(spec.width = 4), "pixels")
  expect_error(ds_paged(plot.legend = TRUE), "legend")
  expect_error(ds_spectrogram(colbg = "black"), "col.bg")
  expect_error(ds_theme(colbg = "black"))
  # paged pages in pixels: images are width x height pixels
  x <- dynaspec(short_sound(1), flim = c(1, 5)) + ds_spectrogram(wl.ms = 10)
  files <- save_dynaspec(x + ds_paged(t.display = 1, width = 600, height = 200, res = 100, legend = TRUE), file.path(tempdir(), "px.png"), play = FALSE, verbose = FALSE)
  expect_equal(dim(png::readPNG(files[1]))[1:2], c(200, 600))
  # col.bg in spectrograms and themes
  png_of <- function(x) { f <- tempfile(fileext = ".png"); save_dynaspec(x + ds_static(width = 300, height = 150), f, play = FALSE, verbose = FALSE); png::readPNG(f) }
  expect_identical(png_of(x + ds_spectrogram(col.bg = "red")), png_of(x + ds_theme(col.bg = "red")))
  # scrolling web pages start at the first speed of ds_player()
  html <- save_dynaspec(x + ds_scrolling(t.display = 0.5, width = 300, height = 150, speed = 0.5), play = FALSE, verbose = FALSE)
  expect_match(html, 'data-speeds="1,0.5,0.25,0.125"', fixed = TRUE)
})

test_that("scrolling spectrograms keep the window within the sound (still ends)", {
  x <- dynaspec(short_sound(2), flim = c(1, 5)) + ds_spectrogram(wl.ms = 10)
  cfg_of <- function(st) { h <- save_dynaspec(x + st, play = FALSE, verbose = FALSE); regmatches(h, regexpr('<script type="application/json">[^<]+', h)) }
  expect_match(cfg_of(ds_scrolling(t.display = 0.5, width = 300, height = 150)), '"stillEnds":true', fixed = TRUE)
  expect_match(cfg_of(ds_scrolling(t.display = 0.5, width = 300, height = 150, still.ends = FALSE)), '"stillEnds":false', fixed = TRUE)
  # (looping spectrograms scroll continuously)
  expect_match(cfg_of(ds_scrolling(t.display = 0.5, width = 300, height = 150, loop = 2)), '"stillEnds":false', fixed = TRUE)
  expect_error(save_dynaspec(x + ds_scrolling(still.ends = "yes"), play = FALSE, verbose = FALSE), "still.ends")

  # videos: the play line moves at the start and the end, and stays at the center in the middle
  skip_if(!have_ffmpeg_exec2(), "ffmpeg not available")
  out <- tempfile(fileext = ".mp4")
  save_dynaspec(x + ds_theme(col.line = "red", lty.line = 1, axis.type = "none") + ds_scrolling(t.display = 1, width = 400, height = 200, fps = 10), out, play = FALSE, verbose = FALSE)
  line_x <- function(t) {
    f <- tempfile(fileext = ".png")
    system2(ffmpeg_exec2(), c("-nostdin -hide_banner -loglevel error -y -ss", t, "-i", shQuote(out), "-frames:v 1", shQuote(f)))
    m <- png::readPNG(f)
    mean(which(colSums(m[, , 1] > 0.7 & m[, , 2] < 0.4 & m[, , 3] < 0.4) > 0)) / dim(m)[2]
  }
  expect_equal(line_x(0.2), 0.2, tolerance = 0.05 / 0.2)
  expect_equal(line_x(1), 0.5, tolerance = 0.05)
  expect_equal(line_x(1.8), 0.8, tolerance = 0.05 / 0.8)
})

