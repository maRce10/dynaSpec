# helpers shared by tests

# short example clip
short_sound <- function(dur = 2) {
  seewave::cutw(dynaSpec::scythebill, from = 1, to = 1 + dur, output = "Wave")
}

# example sound files (barn swallow data written as .wav files, to test reading sound files)
example_wavs <- function() {
  files <- file.path(tempdir(), c("femaleBarnSwallow.wav", "maleBarnSwallow.wav"))
  if (!all(file.exists(files))) {
    tuneR::writeWave(dynaSpec::female_barn_swallow, files[1], extensible = FALSE)
    tuneR::writeWave(dynaSpec::male_barn_swallow, files[2], extensible = FALSE)
  }
  files
}

# plot to a null device (closed when the calling test ends) so no Rplots.pdf is left behind
null_device <- function(env = parent.frame()) {
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  withr::defer(grDevices::dev.off(dev), envir = env)
}

skip_if_no_ffmpeg <- function() {
  skip_if_not(dynaSpec:::have_ffmpeg_exec2(), "ffmpeg not available")
}

# row extents of a static_spectro() (one row per segment: t0, t1, left, right, top, bottom)
player_rows <- function(html) {
  rows <- sub('"$', "", sub('^data-rows="', "", regmatches(html, regexpr('data-rows="[^"]+"', html))))
  m <- do.call(rbind, lapply(strsplit(strsplit(rows, ";")[[1]], ","), as.numeric))
  colnames(m) <- c("t0", "t1", "left", "right", "top", "bottom")
  as.data.frame(m)
}

# run a call with another function, keeping argument expressions (so names of Wave objects are preserved):
# engine tests call the internal engines through these
call_with_int <- function(fun) {
  function(...) {
    cl <- sys.call()
    cl[[1]] <- fun
    eval(cl, parent.frame())
  }
}
static_engine <- call_with_int(quote(dynaSpec:::static_spectro_int))
prep_paged <- call_with_int(quote(dynaSpec:::prep_paged_int))
paged_engine <- call_with_int(quote(dynaSpec:::paged_spectro_int))
scrolling_engine <- call_with_int(quote(dynaSpec:::scrolling_spectro_int))
