# helpers shared by tests

# short example clip
short_wren <- function(dur = 2) {
  seewave::cutw(dynaSpec::canyon_wren, from = 0, to = dur, output = "Wave")
}

# example sound files included in the package
example_wavs <- function() {
  list.files(system.file(package = "dynaSpec"), pattern = "wav$", full.names = TRUE)
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
