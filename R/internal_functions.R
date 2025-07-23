# internal functions not to be called by users
# stop function that doesn't print call
stop2 <- function (...)
{
  stop(..., call. = FALSE)
}

# stop function that doesn't print call
warning2 <- function (...)
{
  warning(..., call. = FALSE)
}

# functions adopted from ari to check if ffmpeg is installed
ffmpeg_exec2 <- function(quote = FALSE) {
  candidates <- c(Sys.getenv("ffmpeg"), Sys.which("ffmpeg"))
  ffmpeg <- candidates[nchar(candidates) > 0][1]
  if (is.na(ffmpeg)) {
    stop("Could not find ffmpeg")
  }
  if (!ffmpeg %in% c("ffmpeg", "ffmpeg.exe")) {
    ffmpeg = normalizePath(ffmpeg, winslash = "/")
  }
  if (quote) {
    ffmpeg = shQuote(ffmpeg)
  }
  return(ffmpeg)
}

have_ffmpeg_exec2 <- function(){
  exec = try({
    ffmpeg_exec2()
  }, silent = TRUE)
  !inherits(exec, "try-error")
}
