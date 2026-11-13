# web pages: writing, showing and embedding (images, sounds and settings)

# black or white, whichever contrasts best with the background color
contrast_col_int <- function(bg) {
  bgRGB <- grDevices::col2rgb(bg)
  if (bgRGB["red", 1] * 0.299 + bgRGB["green", 1] * 0.587 + bgRGB["blue", 1] * 0.114 > 149)
    "#000000" else "#ffffff"
}


# sound to embed: wave object (for plotting) and data uri
# sound files are embedded as they are; Wave objects are written as wav
sound_data_int <- function(x, read = TRUE) {
  if (inherits(x, "Wave")) {
    wav_file <- tempfile(fileext = ".wav")
    on.exit(unlink(wav_file), add = TRUE)
    # wav files must be integer (normalize non-integer waves)
    w <- if (x@pcm) x else tuneR::normalize(x, unit = "16")
    tuneR::writeWave(w, filename = wav_file)
    return(list(wave = x, uri = base64_uri_int(wav_file, "audio/wav")))
  }

  ext <- tolower(tools::file_ext(x))
  list(
    wave = if (read) read_sound_int(x)$wave,
    uri = base64_uri_int(x, if (ext == "mp3") "audio/mpeg" else "audio/wav")
  )
}


# minimal JSON for settings passed to the web pages (named lists are objects, vectors of length > 1 are arrays)
json_int <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    vals <- vapply(x, json_int, character(1))
    return(if (!is.null(names(x))) paste0("{", paste0('"', names(x), '":', vals, collapse = ","), "}") else paste0("[", paste(vals, collapse = ","), "]"))
  }
  vals <- if (is.character(x)) {
    # escape quotes, backslashes and "<" (so labels cannot close the script element)
    paste0('"', gsub("<", "\\\\u003c", gsub('(["\\\\])', "\\\\\\1", x)), '"')
  } else if (is.logical(x)) tolower(x) else vapply(unname(x), format, character(1), digits = 10, scientific = FALSE)
  if (length(x) == 1) vals else paste0("[", paste(vals, collapse = ","), "]")
}


# base64 encode a file as a data uri
base64_uri_int <- function(file, mime) {
  paste0("data:", mime, ";base64,", base64_enc_int(readBin(file, "raw", n = file.size(file))))
}


# base64 encoding of a raw vector
base64_enc_int <- function(x) {
  chars <- c(LETTERS, letters, 0:9, "+", "/")
  n <- length(x)
  if (n == 0) return("")
  pad <- (3 - n %% 3) %% 3
  m <- matrix(c(as.integer(x), integer(pad)), nrow = 3)
  v <- m[1, ] * 65536L + m[2, ] * 256L + m[3, ]
  out <- chars[rbind(v %/% 262144L, (v %/% 4096L) %% 64L, (v %/% 64L) %% 64L, v %% 64L) + 1L]
  if (pad > 0) out[length(out) - seq_len(pad) + 1] <- "="
  paste(out, collapse = "")
}


# color as hex
col2hex_int <- function(col, alpha = FALSE) {
  rgb <- grDevices::col2rgb(col, alpha = alpha)
  if (alpha) grDevices::rgb(rgb[1], rgb[2], rgb[3], rgb[4], maxColorValue = 255) else grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
}


html_escape_int <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}


# return html (embedded when knitting), save it as a web page and open it
output_html_int <- function(html, file.name, play) {
  html <- structure(html, class = "dynaSpec_html")

  if (!is.null(file.name))
    write_page_int(html, file.name)

  if (isTRUE(getOption("knitr.in.progress")) && requireNamespace("knitr", quietly = TRUE))
    return(knitr::asis_output(paste0("\n```{=html}\n", html, "\n```\n")))

  if (play) {
    if (!is.null(file.name))
      open_file_int(normalizePath(file.name))
    else if (interactive())
      view_html_int(html)
  }

  invisible(html)
}


# write html as a standalone web page
write_page_int <- function(html, file.name, title = tools::file_path_sans_ext(basename(file.name))) {
  writeLines(c(
    "<!DOCTYPE html>",
    '<html lang="en">',
    "<head>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    paste0("<title>", html_escape_int(title), "</title>"),
    "<style>body{font-family:system-ui,sans-serif;max-width:1000px;margin:2em auto;padding:0 16px;background:#fff;color:#222}</style>",
    "</head>",
    "<body>",
    html,
    "</body>",
    "</html>"
  ), con = file.name, useBytes = TRUE)
}


# preview html in the IDE viewer (e.g. RStudio) or the web browser
view_html_int <- function(html) {
  # the viewer only shows files in the session temporary directory
  view_dir <- tempfile("dynaSpec_view_")
  dir.create(view_dir)
  file <- file.path(view_dir, "index.html")
  write_page_int(html, file, title = "dynaSpec player")

  viewer <- getOption("viewer")
  if (is.function(viewer)) viewer(file) else utils::browseURL(file)
}
