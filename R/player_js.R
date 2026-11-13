# javascript of the web players (inst/js/player.js)

# javascript for the players, in a script element (read once per session; the players are defined once per page, even if
# included several times)
player_js_int <- function() {
  if (is.null(.dynaspec_env$player_js)) {
    file <- system.file("js", "player.js", package = "dynaSpec")
    if (!nzchar(file))
      stop2("the javascript of the web players (js/player.js) was not found in the dynaSpec installation")
    js <- readLines(file, warn = FALSE, encoding = "UTF-8")
    # (the comments at the top of the file are not included in web pages)
    js <- js[-seq_len(match(FALSE, startsWith(js, "//")) - 1)]
    assign("player_js", paste0("<script>\n", paste(js, collapse = "\n"), "\n</script>"), envir = .dynaspec_env)
  }
  .dynaspec_env$player_js
}
