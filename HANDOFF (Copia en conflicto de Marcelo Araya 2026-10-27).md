# dynaSpec layered syntax (branch `layered-syntax`): handoff notes

Last updated 2026-10-27 (Claude Code session). Give this file to a new session and ask it to read it first.

## Where things are

- Repository: `~/Dropbox/R_package_testing/dynaSpec`. Branch **`layered-syntax`** = dynaSpec **2.0.0.9000** (new layered syntax, HTML web players, videos). `master` stays at 1.1.0 (video-only functions); do not touch it unless asked.
- The separate `ggdynaspec` package was dropped (folder deleted); its work was committed on this branch as `8b92015`.
- Commits on the branch (not pushed): `8b92015` (layered syntax) and **`25cf9a0` "pimp themes"** (2026-10-27 09:45): everything up to the themes, player buttons/slider, left spectrum axis, `spectro_int()`, vignette style and the R CMD check fixes.
- **Uncommitted** (working tree, see `git status`): the sections "Xeno-canto theme", "Example sound" and "Macaulay theme and zoom" below (`canyon_wren` deleted; `scaly_breasted_hummingbird` files untracked: `R/scaly_breasted_hummingbird-data.R`, `data/`, `man/scaly_breasted_hummingbird.Rd`). Commit only when the user asks.

## Current interface (exported)

```r
x <- dynaspec(wave, flim = c(2, 11), crop = c(0, 12)) +    # Wave object or local .wav/.mp3 (example data: scaly_breasted_hummingbird)
  spectrogram(wl = 300, ovlp = 90, wn, pal = viridis::mako, collevels = seq(-60, 0, 1), colbg = "white", amp.trans = 1, ...) +
  oscillogram(position = "below", col = NULL, height = NULL) +     # "above"/"below"; col NULL = theme col.wave or "black"
  envelope(position = "below", col = NULL, height = NULL) +        # amplitude envelope; col NULL = theme or "#31688E"
  spectrum(side = "right", col = NULL, width = 0.15) +             # power spectrum, "left" puts the frequency axis on it
  overview(height = NULL) +                                          # lower whole-sound panel (scrolling only)
  selections(st, col = "#E69F00", alpha = 0.3, border = NULL, lwd = 1, labels = NULL, cex = 1, freq = TRUE, sound.file = NULL) +
  captions(labels, start, end, x = 0.5, y = 0.9, fading = 0, col = "black", cex = 1, font = 1) +
  dynatheme(xlab, ylab, cex.lab, cex.axis, font.lab, col.lab, axis.type, bg, family, las, tick.length, box, col.box,
            grid, col.grid, col.panel, col.title, cex.title, pal, colbg, col.wave, col.line, col.highlight, col.bar, bg.bar,
            lty.line, lwd.line, bar.style) +   # axis.type also "inside"/"compact"; bar.style "rounded"/"square"
  # or presets: dynatheme_dark(), _presentation(), _paper(), _minimal(), _transparent(), _void(), _xenocanto(), _macaulay()
  player(speeds = c(1, 0.5, 0.25, 0.125), keep.pitch = FALSE, buttons = "time") +  # + "loop", "volume", "download", "fullscreen", "progress", "zoom" (scrolling)
  static(rows, width, height, res, lcol, alt, fps, play.line = TRUE)  # or scrolling(...) / paged(...)

x                                     # web page in viewer / embedded when knitting
save_dynaspec(x, "song.html")         # also ".mp4" (ffmpeg) and ".png"; cli progress bar
```

Rules decided with the user:
- **Panels only come from layers** (like ggplot): `dynaspec()` alone = empty panel with time axis + play line; `oscillogram()` alone = only oscillogram. `scrolling()`/`paged()` require `spectrogram()` (error otherwise). `static(spectro=)` was removed.
- Layers store only supplied args; style defaults otherwise. `+` merges `spectrogram()`, `dynatheme()` and styles; replaces oscillogram/envelope (same type), spectrum, overview, player; appends selections and captions.
- `captions()` (renamed from `annotations()`): text at a fixed figure position (x/y = proportions of figure), fading in/out, in all styles and videos. `selections()`: boxes/labels tied to time/frequency of the sound.
- `selections()` accepts warbleR selection tables (attributes stripped; filtered by `sound.files` matching the sound, or `sound.file`), but **rejects extended selection tables** (user's decision).
- `dynatheme()` chosen over ggplot2 themes; `axis.type` "standard"/"none"/"minimal" (minimal only in scrolling). **Themes may change plot colors** (user's decision): precedence layer argument > theme > style default. Presets are complete themes (replace earlier themes on `+`).
- Speed controls are a slider snapping to `player(speeds)` (user asked for a slider with fixed values, not buttons).
- Layer names keep no `geom_` prefix (recommended; user did not object).
- Static play-line default color `viridis::mako(10)[9]` (#A0DFB9); `static(play.line = FALSE)` hides it.
- Scrolling `buffer` default 0 (no start delay). Scrolling shows a single lower panel (overview, oscillogram or envelope); spectrum is ignored with a warning.
- `sound_buttons()` was removed (user did not like it).

## Code layout

- `R/dynaspec.R`: exported API (layers, styles, `+.dynaspec`, `save_dynaspec()`, print/knit_print methods).
- `R/internal_functions.R`: all internals — `.pbapply_dyn()` (cli progress, parallel), sound reading, HTML helpers (`captions_json_int`, `caption_overlays_int`, `scrolling_html_int`, `paged_html_int`, `playhead_video_int`, `paged_video_int`, `player_js_int` incl. `captionsInit`), drawing (`theme_int` and theme helpers, `spectro_int`, `envelope_int`, `spectrum_int`, `oscillo_player_int`, `time_axis_int`, `draw_page_int`, `draw_layers_int`), engines (`static_spectro_int`, `scrolling_spectro_int`, `prep_paged_int`, `paged_spectro_int`), object helpers (`new_dynaspec_int`, `save_static_int`, `save_scrolling_int`, `save_paged_int`, `sound_file_selections_int`, `shift_layers_int`).
- `man/` regenerated with roxygen (new: captions, dynatheme (with presets), envelope, overview, spectrum; removed: annotations, sound_buttons).
- DESCRIPTION Imports: utils, grDevices, graphics, tools, seewave, tuneR, grid, png, ggplot2 (>= 3.4.0), viridis, cli, fs, checkmate. Suggests: parallel, imager, warbleR, NatureSounds, testthat, withr, knitr, rmarkdown, covr.
- Tests: `tests/testthat/` (helper.R has `static_engine`, `prep_paged`, `paged_engine`, `scrolling_engine`, `short_sound()`). Test counts over the day: 314 (at commit `25cf9a0`), 329 (after the xeno-canto theme); latest: full suite passes inside R CMD check after the Macaulay theme and zoom (count not recorded).

## Documentation (2026-10-26; style and examples changed later, see below)

- `vignettes/dynaSpec.Rmd`: basic usage of every layer (Layers table, Panels, Styles, Playback, Highlighting/labeling, Themes, Saving). `full <- !identical(Sys.getenv("DYNASPEC_CRAN"), "true")`; chunks default `eval = full`, light ones `eval = TRUE`. Renders: full 7.7 MB, `DYNASPEC_CRAN=true` 2.8 MB.
- `vignettes/articles/customizing.Rmd` (pkgdown-only, `vignettes/articles` is in `.Rbuildignore`): customization of each layer (spectrogram, panels, dynatheme, static/scrolling/paged incl. hermit, nightingale wren, bat, swallow, whale, nighthawk; selections with house wren duet and warbleR table; captions with frogs; player; videos). Renders without warnings (26 MB, 24 players; downloads recordings). Old `customizing_scrolling_spectrograms.Rmd` removed with `git rm`.
- README.Rmd (layer table updated, link to the article) and README.md re-knitted; NEWS.md 2.0.0.9000 section updated.

## Later changes (2026-10-27, committed in `25cf9a0`)

- `player(buttons = )`: "time" (default), "loop", "volume", "download", "fullscreen" (shared JS `extraButtons()` in `player_js_int()`; data attributes via `player_attrs_int()`; download name from `x$name`). Full screen could not be tested in the app's browser pane (refused there), works in regular browsers after a click.
- Speed buttons replaced by a slider snapping to `speeds` (sorted, slowest left, tick marks via datalist).
- `spectrum(side = "left")`: the frequency axis and title are drawn on the left of the spectrum (spectrum column widened by the axis margin); oscillogram/envelope labels use `xpd = NA`.
- Static spectrograms are drawn by `spectro_int()` (seewave::spectro(plot = FALSE) + `.filled.contour`), because seewave::spectro() filled the whole page with `colbg` (dynatheme(bg) was lost unless colbg matched) and resets `par(new)`. Output matches seewave pixel-wise.
- Known: in very short panels the rotated frequency title can be clipped (as before).
- New test for the theme background (corners stay `bg`, with and without a left spectrum). Docs (NEWS, vignette, article) updated; vignette/article not re-rendered since.

## Vignette style (2026-10-27, committed in `25cf9a0`)

- User wants vignettes/articles styled like their other packages (e.g. `~/Dropbox/R_package_testing/suwo/vignettes`): YAML with title/subtitle ("dynaSpec: dynamic spectrogram visualizations")/pagetitle/linked author/date, `html_document` with floating TOC; `&nbsp;` + intro in `::: {.alert .alert-info}`; Installation section; top-level `#` headings; collapsible "Session information" at the end. Applied to `vignettes/dynaSpec.Rmd` (index entry "1. Package overview") and `vignettes/articles/customizing.Rmd`.
- The article must stay `self_contained: yes`: with `false`, rmarkdown's post-processing of the 30 MB page hangs (>10 min at 100% CPU). pkgdown uses its own format, so the site is unaffected.
- `html_document` makes the CRAN vignette 3.7 MB (was 2.8 MB with `html_vignette`).

## Xeno-canto theme (2026-10-27, uncommitted)

- `dynatheme_xenocanto()` (modeled on https://xeno-canto.org/154133/player, gray mode; values read from its CSS: page #F0F0F0, figure white with 1px #DDD border and 8px radius, 44px round #E0E0E0 buttons, 8px #CCC seek track). Color version via `...` (inferno on black, white wave/labels), shown in the article.
- New general pieces: `dynatheme(axis.type = "inside")` in all styles (`inside_freq_axis_int()`, `inside_time_axis_int()`; scrolling JS gets `unit`, `tickLines`, `labLine`), `lty.line`/`lwd.line` (scrolling play line), `bar.style = "rounded"` (CSS `.ds-round`, `setPlay()`, -/+ speed buttons), `player(buttons = "progress")` seek bar (all styles). `player()` now stores only supplied args; rounded themes default to `c("progress", "time")` (resolved in `theme_layers_int()`).
- `box = FALSE` now also hides the scrolling spectrogram/oscillogram strip borders and the division line.
- Checked with headless Chrome (chromote) screenshots of all styles; 329 tests pass.

## Example sound (2026-10-27, uncommitted)

- `canyon_wren` removed (data + Rd); new dataset `scaly_breasted_hummingbird` (user's recording XC154047, CC BY-NC-SA 3.0; low-pass 10.5 kHz + resampled to 22.05 kHz, 8.35 s; `R/scaly_breasted_hummingbird-data.R`). Used in all examples, vignette, article, README, tests (helper `short_sound()` = 1 to 1 + dur s).
- Examples use `flim = c(2, 11)` and `spectrogram(wl = 200, collevels = seq(-50, 0, 1))` (the wren's wl = 300 / 40 dB made the short notes faint). Selections/captions times: first notes 1-2.9 s, last notes 6-7.4 s.
- R CMD check: 1 NOTE (version + pkgdown 404s); installed size 4.6 MB (was 5.0).

## Macaulay theme and zoom (2026-10-27, uncommitted)

- `dynatheme_macaulay()` (from the user's screenshot; macaulaylibrary.org is behind a bot check): gray palette `c("#EBEBEB", "#111111")`, `axis.type = "compact"` (new, all styles: freq labels under ticks in a narrow left column + "kHz", time in m:ss above the figure; `compact_freq_axis_int()`, `compact_time_axis_int()`, `compact_row_axis_int()`, `mmss_int()`, JS `mmss()`), `bar.style = "square"` (new; CSS `.ds-square`, accent = play line color, default buttons `c("volume", "time")`), `col.highlight` now also dims the scrolling overview (cfg `dim`). Best with `overview()` + `scrolling(fix.time = FALSE)`.
- `player(buttons = "zoom")` (scrolling only; warns in static/paged): slider + -/+ from 4x out (at most whole sound) to 4x in; strips rendered at 2x width; time labels drawn by JS when zoom is on (also with fix.time). JS: `zoom`, `winStart()`, `tickStep()`.
- Bug fixes found on the way: moving time labels (fix.time = FALSE) were t.display/2 ahead (strip time instead of sound time) in web pages and videos; paged spectrograms resampled 22050 -> 22000 Hz with `tuneR::downsample()` (aliasing arcs) — now only resample when the rate drops by >10%.
- Tests: full suite passes inside R CMD check (1 NOTE as before).

## Decisions and open proposals (2026-10-27)

- `geom_` prefix: recommended **not** to use it (layers are not ggplot2 geoms; would suggest ggplot2 compatibility). Keep current names; reserve `geom_` for a possible ggplot2-native version. User did not object.
- Themes (implemented 2026-10-27; user decided themes may change plot colors):
  - new `dynatheme()` settings: `family`, `las`, `tick.length`, `box`, `col.box`, `grid`, `col.grid`, `col.panel`, `col.title`, `cex.title`, `pal`, `colbg`, `col.wave`, `col.line`, `col.highlight`, `col.bar`, `bg.bar` (validated in `check_theme_int()`)
  - precedence: layer argument > theme > style default, resolved in `theme_layers_int()` (called by `save_dynaspec()`); `oscillogram()`, `envelope()`, `spectrum()` now default `col = NULL` (defaults in `panel_col_int()`); theme `pal` can be a function, a viridis name or two colors (`pal_fun_int()`)
  - presets `dynatheme_dark()`, `_presentation()`, `_paper()`, `_minimal()`, `_transparent()`, `_void()` (`theme_preset_int()`; complete themes: `+` replaces the previous theme, later `dynatheme()` modifies it; `...` overrides preset settings)
  - helpers: `theme_par_int()` (family, tcl), `theme_box_int()`, `theme_grid_int()`, `panel_frame_int()` (panel backgrounds), `css_family_int()`; player bar colors and web font via data attributes in `player_attrs_int()` and CSS variables (`--ds-fg`, `--ds-bg`, `--ds-line`, `--ds-font`) in `extraButtons()`; captions get the theme family (videos and web)
  - static margins now grow with text size (`bottom_lines`, `top_lines` for the 'Power' label)
  - all presets render in all styles (tests); dark web page checked in the browser
  - docs: vignette "Themes" section, article "Themes" + "Preset themes", README/NEWS
- Other player ideas offered earlier but not requested: stop/back to start, skip back/forward, keyboard shortcuts, play-selection buttons from `selections()`, drag to play a range, band-pass (flim) listening toggle.

## Next steps

1. ~~Run the full test suite~~ Done 2026-10-27: 314 pass, 0 fail/warn/skip.
2. ~~Re-render vignette and article~~ Done 2026-10-27 (rendered to a temp dir, not kept): no errors/warnings; vignette full 9.3 MB, CRAN 2.8 MB; article 30 MB.
3. ~~`R CMD check`~~ Done 2026-10-27 (`--as-cran`, built with `DYNASPEC_CRAN=true`): 1 NOTE only (new version + 404 for the two pkgdown article URLs, which exist once the site is deployed). Fixed on the way: `base64enc` added to Suggests (used in a test), long line in `overview()` example, stray `Rplots.pdf` removed. Installed size was 5.0 MB (at the CRAN threshold) with `canyon_wren`; **4.6 MB** after switching to `scaly_breasted_hummingbird`. Re-run after the Macaulay theme and zoom: still 1 NOTE.
4. Commit the uncommitted work (xeno-canto theme, hummingbird dataset, Macaulay theme and zoom) when the user asks; nothing is pushed.
5. Long term (not started): optional ggplot2-native redesign.

## Useful facts

- R 4.6 on Linux; ffmpeg 4.4 (overlay expressions with `t` evaluated per frame; `drawbox` not).
- cli progress bars only show in interactive sessions after 2 s; to test in Rscript: `options(rlang_interactive = TRUE, cli.progress_show_after = 0)`.
- Web pages embed sounds (~1.4x file size); crop or downsample long recordings.
- Before a CRAN build set `Sys.setenv(DYNASPEC_CRAN = "true")` (vignette ~22 MB otherwise).
