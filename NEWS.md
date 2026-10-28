# *dynaSpec 2.0.0.9000*

* Major redesign with a new layered syntax (the functions of previous versions, `scrolling_spectro()`, `prep_static_ggspectro()` and `paged_spectro()`, and their aliases `prepStaticSpec()` and `pagedSpec()`, are defunct: they give an error showing the new syntax): `dynaspec()` takes the sound (a 'Wave' object or a '.wav' or '.mp3' file, with its frequency and time ranges in `flim` and `tlim`) and layers are added with `+`:
    - `ds_spectrogram()`, `ds_oscillogram()`, `ds_envelope()` (amplitude envelope), `ds_spectrum()` (power spectrum on the left or right side; on the left side it carries the frequency axis) and `ds_overview()` (lower panel with the whole sound in scrolling spectrograms): panels of the figure (only the panels added are shown; `dynaspec()` alone shows an empty panel with a time axis)
    - `ds_annotations()`: translucent boxes highlighting parts of the sound, with optional borders (color, width and line type) and labels, or the spectrogram colored within each selection (`type = "spectrogram"`, as `warbleR::color_spectro()`). Takes data frames and 'warbleR' selection tables (extended selection tables are not supported)
    - `ds_captions()`: text at a fixed position of the figure shown (and optionally fading in and out) during a time range, like subtitles, in all styles
    - `ds_theme()`: axis titles, text sizes, fonts and font family, colors of axes and text (and of axis labels alone, `col.axis`), orientation of axis labels of both axes (`las`), axis type, ticks, boxes, grid lines, backgrounds, plot colors (spectrogram palette and background, panel and play line colors) and colors of the player controls, for all styles. Preset themes: `ds_theme_dark()`, `ds_theme_mako()`, `ds_theme_presentation()`, `ds_theme_paper()`, `ds_theme_minimal()`, `ds_theme_transparent()`, `ds_theme_void()`, `ds_theme_xenocanto()` and `ds_theme_macaulay()` (inspired by the Xeno-canto and Macaulay Library players). `axis.type = "inside"` draws labels with units inside the figure (e.g. "8k", "2s") and `axis.type = "compact"` frequency labels in a narrow column and time labels in minutes and seconds above the figure; `lty.line`/`lwd.line` set the play line of scrolling spectrograms; `bar.style = "rounded"` gives the player round controls and a seek bar and `bar.style = "square"` a flat bar with a square play button; `col.highlight` also dims the overview of scrolling spectrograms. `ds_set_theme()` sets a theme for all spectrograms of the session (e.g. of a document), as `ggplot2::theme_set()`
    - `ds_player()`: playback speed slider (snapping to fixed speeds) and 'keep pitch' toggle of web pages, plus optional time readout, seek bar, zoom slider (scrolling spectrograms: stretch or shrink the spectrogram), loop, volume, download and full screen buttons (`buttons`)
    - `ds_static()` (default), `ds_scrolling()` and `ds_paged()`: style of the dynamic spectrogram (`ds_static(play.line = FALSE)` hides the moving line, whose default color is now `viridis::mako(10)[9]`)
* Layers and themes have a `ds_` prefix (`ds_spectrogram()`, `ds_scrolling()`, `ds_theme()`, ...), so they do not mask functions of other packages with general names (e.g. `spectrogram()`)
* Printing a `dynaspec` object shows an interactive web page (in the RStudio viewer or the web browser), embedded in R Markdown and Quarto documents when knitting. Figures and sounds are embedded in the HTML code, with click-to-play and buttons to slow the sound down (lowering the pitch, or keeping it)
* `save_dynaspec()` saves `dynaspec` objects as web pages ('.html'), videos ('.mp4', requires ffmpeg) or still images ('.png'), showing a progress bar (via 'cli') while rendering frames
* Scrolling spectrograms start moving right away (`buffer` is now 0 by default)
* Scrolling spectrograms whose time axis moves along (`fix.time = FALSE`) labeled the time of the sound with half a window of silence added at the start (labels were `t.display / 2` ahead of the play line); they now show the time of the sound
* Web pages embed their images with `srcset` instead of `src`: R Markdown documents that are not self-contained (e.g. pkgdown articles) pass every `<img src>` through `utils::URLdecode()`, which took minutes for embedded spectrograms (building the package website hung)
* Axis labels: amplitude envelopes and power spectra are labeled 'Amp.' (as oscillograms; previously 'Env.' and 'Power')
* Scrolling spectrograms failed when `flim` reached or exceeded the Nyquist frequency of the sound (it is now limited to it, as in static spectrograms)
* Paged spectrograms no longer resample sounds when the new sampling rate is close to the original one (`tuneR::downsample()` does not filter, which added artifacts)
* New example recordings by Julio E. Sánchez: `scythebill` (song of a brown-billed scythebill, *Campylorhamphus pusillus*, Cartago, Costa Rica, 1998; 11.025 kHz), used in most examples, and `antshrike` (two songs of a great antshrike, *Taraba major*, Finca La Selva, Heredia, Costa Rica, 1999; 11.025 kHz), used in examples of scrolling spectrograms. It replaces `canyon_wren`, which was removed
* Videos of static and paged spectrograms are made by moving the play line or highlight box over still images with ffmpeg, so they are created in seconds
* Spectrograms of static spectrograms are drawn by dynaSpec (computed with `seewave::spectro()`), so they keep the background set with `ds_theme(bg)` in multi-panel figures
* Paged spectrograms are drawn with base R graphics (no 'ggplot2' or 'gganimate' needed)
* Play lines are aligned with the time of the spectrogram windows, and all rows of multi-row figures share the same color and amplitude scale
* Scrolling spectrograms are drawn without grid lines by default (as static spectrograms; use `ds_spectrogram(grid = TRUE)` to add them)

# *dynaSpec 1.1.0*

## New features

* New vignette "Creating dynamic spectrograms" with all examples (moved from the README)
* All exported functions now validate their arguments up front (using 'checkmate'), reporting every invalid argument in a single error message
* `paged_spectro()`: new arguments 'parallel' (render pages in parallel), 'verbose' and 'play'. 'framerate' now sets the frame rate of the video (lower values render faster)
* `prep_static_ggspectro()`: new arguments 'plot' (whether to plot the spectrogram) and 'verbose'
* `scrolling_spectro()`: `axis.type = "minimal"` is now implemented (1 s and 1 kHz scale bars)
* `scrolling_spectro()`: 'parallel' now actually uses parallel computing (via 'pbapply'; no longer relies on non-exported 'warbleR' functions)
* `prepStaticSpec()` and `pagedSpec()` aliases are now exported

## Bug fixes

* `scrolling_spectro()`: no video was produced with recent ffmpeg versions (video frames are now saved as PNG instead of TIFF)
* `scrolling_spectro()`: fixed error when 'flim' is not supplied (default full frequency range)
* `scrolling_spectro()`: fixed extra frame copies when `loop > 1`
* `scrolling_spectro()`: selection tables in 'spectro.call' are now correctly aligned when `loop = 1`, and selections at the end of the clip are repeated at the start when looping
* `scrolling_spectro()`: 'annotation.call' 'start' and 'end' times now match the time at which sounds reach the play line
* `scrolling_spectro()`: informative errors when 'ffmpeg' is missing or fails; file paths with spaces are now supported
* `scrolling_spectro()`: no warnings about 'lower.spectro' or 'buffer' unless those arguments are supplied explicitly
* `prep_static_ggspectro()`: fixed downloading of 'Xeno-Canto' recordings from recording page URLs, which relied on the now deprecated `warbleR::query_xc()`. Files are now downloaded directly from the recording's download link
* `prep_static_ggspectro()`: sound files from URLs are now downloaded in binary mode (fixes corrupted downloads on Windows)
* `prep_static_ggspectro()`: fixed 'destFolder' handling ("wd" and relative paths now resolve to the working directory)
* `prep_static_ggspectro()`: 'resampleRate' is increased when needed so the spectrogram covers the whole 'yLim' range, and recordings are no longer upsampled
* `prep_static_ggspectro()`: 'savePNG' now saves the right plot for each segment; 'crop' start is no longer ignored when the crop end equals (or exceeds) the file duration
* `paged_spectro()`: fixed encoding error when the spectrogram aspect ratio produced an odd video width
* `paged_spectro()`: videos now open on Windows and Linux; temporary files are stored in a uniquely named folder
* `paged_spectro()`: missing destination folders are now created instead of asking interactively (which failed in non-interactive sessions)

## Other changes

* R >= 4.1.0 and ggplot2 >= 3.4.0 are now required
* `prep_static_ggspectro()`: 'optim' is deprecated (it never had any effect)
* `paged_spectro()` now stops with an error (instead of printing a message) when ffmpeg is not found
* Progress messages now use `message()` (and can be silenced with `verbose = FALSE` or `suppressMessages()`)
* Added 'testthat' tests, test coverage reporting, CITATION, CITATION.cff and codemeta.json files
* Fixed deprecated 'ggplot2' syntax, package help page, dependency declarations and multiple documentation issues
* Smaller package size (compressed and removed unused figures)
# *dynaSpec 1.0.5*

* Minor changes to improve stability

# *dynaSpec 1.0.4*

* Fixed destFolder parameter in prep_static_ggspectro() and paged_spectro()
* Should have more expected results for file save locations outside the working directory
* Fixed issue where extra page sometimes created with paged_spectro()
* Remove 'ari' package dependency

# *dynaSpec 1.0.3*

* Fix broken collab URL link
* Improve sound quality of nightingale wren sound file
* Fix embedded video links in both github and pkgdown website


# *dynaSpec 1.0.2*

* Fix logic for crop and xLim in prep_static_ggspectro()
* Other fixes to have more predictable behavior of exported MP4 videos using "Matt's approach" with paged_spectro()
* Added title parameter to add a title to both static and subsequent paged spectrogram (MP4)
* Added resampleRate parameter to prep_static_ggspectro() for control over resolution of spectrogram (and to speed things up)

# *dynaSpec 1.0.1*

* Fix resampling issue when sampling rate of waves != 44.1 kHz
* Fix closing of clusters when OS != windows


# *dynaSpec 1.0.0*

* First release
