# *dynaSpec 2.0.0.9000*

* Major redesign with a new layered syntax (the functions of previous versions, `scrolling_spectro()`, `prep_static_ggspectro()` and `paged_spectro()`, were removed): `dynaspec()` takes the sound (a 'Wave' object or a '.wav' or '.mp3' file) and layers are added with `+`:
    - `spectrogram()`, `oscillogram()`, `envelope()` (amplitude envelope), `spectrum()` (power spectrum on the left or right side; on the left side it carries the frequency axis) and `overview()` (lower panel with the whole sound in scrolling spectrograms): panels of the figure (only the panels added are shown; `dynaspec()` alone shows an empty panel with a time axis)
    - `selections()`: translucent boxes highlighting parts of the sound. Takes data frames and 'warbleR' selection tables (extended selection tables are not supported)
    - `captions()`: text at a fixed position of the figure shown (and optionally fading in and out) during a time range, like subtitles, in all styles
    - `dynatheme()`: axis titles, text sizes, fonts and font family, colors of axes and text, axis type, ticks, boxes, grid lines, backgrounds, plot colors (spectrogram palette and background, panel and play line colors) and colors of the player controls, for all styles. Preset themes: `dynatheme_dark()`, `dynatheme_presentation()`, `dynatheme_paper()`, `dynatheme_minimal()`, `dynatheme_transparent()` and `dynatheme_void()`
    - `player()`: playback speed slider (snapping to fixed speeds) and 'keep pitch' toggle of web pages, plus optional time readout, loop, volume, download and full screen buttons (`buttons`)
    - `static()` (default), `scrolling()` and `paged()`: style of the dynamic spectrogram (`static(play.line = FALSE)` hides the moving line, whose default color is now `viridis::mako(10)[9]`)
* Printing a `dynaspec` object shows an interactive web page (in the RStudio viewer or the web browser), embedded in R Markdown and Quarto documents when knitting. Figures and sounds are embedded in the HTML code, with click-to-play and buttons to slow the sound down (lowering the pitch, or keeping it)
* `save_dynaspec()` saves `dynaspec` objects as web pages ('.html'), videos ('.mp4', requires ffmpeg) or still images ('.png'), showing a progress bar (via 'cli') while rendering frames
* Scrolling spectrograms start moving right away (`buffer` is now 0 by default)
* Videos of static and paged spectrograms are made by moving the play line or highlight box over still images with ffmpeg, so they are created in seconds
* Spectrograms of static spectrograms are drawn by dynaSpec (computed with `seewave::spectro()`), so they keep the background set with `dynatheme(bg)` in multi-panel figures
* Paged spectrograms are drawn with base R graphics (no 'ggplot2' or 'gganimate' needed)
* Play lines are aligned with the time of the spectrogram windows, and all rows of multi-row figures share the same color and amplitude scale
* Scrolling spectrograms are drawn without grid lines by default (as static spectrograms; use `spectrogram(grid = TRUE)` to add them)

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
