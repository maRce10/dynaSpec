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
