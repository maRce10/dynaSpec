# *dynaSpec 1.0.4*
* Fixed destFolder parameter in prep_static_ggspectro() and paged_spectro()
* Should have more expected results for file save locations outside the working directory
* Fixed issue where extra page sometimes created with paged_spectro()

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
