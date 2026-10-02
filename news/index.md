# Changelog

## *dynaSpec 1.0.6*

- Fixed downloading of ‘Xeno-Canto’ recordings from recording page URLs
  in
  [`prep_static_ggspectro()`](https://marce10.github.io/dynaSpec/reference/prep_static_ggspectro.md),
  which relied on the now deprecated
  [`warbleR::query_xc()`](https://marce10.github.io/warbleR/reference/query_xc.html).
  Files are now downloaded directly from the recording’s download link
- Sound files from URLs are now downloaded in binary mode (fixes
  corrupted downloads on Windows)

## *dynaSpec 1.0.5*

CRAN release: 2025-10-28

- Minor changes to improve stability

## *dynaSpec 1.0.4*

CRAN release: 2025-07-23

- Fixed destFolder parameter in prep_static_ggspectro() and
  paged_spectro()
- Should have more expected results for file save locations outside the
  working directory
- Fixed issue where extra page sometimes created with paged_spectro()
- Remove ‘ari’ package dependency

## *dynaSpec 1.0.3*

CRAN release: 2025-04-07

- Fix broken collab URL link
- Improve sound quality of nightingale wren sound file
- Fix embedded video links in both github and pkgdown website

## *dynaSpec 1.0.2*

CRAN release: 2024-09-29

- Fix logic for crop and xLim in prep_static_ggspectro()
- Other fixes to have more predictable behavior of exported MP4 videos
  using “Matt’s approach” with paged_spectro()
- Added title parameter to add a title to both static and subsequent
  paged spectrogram (MP4)
- Added resampleRate parameter to prep_static_ggspectro() for control
  over resolution of spectrogram (and to speed things up)

## *dynaSpec 1.0.1*

CRAN release: 2021-03-09

- Fix resampling issue when sampling rate of waves != 44.1 kHz
- Fix closing of clusters when OS != windows

## *dynaSpec 1.0.0*

CRAN release: 2020-06-22

- First release
