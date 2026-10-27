dynaSpec: dynamic spectrogram visualizations
================

<!-- the website home page (pkgdown/index.md) is created from README.md by pkgdown/make_index.R (run automatically in the pkgdown GitHub Actions workflow) -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/maRce10/dynaSpec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/maRce10/dynaSpec/actions/workflows/R-CMD-check.yaml)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-4.1.0-6666ff.svg)](https://cran.r-project.org/)
<!-- badges: end -->

<img src="man/figures/dynaSpec_sticker.png" alt="sketchy sticker" align="right" width = "25%" height="25%"/>

**dynaSpec** creates dynamic spectrograms: spectrograms that move
along with the sound, ready for presentations, teaching or sharing
online, *without taking screen recordings* of another program. They are
shown as interactive web pages (with the sound embedded, click-to-play
and buttons to slow the sound down), which work in R Markdown and Quarto
documents, and can also be saved as videos (mp4). It builds on
[seewave](https://CRAN.R-project.org/package=seewave) and
[tuneR](https://CRAN.R-project.org/package=tuneR).

Dynamic spectrograms are built in layers, as ‘ggplot2’ graphs:
`dynaspec()` takes the sound, and layers are added with `+`:

| Layer                                  | What it does                                                                           |
| -------------------------------------- | -------------------------------------------------------------------------------------- |
| `spectrogram()`                        | How the spectrogram is computed and drawn (window length, palette, amplitude range, …) |
| `oscillogram()`                        | Adds an oscillogram (waveform)                                                         |
| `selections()`                         | Highlights parts of the sound with translucent boxes (e.g. from a selection table)     |
| `annotations()`                        | Labels parts of the sound                                                              |
| `player()`                             | Playback speed buttons of web pages                                                    |
| `static()`, `scrolling()` or `paged()` | Style of the dynamic spectrogram (see below)                                           |

The package offers three styles of dynamic spectrogram:

| Style                        | What it looks like                                                                                           |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Static (`static()`, default) | A play line moves over the whole spectrogram (and oscillogram), which can be split in several rows           |
| Scrolling (`scrolling()`)    | The spectrogram scrolls from right to left past a fixed “play” line, as in the Macaulay Library              |
| Paged (`paged()`)            | A spectrogram (or several “pages”) revealed by a sliding highlight box, as in Adobe Audition’s spectral view |

**Contents:** [Installation](#installation) · [Quick
start](#quick-start) · [Background](#background) · [Citation](#citation)

> 📖 **Examples of the three styles are in the vignette [Creating dynamic
> spectrograms](https://marce10.github.io/dynaSpec/articles/dynaSpec.html)**
> (or run `vignette("dynaSpec")` in R).

## Installation

Install the development version from GitHub (requires
[remotes](https://cran.r-project.org/package=remotes)):

``` r
remotes::install_github("maRce10/dynaSpec")
```

### ffmpeg

Web pages do not need any external software. To create videos dynaSpec
uses [ffmpeg](https://ffmpeg.org/download.html), which must be installed
and available in the system path:

| Operating system | Command (in a terminal)                                                                                                                              |
| ---------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| Windows          | `winget install --id Gyan.FFmpeg -e` (or download a build from [gyan.dev](https://www.gyan.dev/ffmpeg/builds/) and add its `bin` folder to the PATH) |
| macOS            | `brew install ffmpeg`                                                                                                                                |
| Debian/Ubuntu    | `sudo apt install ffmpeg`                                                                                                                            |

You can check that R finds it with:

``` r
Sys.which("ffmpeg")
```

If ffmpeg is installed somewhere else, point R to the executable with
`Sys.setenv(ffmpeg = "path/to/ffmpeg")`.

## Quick start

``` r
library(dynaSpec)

# example recording included in the package
data("canyon_wren")

# static spectrogram with oscillogram (shown in the RStudio viewer or the web browser)
x <- dynaspec(canyon_wren, flim = c(1, 10)) +
  spectrogram(wl = 300) +
  oscillogram()

x

# scrolling and paged styles
x + scrolling(t.display = 1.7)
x + paged(t.display = 2)

# save as a web page or as a video
save_dynaspec(x, "canyon_wren.html")
save_dynaspec(x + scrolling(t.display = 1.7), "canyon_wren.mp4")
```

Sounds can be ‘Wave’ objects or ‘.wav’ and ‘.mp3’ files. Invalid
arguments are reported all together before any processing starts, so
mistakes are caught early. See the
[vignette](https://marce10.github.io/dynaSpec/articles/dynaSpec.html)
for many more options and examples.

## Background

This package is a collaboration between [Marcelo
Araya-Salas](https://marce10.github.io/) and [Matt
Wilkins](https://www.mattwilkinsbio.com/), and the successor of
[dynaSpec](https://github.com/maRce10/dynaSpec). The goal is to create
static and dynamic visualizations of sounds, ready for publication or
presentation. Marcelo’s approach (scrolling spectrograms) shows a
spectrogram sliding past a fixed point as sounds are played, similar to
that used in Cornell’s Macaulay Library. Matt’s approach creates “paged”
spectrograms that are revealed by a sliding highlight box as sounds are
played, akin to Adobe Audition’s spectral view. dynaSpec brings both
approaches, and static spectrograms with a moving play line, together
under a single layered syntax, and adds interactive web pages.

A taste of what you can do (a video made with dynaSpec):

<https://github.com/user-attachments/assets/8323b6cd-8ddd-4d4f-9e42-4adad90f2c74>

-----

## Citation

Please cite dynaSpec as follows:

Araya-Salas, M. & Wilkins, M. R. (2026). *dynaSpec: dynamic
spectrogram visualizations in R*. R package version 0.0.0.9000.
<https://github.com/maRce10/dynaSpec>

Bugs and feature requests can be reported at
<https://github.com/maRce10/dynaSpec/issues>.
