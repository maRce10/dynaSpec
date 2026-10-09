#' Make a paged dynamic spectrogram similar to spectral display in Adobe Audition
#'
#' This function works on an object generated with [prep_static_ggspectro()] (or its alias `prepStaticSpec()`).
#' Video generation is very time consuming, and all the desired spectrogram parameters should be set
#' in the prep step. The output is an mp4 video of a dynamic spectrogram video. If the input sound file was
#' segmented in the prep step, the resulting video will be a concatenation of multiple dynamic spectrogram "pages."
#' Each page has a sliding window revealing the part of the static spectrogram being played. Temporal width of each page
#' is defined by the xLim parameter in [prep_static_ggspectro()]. You can also output temporary segmented files, if desired.
#'
#' @aliases pagedSpec
#' @param specParams an object returned from [prep_static_ggspectro()]
#' @param destFolder destination of output video; this setting overwrites setting from specParams object
#' @param vidName expects "FileName", .mp4 not necessary; if not supplied, will be named after the file you used in prep_static_ggspectro()
#' @param highlightCol default "#4B0C6BFF" (a purple color to match the default viridis 'inferno' palette)
#' @param highlightAlpha opacity of the highlight box; default is 0.6
#' @param cursorCol    Color of the leading edge of the highlight box; default "white"
#' @param delete_temp_files   Default= TRUE, deletes temporary files (specs & WAV files used to create concatenated video). Temporary files are stored in a uniquely named subfolder of destFolder.
#' @param framerate frames per second of the video; default 30. Lower values render faster (rendering time is proportional to the number of frames).
#' @param play Logical argument to control if the video is played after generated. Default is `TRUE`.
#' @param parallel Numeric vector of length 1. Number of cores used to render pages in parallel (only used when the sound was split into several pages). Default is 1 (no parallel computing).
#' @param verbose Logical argument to control if progress messages are printed. Default is `TRUE`.
#' @return Invisibly returns the path to the video file. Video plays after rendering if `play = TRUE`.
#' @details 'ffmpeg' and the 'av' package are required to render videos. If 'destFolder' does not exist it is created.
#' @seealso [prep_static_ggspectro()]
#' @author Matthew R Wilkins (\email{matt@@galacticpolymath.com})
#' @references
#' Araya-Salas M & Wilkins M R. (2020). *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.
#' @export paged_spectro pagedSpec
#' @examples \dontrun{
#' #show wav files included with dynaSpec
#' f <- list.files(pattern=".wav", full.names = TRUE,
#'      path = system.file(package="dynaSpec"))
#'
#' femaleBarnSwallow<-prep_static_ggspectro(f[1],destFolder=tempdir(),
#'                    onlyPlotSpec = FALSE, bgFlood= TRUE)
#' paged_spectro(femaleBarnSwallow,destFolder=tempdir())
#'
#' maleBarnSwallow<-prep_static_ggspectro(f[2],destFolder=tempdir(),
#'                  onlyPlotSpec = FALSE, bgFlood= TRUE,min_dB=-40)
#'
#' paged_spectro(maleBarnSwallow,destFolder=tempdir())
#'
#' # Make a multipage dynamic spec of a humpback whale song
#' # Note, we're saving PNGs of our specs in the working directory; to add
#' # axis labels, we set onlyPlotSpec to F, and to make the same background
#' # color for the entire figure, we set bgFlood= TRUE;
#' # The yLim is set to only go to 0.7kHz, where the sounds are for these big whales;
#' #also applying an amplitude transform to boost signal.
#' #This is a longer file, so we're taking the first 12 seconds with crop=12
#' #xLim=3 means each "page" will be 3 seconds, so we'll have 4 dynamic spec pages that get combined
#'
#' humpback <- prep_static_ggspectro(
#' "http://www.oceanmammalinst.org/songs/hmpback3.wav",destFolder=tempdir(),savePNG= FALSE,
#' onlyPlotSpec=FALSE,bgFlood= TRUE,yLim=c(0,.7),crop=12,xLim=3,ampTrans=3)
#'
#' #to generate multipage dynamic spec (movie), run the following
#' paged_spectro(humpback,destFolder=tempdir())
#'
#' # see more examples at https://marce10.github.io/dynaSpec/
#' }

paged_spectro <- function(specParams,
                          destFolder = NULL,
                          vidName = NULL,
                          framerate = 30,
                          highlightCol = "#4B0C6BFF",
                          highlightAlpha = .6,
                          cursorCol = "white",
                          delete_temp_files = TRUE,
                          play = TRUE,
                          parallel = 1,
                          verbose = TRUE)
{
  xmin <- ymin <- xmax <- ymax <- NULL
  #This ^^ suppresses note about "no visible binding for global variable ‘xmax’"

  # check arguments
  check_results <- .check_arguments(
    fun = "paged_spectro",
    args = list(specParams = specParams, destFolder = destFolder, vidName = vidName, framerate = framerate, highlightCol = highlightCol, highlightAlpha = highlightAlpha, cursorCol = cursorCol, delete_temp_files = delete_temp_files, play = play, parallel = parallel, verbose = verbose)
  )

  # report errors
  .report_assertions(check_results)

  if (!requireNamespace("av", quietly = TRUE))
    stop2("must install 'av' to render videos with paged_spectro()")

  # stop if ffmpeg is not found
  if (!have_ffmpeg_exec2())
    stop2("'ffmpeg' could not be found. It must be installed (and available in the system path) to create videos (see https://github.com/maRce10/dynaSpec#ffmpeg)")

  # video destination (defaults to the folder used in prep_static_ggspectro())
  if (is.null(destFolder))
    destFolder <- specParams$destFolder

  #base name for output, sans extension
  if (!is.null(vidName))
    iName0 <- tools::file_path_sans_ext(basename(vidName)) else
      iName0 <- tools::file_path_sans_ext(specParams$outFilename)

  vidName <- fs::path(destFolder, paste0(iName0, ".mp4"))

  # ffmpeg cannot handle single quotes in file names
  if (grepl("'", iName0))
    stop2("the video file name contains a single quote ('), which ffmpeg cannot handle. Please remove it: ", iName0)

  if (grepl("'", destFolder))
    stop2("the destination folder contains a single quote ('), which ffmpeg cannot handle. Please remove it: ", destFolder)

  # create destination folder if needed
  if (!dir.exists(destFolder)) {
    dir.create(destFolder, recursive = TRUE)
    if (verbose) message("Created destination folder: ", destFolder)
  }

  # uniquely named temporary folder (so no user folder is ever deleted)
  temp_folder <- tempfile(pattern = "dynaSpec_temp_", tmpdir = destFolder)
  dir.create(temp_folder)
  if (verbose) message("Temporary files saved at: ", temp_folder)

  #always export the newWav version that has been cropped/padded according to user parameters
  newWavOut <- fs::path(temp_folder, paste0(iName0, "_forVideo.wav"))
  tuneR::writeWave(specParams$newWav, filename = newWavOut)

  n_pages <- length(specParams$segWavs)
  segmented <- n_pages > 1

  #export wav files if spec is to be segmented; not necessary if wav is unaltered
  if (segmented) {
    outWAV <- fs::path(temp_folder, paste0(iName0, "_", seq_len(n_pages), "_.wav"))
    for (x in seq_len(n_pages))
      tuneR::writeWave(specParams$segWavs[[x]], filename = outWAV[x])
  }

  # base theme (created here so render_page() does not need package internals when run on parallel workers)
  ##Animate() seems to shrink font size a bit
  base_theme <- mytheme_lg(specParams$bg)

  # render one page (dynamic spectrogram of the i^th wav segment)
  render_page <- function(i) {

    iName <- paste0(iName0, if (segmented) paste0("_", i, "_") else "")

    #Save background spectrogram PNG to temp directory using tested parameters
    outPNG <- fs::path(temp_folder, paste0(iName, ".png"))
    outTmpVid <- fs::path(temp_folder, paste0(iName, ".mp4"))

    #output spec without axes, b/c we'll have to
    ggplot2::ggsave(
      filename = outPNG,
      plot = specParams$spec[[i]] + ggplot2::theme_void() + ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = specParams$bg),
        legend.position = 'none'
      ),
      dpi = 300,
      width = specParams$specWidth,
      height = specParams$specHeight,
      units = "in"
    )

    #Read PNG bitmap back in
    spec_PNG <- png::readPNG(outPNG)
    spec_width_px <- dim(spec_PNG)[2]
    spec_height_px <- dim(spec_PNG)[1]

    #Create data frame for highlighting box animation for i^th wav segment
    range_i <- c((i - 1) * specParams$xLim[2],
                 (i - 1) * specParams$xLim[2] + specParams$xLim[2])
    cursor <- seq(range_i[1], range_i[2], specParams$xLim[2] / framerate)
    played <- data.frame(
      xmin = cursor,
      xmax = rep(range_i[2], length(cursor)),
      ymin = rep(specParams$yLim[1], length(cursor)),
      ymax = rep(specParams$yLim[2], length(cursor))
    )

    #Make ggplot overlay of highlight box on spectrogram
    vidSegment <-
      ggplot2::ggplot(played) + ggplot2::xlim(range_i) + ggplot2::ylim(specParams$yLim) +
      #Labels
      ggplot2::labs(
        x = "Time (s)",
        y = "Frequency (kHz)",
        title = specParams$title
      ) +
      base_theme +

      #Conditional theming based on user prefs (note, legend not currently supported)
      #Since I'm reimporting spec as a raster, legend would need to rebuilt manually...gets a little
      #warped if I embed it in the raster...doesn't look good.
      {
        #If user supplied fontAndAxisCol, change those settings (regardless of whether bg is flooded or not)
        if (!specParams$autoFontCol) {
          ggplot2::theme(
            axis.text = ggplot2::element_text(colour = specParams$fontAndAxisCol),
            text = ggplot2::element_text(colour = specParams$fontAndAxisCol),
            axis.line = ggplot2::element_line(colour = specParams$fontAndAxisCol),
            axis.ticks = ggplot2::element_line(colour = specParams$fontAndAxisCol)
          )
        }
      } + {
        #get rid of axes & legend if requested
        if (specParams$onlyPlotSpec) {
          ggplot2::theme_void() + ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = specParams$bg),
            text = ggplot2::element_text(colour = specParams$fontAndAxisCol)
          )
        } else if (specParams$bgFlood) {
          #if axes to be plotted, flood panel bg color over axis area
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = specParams$bg),
            axis.text = ggplot2::element_text(colour = specParams$fontAndAxisCol),
            text = ggplot2::element_text(colour = specParams$fontAndAxisCol),
            axis.line = ggplot2::element_line(colour = specParams$fontAndAxisCol),
            axis.ticks = ggplot2::element_line(colour = specParams$fontAndAxisCol),
            legend.background = ggplot2::element_rect(fill = specParams$bg)
          )
        }
      } +

      #Add spectrogram
      ggplot2::annotation_custom(
        grid::rasterGrob(
          spec_PNG,
          width = ggplot2::unit(1, "npc"),
          height = ggplot2::unit(1, "npc")
        ),
        -Inf,
        Inf,
        -Inf,
        Inf
      ) +

      #Add box highlights for playback reveal
      ggplot2::geom_rect(
        data = played,
        ggplot2::aes(
          xmin = xmin,
          ymin = ymin,
          xmax = xmax,
          ymax = ymax
        ),
        fill = highlightCol,
        alpha = highlightAlpha
      ) +

      #Add cursor
      ggplot2::geom_segment(
        data = played,
        ggplot2::aes(
          x = xmin,
          xend = xmin,
          y = ymin,
          yend = ymax
        ),
        col = cursorCol,
        linewidth = 2
      ) +

      #Add animation
      gganimate::transition_reveal(xmin)

    #### Export animated ggplot specs (time consuming step)
    # save with sound in 1 step only if not segmented
    #note, height is set to 500px due to an issue w/ output being garbled at some resolutions; width according to aspect ratio (rounded to an even number, required by the video encoder)
    gganimate::animate(
      vidSegment,
      renderer = gganimate::av_renderer(if (segmented) outTmpVid else vidName, audio = if (segmented) outWAV[i] else newWavOut),
      duration = specParams$xLim[2],
      fps = framerate,
      width = 2 * round(250 * (spec_width_px / spec_height_px)),
      height = 500,
      units = "px"
    )

    invisible(NULL)
  }

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1 & segmented) {
    cl <- parallel::makePSOCKcluster(min(parallel, n_pages))
    on.exit(parallel::stopCluster(cl), add = TRUE)
  } else cl <- if (parallel > 1) parallel else NULL

  # turn progress bar on or off
  pbo <- pbapply::pboptions(type = if (verbose & segmented) "timer" else "none")
  on.exit(pbapply::pboptions(pbo), add = TRUE)

  if (verbose) message("Rendering ", n_pages, " page(s)", if (parallel > 1 & segmented) paste0(" using ", min(parallel, n_pages), " cores"), " ...")

  pbapply::pblapply(X = seq_len(n_pages), cl = if (segmented) cl else NULL, FUN = render_page)

  #if necessary, combine segments
  if (segmented) {
    segVids <- fs::path(temp_folder, paste0(iName0, "_", seq_len(n_pages), "_.mp4"))
    writeLines(paste0("file '", segVids, "' duration ", specParams$xLim[2]), fs::path(temp_folder, "mp4Segments.txt"))

    #concatenate mp4 segments
    #slight stutter for continuous sounds across segments
    run_ffmpeg_int(
      paste0(
        '-f concat -ss 00:00:00.000 -safe 0 -i "',
        fs::path(temp_folder, "mp4Segments.txt"),
        '" -codec copy -y "',
        vidName,
        '"'
      ),
      "concatenating video segments"
    )
  }

  if (verbose) message("Video saved at: ", vidName)

  if (play)
    open_file_int(vidName)

  if (delete_temp_files)
    unlink(temp_folder, recursive = TRUE)

  invisible(vidName)
}

#create alias
pagedSpec <- paged_spectro
