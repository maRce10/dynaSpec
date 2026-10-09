#' Generate ggplot2-based spectrogram(s), which can be passed to paged_spectro
#'
#' Can be used to generate single or segmented static spectrograms. Works as standalone,
#' but the returned object is also intended to feed into [paged_spectro()]. Workflow: 1) use
#' prep_static_ggspectro to crop, filter, segment and tweak all spectrogram parameters; 2) pass these
#' settings to [paged_spectro()] to generate dynamic spectrogram video.
#'
#' @aliases prepStaticSpec
#'
#' @param soundFile should work with URLs, full and relative paths; handles .mp3 and .wav
#' @param destFolder path to directory to save output (created if it does not exist). Relative paths (e.g. "figures/spectrograms/") are interpreted relative to the working directory. Default = parent folder of soundFile (or the working directory if soundFile is a URL). Specify "wd" to output to the working directory, as returned by [base::getwd()].
#' @param outFilename name for output PNG. default=NULL will use input name in output filename.
#' @param savePNG logical; Save static spectrograms as PNGs? They will be exported to destFolder.
#' @param colPal color palette; one of "viridis","magma","plasma","inferno","cividis" from the viridis package (see [viridis::viridis()]) OR a 2 value vector (e.g. c("white","black")), defining the start and end of a custom color gradient
#' @param crop subset of recording to include; default crop=NULL will use whole file, up to 10 sec; if a number, interpreted as crop first X.X sec; if c(X1,X2), interpreted as trimming out a specific time interval in sec; if crop=FALSE, will not crop at all, even for recordings over 10 sec.
#' @param xLim the time limit (x-axis width) in seconds for all spectrograms; i.e. page width in seconds for multi-page dynamic spectrograms (defaults to WAV file length, unless file duration >5s). To override the 5s limit, put xLim=Inf or specify the desired spectrogram x-axis limit.
#' @param yLim the frequency limits (y-axis); default is c(0,10) aka 0-10kHz
#' @param title string for title of plots; default=NULL
#' @param plotLegend logical; include a legend showing amplitude colors? default=FALSE
#' @param onlyPlotSpec logical; do you want to just plot the spec and leave out the legend, axes, and axis labels? default= TRUE
#' @param resampleRate a number in Hz to downsample audio for spectrogram only. This will simplify audio data and speed up generation of spectrogram. Passed to [tuneR::downsample()]. Default=15000 shaves off a few seconds without losing much quality. If lower than twice the upper frequency limit (`yLim[2]`) it is increased to that value so the spectrogram covers the whole frequency range; recordings with a sampling rate below resampleRate are not resampled. Put NULL to keep original sample rate for spectrogram. Audiofile will not be resampled for MP4.
#' @param ampTrans amplitude transform for boosting spectrum contrast; default=1 (actual dB values); specify a decimal number for the lambda value of scales::modulus_trans(); 2.5 is a good place to start. (This amplifies your loud values the most, while not increasing background noise much at all)
#' @param min_dB the minimum decibel (quietest sound) to include in the spec; defaults to -30 (-40 would include quieter sounds; -20 would cut out all but very loud sounds)
#' @param filter apply a band-stop filter? Defaults to none (NULL). Expects a frequency range in kHz, e.g. 'c(0,2)', where sound from 0 to 2kHz would be filtered out
#' @param bg  background color (defaults to 1st value of chosen palette)
#' @param wl  window length for the spectrogram (low values= higher temporal res; high values= higher freq. res). Default 512 is a good tradeoff; human speech would look better at 1024 or higher, giving higher frequency resolution.
#' @param ovlp how much overlap (as percent) between sliding windows to generate spec? Default 90 looks good, but takes longer
#' @param wn window name (slight tweaks on algorithm that affect smoothness of output) see [seewave::spectro()]
#' @param specWidth what width (in inches) would you like to make your PNG output be, if saving a static spec?
#' @param specHeight what height (in inches) would you like to make your PNG output be, if saving a static spec?
#' @param colbins default 30: increasing can smooth the color contours, but take longer to generate spec
#' @param ampThresh amplitude threshold as a percent to cut out of recording (try 5 to start); default= no filtering (high data loss with this; not recommended; play with min_dB  and ampTrans first)
#' @param bgFlood do you want the background color to spill into the axis margins? Default=FALSE (i.e. white margins)
#' @param fontAndAxisCol the color of legend text if onlyPlotSpec=TRUE (since margins will be white, with black text); if bgFlood=TRUE, this will be the color of axis margins, labels and legend text. If you don't supply this, it will be picked automatically to be white or black based on supplied bg color
#' @param plot logical; plot the (first) spectrogram? Default is `TRUE`.
#' @param verbose logical; print progress messages? Default is `TRUE`.
#' @param optim Deprecated (it never had any effect) and will be removed in a future version.
#' @param ... Other arguments to be passed for rendering the spec (i.e. to seewave::spectro)
#'
#' @return a list with all spectrogram parameters, segmented WAV files (segWavs) and spectrograms spec; importantly, spec is a list of n=number of "pages"/segments; the first page is displayed by default
#' @seealso [paged_spectro()]
#' @author Matthew R Wilkins (\email{matt@@galacticpolymath.com})
#' @references
#' Araya-Salas M & Wilkins M R. (2020). *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.
#' @export prep_static_ggspectro prepStaticSpec
#' @name prep_static_ggspectro
#' @examples \dontrun{
#' require(dynaSpec)
#' f <- list.files(pattern=".wav", full.names = TRUE, path = system.file(package="dynaSpec"))
#'
#' # default behavior should be a decent start for good recordings; doesn't save anything, just plots
#' prep_static_ggspectro(f[1])
#'
#' # to use with paged_spectro or to do other stuff, you need to assign the
#' # resulting object, but it will still always plot the first spec
#' # let's add axes and boost the signal a smidge
#' femaleBarnSwallow <- prep_static_ggspectro(f[1],destFolder="wd",
#' onlyPlotSpec = FALSE, bgFlood=TRUE,ampTrans=2)
#'
#' # feels like we're missing a little bit of the quieter signals; let's lower
#' # the minimum amplitude threshold a bit
#' femaleBarnSwallow<-prep_static_ggspectro(f[1],destFolder="wd",
#' onlyPlotSpec = FALSE, bgFlood=TRUE,ampTrans=2,min_dB=-35)
#'
#' #now for a male song
#' maleBarnSwallow<-prep_static_ggspectro(f[2],destFolder="wd",onlyPlotSpec = FALSE,
#' bgFlood=TRUE)
#'
#' #Nice, but the trill is fading out; I'm gonna signal boost and lower the min_dB
#' maleBarnSwallow<-prep_static_ggspectro(f[2],destFolder="wd",onlyPlotSpec = FALSE,
#' bgFlood=TRUE,ampTrans=2,min_dB=-40)
#'
#' #much stronger, now let's combine them
#' #(you need the patchwork package to use the / operator to stack plots)
#' library(patchwork)
#' (femaleBarnSwallow$spec[[1]]+ggplot2::xlim(0,5)) /
#' (maleBarnSwallow$spec[[1]]+ggplot2::xlim(0,5))  +
#' patchwork::plot_annotation(title="Female and Male barn swallow songs",
#' caption="Female song (top) is much shorter, but similar
#' complexity to males. See: MR Wilkins et al. (2020) Animal
#'  Behaviour 168")
#'
#' # ggplot2::ggsave("M&F_barn_swallow_song_specs.jpeg",width=11,height=7)
#'
#' # see more examples at https://marce10.github.io/dynaSpec/
#' }

prep_static_ggspectro <-
  function(soundFile,
           destFolder = NULL,
           outFilename = NULL,
           savePNG = FALSE,
           colPal = "inferno",
           crop = NULL,
           bg = NULL,
           filter = NULL,
           xLim = NULL,
           yLim = c(0, 10),
           title = NULL,
           plotLegend = FALSE,
           onlyPlotSpec = TRUE,
           ampTrans = 1,
           resampleRate = 15000,
           min_dB = -30,
           wl = 512,
           ovlp = 90,
           wn = "blackman",
           specWidth = 9,
           specHeight = 3,
           colbins = 30,
           ampThresh = 0,
           bgFlood = FALSE,
           fontAndAxisCol = NULL,
           plot = TRUE,
           verbose = TRUE,
           optim = NULL,
           ...)
  {
    # check arguments
    check_results <- .check_arguments(
      fun = "prep_static_ggspectro",
      args = list(soundFile = soundFile, destFolder = destFolder, outFilename = outFilename, savePNG = savePNG, colPal = colPal, crop = crop, bg = bg, filter = filter, xLim = xLim, yLim = yLim, title = title, plotLegend = plotLegend, onlyPlotSpec = onlyPlotSpec, ampTrans = ampTrans, resampleRate = resampleRate, min_dB = min_dB, wl = wl, ovlp = ovlp, wn = wn, specWidth = specWidth, specHeight = specHeight, colbins = colbins, ampThresh = ampThresh, bgFlood = bgFlood, fontAndAxisCol = fontAndAxisCol, plot = plot, verbose = verbose)
    )

    # report errors
    .report_assertions(check_results)

    if (!is.null(optim))
      warning2("'optim' is deprecated and ignored (it never had any effect); it will be removed in a future version")

    #Put in soundFile directory if unspecified (working directory for URLs)
    if (is.null(destFolder)) {
      if (is.url(soundFile)) {
        destFolder = getwd()
      } else{
        destFolder = dirname(tools::file_path_as_absolute(soundFile))
      }
    } else if (identical(destFolder, "wd")) {
      #Put files in working dir if requested
      destFolder <- getwd()
    } else if (!fs::is_absolute_path(destFolder)) {
      #relative paths are relative to the working directory
      destFolder <- file.path(getwd(), destFolder)
    }

    #create destination folder if needed
    if (!dir.exists(destFolder)) {
      fs::dir_create(destFolder)
      if (verbose) message("Created destination folder: ", destFolder)
    }

    if (!grepl("/$", destFolder)) {
      destFolder = paste0(destFolder, "/")
    }#if destFolder missing terminal /, add it
    
    if (is.url(soundFile)) {
      #handle refs to a recording page URL from Xeno-Canto that don't have .mp3 in the name
      if (grepl("xeno-canto\\.org/.*", soundFile) &
          !(grepl("\\.mp3", soundFile))) {
        # download directly from the recording's download link. The file can be
        # MP3 or WAV, so take the real file name from the response headers
        xc_rec_num <- gsub("^.*\\.org/(\\d*).*", "\\1", soundFile)
        dl_src <- paste0("https://xeno-canto.org/", xc_rec_num, "/download")
        xc_filename <- paste0("XC", xc_rec_num, ".mp3")
        hdrs <- tryCatch(curlGetHeaders(dl_src), error = function(e) character(0))
        cd <- grep("^content-disposition:", hdrs, ignore.case = TRUE, value = TRUE)
        if (length(cd) > 0) {
          fn <- sub('^.*?filename="?([^";]+)"?.*$', "\\1", trimws(cd[length(cd)]), perl = TRUE)
          if (nzchar(fn) && !identical(fn, trimws(cd[length(cd)]))) {
            xc_filename <- gsub("[/\\\\]", "_", fn)
          }
        }
        soundFile <- paste0(destFolder, xc_filename)
        dest_file <- soundFile
        
      } else{
        dl_src <- soundFile
        dest_file <- paste0(destFolder, basename(soundFile))
        soundFile <- paste0(destFolder, basename(soundFile))
      }
      
      #avoid redownloading every time
      if (file.exists(dest_file)) {
        if (verbose) message("File found. Skipping download of: ", dest_file)
      } else{
        utils::download.file(dl_src, destfile = dest_file, mode = "wb", quiet = !verbose)
      }
    }
    
    
    #Handle file naming for spec
    if (is.null(outFilename)) {
      outFilename = paste0(tools::file_path_sans_ext(basename(soundFile)), ".png")
    }
    if (!grepl(".png|PNG", outFilename)) {
      outFilename = paste0(outFilename, ".png")
    }#if user didn't put suffix onto output filename, add .png
    
    #Are we dealing with a custom or a viridis palette?
    if (length(colPal) == 1) {
      isViridis <- TRUE
    } else{
      isViridis <- FALSE
    }
    
    #set background color as palette level 1 if missing
    if (is.null(bg)) {
      if (isViridis) {
        pal = eval(parse(text = paste0("viridis::", colPal)))
        bg = pal(1)
      } else{
        bg = colPal[1]
      }
    }
    
    #Convert MP3s to WAV
    if (tools::file_ext(soundFile) %in% c("mp3", "MP3")) {
      if (verbose) message("Converting mp3 to wav")
      wav0 <- tuneR::readMP3(soundFile)
    } else{
      wav0 <- tuneR::readWave(soundFile)
    }
    
    #Make sure it's mono (arbitrarily using left channel)
    if (wav0@stereo) {
      wav0 <- tuneR::mono(wav0, which = "left")
    }
    
    #######
    #crop, filter, normalize, deal with missing parameters for soundFile
    prepped <-
      processSound(
        wav0,
        crop = crop,
        xLim = xLim,
        filter = filter,
        ampThresh = ampThresh,
        verbose = verbose
      )
    
    if (length(yLim) == 1) {
      yLim = c(0, yLim)
    }
    
    # Send processed wave file and segment length info to spec generation function
    specOutList <-
      ggSpec(
        wav = prepped$newWav,
        soundFile = soundFile,
        resampleRate = resampleRate,
        segLens = prepped$segLens,
        savePNG = savePNG,
        specWidth = specWidth,
        specHeight = specHeight,
        destFolder = destFolder,
        title = title,
        colPal = colPal,
        isViridis = isViridis,
        bg = bg,
        yLim = yLim,
        plotLegend = plotLegend,
        onlyPlotSpec = onlyPlotSpec,
        ampTrans = ampTrans,
        min_dB = min_dB,
        wl = wl,
        ovlp = ovlp,
        wn = wn,
        colbins = colbins,
        bgFlood = bgFlood,
        fontAndAxisCol = fontAndAxisCol,
        verbose = verbose,
        ...
      )
    
    if (plot) {
      plot(specOutList$specList[[1]])

      if (verbose && length(prepped$segWavs) > 1)
        message("For segmented spectrogram, only segment 1 shown")
    }
    
    #Make list of all info needed to recreate spec look in videos
    specParams = list(
      soundFile = soundFile,
      destFolder = destFolder,
      outFilename = outFilename,
      n_pages = length(prepped$segWavs),
      crop = crop,
      colPal = colPal,
      isViridis = isViridis,
      xLim = prepped$xLim,
      yLim = yLim,
      title = title,
      plotLegend = plotLegend,
      onlyPlotSpec = onlyPlotSpec,
      ampTrans = ampTrans,
      ampThresh = ampThresh,
      min_dB = min_dB,
      bg = bg,
      wl = wl,
      ovlp = ovlp,
      wn = wn,
      specWidth = specWidth,
      specHeight = specHeight,
      colbins = colbins,
      bgFlood = bgFlood,
      autoFontCol = specOutList$autoFontCol,
      fontAndAxisCol = specOutList$fontAndAxisCol,
      spec = specOutList$specList,
      newWav = prepped$newWav,
      segWavs = prepped$segWavs
    )
    return(specParams)
  }#end testSpec

#create alias
prepStaticSpec <- prep_static_ggspectro
