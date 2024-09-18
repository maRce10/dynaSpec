#' Generate ggplot2-based spectrogram(s), which can be passed to paged_spectro
#'
#' Can be used to generate single or segmented static spectrograms. Works as standalone,
#' but the returned object is also intended to feed into \code{\link{paged_spectro}}. Workflow: 1) use
#' prep_static_ggspectro to crop, filter, segment and tweak all spectrogram parameters; 2) pass these
#' settings to \code{\link{paged_spectro}} to generate dynamic spectrogram video.
#'
#' @aliases prepStaticSpec prepStaticGGspec
#'
#' @usage prep_static_ggspectro(soundFile,destFolder,outFilename,savePNG=FALSE,colPal="inferno",
#' crop=NULL,bg=NULL,filter=NULL,xLim=NULL,yLim=c(0,10),plotLegend=FALSE,onlyPlotSpec=TRUE,
#' ampTrans=1,min_dB=-30,wl=512, ovlp=90,wn="blackman",specWidth=9,specHeight=3,
#' colbins=30,ampThresh=0,bgFlood=FALSE,fontAndAxisCol=NULL,optim=NULL,...)
#'
#' @param soundFile should work with URLs, full and relative paths; handles .mp3 and .wav
#' @param destFolder path to directory to save output. Needs to be like "figures/spectrograms/" to be relative to working directory. Default=parent folder of soundFile. Specify "wd" to output to the working directory, gotten from [get_wd()]
#' @param outFilename name for output PNG. default=NULL will use input name in output filename.
#' @param savePNG logical; Save static spectrograms as PNGs? They will be exported to destFolder.
#' @param colPal color palette; one of "viridis","magma","plasma","inferno","cividis" from the \code{\link[viridis]{viridis}} package OR a 2 value vector (e.g. c("white","black")), defining the start and end of a custom color gradient
#' @param crop subset of recording to include; default crop=NULL will use whole file, up to 10 sec; if a number, interpreted as crop first X.X sec; if c(X1,X2), interpreted as trimming out a specific time interval in sec; if crop=FALSE, will not crop at all, even for recordings over 10 sec.
#' @param xLim the time limit (x-axis width) in seconds for all spectrograms; i.e. page width in seconds for multi-page dynamic spectrograms (defaults to WAV file length, unless file duration >5s). To override the 5s limit, put xLim=Inf or specify the desired spectrogram x-axis limit.
#' @param yLim the frequency limits (y-axis); default is c(0,10) aka 0-10kHz
#' @param title string for title of plots; default=NULL
#' @param plotLegend logical; include a legend showing amplitude colors? default=FALSE
#' @param onlyPlotSpec logical; do you want to just plot the spec and leave out the legend, axes, and axis labels? default= TRUE
#' @param resampleRate a number in Hz to downsample audio for spectrogram only. This will simplify audio data and speed up generation of spectrogram. Passed to [tuneR::downsample()]. Put NULL to keep original sample rate for spectrogram.
#' @param ampTrans amplitude transform for boosting spectrum contrast; default=1 (actual dB values); specify a decimal number for the lambda value of scales::modulus_trans(); 2.5 is a good place to start. (This amplifies your loud values the most, while not increasing background noise much at all)
#' @param min_dB the minimum decibel (quietest sound) to include in the spec; defaults to -30 (-40 would include quieter sounds; -20 would cut out all but very loud sounds)
#' @param filter apply a bandpass filter? Defaults to none (NULL). Expects 'c(0,2)' where sound from 0 to 2kHz would be filtered out
#' @param bg  background color (defaults to 1st value of chosen palette)
#' @param wl  window length for the spectrogram (low values= higher temporal res; high values= higher freq. res). Default 512 is a good tradeoff; human speech would look better at 1024 or higher, giving higher frequency resolution.
#' @param ovlp how much overlap (as percent) between sliding windows to generate spec? Default 90 looks good, but takes longer
#' @param wn window name (slight tweaks on algorithm that affect smoothness of output) see \code{\link[seewave]{spectro}}
#' @param specWidth what width (in inches) would you like to make your PNG output be, if saving a static spec?
#' @param specHeight what height (in inches) would you like to make your PNG output be, if saving a static spec?
#' @param colbins default 30: increasing can smooth the color contours, but take longer to generate spec
#' @param ampThresh amplitude threshold as a percent to cut out of recording (try 5 to start); default= no filtering (high data loss with this; not recommended; play with min_dB  and ampTrans first)
#' @param bgFlood do you want the background color to spill into the axis margins? Default=FALSE (i.e. white margins)
#' @param fontAndAxisCol the color of legend text if onlyPlotSpec=TRUE (since margins will be white, with black text); if bgFlood=TRUE, this will be the color of axis margins, labels and legend text. If you don't supply this, it will be picked automatically to be white or black based on supplied bg color
#' @param optim NULL by default; this is an experimental feature to simplify the dataframe of the FFT-processed waveform used to generate the spectrogram (currently does nothing)
#' @param ... Other arguments to be passed for rendering the spec (i.e. to seewave::spectro)
#'
#' @return a list with all spectrogram parameters, segmented WAV files (segWavs) and spectrograms spec; importantly, spec is a list of n=number of "pages"/segments; the first page is displayed by default
#' @seealso \code{\link{paged_spectro}}
#' @author Matthew R Wilkins (\email{matt@@galacticpolymath.com})
#' @references {
#' Araya-Salas M & Wilkins M R. (2020). *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.
#' }
#' @export
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
#' (you need the patchwork package to use the / operator to stack plots)
#' library(patchwork)
#' (femaleBarnSwallow$spec[[1]]+ggplot2::xlim(0,5)) /
#' (maleBarnSwallow$spec[[1]]+ggplot2::xlim(0,5))  + 
#' patchwork::plot_annotation(title="Female and Male barn swallow songs",
#' caption="Female song (top) is much shorter, but similar complexity to males. See: MR Wilkins et al. (2020) Animal Behaviour 168")
#'
#' # ggplot2::ggsave("M&F_barn_swallow_song_specs.jpeg",width=11,height=7)
#'
#' # see more examples at https://marce10.github.io/dynaSpec/
#' }

##################
#set defaults for stepping into function for testing
# colPal="inferno";crop=NULL;bg=NULL;filter=NULL;xLim=NULL;yLim=c(0,10);plotLegend=FALSE;onlyPlotSpec=TRUE;ampTrans=2.5;min_dB=-30;wl=512;ovlp=90;wn="blackman";specWidth=6;specHeight=2;colbins=30;ampThresh=0;bgFlood=FALSE;fontAndAxisCol=NULL
##################

prep_static_ggspectro <-
  function(soundFile,
           destFolder,
           outFilename = NULL,
           savePNG = FALSE,
           colPal = "inferno",
           crop = NULL,
           bg = NULL,
           filter = NULL,
           xLim = NULL,
           yLim = c(0, 10),
           title=NULL,
           plotLegend = FALSE,
           onlyPlotSpec = TRUE,
           ampTrans = 1,
           resampleRate = NULL,
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
           optim = NULL,
           ...)
  {
    #Put in soundFile directory if unspecified
    if (missing(destFolder)) {
      if (is.url(soundFile)) {
        destFolder = getwd()
      } else{
        destFolder = dirname(tools::file_path_as_absolute(soundFile))
      }
    }
    destFolder0 <- destFolder
    #handle relative destFolder path that ends with /, but doesn't start with /
    if(!fs::is_absolute_path(destFolder0)){
      destFolder <- fs::path(fs::path_dir(soundFile),destFolder0)
      fs::dir_create(destFolder)
      message("Relative destFolder supplied:'",destFolder0,"'. Will save files to soundFile parent dir:\n > ",destFolder)
    }
    
    #Put soundFile in working dir if requested
    if (destFolder0 == "wd") {
      destFolder <- getwd()
    }
    
    if (!grepl("/$", destFolder0)) {
      destFolder = paste0(destFolder0, "/")
    }#if destFolder missing terminal /, add it
    
    if (is.url(soundFile)) {
      #handle refs to a recording page URL from Xeno-Canto that don't have .mp3 in the name
      if (grepl("xeno-canto\\.org/.*", soundFile) &
          !(grepl("\\.mp3", soundFile))) {
        #lookup filename using warbler to query the API. Sometimes MP3, sometimes WAV
        xc_rec_num <- gsub("^.*\\.org/(\\d*).*", "\\1", soundFile)
        xc_query_result <- warbleR::query_xc(paste0("nr:", xc_rec_num) )
        xc_filename <- xc_query_result$file.name
        dl_src <-  xc_query_result$Audio_file
        soundFile <- paste0(destFolder, xc_filename)
        dest_file <- soundFile
        
      } else{
        dl_src <- soundFile
        dest_file <- paste0(destFolder, basename(soundFile))
        soundFile <- paste0(destFolder, basename(soundFile))
      }
      
      #avoid redownloading every time
      if(file.exists(dest_file)){
        message("File found. Skipping download of: ",dest_file)
      }else{
        utils::download.file(dl_src,destfile = dest_file)
      }
    }
    
    
    #Handle file naming for spec
    if (is.null(outFilename)) {
      outFilename = paste0(tools::file_path_sans_ext(basename(soundFile)), ".png")
    }
    if (!grepl(".png|PNG", outFilename)) {
      outFilename = paste0(outFilename, ".png")
    }#if user didn't put suffix onto output filename, add .jpeg
    
    #Are we dealing with a custom or a viridis palette?
    if (length(colPal) == 1) {
      isViridis <- T
    } else{
      isViridis <- F
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
    if (tools::file_ext(soundFile) %in% c("mp3","MP3")) {
      print("***Converting mp3 to wav***")
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
      processSound(wav0,
                   crop = crop,
                   xLim = xLim,
                   filter = filter,
                   ampThresh=ampThresh)
    
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
        crop = crop,
        bg = bg,
        filter = filter,
        xLim = prepped$xLim,
        yLim = yLim,
        plotLegend = plotLegend,
        onlyPlotSpec = onlyPlotSpec,
        ampTrans = ampTrans,
        ampThresh = ampThresh,
        min_dB = min_dB,
        wl = wl,
        ovlp = ovlp,
        wn = wn,
        colbins = colbins,
        bgFlood = bgFlood,
        fontAndAxisCol = fontAndAxisCol,
        optim = optim,
        ...
      )
    
    plot(specOutList$specList[[1]])
    
    if (length(prepped$segWavs) > 1) {
      cat("\nFor segmented spectrogram, only segment 1 shown\n")
    }
    
    #Make list of all info needed to recreate spec look in videos
    specParams = list(
      soundFile = soundFile,
      destFolder = destFolder,
      outFilename = outFilename,
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
