# processSound: filter, crop, and segment wav (internal function)

processSound <- function(wav0, filter, ampThresh, crop, xLim, ...) {
 
  
  smplRt <- wav0@samp.rate
  
  fileDur0 <- max(length(wav0@left), length(wav0@right)) / smplRt
  
  ##Figure out crop & spectrogram temporal window width
  
  #If crop provided as single digit, interpret as first X sec
  if (!is.null(crop)) {
    if (length(crop) == 1) {
      if (crop == F) {
        crop <- c(0, fileDur0)
      } else{
        crop <- c(0, crop)
      }
    }
    #If supplied crop longer than wav, treated as max length & crop set to duration
    if (crop[2] > fileDur0) {
      crop <- c(0, fileDur0)
      cat(
        paste0(
          "\n**** Crop longer than file duration: ",
          round(fileDur0, 2),
          ", treated as max length & ignored"
        )
      )
    }
  } else{
    #If crop not supplied, default crop to duration, unless >10 sec, warning user
    if (fileDur0 > 10) {
      crop = c(0, 10)
      cat(
        "\n*****\nCropping to 1st 10 sec due to slow processing of large WAV files.\nTo override, set crop=F or crop=c(startInSec,stopInSec)\n*****\n"
      )
    } else{
      crop = c(0, fileDur0)
    }
  }
  
  #crop is set now for all cases
  
  
  # Deal with xLim for segmenting wavs --------------------------------------
  # if xLim not provided, default to smaller of 5sec or wav duration
  cropped_dur <- crop[2] - crop[1]
  
  if (is.null(xLim)) {
    xLim <- c(0, min(5, cropped_dur))
    if (xLim[2] == 5) {
      message("\n*****\nxLim set to 5 sec by default; define to override\n*****\n")
    }
  } else{
    #If xLim provided as single digit, interpret as X sec
    if (length(xLim) == 1) {
      xLim <- c(0, xLim)
    }
  }
  
  #If user wants to have xLim be Infinite (override 5sec max for long files)
  if (xLim[2] %in% c(Inf, "Inf")) {
    xLim[2] <- cropped_dur
  }
  
  
  #assign new wave file for use going forward
  #the bit=wav0@bit is REALLY important! Should be standard, but it doesn't work
  
  if (crop[2] == fileDur0) {
    wav <- wav0
  } else{
    wav <- seewave::cutw(
      wav0,
      from = crop[1],
      to = crop[2],
      output = "Wave",
      bit = wav0@bit
    )
  }
  wavDur <- max(length(wav@left), length(wav@right)) / wav@samp.rate
  
  #Apply filters
  if (!is.null(filter)) {
    wav = seewave::ffilter(
      wave = wav,
      from = filter[1] * 1000,
      to = filter[2] * 1000,
      bandpass = F,
      output = "Wave",
      rescale = T
    )
  }
  if (ampThresh != 0) {
    wav <-
      seewave::afilter(
        wav,
        f = smplRt,
        threshold = ampThresh,
        plot = F,
        output = "Wave"
      )
  }
  
  
  
  # Make wav file match page xlims ------------------------------------------
  #Given xLim, what is the full length the recording needs to be?
  max_page_dur <- ceiling( round(wavDur,digits=2) / xLim[2]) * xLim[2]
  
  timeRemainder <-
    (max_page_dur - wavDur)
  
  is_timeRemainder <- timeRemainder > 0
  
  
  #If file is longer than max_page_dur, trim it
  if (wavDur > max_page_dur) {
    #trim the wav file again to match a multiple of xLim pages
    wav <- seewave::cutw(
      wav,
      from = crop[1],
      to = max_page_dur,
      output = "Wave",
      bit = wav0@bit
    )
    
    #Else fill out remainders with filler silence
  } else if (is_timeRemainder) {
    fillerWAV <-
      tuneR::silence(
        duration = timeRemainder,
        samp.rate = smplRt,
        xunit = "time",
        pcm = T,
        bit = wav@bit
      )
    wav <- tuneR::bind(wav, fillerWAV)
  }
  
    #pastew results in intermittent problems! Don't use! bind seems much more dependable
    #seewave::pastew(wave1=fillerWAV,wave2=wav,at="end",output="Wave",join=T,bit=wav0@bit)
    wavDur <- max(length(wav@left), length(wav@right)) / wav@samp.rate
  
  #Segment wav or make list of 1 if no segmentation
  #The ceiling code here is to deal with e.g. duration of 29.999 and xLim=10 that would only produce 2 segments
  
  segLens <- seq(0, ceiling(wavDur / xLim[2]) * xLim[2], xLim[2])
  indx <- 1:(length(segLens) - 1)
  segWavs <-
    lapply(indx, function(i)
      seewave::cutw(
        wav,
        from = segLens[i],
        to = segLens[i + 1],
        output = "Wave",
        bit = wav0@bit
      ))
  n_pages = length(segWavs)
  message(
    "Segmented original file into ",
    n_pages,
    " WAV files, each ",
    diff(xLim),
    " seconds long."
  )
  return(
    list(
      newWav = wav,
      n_pages = n_pages,
      segWavs = segWavs,
      wavDur = wavDur,
      segLens = segLens,
      xLim = xLim
    )
  )
}#End processSound
