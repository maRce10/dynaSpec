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
  
  #assign new wave file for use going forward
  #the bit=wav0@bit is REALLY important! Should be standard, but it doesn't work
  wav <-
    if (crop[2] == fileDur0) {
      wav0
    } else{
      seewave::cutw(
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
  
  
  
  ##Deal with xLim for segmenting wavs
  # if xLim not provided, default to smaller of 5sec or wav duration
  
  if (is.null(xLim)) {
    xLim <- c(0, min(5, wavDur))
    if (xLim[2] == 5) {
      cat("\n*****\nxLim set to 5 sec by default; define to override\n*****\n")
    }
  } else{
    #If xLim provided as single digit, interpret as X sec
    if (length(xLim) == 1) {
      xLim <- c(0, xLim)
    }
  }
  
  #Add silence at the end if (user-supplied) xLim>cropped Duration or xLim doesn't divide into even segments of wave duration
  timeRemainder <-
    (ceiling(wavDur / xLim[2]) * xLim[2] - wavDur) > 0.001#(wavDur%/%xLim[2]-wavDur/xLim[2]
  #If user wants to have xLim be Infinite (override 5sec max for long files)
  if(xLim[2] %in% c(Inf,"Inf")){
    wavDur <- max(length(wav@left), length(wav@right)) / wav@samp.rate
    xLim[2] <- wavDur
  #Else fill out remainders with filler silence
  }else if (xLim[2] > wavDur | timeRemainder ) {
    if (timeRemainder) {
      diffT <- ceiling(wavDur / xLim[2]) * xLim[2] - wavDur
    } else{
      diffT <- xLim[2] - wavDur
    }
    fillerWAV <-
      tuneR::silence(
        duration = diffT,
        samp.rate = smplRt,
        xunit = "time",
        pcm = T,
        bit = wav@bit
      )
    wav <- tuneR::bind(wav, fillerWAV)
    #pastew results in intermittent problems! Don't use! bind seems much more dependable
    #seewave::pastew(wave1=fillerWAV,wave2=wav,at="end",output="Wave",join=T,bit=wav0@bit)
    wavDur <- max(length(wav@left), length(wav@right)) / wav@samp.rate
  }
  #Segment wav or make list of 1 if no segmentation
  segLens <- seq(0, wavDur, xLim[2])
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
  #browser()
  return(list(
    newWav = wav,
    segWavs = segWavs,
    wavDur = wavDur,
    segLens = segLens,
    xLim = xLim
  ))
}#End processSound
