#' Measure blur ratio in the time domain
#' 
#' \code{blur_ratio} measures blur ratio in signals referenced in an extended selection table.
#' @usage blur_ratio(X, parallel = 1, pb = TRUE, method = 1, ssmooth = 200, 
#' msmooth = NULL, output = "est", img = FALSE, res = 150, hop.size = 11.6, wl = NULL, 
#' ovlp = 70, pal = reverse.gray.colors.2, collevels = seq(-60, 0, 5), dest.path = NULL)
#' @param X object of class 'extended_selection_table' created by the function \code{\link[warbleR]{selection_table}} from the warbleR package. The object must include the following additional columns: 'signal.type', 'bottom.freq' and 'top.freq'.
#' @param parallel Numeric vector of length 1. Controls whether parallel computing is applied by specifying the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control if progress bar is shown. Default is \code{TRUE}.
#' @param method Numeric vector of length 1 to indicate the 'experimental design' for measuring envelope correlation. Two methods are available:
#' \itemize{
#' \item \code{1}: compare all signals with their counterpart that was recorded at the closest distance to source (e.g. compare a signal recorded at 5m, 10m and 15m with its counterpart recorded at 1m). This is the default method. 
#' \item \code{2}: compare all signals with their counterpart recorded at the distance immediately before (e.g. a signal recorded at 10m compared with the same signal recorded at 5m, then signal recorded at 15m compared with same signal recorded at 10m and so on).
#' }
#' @param ssmooth Numeric vector of length 1 determining the length of the sliding window (in amplitude samples) used for a sum smooth for amplitude envelope calculation (used internally by \code{\link[seewave]{env}}). Default is 200.
#' @param msmooth Numeric vector of length 2 to smooth the amplitude envelope with a mean sliding window for amplitude envelope calculation. The first element is the window length (in number of amplitude values) and the second one the window overlap (used internally by \code{\link[seewave]{env}}). 
#' @param output Character vector of length 1 to determine if an extended selection table ('est', default), a data frame ('data.frame') or a list ("list") containing the extended selection table (first object in the list) and all (smoothed) wave envelopes (second object in the list) is returned. The envelope data can be used for plotting.
#' @param img Logical argument to control if image files in 'jpeg' format containing the images being compared and the corresponding envelopes are produced. Default is no images ( \code{FALSE}).
#' @param res Numeric argument of length 1. Controls image resolution. Default is 150 (faster) although 300 - 400 is recommended for publication/presentation quality.
#' @param hop.size A numeric vector of length 1 specifying the time window duration (in ms). Default is 11.6 ms, which is equivalent to 512 wl for a 44.1 kHz sampling rate. Ignored if 'wl' is supplied.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#' is NULL. If supplied, 'hop.size' is ignored.
#' @param ovlp Numeric vector of length 1 specifying the percent overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Only used when plotting. Default is 70. Applied to both spectra and spectrograms on image files.
#' @param pal A color palette function to be used to assign colors in the 
#'   plot, as in \code{\link[seewave]{spectro}}. Default is reverse.gray.colors.2. 
#' @param collevels	Numeric vector indicating a set of levels which are used to partition the amplitude range of the spectrogram (in dB) as in \code{\link[seewave]{spectro}}. Default is \code{seq(-60, 0, 5)}. 
#' @param dest.path Character string containing the directory path where the image files will be saved. If NULL (default) then the folder containing the sound files will be used instead.
#' @return Data frame similar to input data, but also includes two new columns ('reference' and 'blur.ratio')
#' with the reference signal and blur ratio values. If \code{img = TRUE} it also returns 1 image file (in 'jpeg' format) for each comparison showing spectrograms of both signals and the overlaid amplitude envelopes (as probability mass functions (PMF)). Spectrograms are shown within the frequency range of the reference signal and also show vertical lines with the start and end of signals to allow users to visually check alignment. If \code{output = 'list'} the output would be a list including the data frame just described and a data frame with envelopes (amplitude values) for all signals.
#' @export
#' @name blur_ratio
#' @details Blur ratio measures the degradation of sound as a function of the change in signal energy in the time domain as described by Dabelsteen et al (1993). Low values indicate low degradation of signals. The function measures the blur ratio on signals in which a reference playback has been re-recorded at different distances. Blur ratio is measured as the mismatch between amplitude envelopes (expressed as probability density functions) of the reference signal and the re-recorded signal. The function compares each signal type to the corresponding reference signal within the supplied frequency range (e.g. bandpass) of the reference signal ('bottom.freq' and 'top.freq' columns in 'X'). The 'signal.type' column must be used to tell the function to only compare signals belonging to the same category (e.g. song-types). Two methods for setting the experimental design are provided. All wave objects in the extended selection table must have the same sampling rate so the length of envelopes is comparable.
#' @seealso \code{\link{envelope_correlation}}, \code{\link{spectral_blur_ratio}}
#' @examples
#' {
#' # load example data
#' data("playback_est")
#' 
#' # remove ambient selections
#' playback_est <- playback_est[playback_est$signal.type != "ambient", ]
#' 
#' # using method 1
#'blur_ratio(X = playback_est)
#' 
#' # using method 2
#' blur_ratio(X = playback_est, method = 2)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com}) 
#' @references {
#' Dabelsteen, T., Larsen, O. N., & Pedersen, S. B. (1993). Habitat-induced degradation of sound signals: Quantifying the effects of communication sounds and bird location on blur ratio, excess attenuation, and signal-to-noise ratio in blackbird song. The Journal of the Acoustical Society of America, 93(4), 2206.
#' 
#' Araya-Salas, M. (2020). baRulho: baRulho: quantifying habitat-induced degradation of (animal) acoustic signals in R. R package version 1.0.2
#' }
#last modification on dec-26-2019 (MAS)

blur_ratio <- function(X, parallel = 1, pb = TRUE, method = 1,
                       ssmooth = 200, msmooth = NULL, output = "est", 
                       img = FALSE, res = 150, hop.size = 11.6, wl = NULL, ovlp = 70, pal = reverse.gray.colors.2, collevels = seq(-60, 0, 5), dest.path = NULL){
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)

  # is extended sel tab
  if (!warbleR::is_extended_selection_table(X)) 
    stop("'X' must be and extended selection table")
  
  # set dest.path if not provided
  if (is.null(dest.path)) 
    dest.path <- getwd() else 
      if (!dir.exists(dest.path)) 
    stop("'dest.path' provided does not exist")
  
  # hopsize  
  if (!is.numeric(hop.size) | hop.size < 0) stop("'parallel' must be a positive number") 
  
  # adjust wl based on hope.size
  if (is.null(wl))
    wl <- round(attr(X, "check.results")$sample.rate[1] * hop.size, 0)
  
  # make wl even if odd
  if (!(wl %% 2) == 0) wl <- wl + 1
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # If method is not numeric
  if (!is.numeric(method)) stop("'method' must be a numeric vector of length 1") 
  if (!any(method %in% 1:2)) stop("'method' must be either 1 or 2")
  
  # check signal.type column 
  if (is.null(X$signal.type)) stop("'X' must containe a 'signal.type' column")
  
  #check output
  if (!any(output %in% c("est", "data.frame", "list"))) stop("'output' must be 'est', 'data.frame' or 'list'")  
  
  # must have the same sampling rate
  if (length(unique(attr(X, "check.results")$sample.rate)) > 1) 
    stop("all wave objects in the extended selection table must have the same sampling rate (they can be homogenized using warbleR::resample_est())")
  
  # set pb options 
  pbapply::pboptions(type = ifelse(as.logical(pb), "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # ingnore ssmooth if msmooth is supplied
  if (!is.null(msmooth)) 
    ssmooth <- NULL
  
  # add sound file selec column and names to X (weird column name so it does not overwrite user columns)
  X <- prep_X_bRlo_int(X, method = method, parallel = parallel, pb = pb)
    
  # print message
  if (pb) write(file = "", x = "calculating amplitude envelopes (step 1 of 2):")
  
  # calculate all envelops apply function
  envs <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(y, ssmth = ssmooth, msmth = msmooth)   {
    
    # load clip
    clp <- warbleR::read_wave(X = X, index = y)
    
    # define bandpass based on reference
    bp <- c(X$bottom.freq[X$TEMP....sgnl == X$reference[y]], X$top.freq[X$TEMP....sgnl == X$reference[y]])
    
    # bandpass filter
    clp <- seewave::ffilter(clp, from = bp[1] * 1000, 
                            ovlp = ovlp, to = bp[2] * 1000, bandpass = TRUE, 
                            wl = wl, output = "Wave")
    
    # calculate envelope
    nv <- seewave::env(wave = clp, f = clp@samp.rate, ssmooth = ssmth, msmooth = msmth, plot = FALSE)[, 1]
    
    return(nv)
  }) 
  
  # add sound file selec column and names to envelopes
  names(envs) <- X$TEMP....sgnl
  
  ## function to measure blur ratio
  # y and z are the sound.files+selec names of the signals and reference signal (model)
  # envelope mismatch ratio 
  blur_FUN <- function(x, res, ovlp, wl, collevels, pal, ...){
    
      # get names of signal and reference
      sgnl <-  X$TEMP....sgnl[x]
      rfrnc <- X$reference[x]
      
    # if signals are the same or the selection is noise return NA
    if (sgnl == rfrnc | any(c(X$signal.type[X$TEMP....sgnl == sgnl], X$signal.type[X$reference == rfrnc]) == "ambient")) out <- NA else {
      
      # extract envelope for signal and model 
      sgnl.env <- envs[[which(names(envs) == sgnl)]]
      rfrnc.env <- envs[[which(names(envs) == rfrnc)]]
      
      # make them the same length as the shortest one
      if(length(sgnl.env) > length(rfrnc.env)) sgnl.env <- sgnl.env[1:length(rfrnc.env)]
      if(length(rfrnc.env) > length(sgnl.env)) rfrnc.env <- rfrnc.env[1:length(sgnl.env)]
      
      # duration (any works as they all must have the same sampling rate)
      dur <- length(sgnl.env) / (attr(X, "check.results")$sample.rate[1] * 1000)
      
      # convert envelopes to PMF (probability mass function)
      rfrnc.pmf <- rfrnc.env / sum(rfrnc.env)
      sgn.pmf <- sgnl.env / sum(sgnl.env)
      
      # get blur ratio as half the sum of absolute differences between envelope PMFs
      bl.rt <- sum(abs(rfrnc.pmf - sgn.pmf)) / 2
      
      # plot
      if (img)
      {
        warbleR:::img_wrlbr_int(filename = paste0("blur_ratio_", X$signal.type[x], "-", rfrnc, "-", sgnl, ".jpeg"), path = dest.path, width = 10.16 * 1.5, 
                      height = 10.16 , units = "cm", res = res)
        
        # time values for plots
        time.vals <- seq(0, dur, length.out = length(sgnl.env))

        # difference between envelopes
        env.diff <- rfrnc.pmf - sgn.pmf
        
        # matrix for layout
        ly.mat <- matrix(
          c(0, 0.3, 0, 0.5, # bottom left spectrogram
            0, 0.3, 0.5, 1, # top left spectrogram
            0.2, 1, 0, 1),  # right pannel envelopes
          nrow = 3, byrow = TRUE)
        
        # save par settings
        oldpar <- par(no.readonly = TRUE)   
        on.exit(par(oldpar)) 
        
        # close if open any screen
        invisible(close.screen(all.screens = TRUE))
        
        
        split.screen(ly.mat)
        
        # plot envelopes
        screen(3)
        
        
        # set image margins
        par(mar = rep(4, 0, 4, 4))
        
        # reference envelope first        
        plot(time.vals, rfrnc.pmf, type = "l", xlab = "", ylab = "", col = "#E37222", ylim = c(min(rfrnc.pmf, sgn.pmf), max(rfrnc.pmf, sgn.pmf) * 1.1), cex.main = 0.8, lwd = 1.2, yaxt = "n")
      
        # add x axis label
        mtext(text = "Time (s)", side = 1, line = 2.5)
        
        # add title
        mtext(text = paste("Signal type:", X$signal.type[x]), side = 3, line = 3, cex = 0.7)
        mtext(text = paste("Reference:", rfrnc), side = 3, line = 2, col = "#E37222", cex = 0.7)
        mtext(text = paste("Signal:", sgnl), side = 3, line = 1, col = "#07889B", cex = 0.7)
            
        # add y axis
        axis(side = 4)
        mtext(text = "Amplitude (PMF)", side = 4, line = 2.5)
        
        # add signal envelope
        lines(time.vals, sgn.pmf, col= "#07889BFF", lwd = 1.2)
        
        # signal envelope on top
        polygon(x = c(time.vals, rev(time.vals)), y = c(sgn.pmf, rev(rfrnc.pmf)), col = "#07889B33", border = NA)
        
        # get plotting area limits
        usr <- par("usr")
        
        # and blu ratio value
        text(x = ((usr[1] + usr[2]) / 2) + usr[1], y = usr[4] * 0.9, paste("Blur ratio:", round(bl.rt, 2)), cex = 0.8)
        
        # index of reference
        rf.indx <- which(paste(X$sound.files, X$selec, sep = "-") == rfrnc)

        # freq limit of reference
        flim <- c(X$bottom.freq[rf.indx], X$top.freq[rf.indx])
        
        # calculate margin for spectrogram, before and after
        mar.rf.af <- mar.rf.bf <- attr(X, "check.results")$duration[rf.indx] / 4
        
        # start for signal and reference
        strt.sgnl <- X$start[x] - mar.rf.bf
        if (strt.sgnl < 0) strt.sgnl <- 0
        strt.rf <- X$start[rf.indx] - mar.rf.bf
        if (strt.rf < 0) strt.rf <- 0
        
        # end for signal and reference
        rf.info <- warbleR::read_wave(X = X, index = rf.indx, header = TRUE)
        rf.dur <- rf.info$samples / rf.info$sample.rate 
        
        end.sgnl <- X$end[x] + mar.rf.af
        if (end.sgnl > rf.dur) end.sgnl <- rf.dur
        end.rf <- X$end[rf.indx] + mar.rf.af
        if (end.rf > rf.dur) end.rf <- rf.dur
        
        # extract clip reference and signal
        clp.sgnl <- warbleR::read_wave(X = X, index = x, from = strt.sgnl, to = end.sgnl)
        clp.rfnc <- warbleR::read_wave(X = X, index = rf.indx, from = strt.rf, to = end.rf)
        
        ## plot spectros
        # signal at bottom left
        screen(1)
        par(mar = c(0.3, 0.3, 0.15, 0.3))
        
        warbleR:::spectro_wrblr_int2(wave = clp.sgnl, f = clp.sgnl@samp.rate, 
                                     flim = flim,  axisX = FALSE, axisY = FALSE, 
                                     tlab = NULL, flab = NULL, main = NULL, grid = FALSE, rm.zero = TRUE, cexaxis = 1.2, add = TRUE, ovlp = ovlp, wl = wl, collevels = collevels, palette = pal)
        
        # lines showing position of signal
        abline(v = c(mar.rf.bf, X$end[x] - X$start[x] + mar.rf.bf), col = "#07889BFF", lty = 2)
                
        # add box with signal color
        box(col = "#07889BFF", lwd = 3)
        
        # reference at top left
        screen(2)
        par(mar = c(0.15, 0.3, 0.3, 0.3))
        
        warbleR:::spectro_wrblr_int2(wave = clp.rfnc, f = clp.rfnc@samp.rate, 
           flim = flim, axisX = FALSE, axisY = FALSE, 
           tlab = NULL, flab = NULL, main = NULL, grid = FALSE, rm.zero = TRUE, cexaxis = 1.2, add = TRUE, ovlp = ovlp, wl = wl, collevels = collevels, palette = pal)

        # lines showing position of signal
        abline(v = c(mar.rf.bf, X$end[rf.indx] - X$start[rf.indx] + mar.rf.bf), col = "#E37222CC", lty = 2)
        
        # add box with reference color
        box(col = "#E37222", lwd = 3)
                
        # close graph    
        dev.off()        
      }
      
      # return maximum correlation
      return(bl.rt)
    }
    return(out)
  } 
  
  if (pb & !img) write(file = "", x = "calculating blur ratio (step 2 of 2):")
  if (pb & img) write(file = "", x = "calculating blur ratio and producing images (step 2 of 2):")
    
  # get blur ratio
  # calculate all envelops apply function
  X$blur.ratio <- pbapply::pbsapply(X = 1:nrow(X), cl = cl, FUN = function(x, rs = res, wle = wl, colvs = collevels, pl = pal, ovp = ovlp)   {
    blur_FUN(x, res = rs, ovlp = ovp, wl = wle, collevels = colvs, pal = pl)
  }) 
  
  # remove temporal column
  X$TEMP....sgnl <- NULL
  
  # convert to list instead of extended selection table, add envelopes
  if (output == "list") 
  {
    
    env.dfs <- pbapply::pblapply(1:length(envs), cl = cl, function(y){
      
      # extract 1 envelope
      x <- envs[[y]]
      
      # convert envelopes to PMF (probability mass function)
      x <- x / sum(x)       
      
      # put in data framme
      out <- data.frame(signal = names(envs)[y], signal.type = X$signal.type[paste(X$sound.files, X$selec, sep = "-") == names(envs)[y]], distance  = X$distance[paste(X$sound.files, X$selec, sep = "-") == names(envs)[y]], time = seq(from = 0, to = length(x) / (attr(X, "check.results")$sample.rate[1] * 1000), along.with =  x), amp = x)
      
      return(out)
    })
    
    # put together in a single data frame
    env.df <- do.call(rbind, env.dfs)
    
    # put est and envelopes in a list
    X <- list(est = X, envelopes = env.df)
    }
  
  # return data frame
  if (output == "data.frame") X <- as.data.frame(X)
  
  return(X)
}
