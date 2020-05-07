#' Measure noise profiles
#' 
#' \code{noise_profile} Measure noise profiles in sound files or extended selection tables.
#' @usage noise_profile(X = NULL, files = NULL, mar = NULL, 
#' noise.ref = "adjacent", parallel = 1, pb = TRUE, path = NULL,
#' bp = NULL, hop.size = 1, wl = NULL, PSD = FALSE, norm = TRUE, dB = "A", averaged = TRUE)
#' @param X object of class 'extended_selection_table' created by the function \code{\link[warbleR]{selection_table}} from the warbleR package. Default is \code{NULL}.
#' @param files Character vector with names of wave files to be analyzed. Files must be found in 'path' supplied (or in the working directory if 'path' is not supplied). Default is \code{NULL}.
#' @param mar numeric vector of length 1. Specifies the margins adjacent to
#'   the start and end points of selection over which to measure ambient noise. Required if 'X' is supplied and ignored if not supplied. Default is \code{NULL}.
#' @param noise.ref Character vector of length 1 to determined which noise segment must be used for measuring ambient noise. Ignored if 'X' is not supplied. Two options are available: 
#' \itemize{
#' \item \code{adjacent}: measure ambient noise right before the signal (using argument 'mar' to define duration of ambient noise segments). 
#' \item \code{custom}: measure ambient noise segments referenced in the selection table (labeled as 'ambient' in the 'signal.type' column).
#' }
#' @param parallel Numeric vector of length 1. Controls whether parallel computing is applied by specifying the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control if progress bar is shown. Default is \code{TRUE}.
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a frequency bandpass filter (in kHz). Default is \code{NULL}.
#' @param hop.size A numeric vector of length 1 specifying the time window duration (in ms). Default is 1 ms, which is equivalent to ~45 wl for a 44.1 kHz sampling rate. Ignored if 'wl' is supplied.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#' is NULL. Ignored if \code{bp = NULL}. If supplied, 'hop.size' is ignored. 
#' Note that lower values will increase time resolution, which is more important for amplitude ratio calculations. 
#' @param PSD Logical to control whether the Probability Mass Function (the probability distribution of frequencies). See \code{\link[seewave]{meanspec}}. Default is \code{FALSE}.
#' @param norm Logical to control whether amplitude values are normalized (divided by the maximum) so the highest value is 1. See \code{\link[seewave]{meanspec}}. Default is \code{TRUE}.
#' @param dB A character string of length 1 specifying the type dB to return: "max0" for a maximum dB value at 0, "A", "B", "C", "D", and "ITU" for common dB weights. See \code{\link[seewave]{meanspec}}. Default is \code{"A"}.
#' @param averaged Logical to control if frequency spectra are averaged within a sound file. Default is \code{TRUE}.
#' @return A list containing the the frequency spectra for each sound file or wave object (if 'X' is supplied).
#' @export
#' @name noise_profile
#' @details The function `noise_profile()` allows to estimate the frequency spectrum of ambient noise. This can be done on extended selection tables (using the segments containing no signal) or over complete sound files in the working directory (or path supplied). The function uses \code{\link[seewave]{meanspec}} internally to calculate frequency spectra. 
#' @examples
#' {
#' # load example data
#' data("playback_est")
#'
#' # custom noise reference
#' noise_profile(X = playback_est, mar = 0.01, pb = FALSE, noise.ref = "custom")
#' 
#' # remove noise selections
#' pe <- playback_est[playback_est$signal.type != "ambient", ]
#'  
#'  noise_profile(X = pe, mar = 0.01, pb = FALSE, noise.ref = "adjacent")
#' }
#' 
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#' @seealso \code{\link{excess_attenuation}}
#' @references {
#' Araya-Salas, M. (2020). baRulho: baRulho: quantifying habitat-induced degradation of (animal) acoustic signals in R. R package version 1.0.2.
#' 
#' }
#last modification on nov-01-2019 (MAS)

noise_profile <- function(X = NULL, files = NULL, mar = NULL, noise.ref = "adjacent", parallel = 1, pb = TRUE, path = NULL,
                      bp = NULL, hop.size = 1, wl = NULL, PSD = FALSE, norm = TRUE, dB = "A",  averaged = TRUE){
  
  # get call argument names
  argus <- names(as.list(base::match.call()))
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  # if X was supplied
  if (!is.null(X)){
    
    # set files to null
    files <- NULL
    
    # is extended sel tab
    if (!warbleR::is_extended_selection_table(X)) 
      stop("'X' must be and extended selection table") 
      
      # check signal.type column 
      if (is.null(X$signal.type)) stop("'X' must containe a 'signal.type' column")
    
    # invert selections so gaps become selections instead if noise.ref != ambient
    if (noise.ref == "custom" & !any(X$signal.type == "ambient")) stop("'noise.ref = custom' but no 'ambient' label found in 'signal.type' column ") 
    
    # keep only 'ambient' selections
    if (noise.ref == "custom")  
    X <- X[X$signal.type == "ambient", ]
     
    if (noise.ref == "adjacent" & is.null(mar)) stop("'mar' must be supplied when 'noise.ref == 'adjacent''")
    
  } else # if  no  files and no X get files in path
  if (is.null(files)){
    
    #check path to working directory
    if (is.null(path)) path <- getwd() else 
      if (!dir.exists(path)) stop("'path' provided does not exist") else
        path <- normalizePath(path)
      
      files <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE) 
      
      if (length(files) == 0) stop("No files found in working directory (alternatively supply 'X')")
      }
  
  # check files
  if (!is.null(files)){
   if (any(!file.exists(file.path(path, files)))) stop(paste(paste(files[!file.exists(files)], collapse = "/"), "was (were) not found"))
  
  # created selection table from sound files
  X <- warbleR::selection_table(whole.recs = TRUE, pb = FALSE, path = path)
  
  # filter sound files in files
  X <- X[X$sound.files %in% files, ]
  
  # add signal column
  X$signal.type <- "ambient"
  
  # set noise.ref to ambient so the whole sound file is measured
  noise.ref <- "custom"
    }
  
  # if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # hopsize  
  if (!is.numeric(hop.size) | hop.size < 0) stop("'parallel' must be a positive number") 
  
  # adjust wl based on hope.size
  if (is.null(wl))
    wl <- round(attr(X, "check.results")$sample.rate[1] * hop.size, 0)
  
  # set pb options 
  pbapply::pboptions(type = ifelse(as.logical(pb), "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # calculate STR 
  noise.profiles <- pbapply::pblapply(1:nrow(X), cl = cl, function(y){
      
     # extract  complete sound file for custom or files in folder
      if (noise.ref == "custom")
        noise.wv <- warbleR::read_wave(X = X, index = y, from = 0, to = Inf, path = path)

      if (noise.ref == "adjacent")
        {
        #reset time coordinates of signals if lower than 0 o higher than duration
        stn <- X$start[y] - mar

        if (stn < 0)     
          stn <- 0
        
        # read ambient noise
        noise.wv <- warbleR::read_wave(X = X, index = y, from = stn, to = X$start[y])
        }
      
      # mean spec
      mspc <- meanspec(wave = noise.wv, f = noise.wv@samp.rate, plot = FALSE, wl = wl, ovlp = 0, PSD = PSD, PMF = FALSE, norm = norm, dB = dB)  

      # name columns
      colnames(mspc) <- c("freq", "amp")

      # add sound file name
      mspc <- data.frame(sound.files = X$sound.files[y], selec= X$selec[y], mspc)

      # add band-pass frequency filter
      if (!is.null(bp)) 
        mspc <- mspc[mspc$freq >= bp[1] & mspc$freq <= bp[2], ]
      
      return(mspc)
      })
  
  # get numbers of rows
  rws <- sapply(noise.profiles, nrow)
  
  # make all the same length if noise.ref is adjacent
  if (length(unique(rws)) > 1 & noise.ref == "adjacent"){
    
    # gt freq range of minimum
    fr.range <- range(noise.profiles[[which.min(rws)]]$frequency)
  
    # interpolate so all have the same number of frequency bins
    noise.profiles <- lapply(noise.profiles, function(Y){
      
      # interpolate
      Yappr <- approx(x = Y$freq, 
             y = Y$amp, 
             xout = seq(from = fr.range[1], to = fr.range[2],
            length.out = min(rws)), 
             method = "linear")  
      
      Ydf <- data.frame(sound.files = Y$sound.files[1], selec = Y$selec[1], freq = Yappr$x, amp = Yappr$y)
      
      return(Ydf)
    })  
    
  }

  # put together in 1 data frame
  noise.profile <- do.call(rbind, noise.profiles)
  
  # get mean by sound file
  if (averaged)
    noise.profile <- aggregate(formula = amp ~ sound.files + freq, data = noise.profile, FUN = mean)  
  
  return(noise.profile)
  }
