#' dynaSpec: Dynamic spectrogram visualization
#' 
#' A set of tools to generate dynamic spectrogram visualizations in video format.
#' 
#' @import ggplot2
#' @import seewave
#' @import gganimate
#' @import viridis
#' @importFrom tuneR writeWave readWave readMP3 mono silence bind 
#' @importFrom png readPNG
#' @importFrom warbleR pblapply_wrblr_int
#' @importFrom scales modulus_trans
#' @importFrom ari have_ffmpeg_exec ffmpeg_exec
#' @importFrom png readPN
#' @importFrom grid grid.raster
#' @importFrom parallel makePSOCKcluster
#' @importFrom grDevices dev.off tiff adjustcolor
#' @importFrom graphics abline plot rect axis mtext par
#' @author Marcelo Araya-Salas & Matthew R. Wilkins
#'         Maintainer: Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'   
#' @docType package
#' @name dynaSpec
#' @details License: GPL (>= 2)  
#' > NULL 
#'
