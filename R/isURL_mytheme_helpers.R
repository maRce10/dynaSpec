#' Functions to check if a string is a URL or change a ggplot theme
#' 
#' Test if string is URL; Not intended for direct use.
#' 
#' @param x a string to test for being a URL
#' @noRd
#' @return TRUE or FALSE 

is.url <-function(x) { 
    grepl("www.|http:|https:", x)
}

#' @description Custom ggplot theme helper functions
#' @param bg a background color to be passed to the custom theming function
#' @noRd
#' @return a ggplot theme

#base ggplot theme for all specs used in testSpec.R
mytheme<-function(bg){
    ggplot2::theme_bw() +ggplot2::theme(
    panel.background=ggplot2::element_rect(fill=bg),
    panel.grid.major=ggplot2::element_blank(),
    panel.grid.minor=ggplot2::element_blank(),
    axis.text=ggplot2::element_text(size=16),
    plot.margin=ggplot2::margin(8,8,8,8,unit="pt"),
    plot.title=ggplot2::element_text(size=20,face="bold"),
    plot.subtitle=ggplot2::element_text(size=16),
    axis.title=ggplot2::element_text(size=20,face="bold"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10, r = 2, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 10, b = 0, l = 0)),
    legend.text=ggplot2::element_text(size=16),
    legend.title=ggplot2::element_text(face="bold",size=16)
    )}

#' @description Custom ggplot (larger) theme helper function 
#' @param bg a background color to be passed to the custom theming function
#' @noRd
#' @return a ggplot theme

#base ggplot theme for all specs used in paged_spectro
mytheme_lg<-function(bg){
    ggplot2::theme_bw() +ggplot2::theme(
    panel.background=ggplot2::element_rect(fill=bg),
    panel.grid.major=ggplot2::element_blank(),
    panel.grid.minor=ggplot2::element_blank(),
    axis.text=ggplot2::element_text(size=36),
    plot.margin=ggplot2::margin(18,18,18,18,unit="pt"),
    plot.title=ggplot2::element_text(size=40,face="bold"),
    plot.subtitle=ggplot2::element_text(size=36),
    axis.title=ggplot2::element_text(size=40,face="bold"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10, r = 2, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 10, b = 0, l = 0)),
    legend.text=ggplot2::element_text(size=36),
    legend.title=ggplot2::element_text(face="bold",size=40)
  )}