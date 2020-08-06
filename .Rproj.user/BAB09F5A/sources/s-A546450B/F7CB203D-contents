#' Helper functions for paged_spectro.R
#' 
#' 
is.url <-function(x) { 
    grepl("www.|http:|https:", x)
}

#base ggplot theme for all specs used in testSpec.R
mytheme<-function(bg){
    ggplot2::theme_bw() +ggplot2::theme(
    panel.background=ggplot2::element_rect(fill=bg),
    panel.grid.major=ggplot2::element_blank(),
    panel.grid.minor=ggplot2::element_blank(),
    axis.text=ggplot2::element_text(size=16),
    plot.margin=margin(8,8,8,8,unit="pt"),
    plot.title=element_text(size=20,face="bold"),
    plot.subtitle=element_text(size=16),
    axis.title=element_text(size=20,face="bold"),
    axis.title.x = element_text(margin = margin(t = 10, r = 2, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 20, r = 10, b = 0, l = 0)),
    legend.text=element_text(size=16),
    legend.title=element_text(face="bold",size=16)
    )}

#base ggplot theme for all specs used in paged_spectro
mytheme_lg<-function(bg){
    ggplot2::theme_bw() +ggplot2::theme(
    panel.background=ggplot2::element_rect(fill=bg),
    panel.grid.major=ggplot2::element_blank(),
    panel.grid.minor=ggplot2::element_blank(),
    axis.text=ggplot2::element_text(size=36),
    plot.margin=margin(18,18,18,18,unit="pt"),
    plot.title=element_text(size=40,face="bold"),
    plot.subtitle=element_text(size=36),
    axis.title=element_text(size=40,face="bold"),
    axis.title.x = element_text(margin = margin(t = 10, r = 2, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 20, r = 10, b = 0, l = 0)),
    legend.text=element_text(size=36),
    legend.title=element_text(face="bold",size=40)
  )}