# ggSpec: helper function to create ggplot spectrogram of wave file

ggSpec<-function(wav,soundFile,segLens,savePNG,specWidth,specHeight,destFolder,ovlp,wl,wn,yLim,xLim,colPal,colbins,ampTrans,plotLegend,onlyPlotSpec,isViridis,bg,fontAndAxisCol,min_dB,bgFlood,optim,timeAdj,...)
{
  if(missing(segLens)){nSegs=1}else{
    nSegs=length(segLens)-1
  }
  
  
  # bg="#ebe834"
  #Font color adapted from https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color
   
  if(is.null(fontAndAxisCol)){
  autoFontCol=TRUE
  bgRGB<-grDevices::col2rgb(bg)
  fontAndAxisCol<-if (bgRGB["red",1]*0.299 + bgRGB["green",1]*0.587 + bgRGB["blue",1]*0.114 > 149){"#000000"}else{"#ffffff"}
  }else{autoFontCol=FALSE}
  #For testing font contrast against bg
  # par(bg=bg)
  # plot(1,1,col="transparent")
  # text(1,1,"READABLE?",cex=5,col=contrastFont)
  
  
  #Modified from seewave::ggspectro
  #--->
  # norm =FALSE is important for having similar gain across different recordings
  # spectrogram<-seewave::spectro(wav,plot=FALSE,ovlp=ovlp,wl=wl,wn=wn,norm=FALSE)
  #normalize(wav,center=FALSE,pcm=TRUE)
  spectrogram<-seewave::spectro(wav,plot=FALSE,ovlp=ovlp,wl=wl,wn=wn,...)
  freq <- rep(spectrogram$freq, times = ncol(spectrogram$amp))
  time <- rep(spectrogram$time, each = nrow(spectrogram$amp))
  amplitude <- as.vector(spectrogram$amp)
  #<--------
  df <- data.frame(time,freq,amplitude)

  
  #experimental code to simplify the tibble for generating spec data
  if(!is.null(optim)){
    #nothing here yet
  }
  
  #####
  #Plot spec for every segment, saving specs as list for output
  Glist<-list()
  for (i in 1:(nSegs)){
    
    #subset df for segments
    if(nSegs>1){
      if(i==1){
        cat("\nSpectrogram ggplots of segmented WAVs being generated\n")
        }      
      if(i==nSegs){
        df_i <- subset(df,time>=segLens[i]&time<=segLens[i+1]) #last segment is inclusive on left & right bounds
      }else{
        df_i<-subset(df,time>=segLens[i]&time<segLens[i+1]) #all other segs inclusive on left
      }
      
    }else{df_i=df}
 
    ..level.. <- NULL
    
    #Plot that thang
    Glist[[i]]<-ggplot2::ggplot(df_i,ggplot2::aes(x=time,y=freq,z=amplitude))+ggplot2::xlim(segLens[i],segLens[i+1])+ggplot2::ylim(yLim)+
      #Labels
      ggplot2::labs(x="Time (s)",y="Frequency (kHz)",fill="Amplitude\n(dB)\n")+{
       #Set scale according to viridis or custom color scheme
        if(isViridis){viridis::scale_fill_viridis(limits=c(min_dB,0),na.value="transparent",option=colPal,trans=scales::modulus_trans(p=ampTrans))}else{
          ggplot2::scale_fill_gradient(limits=c(min_dB,0),na.value="transparent",low=colPal[1],high=colPal[2],trans=scales::modulus_trans(p=ampTrans))}
        }+
    #Make contours  
    ggplot2::stat_contour(geom="polygon",ggplot2::aes(fill=..level..),bins=colbins,na.rm=TRUE)+
    #Set base theme  
    mytheme(bg)+{
       #If user supplied fontAndAxisCol, change those settings (regardless of whether bg is flooded or not)
           if(!autoFontCol){
            ggplot2::theme(axis.text=ggplot2::element_text(colour=fontAndAxisCol),text=ggplot2::element_text(colour=fontAndAxisCol),axis.line = ggplot2::element_line(colour=fontAndAxisCol),axis.ticks=ggplot2::element_line(colour=fontAndAxisCol))
           }else{}
      }+{
      #get rid of axes & legend if requested
        if(onlyPlotSpec){ggplot2::theme_void()+ ggplot2::theme(plot.background=ggplot2::element_rect(fill=bg),text=ggplot2::element_text(colour=fontAndAxisCol))
        }else{
          #For cases where axes are plotted
            # if axes to be plotted, flood panel bg color over axis area?
            if(bgFlood){ggplot2::theme(plot.background=ggplot2::element_rect(fill=bg),axis.text=ggplot2::element_text(colour=fontAndAxisCol),text=ggplot2::element_text(colour=fontAndAxisCol),axis.line = ggplot2::element_line(colour=fontAndAxisCol),axis.ticks=ggplot2::element_line(colour=fontAndAxisCol),legend.background=ggplot2::element_rect(fill=bg))}else{}
            }
        }+{
        #Get rid of plotLegend if requested
        if(!plotLegend){ggplot2::theme(legend.position = "none")}else{ggplot2::theme(legend.position = "right")}
        }#end GGPLOT stuffs
    
    #Save plots if requested
    if(savePNG){
      if(i==1){
        baseNom<-basename(tools::file_path_sans_ext(soundFile))
        subDest<-fs::path(destFolder,paste0(baseNom,"_static_specs"))
        dir.create(subDest,showWarnings = FALSE)
        }
      fn_i=fs::path(subDest,paste0(baseNom,"_",i),ext="png")
      ggplot2::ggsave(fn_i,width=specWidth,height=specHeight,units="in")
      cat(paste0("\nStatic spec saved @",fn_i))
    }
    
  }#end for loop
  
  rm(spectrogram)

return(list(specList=Glist,fontAndAxisCol=fontAndAxisCol,autoFontCol=autoFontCol))
}#end

