#requires installation of imagemagick 
# on a mac, run 'brew install imagemagick' in terminal (provided you've already set up Homebrew (https://brew.sh/))  
# also requires ffmpeg, which should be installed with gganimate...you can install/update it on mac w/ 'brew install ffmpeg'




#Install/load pacman
if(!require(pacman)){install.packages("pacman");require(pacman)}
#Install/load tons of packages
p_load(ggplot2,seewave,tuneR,viridis,scales,gganimate,av,grid,tidyverse,png,warbleR,tools,devtools,ari)

#set WavPlayer if using Mac; otherwise, mixed results trying to play sounds from R
if(Sys.info()["sysname"]=="Darwin"){setWavPlayer("/usr/bin/afplay")}

#####################################################
#### FUNCTION DEFINITIONS
##
#########
#function taken from https://github.com/trinker/reports/blob/master/R/is.url.R
is.url <-function(x) { 
    grepl("www.|http:|https:", x)
}

#base ggplot theme for all specs
mytheme<-function(bg){
    ggplot2::theme_bw() +ggplot2::theme(
    panel.background=ggplot2::element_rect(fill=bg),
    panel.grid.major=ggplot2::element_blank(),
    panel.grid.minor=ggplot2::element_blank(),
    axis.text=ggplot2::element_text(size=18),
    plot.title=element_text(size=22,face="bold"),
    plot.subtitle=element_text(size=18),
    axis.title=element_text(size=22,face="bold"),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 20, r = 10, b = 0, l = 0)),
    legend.text=element_text(size=18),
    legend.title=element_text(face="bold",size=18)
  )}

############
# processSound: filter, crop, and segment wav (internal function)

processSound<-function(wav0,filter,ampThresh,crop,xLim,...){
  smplRt<-wav0@samp.rate
  fileDur0<-max(length(wav0@left),length(wav0@right))/smplRt 
  ##Figure out crop & spectrogram temporal window width
  
  #If crop provided as single digit, interpret as first X sec
  if(!is.null(crop)){ 
   if(length(crop)==1){if(crop==F){crop <- c(0,fileDur0)}else{
     crop <- c(0,crop)}}
  #If supplied crop longer than wav, treated as max length & crop set to duration
   if(crop[2]>fileDur0){
     crop <- c(0,fileDur0)
     cat(paste0("\n**** Crop longer than file duration: ",round(fileDur0,2),", treated as max length & ignored"))}
  }else{
  #If crop not supplied, default crop to duration, unless >10 sec, warning user
    if(fileDur0>10){
      crop=c(0,10)
      cat("\n*****\nCropping to 1st 10 sec due to slow processing of large WAV files.\nTo override, set crop=F or crop=c(startInSec,stopInSec)\n*****\n")
      }else{crop=c(0,fileDur0)}
  }
  
  #crop is set now for all cases
  
  #assign new wave file for use going forward
  #the bit=wav0@bit is REALLY important! Should be standard, but it doesn't work
  wav<-if(crop[2]==fileDur0){wav0}else{seewave::cutw(wav0,from=crop[1],to=crop[2],output="Wave",bit=wav0@bit)}
  wavDur<- max(length(wav@left),length(wav@right))/wav@samp.rate 
  
  #Apply filters
  if(!is.null(filter)){
    wav=seewave::ffilter(wave=wav,from=filter[1]*1000,to=filter[2]*1000,bandpass=F,output="Wave",rescale=T)}
  if(ampThresh!=0){wav<-seewave::afilter(wav,f=smplRt,threshold=ampThresh,plot=F,output="Wave")}
  
  
  
  ##Deal with xLim for segmenting wavs
  # if xLim not provided, default to smaller of 5sec or wav duration
  if(is.null(xLim)){
    xLim<-c(0,min(5,wavDur))
    if(xLim[2]==5){
      cat("\n*****\nxLim set to 5 sec by default; define to override\n*****\n")  
    }
  }else{
    #If xLim provided as single digit, interpret as X sec
    if(length(xLim)==1){xLim <- c(0,xLim)}
    }
  
  #Add silence at the end if (user-supplied) xLim>cropped Duration or xLim doesn't divide into even segments of wave duration
  timeRemainder<-(ceiling(wavDur/xLim[2])*xLim[2]-wavDur) > 0.001#(wavDur%/%xLim[2]-wavDur/xLim[2]
  if(xLim[2]>wavDur|timeRemainder){
    if(timeRemainder){
      diffT<-ceiling(wavDur/xLim[2])*xLim[2]-wavDur
      }else{
      diffT<-xLim[2]-wavDur
      }
    fillerWAV<-tuneR::silence(duration=diffT,samp.rate=smplRt,xunit="time",pcm=T,bit=wav@bit)
    wav<-tuneR::bind(wav,fillerWAV)
      #pastew results in intermittent problems! Don't use! bind seems much more dependable
      #seewave::pastew(wave1=fillerWAV,wave2=wav,at="end",output="Wave",join=T,bit=wav0@bit)
    wavDur<- max(length(wav@left),length(wav@right))/wav@samp.rate 
    }
  #Segment wav or make list of 1 if no segmentation
  segLens <- seq(0,wavDur,xLim[2])
  indx<- 1:(length(segLens)-1)
  segWavs<-lapply(indx,function(i) seewave::cutw(wav,from=segLens[i],to=segLens[i+1],output="Wave",bit=wav0@bit))
  #browser()
  return(list(newWav=wav,segWavs=segWavs,wavDur=wavDur,segLens=segLens,xLim=xLim))
}#End processSound



##########
# ggSpec: helper function to create ggplot spectrogram of wave file

ggSpec<-function(wav,segLens,ovlp,wl,wn,yLim,xLim,colPal,colbins,ampTrans,plotLegend,onlyPlotSpec,isViridis,bg,fontAndAxisCol,min_dB,bgFlood,optim,timeAdj,...)
{
  if(missing(segLens)){nSegs=1}else{
    nSegs=length(segLens)-1
  }
  
  
  # bg="#ebe834"
  #Font color adapted from https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color
   
  if(is.null(fontAndAxisCol)){
  autoFontCol=T
  bgRGB<-grDevices::col2rgb(bg)
  fontAndAxisCol<-if (bgRGB["red",1]*0.299 + bgRGB["green",1]*0.587 + bgRGB["blue",1]*0.114 > 149){"#000000"}else{"#ffffff"}
  }else{autoFontCol=F}
  #For testing font contrast against bg
  # par(bg=bg)
  # plot(1,1,col="transparent")
  # text(1,1,"READABLE?",cex=5,col=contrastFont)
  
  
  #Modified from seewave::ggspectro
  #--->
  # norm = F is important for having similar gain across different recordings
  # spectrogram<-seewave::spectro(wav,plot=F,ovlp=ovlp,wl=wl,wn=wn,norm=F)
  #normalize(wav,center=F,pcm=T)
  spectrogram<-seewave::spectro(wav,plot=F,ovlp=ovlp,wl=wl,wn=wn)
  freq <- rep(spectrogram$freq, times = ncol(spectrogram$amp))
  time <- rep(spectrogram$time, each = nrow(spectrogram$amp))
  amplitude <- as.vector(spectrogram$amp)
  #<--------
  df<-dplyr::tibble(time,freq,amplitude)

  
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
        pb<-utils::txtProgressBar(0,100,style=3)
        setTxtProgressBar(pb,0)
        }      
      if(i==nSegs){
        df_i <- subset(df,time>=segLens[i]&time<=segLens[i+1]) #last segment is inclusive on left & right bounds
      }else{
        df_i<-subset(df,time>=segLens[i]&time<segLens[i+1]) #all other segs inclusive on left
      }
      
    }else{df_i=df}
 
  
    #Plot that thang
    Glist[[i]]<-ggplot2::ggplot(df_i,ggplot2::aes(x=time,y=freq,z=amplitude))+xlim(segLens[i],segLens[i+1])+ylim(yLim)+
      #Labels
      ggplot2::labs(x="Time (s)",y="Frequency (kHz)",fill="Amplitude\n(dB)\n")+{
       #Set scale according to viridis or custom color scheme
        if(isViridis){scale_fill_viridis(limits=c(min_dB,0),na.value="transparent",option=colPal,trans=scales::modulus_trans(p=ampTrans))}else{
        scale_fill_gradient(limits=c(min_dB,0),na.value="transparent",low=colPal[1],high=colPal[2],trans=scales::modulus_trans(p=ampTrans))}
        }+
    #Make contours  
    ggplot2::stat_contour(geom="polygon",ggplot2::aes(fill=..level..),bins=colbins,na.rm=T)+
    #Set base theme  
    mytheme(bg)+{
       #If user supplied fontAndAxisCol, change those settings (regardless of whether bg is flooded or not)
           if(!autoFontCol){
            ggplot2::theme(axis.text=ggplot2::element_text(colour=fontAndAxisCol),text=ggplot2::element_text(colour=fontAndAxisCol),axis.line = ggplot2::element_line(colour=fontAndAxisCol),axis.ticks=element_line(colour=fontAndAxisCol))
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
    
    if(nSegs>1){
      setTxtProgressBar(pb,(100/nSegs*i))
      }
  }#end for loop
  
  rm(spectrogram)

return(list(specList=Glist,fontAndAxisCol=fontAndAxisCol,autoFontCol=autoFontCol))
}#end




##########
# testSpec: Custom function for generating ggplot spectrogram objects in R

#Parameter descriptions:
  # *- These parameters are really important for the look of your spectrogram

  # soundFile: filenames should be relative to working directory (e.g. "song examples/1.wav"); handles .mp3 and .wav
  # destFolder: needs to be like "figures/spectrograms/" to refer within working directory; goes to soundFile folder by default
  # if outFilename is left out, will use input name in output filename
  # *colPal: color palette; one of "viridis","magma","plasma","inferno","cividis" from the viridis package (see: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html) OR a 2 value vector (e.g. c("white","black)), defining the start and end of a custom color gradient
  # *crop: subset of recording to include; if crop=NULL, use whole file; if number, interpreted as crop first X.X sec; if c(X1,X2), interpreted as specific time interval in sec
  # *xLim: is the time limit in seconds for all spectrograms; i.e. page width in seconds for multi-page dynamic spectrograms (defaults to WAV file length, unless file duration >5s)
  # *yLim: is the frequency limits (y-axis); default is c(0,10) aka 0-10kHz
  # *ampTrans: amplitude transform; defaults to identity (actual dB values); specify a decimal number for the lambda value of scales::modulus_trans(); 2.5 is a good place to start. (This amplifies your loud values the most, while not increasing background noise much at all)
  # *min_db: the minimum decibel (quietest sound) to include in the spec; defaults to -30 (-40 would include quieter sounds; -20 would cut out all but very loud sounds)
  # filter: apply a bandpass filter? Defaults to none (NULL). Expects 'c(0,2)' where sound from 0 to 2kHz would be filtered out 
  # bg:background color (defaults to 1st value of chosen palette)
  # *wl: window length for the spectrogram (low vals= higher temporal res; high vals= higher freq. res). Default 512 is a good tradeoff
  # ovlp: how much overlap (as %) between sliding windows to generate spec? Default 90 looks good, but takes longer
  # wn: window name (slight tweaks on algorithm that affect smoothness of output) see ?spectro
  # colbins: default 30: increasing can smooth the color contours, but take longer to generate spec
 # ampThresh: amplitude threshold as a % to cut out of recording (try 5 to start); default= no filtering (high data loss with this; not recommended; play with min_dB first)
# bgFlood: do you want the background color to spill into the axis margins?
#fontAndAxisCol: the color of legend text if onlyPlotSpec=T (since margins will be white, with black text); if bgFlood=T, this will be the color of axis margins, labels & legend text. If you don't supply this, it will be picked automatically to be white or black based on supplied bg color
#optim: NULL by default; this is an experimental feature to simplify the dataframe of the FFT-processed waveform used to generate the spectrogram

##################
#set defaults for stepping into function for testing
# colPal="inferno";crop=NULL;bg=NULL;filter=NULL;xLim=NULL;yLim=c(0,10);plotLegend=F;onlyPlotSpec=T;ampTrans=2.5;min_dB=-30;wl=512;ovlp=90;wn="blackman";specWidth=6;specHeight=2;colbins=30;ampThresh=0;bgFlood=F;fontAndAxisCol=NULL
##################

testSpec<-function(soundFile,destFolder,outFilename,colPal="inferno",crop=NULL,bg=NULL,filter=NULL,xLim=NULL,yLim=c(0,10),plotLegend=F,onlyPlotSpec=T,ampTrans=1,min_dB=-30,wl=512,ovlp=90,wn="blackman",specWidth=6,specHeight=2,colbins=30,ampThresh=0,bgFlood=F,fontAndAxisCol=NULL,optim=NULL,...)
  {
 
   #Put in soundFile directory if unspecified
  if(missing(destFolder)){
      if(is.url(soundFile)){destFolder=getwd()
      }else{destFolder=dirname(file_path_as_absolute(soundFile))}
    }
  if(!grepl("/$",destFolder)){destFolder=paste0(destFolder,"/")}#if destFolder missing terminal /, add it  
  
  if(is.url(soundFile)){
    download.file(soundFile,paste0(destFolder,basename(soundFile)))
    soundFile=paste0(destFolder,basename(soundFile))
    }
 
  
  #Handle file naming for spec
  if(missing(outFilename)){outFilename=paste0(file_path_sans_ext(basename(soundFile)),".PNG")}
  if(!grepl(".png|PNG",outFilename)){outFilename=paste0(outFilename,".png")}#if user didn't put suffix onto output filename, add .jpeg
  
  #Are we dealing with a custom or a viridis palette?
  if(length(colPal)==1){isViridis<-T}else{isViridis<-F}
  
  #set background color as palette level 1 if missing
  if(is.null(bg)){ 
    if(isViridis){pal=eval(parse(text=paste0("viridis::",colPal)));bg=pal(1)}else{bg=colPal[1]}
    }
  
  #Convert MP3s to WAV
  if(file_ext(soundFile)=="mp3"){
      print("***Converting mp3 to wav***")
      wav0 <- readMP3(soundFile)
    }else{wav0<-readWave(soundFile)}
  
  #Make sure it's mono (arbitrarily using left channel)
  if(wav0@stereo){wav0 <- tuneR::mono(wav0,which="left")}
  
  #######
  #crop, filter, normalize, deal with missing parameters for soundFile
  prepped<-processSound(wav0,crop=crop,xLim=xLim,filter=filter,ampThresh)
  
  if(length(yLim)==1){yLim=c(0,yLim)}
  
  # Send processed wave file & segment length info to spec generation function
  specOutList<-ggSpec(wav=prepped$newWav,segLens=prepped$segLens,colPal=colPal,isViridis=isViridis,crop=crop,bg=bg,filter=filter,xLim=prepped$xLim,yLim=yLim,plotLegend=plotLegend,onlyPlotSpec=onlyPlotSpec,ampTrans=ampTrans,ampThresh=ampThresh,min_dB=min_dB,wl=wl,ovlp=ovlp,wn=wn,colbins=colbins,bgFlood=bgFlood,fontAndAxisCol = fontAndAxisCol,optim=optim)
    
  plot(specOutList$specList[[1]])
 
  if(length(prepped$segWavs)>1){
         cat("\nFor segmented spectrogram, only segment 1 shown\n")
  }    
  #browser()
  #Make list of all info needed to recreate spec look in videos  
 specParams=list(soundFile=soundFile,destFolder=destFolder,outFilename=outFilename,crop=crop,colPal=colPal,isViridis=isViridis,xLim=prepped$xLim,yLim=yLim,plotLegend=plotLegend,onlyPlotSpec=onlyPlotSpec,ampTrans=ampTrans,ampThresh=ampThresh,min_dB=min_dB,bg=bg,wl=wl,ovlp=ovlp,wn=wn,specWidth=specWidth,specHeight=specHeight,colbins=colbins,bgFlood=bgFlood,autoFontCol=specOutList$autoFontCol,fontAndAxisCol = specOutList$fontAndAxisCol,spec=specOutList$specList,newWav=prepped$newWav,segWavs=prepped$segWavs)      
 return(specParams)
}#end TestSpec



#############################################################
# rspectVid: Function for outputting a video of spectrogram

# specParams   an object returned from testSpec()
# vidName      expects "FileName.mp4"; if not supplied will be named after the file you used in testSpec()
# framerate    by default, set to 30
# highlightCol default "gray50"
# cursorCol    default "white"
# delTemps=T   By default, deletes temporary files (specs & WAV files used to create video spec) after .mp4 generated
########
rspectVid <-function(specParams,destFolder,vidName,framerate=30,highlightCol="#4B0C6BFF",highlightAlpha=.6,cursorCol="#4B0C6BFF",delTemps=T,... )
{
if(!ari::have_ffmpeg_exec()){
  cat("\n*****This script needs ffmpeg to work*****\n")
  cat("If you have a mac, with HomeBrew installed, you can fix this easily in terminal with:\n")
  cat("\n>\tbrew install ffmpeg\n")
  cat("\nIf not, download and install it from ffmpeg.org")
  }else{

if(missing(destFolder)){destFolder <- specParams$destFolder}
    
if(!missing(vidName)){
    iName0=tools::file_path_sans_ext(vidName)
    vidName=paste0(destFolder,iName0,".mp4")
    }else{
    iName0<-tools::file_path_sans_ext(specParams$outFilename)
    vidName=paste0(destFolder,iName0,".mp4")
    }#base name for output, sans extension

    #To avoid probs if a file contains ' 
    vidName<-gsub("'",".",vidName)
    iName0<-gsub("'",".",iName0)
    
  tempdir<-paste0(destFolder,"temp/")
  dir.create(tempdir,showWarnings=F)
  
  
    #always export the newWav version that has been cropped/padded according to user parameters
    cat(paste0("Temporary files saved at: ",tempdir))
    newWavOut=paste0(tempdir,iName0,"_forVideo.wav")
    tuneR::writeWave(specParams$newWav,filename=newWavOut)
    
    #export wav files if spec is to be segmented; not necessary if wav is unaltered
    if(length(specParams$segWavs)>1){
    #create list of names for WAV audio segments
    outWAV<-lapply(1:length(specParams$segWavs),function(x) {paste0(tempdir,iName0,"_",x,"_.wav")}) 
    invisible(
      lapply(1:length(specParams$segWavs), function(x){fn=outWAV[[x]]
          tuneR::writeWave(specParams$segWavs[[x]],file=fn)
          cat(paste0("\nSaved temp wav segment: ",fn))}))
      }
    

for(i in 1:length(specParams$segWavs))
{
  #Address missing variables
  
  iName<-paste0(iName0,ifelse(length(specParams$segWavs)==1,"",paste0("_",i,"_")))

    #Save background spectrogram PNG to temp directory using tested parameters
    outPNG<-paste0(tempdir,paste0(iName,".png"))
    outTmpVid<-paste0(tempdir,paste0(iName,".mp4"))
    
    
    #output spec without axes, b/c we'll have to 
    ggsave(filename=outPNG,plot=specParams$spec[[i]]+ggplot2::theme_void()+ggplot2::theme(panel.background=ggplot2::element_rect(fill=specParams$bg),legend.position = 'none'),dpi=300,width=specParams$specWidth,height=specParams$specHeight,units="in")
    print(paste0("Spec saved @ ",outPNG))
 #Read PNG bitmap back in
  spec_PNG<-readPNG(outPNG)
  spec_width_px<-attributes(spec_PNG)$dim[2]
  spec_height_px<-attributes(spec_PNG)$dim[1]
    
  #Create data frame for highlighting box animation for i^th wav segment
   range_i<-c((i-1)*specParams$xLim[2],(i-1)*specParams$xLim[2]+specParams$xLim[2])
   cursor<-seq(range_i[1],range_i[2],specParams$xLim[2]/framerate)
  played<-tibble(xmin=cursor,xmax=rep(range_i[2],length(cursor)),ymin=rep(specParams$yLim[1],length(cursor)),ymax=rep(specParams$yLim[2], length(cursor)))
  

  #Make ggplot overlay of highlight box on spectrogram
  vidSegment<-{
    ggplot2::ggplot(played)+ggplot2::xlim(range_i)+ggplot2::ylim(specParams$yLim)+
      #Labels
      ggplot2::labs(x="Time (s)",y="Frequency (kHz)",fill="Amplitude\n(dB)\n")+
      #Set base theme  
      mytheme(specParams$bg)+
      
       #Animate() seems to shrink font size a bit, so let's bump it back up
      ggplot2::theme(axis.text=ggplot2::element_text(size=24),
                     plot.title=element_text(size=26,face="bold"),
                     plot.subtitle=element_text(size=24),
                     axis.title=element_text(size=26,face="bold"),
                     legend.text=element_text(size=24),
                     legend.title=element_text(face="bold",size=24)
                     )+
       
      #Conditional theming based on user prefs (note, legend not currently supported)
      #Since I'm reimporting spec as a raster, legend would need to rebuilt manually...gets a little
      #warped if I embed it in the raster...doesn't look good.
       {
            
        #If user supplied fontAndAxisCol, change those settings (regardless of whether bg is flooded or not)
         if(!specParams$autoFontCol){
            ggplot2::theme(axis.text=ggplot2::element_text(colour=specParams$fontAndAxisCol),text=ggplot2::element_text(colour=specParams$fontAndAxisCol),axis.line = ggplot2::element_line(colour=specParams$fontAndAxisCol),axis.ticks=element_line(colour=specParams$fontAndAxisCol))
            }else{}
       }+{
          
         #get rid of axes & legend if requested
          if(specParams$onlyPlotSpec){ggplot2::theme_void()+ ggplot2::theme(plot.background=ggplot2::element_rect(fill=specParams$bg),text=ggplot2::element_text(colour=specParams$fontAndAxisCol))
            }else{
              
              #For cases where axes are plotted
              #if axes to be plotted, flood panel bg color over axis area?
              if(specParams$bgFlood){ggplot2::theme(plot.background=ggplot2::element_rect(fill=specParams$bg),axis.text=ggplot2::element_text(colour=specParams$fontAndAxisCol),text=ggplot2::element_text(colour=specParams$fontAndAxisCol),axis.line = ggplot2::element_line(colour=specParams$fontAndAxisCol),axis.ticks=ggplot2::element_line(colour=specParams$fontAndAxisCol),legend.background=ggplot2::element_rect(fill=specParams$bg))}else{}
            }
       }+
      
      #Add spectrogram
      ggplot2::annotation_custom(rasterGrob(spec_PNG,width = unit(1,"npc"), height = unit(1,"npc")),- Inf, Inf, -Inf, Inf)+
      
      #Add box highlights for playback reveal    
      ggplot2::geom_rect(data=played,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill=highlightCol,alpha=highlightAlpha)+
      
      #Add cursor
      ggplot2::geom_segment(data=played,aes(x=xmin,xend=xmin,y=ymin,yend=ymax),col=cursorCol,size=2) +
  
      #Add animation
      #**** Time consuming animation stage *****
      gganimate::transition_reveal(xmin)
   
    }#end GGPLOT stuffs
  
  #Increase plot margin slightly b/c it gets changed when exporting to video for some reason
  if(!specParams$onlyPlotSpec){axisMargin=40}else{axisMargin=0}
  
  #### Export animated ggplot specs  
  #save Audio File with sound in 1 step only if not segmented
  if(length(specParams$segWavs)==1){
  animate(vidSegment,renderer=av_renderer(vidName,audio=newWavOut),duration=specParams$xLim[2],width=spec_width_px,height=spec_height_px+axisMargin) #Need to save audio for segments!!
  }else{
    animate(vidSegment,renderer=av_renderer(outTmpVid,audio=outWAV[[i]]),duration=specParams$xLim[2],width=spec_width_px,height=spec_height_px) #Need to save audio for segments!!
    }
}#end for loop extracting video pieces

  #if necessary, combine segments
  if(length(specParams$segWavs)>1){
    tmpPaths<-paste0("file '",gsub(".wav","",unlist(outWAV)),".mp4' duration ",specParams$xLim[2])
    writeLines(tmpPaths,paste0(tempdir,"mp4Segments.txt"))
    #Turns out this was wrong or has been fixed!! MP4s CAN be combined!
    # #Unfortunately, can't just slap MP4 files together, so have to have an intermediate .ts file step
    # ffmpegTransCode<-paste0(ffmpeg_exec(),' -y -i "',unlist(file_path_sans_ext(outWAV)),'.mp4" -vsync 1 -c copy "',unlist(file_path_sans_ext(outWAV)),'.mkv"')
    # invisible(sapply(ffmpegTransCode,system))
    #now combine .ts files into .mp4
   
    #For matching audio & video lengths:
    cropSmplRt<-specParams$newWav@samp.rate
    cropFileDur<-max(length(specParams$newWav@left),length(specParams$newWav@right))/cropSmplRt
    cropFileDur2<-seconds_to_period(cropFileDur)
    cropFileDur3<-sprintf(fmt='%02d:%02d:%2.3f',hour(cropFileDur2),minute(cropFileDur2),second(cropFileDur2))
    
    #Concat Step 1
    #concatenate mp4 segments
    #slight stutter for continuous sounds across segments, but the alternative step below doesn't work quite right, so good enough
    system(paste0(ari::ffmpeg_exec(),' -f concat -ss 00:00:00.000 -safe 0 -i "',paste0(tempdir,"mp4Segments.txt"),'" -codec copy -y "',vidName,'"') )
    
    
    #Concat Step 2
    #Add audio track back in (couldn't figure how to combine these steps)
    #THIS STEP CURRENTLY DOESN'T WORK WELL (DROPS LAST FEW FRAMES B/C MISMATCH IN A/V LENGTHS)
    # system(paste0(ari::ffmpeg_exec(),' -ss 0 -i "',paste0(tempdir,"deleteme.mp4"),'" -i "',newWavOut,'"  -c:v libx264 -map 0:v:0 -map 1:a:0 -c:a aac -ac 1 -b:a 192k -y -vsync 1 -t ',cropFileDur3,' "',vidName,'"'))
    
    
    #Old Concat Step 1 (when step 2 is implemented); results in deleteme.mp4 intermediate
    # system(paste0(ari::ffmpeg_exec(),' -f concat -safe 0 -i "',paste0(tempdir,"mp4Segments.txt"),'" -codec copy -y "',paste0(tempdir,"deleteme.mp4"),'"')) 
    
   
  }

  cat("\n\nAll done!\n")
  cat(paste0("file saved @",vidName))
  system(paste0('open "',vidName,'"'))
  
  if(delTemps){unlink(tempdir,recursive=T);print(paste0("FYI temporary file directory deleted @ ",tempdir))}
}#end else which passed FFMPEG check
}#end rspectVid definition
  
  

