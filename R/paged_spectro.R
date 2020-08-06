#' Make a paged dynamic spectrogram similar to spectral display in Adobe Audition
#' 
#' This function works on an object generated with \code{\link{prep_static_ggspectro}}, an alias for prepStaticSpec().
#' Video generation is very time consuming, and all the desired spectrogram parameters should be set
#' in the prep step. The output is an mp4 video of a dynamic spectrogram video. If the input sound file was 
#' segmented in the prep step, the resulting video will be a concatenation of multiple dynamic spectrogram "pages."
#' Each page has a sliding window revealing the part of the static spectrogram being played. Temporal width of each page
#' is defined by the xLim parameter in \code{\link{prep_static_ggspectro}}. You can also output temporary segmented files, if desired.
#' 
#' @aliases pagedSpectro pagedSpec
#' @usage paged_spectro(specParams,destFolder,vidName,framerate=30,highlightCol="#4B0C6BFF",
#' highlightAlpha=.6,cursorCol="#4B0C6BFF",delTemps=T,...)
#' @param specParams an object returned from \code{\link{prep_static_ggspectro}}
#' @param destFolder destination of output video; this setting overwrites setting from specParams object
#' @param vidName expects "FileName", .mp4 not necessary; if not supplied, will be named after the file you used in prep_static_ggspectro()
#' @param highlightCol default "#4B0C6BFF" (a purple color to match the default viridis 'inferno' palette)
#' @param highlightAlpha opacity of the highlight box; default is 0.6
#' @param cursorCol    Color of the leading edge of the highlight box; default "#4B0C6BFF"
#' @param delTemps   Default=T, deletes temporary files (specs & WAV files used to create concatenated video)
#' @param framerate by default, set to 30 (currently this is not supported, as animate doesn't honor the setting)
#' @return Nothing is returned, though progress and file save locations are output to user. Video should play after rendering.
#' @seealso \code{\link{prep_static_ggspectro}}
#' @author Matthew R Wilkins (\email{matt@@galacticpolymath.com})
#' @references {
#' Araya-Salas M & Wilkins M R. (2020). *dynaSpec: dynamic spectrogram visualizations in R*. R package version 1.0.0.
#' }
#' @examples {
#' #show wav files included with dynaSpec
#' f <- list.files(pattern=".wav", full.names = TRUE, path = system.file(package="dynaSpec"))
#' 
#' femaleBarnSwallow<-prep_static_ggspectro(f[1],destFolder="wd",onlyPlotSpec = F, bgFlood=T)
#' paged_spectro(femaleBarnSwallow)
#' 
#' maleBarnSwallow<-prep_static_ggspectro(f[2],destFolder="wd",onlyPlotSpec = F, bgFlood=T,min_dB=-40)
#' paged_spectro(femaleBarnSwallow)
#' 
#' # Make a multipage dynamic spec of a humpback whale song
#' # Note, we're saving PNGs of our specs in the working directory; to add
#'# axis labels, we set onlyPlotSpec to F, and to make the same background
#' # color for the entire figure, we set bgFlood=T;
#' # The yLim is set to only go to 0.7kHz, where the sounds are for these big whales; 
#' #also applying an amplitude transform to boost signal.
#' #This is a longer file, so we're taking the first 12 seconds with crop=12
#' #xLim=3 means each "page" will be 3 seconds, so we'll have 4 dynamic spec pages that get combined
#' 
#' humpback <- prep_static_ggspectro(
#' "http://www.oceanmammalinst.org/songs/hmpback3.wav",savePNG=T,destFolder="wd",
#' onlyPlotSpec=F,bgFlood=T,yLim=c(0,.7),crop=12,xLim=3,ampTrans=3) 
#' 
#' #to generate multipage dynamic spec (movie), run the following
#' paged_spectro(humpback)
#' 
#' # see more examples at https://marce10.github.io/dynaSpec/
#' }

paged_spectro <-function(specParams,destFolder,vidName,framerate=30,highlightCol="#4B0C6BFF",highlightAlpha=.6,cursorCol="#4B0C6BFF",delTemps=T,... )
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
  played<-data.frame(xmin=cursor,xmax=rep(range_i[2],length(cursor)),ymin=rep(specParams$yLim[1],length(cursor)),ymax=rep(specParams$yLim[2], length(cursor)))
  

  #Make ggplot overlay of highlight box on spectrogram
  vidSegment<-{
    ggplot2::ggplot(played)+ggplot2::xlim(range_i)+ggplot2::ylim(specParams$yLim)+
      #Labels
      ggplot2::labs(x="Time (s)",y="Frequency (kHz)",fill="Amplitude\n(dB)\n")+
      ##Animate() seems to shrink font size a bit
      mytheme_lg(specParams$bg)+
      
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
      ggplot2::annotation_custom(grid::rasterGrob(spec_PNG,width = unit(1,"npc"), height = unit(1,"npc")),- Inf, Inf, -Inf, Inf)+
      
      #Add box highlights for playback reveal    
      ggplot2::geom_rect(data=played,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill=highlightCol,alpha=highlightAlpha)+
      
      #Add cursor
      ggplot2::geom_segment(data=played,aes(x=xmin,xend=xmin,y=ymin,yend=ymax),col=cursorCol,size=2) +
  
      #Add animation
      #**** Time consuming animation stage *****
      gganimate::transition_reveal(xmin)
   
    }#end GGPLOT stuffs
  
  # #Increase plot margin slightly b/c it gets changed when exporting to video for some reason
  # if(!specParams$onlyPlotSpec){axisMargin=40}else{axisMargin=0}
  
  #### Export animated ggplot specs  
  #save Audio File with sound in 1 step only if not segmented
  if(length(specParams$segWavs)==1){
    #note, height is set to 500px due to an issue w/ output being garbled at some resolutions; width according to aspect ratio
  animate(vidSegment,renderer=av_renderer(vidName,audio=newWavOut),duration=specParams$xLim[2],width=500*(spec_width_px/spec_height_px),height=500,units="px") #Need to save audio for segments!!
  }else{
    animate(vidSegment,renderer=av_renderer(outTmpVid,audio=outWAV[[i]]),duration=specParams$xLim[2],width=500*(spec_width_px/spec_height_px),height=500,units="px") #Need to save audio for segments!!
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
    # cropFileDur2<-seconds_to_period(cropFileDur)
    # cropFileDur3<-sprintf(fmt='%02d:%02d:%2.3f',hour(cropFileDur2),minute(cropFileDur2),second(cropFileDur2))
    
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
}#end paged_spectro definition

#create alias
pagedSpec<-paged_spectro