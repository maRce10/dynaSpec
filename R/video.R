# videos: dynaspec() of video files and ds_video()

#' Video layer of dynamic spectrograms
#'
#' `ds_video()` shows the video of a [dynaspec()] object made from a video file together with the spectrogram. The video plays in step with the sound, also when playback is slowed down, in web pages and videos ('.mp4') of all styles.
#' @param position Where the video goes, relative to the spectrogram:
#' - "above" (default): the video on top and the spectrogram right below it.
#' - "below": the spectrogram on top and the video below it.
#'
#' Other panels (e.g. oscillograms) stay with the spectrogram.
#' @param height Height of the video relative to the height of the figure with the spectrogram (including its axes and other panels), as 'height' in [ds_oscillogram()]: e.g. 3 makes the video three times as high as the figure, and 0.5 half as high. The video always takes the width of the figure (with its own proportions), so 'height' sets the height of the figure: it replaces 'height' of the style ([ds_static()], [ds_scrolling()] or [ds_paged()]), keeping its width. Default is `NULL`: the figure keeps the height of its style.
#' @return A layer to be added to a [dynaspec()] object with `+`.
#' @details The sound of the dynamic spectrogram is the sound of the video, so 'tlim' in [dynaspec()] also cuts the video.
#'
#' Outputs of [save_dynaspec()]:
#' - Videos ('.mp4'): the video and the spectrogram are shown one on top of the other, with the sound of the video. In scrolling spectrograms the video also slows down with the sound ('speed' of [ds_scrolling()]), waits during the 'buffer' and is repeated when looping.
#' - Web pages: a silent copy of the video (re-encoded at up to 960 pixels wide) is embedded in the page and follows the sound at any speed. Embedded videos make web pages larger (about 1 MB for each 10 s of video), so cut long videos with 'tlim'.
#' - Still images ('.png'): the video is not shown.
#'
#' Videos are read with 'ffmpeg', so most formats can be used (e.g. '.mp4', '.mov', '.mkv', '.webm' or '.avi', with H.264, H.265, VP9 or AV1 video, as supported by the installed 'ffmpeg'). Videos must have sound (the first sound track is used).
#' @seealso [dynaspec()], [save_dynaspec()]
#' @export
#' @examples
#' \dontrun{
#' # a video file with sound (requires ffmpeg)
#' x <- dynaspec("field_recording.mp4", flim = c(1, 8), tlim = c(0, 20)) +
#'   ds_spectrogram(wl.ms = 12) +
#'   ds_video()
#'
#' # web page (video above the spectrogram)
#' x
#'
#' # video below the spectrogram, as high as the figure
#' x + ds_video(position = "below", height = 1)
#'
#' # video file of a scrolling spectrogram
#' save_dynaspec(x + ds_scrolling(t.display = 2), "field_recording_spectrogram.mp4")
#' }
ds_video <- function(position = "above", height = NULL) {
  check_results <- checkmate::makeAssertCollection()
  checkmate::assert_choice(position, c("above", "below"), add = check_results, .var.name = "position")
  checkmate::assert_number(height, lower = 0.05, finite = TRUE, null.ok = TRUE, add = check_results, .var.name = "height")
  .report_assertions(check_results)
  layer_int("video", list(position = position, height = height))
}


# file extensions of videos (read with ffmpeg)
is_video_int <- function(x) tolower(tools::file_ext(x)) %in% c("mp4", "m4v", "mov", "mkv", "webm", "avi", "mts", "m2ts", "ts", "mpg", "mpeg", "3gp", "wmv", "flv", "ogv")


# streams of a media file, as reported by ffmpeg
media_streams_int <- function(file) {
  out <- suppressWarnings(system2(ffmpeg_exec2(), c("-nostdin", "-hide_banner", "-i", shQuote(file)), stdout = TRUE, stderr = TRUE))
  streams <- grep("Stream #", out, value = TRUE)
  list(audio = any(grepl("Audio:", streams)), video = any(grepl("Video:", streams) & !grepl("attached pic", streams)), text = paste(out, collapse = "\n"))
}


# sound of a video (the first sound track) as a Wave object. Its first sample is at time 0 of the video (sound tracks
# starting later get silence at the start), so the sound matches the frames, whose times are also counted from time 0
video_sound_int <- function(file) {
  st <- media_streams_int(file)
  if (!st$audio)
    stop2("the video has no sound track (", basename(file), ")")
  if (!st$video)
    stop2("no video stream found in '", basename(file), "' (use sound files as '.wav' or '.mp3' files)")
  wav <- tempfile(fileext = ".wav")
  on.exit(unlink(wav), add = TRUE)
  run_ffmpeg_int(paste("-y -i", shQuote(file), "-map 0:a:0 -vn -af aresample=async=1:first_pts=0 -c:a pcm_s16le",
                       if (grepl("Audio:[^\n]*(5\\.1|7\\.1|quad|[3-8] channels)", st$text)) "-ac 2", shQuote(wav)),
                 "reading the sound of the video")
  tuneR::readWave(wav)
}


# video part of a dynaspec object: frames from 'from' to 'to' s of the video at a constant frame rate, 'width' pixels
# wide (even height), at 'speed' (as the sound of scrolling videos), with 'start' s of its first frame before it and
# repeated last frame up to 'total' s. Without sound
video_part_int <- function(video, file, from, to, fps, width, speed = 1, start = 0, total = (to - from) / speed + start, crf = 20, height = NULL, bg = "#FFFFFF") {
  num <- function(v) format(v, digits = 10, scientific = FALSE)
  filter <- paste0(
    "trim=start=", num(from), ":end=", num(to), ",setpts=(PTS-", num(from), "/TB)/", num(speed),
    ",fps=", num(fps), ":start_time=0",
    if (start > 0) paste0(",tpad=start_mode=clone:start_duration=", num(start)),
    ",tpad=stop_mode=clone:stop_duration=", num(total + 1), ",trim=end=", num(total), ",setpts=PTS-STARTPTS",
    # (with 'height' the video keeps its proportions within width x height pixels, centered on the background color)
    if (is.null(height)) paste0(",scale=", 2 * round(width / 2), ":-2") else
      paste0(",scale=", 2 * round(width / 2), ":", 2 * round(height / 2), ":force_original_aspect_ratio=decrease,pad=", 2 * round(width / 2), ":", 2 * round(height / 2),
             ":(ow-iw)/2:(oh-ih)/2:color=0x", substring(bg, 2)),
    ",setsar=1,format=yuv420p"
  )
  run_ffmpeg_int(paste("-y -i", shQuote(video), "-map 0:v:0 -an -vf", shQuote(filter), "-c:v libx264 -preset veryfast -crf", crf,
                       "-movflags +faststart", shQuote(file)), "preparing the video")
  file
}


# video of a dynaspec object with its video: the video above or below the video of the spectrogram ('spec_file'), with
# the sound of the latter. 'timing': speed, buffer and loops of scrolling videos
# size of the figure with a video of ds_video(height): the video takes the width of the figure and the figure is as high as
# the video divided by 'height' (the 'height' of the style, in pixels)
video_figure_size_int <- function(x) {
  rel <- x$video.layer$height
  v <- video_info_int(x$video.file)
  ratio <- v$height / v$width
  st <- x$style$args
  if (x$style$type == "paged") {
    w <- if (is.null(st$width)) 1350 else st$width
    h <- w * ratio / rel
    if (h < 120)
      stop2("ds_video(height = ", rel, ") leaves too little room for the spectrogram (pages ", round(h), " pixels high); use a smaller 'height' or a larger 'width' in ds_paged()")
    x$style$args$height <- round(h)
  } else {
    w <- if (!is.null(st$width)) st$width else if (x$style$type == "scrolling") 700 else 1000
    rows <- if (x$style$type == "static" && !is.null(st$rows)) st$rows else 1
    h <- w * ratio / rel
    if (h / rows < 120)
      stop2("ds_video(height = ", rel, ") leaves too little room for the spectrogram (", round(h), " pixels high); use a smaller 'height' or a larger 'width' in ds_",
            x$style$type, "()")
    x$style$args$height <- round(h / rows)
  }
  x
}

video_stack_int <- function(x, spec_file, out, dur, timing = list(speed = 1, buffer = 0, loop = 1)) {
  info <- video_info_int(spec_file)
  from <- if (is.null(x$tlim)) 0 else x$tlim[1]
  part <- tempfile(fileext = ".mp4")
  on.exit(unlink(part), add = TRUE)
  # (one sound and its buffer per loop in scrolling videos; static and paged videos can be longer than the sound, e.g. with
  # silence after it, and keep the last frame)
  seg <- if (timing$loop > 1 || timing$speed != 1 || timing$buffer > 0) (timing$buffer + dur) / timing$speed else info$duration
  video_part_int(x$video.file, part, from, from + dur, fps = info$fps, width = info$width, speed = timing$speed, start = timing$buffer / timing$speed, total = seg)
  above <- !identical(x$video.layer$position, "below")
  # (both pictures start at time 0: videos of paged spectrograms can start a few ms later, from joining pages)
  stack <- paste0("[0:v]setpts=PTS-STARTPTS[a];[1:v]setpts=PTS-STARTPTS[b];", if (above) "[a][b]" else "[b][a]", "vstack=inputs=2[v]")
  run_ffmpeg_int(paste("-y", if (timing$loop > 1) paste("-stream_loop", timing$loop - 1), "-i", shQuote(part), "-i", shQuote(spec_file),
                       "-filter_complex", shQuote(stack), "-map [v] -map 1:a -c:v libx264 -crf 20 -pix_fmt yuv420p -c:a copy -t",
                       format(info$duration, digits = 10, scientific = FALSE), "-movflags +faststart", shQuote(out)),
                 "adding the video")
  out
}


# web page of a dynaspec object with its video: a silent copy of the video (up to 960 pixels wide) in the player, kept in
# step with the sound by the player ('offset': time of the sound before the video starts, as the buffer of scrolling
# spectrograms)
video_html_int <- function(html, x, dur, offset = 0) {
  st <- media_streams_int(x$video.file)
  size <- regmatches(st$text, regexec("Video:[^\n]*[ ,]([0-9]{2,5})x([0-9]{2,5})", st$text))[[1]]
  width <- min(960, if (length(size) == 3) as.numeric(size[2]) else 960)
  from <- if (is.null(x$tlim)) 0 else x$tlim[1]
  part <- tempfile(fileext = ".mp4")
  on.exit(unlink(part), add = TRUE)
  video_part_int(x$video.file, part, from, from + dur, fps = 30, width = width, crf = 28)
  tag <- paste0(' data-video="', if (identical(x$video.layer$position, "below")) "below" else "above", '" data-video-offset="', offset, '">\n',
                '<video class="ds-video" muted playsinline preload="auto" src="', base64_uri_int(part, "video/mp4"), '"></video>')
  # (the video goes inside the player, after its opening tag)
  at <- regexpr('<div class="dynaspec-(player|paged|scroll)"[^>]*>', html)
  if (at < 0) return(html)
  end <- at + attr(at, "match.length") - 1
  structure(paste0(substr(html, 1, end - 1), tag, substr(html, end + 1, nchar(html))), class = class(html))
}


# duration and size of a video
video_info_int <- function(file) {
  out <- paste(suppressWarnings(system2(ffmpeg_exec2(), c("-nostdin", "-hide_banner", "-i", shQuote(file)), stdout = TRUE, stderr = TRUE)), collapse = "\n")
  dur <- regmatches(out, regexec("Duration: ([0-9]+):([0-9]+):([0-9.]+)", out))[[1]]
  size <- regmatches(out, regexec("Video:[^\n]*[ ,]([0-9]{2,5})x([0-9]{2,5})", out))[[1]]
  fps <- regmatches(out, regexec("Video:[^\n]*[ ,]([0-9.]+) fps", out))[[1]]
  if (length(dur) < 4 || length(size) < 3)
    stop2("ffmpeg could not read the video '", basename(file), "'")
  # (videos recorded upright, e.g. with phones, are stored sideways with a rotation that ffmpeg applies when reading them)
  rotated <- grepl("rotation of -?(90|270)\\.|rotate *: *(90|270)\\b", out)
  w <- as.numeric(size[2]); h <- as.numeric(size[3])
  list(duration = sum(as.numeric(dur[2:4]) * c(3600, 60, 1)), width = if (rotated) h else w, height = if (rotated) w else h,
       fps = if (length(fps) == 2) as.numeric(fps[2]) else 30)
}
