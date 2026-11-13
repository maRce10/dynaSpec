// javascript of the web players of dynaSpec (static, paged and scrolling spectrograms, comparisons and videos).
// Added to each web page by player_js_int() (R/player_js.R), inside a script element; the players are defined
// once per page, even if the script is included several times.
(function () {
  if (!window.dynaSpecPlayers) {
    var all = [];
    var label = function (s) {
      var f = {1: "1", 0.5: "\u00BD", 0.25: "\u00BC", 0.125: "\u215B"};
      return (f[s] || String(s)) + "\u00D7";
    };
    var css = document.createElement("style");
    css.textContent =
      ".dynaspec-player .ds-wrap,.dynaspec-paged .ds-wrap{position:relative;cursor:pointer}" +
      ".dynaspec-player img,.dynaspec-paged img{display:block;width:100%;height:auto;margin:0}" +
      ".dynaspec-player audio,.dynaspec-paged audio{display:none}" +
      ".ds-box{position:absolute;pointer-events:none}" +
      // videos of ds_video(), above or below the figure
      ".dynaspec-player video.ds-video,.dynaspec-paged video.ds-video,.dynaspec-scroll video.ds-video{display:block;width:100%;height:auto;margin:0;padding:0;border:0;background:#000}" +
      // comparison of sounds: stacked players with the controls of the last one
      ".dynaspec-compare>div+div{margin-top:var(--ds-cmp-gap,0)}" +
      ".dynaspec-compare>.ds-round+.ds-round{margin-top:.5em}" +
      ".dynaspec-compare>.ds-round:not(:last-child){margin-bottom:0}" +
      ".dynaspec-compare:fullscreen{background:var(--ds-bg,#fff);padding:1em;box-sizing:border-box;display:flex;flex-direction:column;justify-content:center;overflow:auto}" +
      ".dynaspec-compare:fullscreen .ds-wrap,.dynaspec-compare:fullscreen .ds-bar{margin-left:auto;margin-right:auto;width:100%}" +
      // reset image styles of the hosting page (e.g. backgrounds, borders or padding added by R Markdown themes)
      ".dynaspec-player img,.dynaspec-paged img,.dynaspec-scroll img{background:transparent!important;border:0!important;padding:0!important;border-radius:0!important;box-shadow:none!important;margin:0!important}" +
      ".dynaspec-scroll audio{display:none}" +
      ".dynaspec-scroll .ds-wrap{position:relative;cursor:pointer;background:#fff;overflow:hidden}" +
      ".dynaspec-scroll img.ds-frame{position:relative;z-index:2;display:block;width:100%;height:auto;margin:0}" +
      ".ds-vp{position:absolute;overflow:hidden;z-index:1}" +
      ".ds-vp img.ds-strip{position:absolute;top:0;height:100%;max-width:none;margin:0;display:block}" +
      ".ds-over{position:absolute;z-index:3;pointer-events:none}" +
      ".ds-text{position:absolute;z-index:4;pointer-events:none;white-space:nowrap;font-family:var(--ds-font,Helvetica,Arial,sans-serif);line-height:1}" +
      ".ds-bar .ds-page{font-variant-numeric:tabular-nums;color:var(--ds-fg,#555)}" +
      ".ds-head{position:absolute;width:2px;margin-left:-1px;pointer-events:none}" +
      ".ds-bar{display:flex;flex-wrap:wrap;gap:.4em;align-items:center;margin:.4em 0 1.2em;font-size:.9em}" +
      ".ds-bar button{border:1px solid var(--ds-line,#bbb);background:var(--ds-bg,#fff);color:var(--ds-fg,#222);border-radius:4px;padding:.15em .7em;line-height:1.5;cursor:pointer}" +
      ".ds-bar .ds-play{min-width:5.5em;font-weight:600}" +
      ".ds-bar .ds-time{font-variant-numeric:tabular-nums;color:var(--ds-fg,#555);margin-left:.3em}" +
      ".ds-bar .ds-sep{color:var(--ds-fg,#999);opacity:.75;margin:0 .2em}" +
      ".ds-bar .ds-rate{color:var(--ds-fg,inherit)}" +
      ".ds-bar .ds-speed{display:inline-flex;align-items:center;gap:.4em}" +
      ".ds-bar .ds-speed input{margin:0}" +
      ".ds-bar .ds-zoom{display:inline-flex;align-items:center;gap:.4em;margin-left:.6em}" +
      ".ds-bar .ds-zoom input{width:6em;margin:0}" +
      ".ds-bar .ds-zoom .ds-rate{min-width:3.2em}" +
      ".ds-bar .ds-rate{font-variant-numeric:tabular-nums;min-width:2.2em;font-weight:600}" +
      ".ds-bar .ds-pitch{margin-left:.6em}" +
      ".ds-bar .ds-loop{margin-left:.6em}" +
      ".ds-bar .ds-vol{display:inline-flex;align-items:center;gap:.3em;margin-left:.6em}" +
      ".ds-bar .ds-vol input{width:5em;margin:0}" +
      ".ds-bar .ds-fs{margin-left:auto}" +
      ".ds-bar .ds-progress{flex:1 1 8em;min-width:6em;margin:0 .3em}" +
      // rounded controls (ds_theme(bar.style = "rounded")): the whole player on the bar background, the figure in a
      // frame with rounded corners, round play button, pill-shaped controls and flat sliders
      ".ds-round{background:var(--ds-bg,#F0F0F0);padding:12px;border-radius:10px;margin-bottom:1.2em}" +
      ".ds-round .ds-wrap{border:1px solid color-mix(in srgb,var(--ds-fg,#333) 16%,transparent);border-radius:8px;overflow:hidden}" +
      ".ds-round .ds-bar{margin:10px 0 0;gap:10px;font-size:14px;color:var(--ds-fg,#333);font-family:var(--ds-font,inherit)}" +
      ".ds-round .ds-bar button{border:1px solid color-mix(in srgb,var(--ds-fg,#333) 18%,transparent);background:color-mix(in srgb,var(--ds-fg,#333) 9%,transparent);color:var(--ds-fg,#333);border-radius:22px;padding:.35em .9em;line-height:1.3;font-family:inherit}" +
      ".ds-round .ds-bar .ds-play{width:44px;height:44px;min-width:0;padding:0;border-radius:50%;font-size:15px;font-weight:400}" +
      ".ds-round .ds-bar .ds-time{background:color-mix(in srgb,var(--ds-fg,#333) 9%,transparent);border-radius:4px;padding:5px 10px;margin:0;color:var(--ds-fg,#333)}" +
      ".ds-round .ds-bar .ds-speed,.ds-round .ds-bar .ds-zoom{background:color-mix(in srgb,var(--ds-fg,#333) 9%,transparent);border-radius:22px;padding:5px;gap:6px}" +
      ".ds-round .ds-bar .ds-sep{display:none}" +
      ".ds-round .ds-bar .ds-step{width:28px;height:28px;padding:0;border:0;border-radius:50%;background:color-mix(in srgb,var(--ds-fg,#333) 18%,transparent);font-size:18px;line-height:1}" +
      ".ds-round .ds-bar .ds-rate{padding-right:6px;font-weight:400}" +
      ".ds-round .ds-bar input[type=range]{-webkit-appearance:none;appearance:none;height:6px;border-radius:3px;background:color-mix(in srgb,var(--ds-fg,#333) 18%,transparent);outline:none;margin:0}" +
      ".ds-round .ds-bar input[type=range]::-webkit-slider-thumb{-webkit-appearance:none;appearance:none;width:16px;height:16px;border-radius:50%;background:var(--ds-fg,#333);border:0;cursor:pointer}" +
      ".ds-round .ds-bar input[type=range]::-moz-range-thumb{width:16px;height:16px;border-radius:50%;background:var(--ds-fg,#333);border:0;cursor:pointer}" +
      ".ds-round .ds-bar input.ds-progress{height:8px;border-radius:4px}" +
      // square controls (ds_theme(bar.style = "square")): flat bar, square play button and sliders in the accent
      // color (the play line color)
      ".ds-square .ds-wrap{border:1px solid #9E9E9E}" +
      ".ds-square .ds-bar{gap:14px;color:var(--ds-fg,#fff);font-family:var(--ds-font,inherit)}" +
      ".ds-square .ds-bar button{border:0;background:transparent;color:var(--ds-fg,#fff);border-radius:3px;padding:.3em .6em;font-family:inherit}" +
      ".ds-square .ds-bar .ds-play{width:56px;height:40px;min-width:0;padding:0;background:var(--ds-accent,#3D7299);color:#fff;font-size:20px;font-weight:400}" +
      ".ds-square .ds-bar .ds-time{color:var(--ds-fg,#fff);margin:0}" +
      ".ds-square .ds-bar .ds-speed,.ds-square .ds-bar .ds-zoom{background:color-mix(in srgb,var(--ds-fg,#fff) 16%,transparent);border-radius:4px;padding:4px 8px;gap:6px}" +
      ".ds-square .ds-bar .ds-sep{display:none}" +
      ".ds-square .ds-bar .ds-step{padding:0 .35em;font-size:18px;line-height:1}" +
      ".ds-square .ds-bar .ds-rate{font-weight:400}" +
      ".ds-square .ds-bar input[type=range]{-webkit-appearance:none;appearance:none;height:4px;border-radius:2px;background:color-mix(in srgb,var(--ds-fg,#fff) 35%,transparent);outline:none;margin:0}" +
      ".ds-square .ds-bar input[type=range]::-webkit-slider-thumb{-webkit-appearance:none;appearance:none;width:16px;height:16px;border-radius:50%;background:var(--ds-accent,#3D7299);border:2px solid var(--ds-fg,#fff);cursor:pointer}" +
      ".ds-square .ds-bar input[type=range]::-moz-range-thumb{width:12px;height:12px;border-radius:50%;background:var(--ds-accent,#3D7299);border:2px solid var(--ds-fg,#fff);cursor:pointer}" +
      // full screen: player centered, as large as possible while showing the whole figure and the buttons
      ".dynaspec-player:fullscreen,.dynaspec-paged:fullscreen,.dynaspec-scroll:fullscreen{background:var(--ds-bg,#fff);padding:1em;box-sizing:border-box;display:flex;flex-direction:column;justify-content:center;overflow:auto}" +
      ".dynaspec-player:fullscreen>*,.dynaspec-paged:fullscreen>*,.dynaspec-scroll:fullscreen>*{width:100%;margin-left:auto;margin-right:auto}";
    document.head.appendChild(css);

    // smooth playback time: audio elements update currentTime in steps (often every 10-50 ms), so moving parts
    // drawn at each frame from it stop and jump. The time advances with the clock of the page and is eased toward
    // the time of the audio (snapping to it after seeking or large differences). When the audio loops, the time
    // continues from the end of the sound to its start, so moving parts do not jump, however short the sound is.
    // The time waits for the audio to start (which can take a few hundred ms after pressing play) and, when ahead of
    // the audio, slows down or waits instead of going back
    var smoothClock = function (a) {
      var playing = false, waiting = false, last = 0, wall = 0, t = 0, prev = 0;
      // (when looping, the time passes the end of the sound without waiting for the audio to go back to its start)
      var wrap = function (x) { return a.loop && a.duration ? (x % a.duration + a.duration) % a.duration : Math.max(0, x); };
      var atEnd = function () { return a.loop && a.duration - last < 0.1 * a.playbackRate + 0.002; };
      return function () {
        var now = performance.now(), ct = a.currentTime;
        // browsers loop with a short seek from the end of the sound: the time keeps going
        if (a.seeking && playing && !a.paused && atEnd()) {
          t += (now - prev) / 1000 * a.playbackRate;
          prev = now;
          return wrap(t);
        }
        if (a.paused || a.seeking || !playing) {
          playing = waiting = !a.paused && !a.seeking;
          last = t = ct; wall = prev = now;
          return ct;
        }
        if (waiting) {
          wall = prev = now;
          if (ct === last) return wrap(t);
          waiting = false; last = t = ct;
          return wrap(t);
        }
        if (ct < last && atEnd()) t -= a.duration;
        if (ct !== last) { last = ct; wall = now; }
        var target = last + Math.min(0.25, (now - wall) / 1000) * a.playbackRate;
        var next = t + (now - prev) / 1000 * a.playbackRate;
        prev = now;
        var err = target - next;
        // (a large difference backward only comes from the audio moving back, not from the clock being ahead)
        if (err > 0.15 || err < -Math.max(0.5, 0.5 * a.playbackRate)) t = target; else t = Math.max(t, next + 0.15 * err);
        return wrap(t);
      };
    };

    var setup = function (a, keep) {
      a.preservesPitch = a.mozPreservesPitch = a.webkitPreservesPitch = keep;
      a.addEventListener("play", function () {
        all.forEach(function (o) { if (o !== a) o.pause(); });
      });
      all.push(a);
    };

    // video of ds_video(): placed above or below the figure (wrap) and kept in step with the sound (offset: time of
    // the sound before the video starts). The video is silent; it follows the sound at any speed, seeking when
    // they differ by more than 0.12 s (of the video)
    var videoSync = function (div, a, wrap) {
      var v = div.querySelector("video.ds-video");
      if (!v) return;
      if (div.dataset.video === "below") wrap.insertAdjacentElement("afterend", v); else wrap.parentNode.insertBefore(v, wrap);
      v.muted = true;
      var off = +div.dataset.videoOffset || 0, raf, started = false, last = -1;
      var target = function () { return Math.max(0, Math.min(a.currentTime - off, (v.duration || Infinity) - 0.001)); };
      var seek = function () { v.currentTime = target(); };
      var follow = function () {
        if (a.paused) return;
        var ct = a.currentTime;
        // (the video waits until the sound has started, and while the time before the video passes)
        if (!started && ct !== last && last >= 0) started = true;
        last = ct;
        if (!started || ct < off) {
          if (!v.paused) v.pause();
          if (!started) seek();
        } else {
          if (v.paused) v.play().catch(function () {});
          // small differences are corrected by playing the video slightly faster or slower; larger ones by seeking
          var err = v.currentTime - target(), r = a.playbackRate;
          if (Math.abs(err) > 0.3 * Math.max(1, r)) { seek(); v.playbackRate = r; }
          else v.playbackRate = r * Math.min(1.25, Math.max(0.8, 1 - 2 * err / Math.max(1, r)));
        }
        raf = requestAnimationFrame(follow);
      };
      a.addEventListener("play", function () { started = false; last = -1; v.pause(); seek(); cancelAnimationFrame(raf); follow(); });
      a.addEventListener("pause", function () { cancelAnimationFrame(raf); v.pause(); seek(); });
      a.addEventListener("seeked", function () { if (a.paused) seek(); });
      a.addEventListener("ended", function () { v.pause(); seek(); });
      // (the first frame is shown once the video is loaded)
      v.addEventListener("loadeddata", seek);
    };

    // times as minutes and seconds (as mmss_int() in R)
    var mmss = function (t, d) {
      t = +t.toFixed(d);
      var m = Math.floor(t / 60 + 1e-9), sec = (t - 60 * m).toFixed(d);
      return m + ":" + (sec.length < (d ? 3 + d : 2) ? "0" : "") + sec;
    };

    // text of the play button (only the symbol with rounded and square controls)
    var setPlay = function (b, on) {
      var round = b.closest(".ds-round,.ds-square");
      b.textContent = round ? (on ? "\u275A\u275A" : "\u25B6") : (on ? "\u275A\u275A pause" : "\u25B6 play");
      b.title = on ? "pause" : "play";
      b.setAttribute("aria-label", on ? "pause" : "play");
    };

    var paintBtn = function (b, on, color) {
      b.style.background = on ? color[0] : "";
      b.style.borderColor = on ? color[0] : "";
      b.style.color = on ? color[1] : "";
      // outline so active buttons are visible even with colors similar to the page background
      b.style.boxShadow = on ? "inset 0 0 0 1px rgba(0,0,0,0.35)" : "";
    };

    // speed slider (snapping to the speeds of ds_player(), slowest on the left) and keep pitch toggle; returns a
    // function giving the current speed
    var nSliders = 0;
    var speedGroup = function (bar, speeds, color, keep, audios, onChange) {
      var rate = speeds[0];
      if (speeds.length < 2) return function () { return rate; };
      var sorted = speeds.slice().sort(function (x, y) { return x - y; });
      var group = document.createElement("span");
      group.className = "ds-speed";
      var sep = document.createElement("span");
      sep.className = "ds-sep";
      sep.textContent = "speed";
      var slider = document.createElement("input");
      slider.type = "range";
      slider.min = 0; slider.max = sorted.length - 1; slider.step = 1;
      slider.value = sorted.indexOf(rate);
      slider.style.accentColor = color[0];
      slider.style.width = 1.6 * sorted.length + "em";
      slider.setAttribute("aria-label", "playback speed");
      // tick marks at the fixed speeds
      var ticks = document.createElement("datalist");
      ticks.id = "ds-speeds-" + (++nSliders);
      sorted.forEach(function (s, i) { var o = document.createElement("option"); o.value = i; ticks.appendChild(o); });
      slider.setAttribute("list", ticks.id);
      var out = document.createElement("span");
      out.className = "ds-rate";
      var show = function () {
        out.textContent = label(rate);
        slider.title = rate === 1 ? "real time" : rate < 1 ? "slowed down" : "sped up";
        slider.setAttribute("aria-valuetext", label(rate));
      };
      slider.addEventListener("input", function () { rate = sorted[+slider.value]; show(); onChange(rate); });
      show();
      [sep, slider, ticks, out].forEach(function (x) { group.appendChild(x); });
      bar.appendChild(group);

      // keep pitch toggle
      var p = document.createElement("button");
      p.type = "button";
      p.className = "ds-pitch";
      p.textContent = "keep pitch";
      p.setAttribute("aria-pressed", keep);
      var setPitch = function () {
        audios.forEach(function (a) { a.preservesPitch = a.mozPreservesPitch = a.webkitPreservesPitch = keep; });
        p.setAttribute("aria-pressed", keep);
        p.title = keep ? "pitch is kept when speed changes (click to let pitch follow speed)" : "pitch follows speed, as when slowing down a tape (click to keep pitch)";
        paintBtn(p, keep, color);
      };
      p.addEventListener("click", function () { keep = !keep; setPitch(); });
      setPitch();
      bar.appendChild(p);

      return function () { return rate; };
    };

    // additional controls set by ds_player(buttons): time readout, loop, volume, download and full screen. time is
    // the time readout of the player and redraw updates the player (e.g. after entering full screen)
    var extraButtons = function (div, bar, wrap, a, time, color, redraw, dur) {
      var d = div.dataset, opts = d.buttons ? d.buttons.split(",") : [];
      var has = function (x) { return opts.indexOf(x) > -1; };
      var round = d.barStyle === "rounded", square = d.barStyle === "square", play = bar.querySelector(".ds-play");
      if (round) div.classList.add("ds-round");
      if (square) div.classList.add("ds-square");
      div.style.setProperty("--ds-accent", color[0]);
      setPlay(play, !a.paused);
      // colors of the controls and font family of the ds_theme()
      if (d.barCol) { div.style.setProperty("--ds-fg", d.barCol); div.style.setProperty("--ds-line", d.barLine); }
      if (d.barBg && round) div.style.setProperty("--ds-bg", d.barBg);
      else if (d.barBg) {
        div.style.setProperty("--ds-bg", d.barBg);
        bar.style.background = d.barBg;
        bar.style.padding = ".35em .5em";
        bar.style.marginTop = "0";
      }
      if (d.family) div.style.setProperty("--ds-font", d.family);
      var button = function (cls, text, title) {
        var b = document.createElement("button");
        b.type = "button";
        b.className = cls;
        b.textContent = text;
        b.title = title;
        bar.appendChild(b);
        return b;
      };
      if (!has("time")) time.style.display = "none";

      // seek bar (after the play button, followed by the time)
      if (has("progress")) {
        var prog = document.createElement("input");
        prog.type = "range";
        prog.className = "ds-progress";
        prog.min = 0; prog.max = dur; prog.step = "any"; prog.value = 0;
        prog.style.accentColor = color[0];
        prog.setAttribute("aria-label", "position");
        play.insertAdjacentElement("afterend", prog);
        prog.insertAdjacentElement("afterend", time);
        var dragging = false, rafP;
        var paint = function () {
          if (!dragging) prog.value = Math.min(a.currentTime, dur);
          // (rounded and square controls: the part played is filled)
          if (round || square) {
            var q = 100 * prog.value / dur;
            prog.style.background = "linear-gradient(to right, " + (round ? "var(--ds-fg,#333) " : "var(--ds-accent) ") + q + "%, color-mix(in srgb,var(--ds-fg,#333) " + (round ? 18 : 35) + "%,transparent) " + q + "%)";
          }
        };
        var follow = function () { paint(); if (!a.paused) rafP = requestAnimationFrame(follow); };
        prog.addEventListener("input", function () { dragging = true; a.currentTime = Math.min(dur - 0.001, +prog.value); paint(); });
        prog.addEventListener("change", function () { dragging = false; });
        a.addEventListener("play", follow);
        a.addEventListener("pause", function () { cancelAnimationFrame(rafP); paint(); });
        a.addEventListener("seeked", paint);
        a.addEventListener("ended", paint);
        paint();
      }

      // rounded and square controls: buttons to step through the speeds on both sides of the speed slider
      var group = (round || square) && bar.querySelector(".ds-speed");
      if (group) {
        var sl = group.querySelector("input");
        var step = function (txt, by, title) {
          var b = document.createElement("button");
          b.type = "button";
          b.className = "ds-step";
          b.textContent = txt;
          b.title = title;
          b.addEventListener("click", function () {
            var v = Math.max(0, Math.min(+sl.max, +sl.value + by));
            if (v !== +sl.value) { sl.value = v; sl.dispatchEvent(new Event("input")); }
          });
          return b;
        };
        group.insertBefore(step("\u2212", -1, "slower"), sl);
        sl.insertAdjacentElement("afterend", step("+", 1, "faster"));
      }

      if (has("loop")) {
        var lp = button("ds-loop", "\u21BB loop", "");
        var setLoop = function () {
          lp.setAttribute("aria-pressed", a.loop);
          lp.title = a.loop ? "playing repeatedly (click to stop looping)" : "click to play repeatedly";
          paintBtn(lp, a.loop, color);
        };
        lp.addEventListener("click", function () { a.loop = !a.loop; setLoop(); });
        setLoop();
      }

      if (has("volume")) {
        var vol = document.createElement("span");
        vol.className = "ds-vol";
        var mute = document.createElement("button");
        mute.type = "button";
        var slider = document.createElement("input");
        slider.type = "range";
        slider.min = 0; slider.max = 1; slider.step = 0.05; slider.value = a.volume;
        slider.style.accentColor = color[0];
        slider.setAttribute("aria-label", "volume");
        var setVol = function () {
          var off = a.muted || a.volume === 0;
          mute.textContent = off ? "\uD83D\uDD07" : "\uD83D\uDD0A";
          mute.title = off ? "unmute" : "mute";
          mute.setAttribute("aria-pressed", off);
          slider.value = a.muted ? 0 : a.volume;
        };
        mute.addEventListener("click", function () {
          if (a.volume === 0) { a.volume = 1; a.muted = false; } else a.muted = !a.muted;
          setVol();
        });
        slider.addEventListener("input", function () { a.volume = +slider.value; a.muted = false; setVol(); });
        vol.appendChild(mute);
        vol.appendChild(slider);
        bar.appendChild(vol);
        setVol();
      }

      if (has("download")) {
        var mp3 = a.src.indexOf("data:audio/mpeg") === 0;
        button("ds-dl", "\u2913 download", "save the sound").addEventListener("click", function () {
          var link = document.createElement("a");
          link.href = a.src;
          link.download = (d.name || "sound") + (mp3 ? ".mp3" : ".wav");
          document.body.appendChild(link);
          link.click();
          link.remove();
        });
      }

      if (has("fullscreen") && div.requestFullscreen && document.fullscreenEnabled) {
        var fs = button("ds-fs", "\u26F6 full screen", "show in full screen");
        fs.addEventListener("click", function () {
          // the request can be refused (e.g. in embedded frames)
          if (document.fullscreenElement === div) document.exitFullscreen(); else div.requestFullscreen().catch(function () {});
        });
        document.addEventListener("fullscreenchange", function () {
          var on = document.fullscreenElement === div, img = wrap.querySelector("img");
          fs.textContent = on ? "\u2715 exit full screen" : "\u26F6 full screen";
          fs.title = on ? "exit full screen" : "show in full screen";
          // widest size at which the figure and the buttons fit in the screen
          var w = on && img.naturalHeight ? Math.max(200, (window.innerHeight - bar.offsetHeight - 64) * img.naturalWidth / img.naturalHeight) + "px" : "";
          wrap.style.maxWidth = bar.style.maxWidth = w;
          redraw();
        });
      }
    };

    // captions: text at a fixed position of the figure shown (fading in and out) during a time range. Settings
    // are in a script element of class ds-captions; returns a function that updates captions at time t
    var captionsInit = function (div, wrap) {
      var el = div.querySelector("script.ds-captions");
      if (!el) return function () {};
      var c = JSON.parse(el.textContent);
      var arr = function (x) { return Array.isArray(x) ? x : [x]; };
      var items = c.layers.map(function (l) {
        var node = document.createElement("div");
        node.className = "ds-text";
        node.style.color = l.col;
        node.style.fontWeight = l.font === 2 || l.font === 4 ? "bold" : "normal";
        node.style.fontStyle = l.font === 3 || l.font === 4 ? "italic" : "normal";
        if (l.family) node.style.fontFamily = l.family;
        node.style.left = 100 * l.x + "%";
        node.style.top = 100 * l.y + "%";
        node.style.transform = "translate(-50%,-50%)";
        wrap.appendChild(node);
        return {node: node, l: l, labels: arr(l.labels), start: arr(l.start), end: arr(l.end)};
      });
      return function (t) {
        // text size follows the size of the figure (12 points at the resolution of the figure)
        var px = 12 * c.res / 72 * wrap.clientWidth / c.width;
        items.forEach(function (it) {
          var op = 0, txt = "", f = Math.max(it.l.fading, 1e-3);
          it.labels.forEach(function (lab, i) {
            var t0 = it.start[i], t1 = it.end[i];
            if (t >= t0 && t <= t1) { var o = Math.min(1, (t - t0) / f, (t1 - t) / f); if (o > op) { op = o; txt = lab; } }
          });
          it.node.textContent = txt;
          it.node.style.opacity = op;
          it.node.style.fontSize = it.l.cex * px + "px";
        });
      };
    };

    var initPlayer = function (div) {
      var d = div.dataset, img = div.querySelector("img"), a = div.querySelector("audio");
      var dur = +d.duration;
      // one row per sound segment: start and end time, and extents of the panels (as proportions of the figure)
      var rows = d.rows.split(";").map(function (r) {
        r = r.split(",").map(Number);
        return {t0: r[0], t1: r[1], left: r[2], right: r[3], top: r[4], bottom: r[5]};
      });
      var rowAt = function (t) {
        for (var i = 0; i < rows.length - 1; i++) if (t < rows[i].t1) return rows[i];
        return rows[rows.length - 1];
      };
      var speeds = d.speeds.split(",").map(Number);
      var keep = d.keepPitch === "true";
      setup(a, keep);
      a.playbackRate = speeds[0];

      var wrap = document.createElement("div");
      wrap.className = "ds-wrap";
      img.parentNode.insertBefore(wrap, img);
      wrap.appendChild(img);
      var head = document.createElement("div");
      head.className = "ds-head";
      head.style.background = d.color;
      // no moving line when play.line is FALSE (clicking on the figure still plays from that point)
      if (d.line === "false") head.style.display = "none";
      wrap.appendChild(head);
      // width (pixels of the figure, scaled with it; default 2 pixels of the page) and dashes (lengths in line widths)
      var dash = d.dash ? d.dash.split(",").map(Number) : null;
      var lineStyle = function () {
        var w = d.lwd && img.naturalWidth ? Math.max(1, +d.lwd * wrap.clientWidth / img.naturalWidth) : 2;
        head.style.width = w + "px";
        head.style.marginLeft = -w / 2 + "px";
        if (dash) {
          var stops = [], y = 0;
          dash.forEach(function (len, i) {
            var c = i % 2 ? "transparent" : d.color;
            stops.push(c + " " + y + "px", c + " " + (y + len * w) + "px");
            y += len * w;
          });
          head.style.background = "repeating-linear-gradient(to bottom," + stops.join(",") + ")";
        }
      };

      var bar = document.createElement("div");
      bar.className = "ds-bar";
      var play = document.createElement("button");
      play.type = "button";
      play.className = "ds-play";
      play.textContent = "\u25B6 play";
      bar.appendChild(play);
      speedGroup(bar, speeds, [d.color, d.textColor], keep, [a], function (r) { a.playbackRate = r; });
      var time = document.createElement("span");
      time.className = "ds-time";
      bar.appendChild(time);
      wrap.insertAdjacentElement("afterend", bar);
      videoSync(div, a, wrap);
      var captions = captionsInit(div, wrap);

      var raf, clock = smoothClock(a), fmt = function (t) { return dur < 5 ? t.toFixed(3) : t.toFixed(1); };
      var draw = function () {
        var t = Math.min(clock(), dur), r = rowAt(t);
        captions(t);
        lineStyle();
        head.style.left = 100 * (r.left + (r.right - r.left) * (t - r.t0) / (r.t1 - r.t0)) + "%";
        head.style.top = 100 * r.top + "%";
        head.style.height = 100 * (r.bottom - r.top) + "%";
        time.textContent = fmt(t) + " / " + fmt(dur) + " s";
        if (!a.paused) raf = requestAnimationFrame(draw);
      };
      draw();
      extraButtons(div, bar, wrap, a, time, [d.color, d.textColor], draw, dur);
      a.addEventListener("play", function () { setPlay(play, true); draw(); });
      a.addEventListener("pause", function () { setPlay(play, false); cancelAnimationFrame(raf); draw(); });
      a.addEventListener("ended", function () { a.currentTime = 0; draw(); });
      a.addEventListener("seeked", draw);
      play.addEventListener("click", function () { a.paused ? a.play() : a.pause(); });
      wrap.addEventListener("click", function (e) {
        var b = wrap.getBoundingClientRect(), y = (e.clientY - b.top) / b.height;
        // row clicked (or the closest one)
        var r = rows.reduce(function (best, x) {
          var dist = y < x.top ? x.top - y : y > x.bottom ? y - x.bottom : 0;
          return dist < best.dist ? {row: x, dist: dist} : best;
        }, {row: rows[0], dist: Infinity}).row;
        var f = ((e.clientX - b.left) / b.width - r.left) / (r.right - r.left);
        a.currentTime = Math.max(0, Math.min(dur - 0.001, r.t0 + f * (r.t1 - r.t0)));
        a.play();
      });
    };

    // paged spectrograms: one image per page, a highlight box over the part of the page not played yet
    // and a cursor at its leading edge (as in paged_spectro() videos)
    var initPaged = function (div) {
      var d = div.dataset, imgs = Array.prototype.slice.call(div.querySelectorAll("img")), a = div.querySelector("audio");
      var dur = +d.duration;
      var pages = d.rows.split(";").map(function (r) {
        r = r.split(",").map(Number);
        return {t0: r[0], t1: r[1], left: r[2], right: r[3], top: r[4], bottom: r[5]};
      });
      var pageAt = function (t) {
        for (var i = 0; i < pages.length - 1; i++) if (t < pages[i].t1) return i;
        return pages.length - 1;
      };
      var speeds = d.speeds.split(",").map(Number);
      var keep = d.keepPitch === "true";
      setup(a, keep);
      a.playbackRate = speeds[0];

      var wrap = document.createElement("div");
      wrap.className = "ds-wrap";
      imgs[0].parentNode.insertBefore(wrap, imgs[0]);
      imgs.forEach(function (img, i) { wrap.appendChild(img); img.style.display = i ? "none" : "block"; });
      var box = document.createElement("div");
      box.className = "ds-box";
      box.style.background = d.highlight;
      box.style.opacity = d.alpha;
      wrap.appendChild(box);
      var cursor = document.createElement("div");
      cursor.className = "ds-box";
      cursor.style.background = d.cursor;
      cursor.style.width = "4px";
      cursor.style.marginLeft = "-2px";
      wrap.appendChild(cursor);

      var bar = document.createElement("div");
      bar.className = "ds-bar";
      var play = document.createElement("button");
      play.type = "button";
      play.className = "ds-play";
      play.textContent = "\u25B6 play";
      bar.appendChild(play);
      var prev = document.createElement("button"), next = document.createElement("button"), label = document.createElement("span");
      prev.type = next.type = "button";
      prev.textContent = "\u25C0";
      prev.title = "previous page";
      next.textContent = "\u25B6";
      next.title = "next page";
      label.className = "ds-page";
      if (pages.length > 1) [prev, label, next].forEach(function (x) { bar.appendChild(x); });
      speedGroup(bar, speeds, [d.color, d.textColor], keep, [a], function (r) { a.playbackRate = r; });
      var time = document.createElement("span");
      time.className = "ds-time";
      bar.appendChild(time);
      wrap.insertAdjacentElement("afterend", bar);
      videoSync(div, a, wrap);
      var captions = captionsInit(div, wrap);

      var raf, shown = 0, clock = smoothClock(a), fmt = function (t) { return dur < 5 ? t.toFixed(3) : t.toFixed(1); };
      var draw = function () {
        var t = Math.min(clock(), dur), k = pageAt(t), p = pages[k];
        captions(t);
        if (k !== shown) { imgs[shown].style.display = "none"; imgs[k].style.display = "block"; shown = k; }
        var x = p.left + (p.right - p.left) * Math.min(1, (t - p.t0) / (p.t1 - p.t0));
        [box, cursor].forEach(function (el) {
          el.style.top = 100 * p.top + "%";
          el.style.height = 100 * (p.bottom - p.top) + "%";
          el.style.left = 100 * x + "%";
        });
        box.style.width = 100 * Math.max(0, p.right - x) + "%";
        label.textContent = (k + 1) + " / " + pages.length;
        time.textContent = fmt(t) + " / " + fmt(dur) + " s";
        if (!a.paused) raf = requestAnimationFrame(draw);
      };
      draw();
      extraButtons(div, bar, wrap, a, time, [d.color, d.textColor], draw, dur);
      var go = function (k) {
        a.currentTime = pages[Math.max(0, Math.min(pages.length - 1, k))].t0;
        draw();
      };
      prev.addEventListener("click", function () { go(pageAt(a.currentTime) - 1); });
      next.addEventListener("click", function () { go(pageAt(a.currentTime) + 1); });
      a.addEventListener("play", function () { setPlay(play, true); draw(); });
      a.addEventListener("pause", function () { setPlay(play, false); cancelAnimationFrame(raf); draw(); });
      a.addEventListener("ended", function () { a.currentTime = 0; draw(); });
      a.addEventListener("seeked", draw);
      play.addEventListener("click", function () { a.paused ? a.play() : a.pause(); });
      wrap.addEventListener("click", function (e) {
        var b = wrap.getBoundingClientRect(), p = pages[shown];
        var f = Math.max(0, Math.min(1, ((e.clientX - b.left) / b.width - p.left) / (p.right - p.left)));
        a.currentTime = Math.min(dur - 0.001, p.t0 + f * (p.t1 - p.t0));
        a.play();
      });
    };

    // scrolling spectrograms: the full spectrogram (and oscillogram) moves below a frame with the parts that do
    // not move (axes, play line, lower spectrogram), as in scrolling_spectro() videos
    var initScrolling = function (div) {
      var d = div.dataset, c = JSON.parse(div.querySelector("script[type='application/json']").textContent);
      var frame = div.querySelector("img.ds-frame"), strips = div.querySelectorAll("img.ds-strip"), a = div.querySelector("audio");
      var speeds = d.speeds.split(",").map(Number), keep = d.keepPitch === "true";
      setup(a, keep);
      a.playbackRate = speeds[0];
      var pct = function (x) { return 100 * x + "%"; };
      var place = function (el, left, right, top, bottom) {
        el.style.left = pct(left); el.style.width = pct(Math.max(0, right - left));
        el.style.top = pct(top); el.style.height = pct(Math.max(0, bottom - top));
      };

      var wrap = document.createElement("div");
      wrap.className = "ds-wrap";
      wrap.style.background = c.bg;
      frame.parentNode.insertBefore(wrap, frame);
      wrap.appendChild(frame);

      // scrolling strips (viewports clip the part shown)
      var viewport = function (strip, top, bottom) {
        var vp = document.createElement("div");
        vp.className = "ds-vp";
        place(vp, c.left, c.right, top, bottom);
        vp.appendChild(strip);
        wrap.appendChild(vp);
        return strip;
      };
      // zoom: duration shown at once is tDisplay * zoom (strips stretch or shrink around the play line)
      var zoom = 1;
      var moving = [viewport(strips[0], c.upTop, c.upBottom)];
      if (c.lower === "osc") moving.push(viewport(strips[1], c.lowTop, c.lowBottom));

      // window in the lower spectrogram: gray boxes outside the window and a thin box around it
      var over = function (bg, border) {
        var el = document.createElement("div");
        el.className = "ds-over";
        if (bg) el.style.background = bg;
        if (border) el.style.border = "1px solid rgba(0,0,0,0.55)";
        el.style.boxSizing = "border-box";
        wrap.appendChild(el);
        return el;
      };
      var grays = [], boxes = [];
      if (c.lower === "spectro") {
        grays = [over(c.dim), over(c.dim)];
        boxes = [over(null, true), over(null, true)];
      }
      // horizontal segment [x0, x1] of the lower spectrogram (proportions of its width)
      var seg = function (el, x0, x1) {
        if (x1 <= x0) { el.style.display = "none"; return; }
        el.style.display = "block";
        place(el, c.left + (c.right - c.left) * x0, c.left + (c.right - c.left) * x1, c.lowTop, c.lowBottom);
      };

      // moving time axis (when time is not fixed)
      var ticks = [];
      var label = function () {
        var el = document.createElement("div");
        el.className = "ds-text";
        el.style.transform = "translateX(-50%)";
        var tick = document.createElement("div");
        tick.className = "ds-over";
        tick.style.width = "1px";
        tick.style.background = c.colLab;
        el.style.color = c.colAxis;
        // vertical time labels (las = 2 or 3): read from bottom to top, ending at the axis
        if (c.xVert) { el.style.writingMode = "vertical-rl"; el.style.transform = "translateX(-50%) rotate(180deg)"; }
        wrap.appendChild(el);
        wrap.appendChild(tick);
        return {text: el, tick: tick};
      };

      var bar = document.createElement("div");
      bar.className = "ds-bar";
      var play = document.createElement("button");
      play.type = "button";
      play.className = "ds-play";
      play.textContent = "\u25B6 play";
      bar.appendChild(play);
      speedGroup(bar, speeds, [d.color, d.textColor], keep, [a], function (r) { a.playbackRate = r; });
      // zoom slider (ds_player(buttons = "zoom")): from showing up to 4 times longer (at most the whole sound) to 4
      // times shorter; slider values are -log2(zoom), so zooming in is to the right
      if (c.zoom) {
        var zg = document.createElement("span"), zs = document.createElement("input"), zv = document.createElement("span");
        zg.className = "ds-zoom";
        var zsep = document.createElement("span");
        zsep.className = "ds-sep";
        zsep.textContent = "zoom";
        zs.type = "range";
        zs.min = -Math.log2(Math.max(1, Math.min(4, c.durWave / c.tDisplay))); zs.max = 2; zs.step = 0.25; zs.value = 0;
        zs.style.accentColor = d.color;
        zs.setAttribute("aria-label", "zoom");
        zv.className = "ds-rate";
        var setZoom = function (v) {
          v = Math.max(+zs.min, Math.min(+zs.max, v));
          zs.value = v;
          zoom = Math.pow(2, -v);
          var T = c.tDisplay * zoom;
          zv.textContent = (T < 1 ? T.toFixed(2) : T < 10 ? T.toFixed(1) : T.toFixed(0)) + " s";
          zs.title = "time shown: " + zv.textContent;
          draw();
        };
        var zstep = function (txt, by, title) {
          var b = document.createElement("button");
          b.type = "button";
          b.className = "ds-step";
          b.textContent = txt;
          b.title = title;
          b.addEventListener("click", function () { setZoom(+zs.value + by); });
          return b;
        };
        zs.addEventListener("input", function () { setZoom(+zs.value); });
        [zsep, zstep("\u2212", -0.5, "zoom out"), zs, zstep("+", 0.5, "zoom in"), zv].forEach(function (x) { zg.appendChild(x); });
        bar.appendChild(zg);
      }

      var time = document.createElement("span");
      time.className = "ds-time";
      bar.appendChild(time);
      wrap.insertAdjacentElement("afterend", bar);
      videoSync(div, a, wrap);
      var captions = captionsInit(div, wrap);

      // time of the sound at audio time t, and center of the window shown (with still ends the window stays within the
      // sound, so the spectrogram is still at the start and the end and the play line moves instead)
      var soundTime = function (t) { return Math.min(Math.max(0, t * c.k - c.buffer), c.durWave); };
      var center = function (t) {
        var ts = soundTime(t), T = c.tDisplay * zoom;
        return c.stillEnds ? Math.min(Math.max(ts, T / 2), c.durWave - T / 2) : ts;
      };
      // start of the window shown (time of the strip, which has half a window of silence at both ends), with zoom
      var winStart = function (t) { return center(t) + c.tDisplay * (1 - zoom) / 2; };
      // moving play line (still ends; otherwise the line is in the frame, at the center)
      var line = null;
      if (c.stillEnds) {
        line = document.createElement("div");
        line.className = "ds-over";
        wrap.appendChild(line);
      }
      var drawLine = function (t) {
        if (!line) return;
        var T = c.tDisplay * zoom, at = 0.5 + (soundTime(t) - center(t)) / T;
        var w = c.lineWidth * wrap.clientWidth / c.width;
        line.style.display = w > 0 ? "block" : "none";
        line.style.width = w + "px";
        line.style.marginLeft = -w / 2 + "px";
        line.style.left = pct(c.left + (c.right - c.left) * at);
        line.style.top = pct(c.upTop);
        line.style.height = pct(c.lowBottom - c.upTop);
        if (Array.isArray(c.lineDash)) {
          var stops = [], y = 0;
          c.lineDash.forEach(function (len, i) {
            var col = i % 2 ? "transparent" : c.lineColor;
            stops.push(col + " " + y + "px", col + " " + (y + len * w) + "px");
            y += len * w;
          });
          line.style.background = "repeating-linear-gradient(to bottom," + stops.join(",") + ")";
        } else line.style.background = c.lineColor;
      };
      // tick step of the time labels (as R pretty() for the window of the figure)
      var tickStep = function (T) {
        if (zoom === 1) return c.tickStep;
        var raw = T / 5, mag = Math.pow(10, Math.floor(Math.log10(raw))), r = raw / mag;
        return (r < 1.5 ? 1 : r < 3 ? 2 : r < 7 ? 5 : 10) * mag;
      };

      var dur = c.duration, raf, clock = smoothClock(a), fmt = function (t) { return dur < 5 ? t.toFixed(3) : t.toFixed(1); };
      var draw = function () {
        var t = Math.min(clock(), dur), T = c.tDisplay * zoom;
        // text size follows the size of the figure (12 points at the resolution of the frame)
        var px = 12 * c.res / 72 * wrap.clientWidth / c.width;

        var s = winStart(t);
        moving.forEach(function (strip) {
          var w = pct(c.durSil / T);
          if (strip.style.width !== w) { strip.style.width = w; strip.style.left = "0"; strip.style.willChange = "transform"; }
          // (translateX in % of the strip width)
          strip.style.transform = "translate3d(" + (-100 * s / c.durSil) + "%,0,0)";
        });

        drawLine(t);

        if (c.lower === "spectro") {
          var w = T / c.durWave, mid = c.stillEnds ? center(t) / c.durWave : Math.max(0, t * c.kl - c.buffer / c.durWave), x0 = mid - w / 2, x1 = mid + w / 2;
          if (c.loop && x1 > 1) {
            seg(boxes[0], x0, 1); seg(boxes[1], 0, x1 - 1); seg(grays[0], x1 - 1, x0); seg(grays[1], 0, 0);
          } else if (c.loop && x0 < 0) {
            seg(boxes[0], 0, x1); seg(boxes[1], 1 + x0, 1); seg(grays[0], x1, 1 + x0); seg(grays[1], 0, 0);
          } else {
            seg(boxes[0], Math.max(0, x0), Math.min(1, x1)); seg(boxes[1], 0, 0);
            seg(grays[0], 0, Math.max(0, x0)); seg(grays[1], Math.min(1, x1), 1);
          }
        }

        if (c.movingAxis) {
          // labels show the time of the sound (the strip starts with half a window of silence, not labeled), or the
          // time of the window when time is fixed
          var step = tickStep(T), digits = Math.max(0, -Math.floor(Math.log10(step) + 1e-9));
          var half = c.fixTime ? s : c.tDisplay / 2, first = Math.ceil((s - half) / step - 1e-9), n = Math.floor((s + T - half) / step + 1e-9) - first + 1;
          while (ticks.length < n) ticks.push(label());
          ticks.forEach(function (tk, i) {
            var v = (first + i) * step, on = i < n && v >= -1e-9 && (c.fixTime || v <= c.durWave + 1e-9), x = c.left + (c.right - c.left) * (v + half - s) / T;
            tk.text.style.display = tk.tick.style.display = on ? "block" : "none";
            if (!on) return;
            // (labels with units without trailing zeros, e.g. "3s", as in R; compact axes: minutes and seconds)
            tk.text.textContent = c.top ? mmss(v, digits) : c.unit ? parseFloat(v.toFixed(digits)) + c.unit : v.toFixed(digits);
            tk.text.style.fontSize = c.cexAxis * px + "px";
            tk.text.style.left = pct(x);
            // labels and ticks placed as in R axes (measured on frames drawn by R; a line of text is 1.2 px)
            tk.text.style.top = "calc(" + pct(c.lowBottom) + " + " + (1.2 * c.labLine + 0.15) * px + "px)";
            // (labels with units are hidden close to the figure edges, as in R)
            if (c.unit) tk.text.style.visibility = x < 0.02 || x > 0.98 ? "hidden" : "visible";
            tk.tick.style.left = pct(x);
            tk.tick.style.top = pct(c.lowBottom);
            tk.tick.style.height = 1.2 * c.tickLines * px + "px";
            // compact axes: ticks across the top margin and labels to their right
            if (c.top) {
              tk.text.style.transform = "translateY(-50%)";
              tk.text.style.left = "calc(" + pct(x) + " + " + 0.2 * px + "px)";
              tk.text.style.top = pct(c.upTop / 2);
              tk.text.style.visibility = x > 0.96 ? "hidden" : "visible";
              tk.tick.style.top = "0";
              tk.tick.style.height = pct(c.upTop);
            }
          });
        }

        captions(t);

        time.textContent = fmt(t) + " / " + fmt(dur) + " s";
        if (!a.paused) raf = requestAnimationFrame(draw);
      };
      draw();
      if (c.zoom) setZoom(0);
      window.addEventListener("resize", draw);
      extraButtons(div, bar, wrap, a, time, [d.color, d.textColor], draw, dur);
      a.addEventListener("play", function () { setPlay(play, true); draw(); });
      a.addEventListener("pause", function () { setPlay(play, false); cancelAnimationFrame(raf); draw(); });
      a.addEventListener("ended", function () { a.currentTime = 0; draw(); });
      a.addEventListener("seeked", draw);
      play.addEventListener("click", function () { a.paused ? a.play() : a.pause(); });

      // clicking on the scrolling spectrogram moves that point to the play line; clicking on the lower
      // spectrogram plays from that point of the sound
      wrap.addEventListener("click", function (e) {
        var b = wrap.getBoundingClientRect(), x = (e.clientX - b.left) / b.width, y = (e.clientY - b.top) / b.height;
        var f = Math.max(0, Math.min(1, (x - c.left) / (c.right - c.left))), orig;
        if (c.lower !== "none" && y > c.lowTop) {
          orig = f * c.durWave;
        } else {
          orig = winStart(a.currentTime) + f * c.tDisplay * zoom - c.tDisplay / 2;
        }
        a.currentTime = Math.max(0, Math.min(dur - 0.001, (Math.max(0, orig) + c.buffer) / c.k));
        a.play();
      });
    };


    // comparison of sounds: the players of all sounds are stacked, with the controls of the last one for all of them.
    // Sounds are played in sequence, with a silence between them
    var initCompare = function (box) {
      var p = [].slice.call(box.children).filter(function (e) { return e.matches(".dynaspec-player,.dynaspec-paged,.dynaspec-scroll"); });
      if (p.length < 2) return;
      var n = p.length, a = p.map(function (e) { return e.querySelector("audio"); }), durs = box.dataset.durations.split(",").map(Number);
      var bars = p.map(function (e) { return e.querySelector(".ds-bar"); }), bar = bars[n - 1], hidden = bars.slice(0, n - 1);
      var gap = +box.dataset.gap, d = p[n - 1].dataset, color = [d.color, d.textColor], play = bar.querySelector(".ds-play");
      var opts = d.buttons ? d.buttons.split(",") : [], round = d.barStyle === "rounded", square = d.barStyle === "square";
      hidden.forEach(function (b) { b.style.display = "none"; });
      var active = 0, timer = null, raf;
      // (the players do not loop by themselves: the whole sequence is repeated)
      var loop = a.some(function (x) { return x.loop; });
      a.forEach(function (x) { x.loop = false; });
      var dur = function (i) { return a[i].duration || durs[i]; };
      // time at which a sound starts in the seek bar
      var offset = function (i) { var o = 0; for (var k = 0; k < i; k++) o += dur(k); return o; };
      var running = function () { return timer !== null || a.some(function (x) { return !x.paused; }); };
      var clear = function () { if (timer !== null) { clearTimeout(timer); timer = null; } };
      var start = function (i) {
        var q = a[i].play();
        // (browsers can refuse to start a sound without a click: the sequence waits for the play button)
        if (q && q.catch) q.catch(function () { show(); });
      };
      var next = function (i) {
        active = i;
        a[i].currentTime = 0;
        timer = setTimeout(function () { timer = null; start(i); }, gap * 1000);
      };

      // time readout of the sound being played and seek bar of all sounds
      var time = bar.querySelector(".ds-time"), readout = document.createElement("span");
      readout.className = "ds-time";
      time.insertAdjacentElement("afterend", readout);
      time.style.display = "none";
      if (opts.indexOf("time") < 0) readout.style.display = "none";
      var old = bar.querySelector(".ds-progress"), prog = null, dragging = false;
      if (old) {
        prog = document.createElement("input");
        prog.type = "range";
        prog.className = "ds-progress";
        prog.min = 0; prog.step = "any"; prog.value = 0;
        prog.style.accentColor = color[0];
        prog.setAttribute("aria-label", "position");
        old.insertAdjacentElement("afterend", prog);
        old.style.display = "none";
        prog.addEventListener("input", function () {
          dragging = true;
          var v = +prog.value, i = 0, on = running();
          while (i < n - 1 && v >= offset(i + 1)) i++;
          clear();
          if (i !== active) { a[active].pause(); a[active].currentTime = 0; active = i; }
          a[i].currentTime = Math.min(dur(i) - 0.001, v - offset(i));
          if (on) start(i);
          show();
        });
        prog.addEventListener("change", function () { dragging = false; });
      }
      // buttons to move between pages of the sound being played (paged spectrograms; sounds can have different numbers
      // of pages)
      var nav = bars.map(function (b) {
        var l = b.querySelector(".ds-page");
        return l && {label: l, prev: l.previousElementSibling, next: l.nextElementSibling};
      });
      var pageLabel = null;
      if (nav.some(function (x) { return x; })) {
        var mk = function (txt, title, which) {
          var b = document.createElement("button");
          b.type = "button";
          b.textContent = txt;
          b.title = title;
          b.addEventListener("click", function () { if (nav[active]) nav[active][which].click(); show(); });
          return b;
        };
        pageLabel = document.createElement("span");
        pageLabel.className = "ds-page";
        var at = nav[n - 1] ? nav[n - 1].next : play;
        [mk("\u25C0", "previous page", "prev"), pageLabel, mk("\u25B6", "next page", "next")].reverse().forEach(function (e) { at.insertAdjacentElement("afterend", e); });
        if (nav[n - 1]) [nav[n - 1].prev, nav[n - 1].label, nav[n - 1].next].forEach(function (e) { e.style.display = "none"; });
      }
      var fmt = function (t, D) { return D < 5 ? t.toFixed(3) : t.toFixed(1); };
      var show = function () {
        setPlay(play, running());
        var t = a[active].currentTime, D = dur(active);
        readout.textContent = (active + 1) + ": " + fmt(Math.min(t, D), D) + " / " + fmt(D, D) + " s";
        if (pageLabel) pageLabel.textContent = nav[active] ? nav[active].label.textContent : "1 / 1";
        if (prog) {
          prog.max = offset(n);
          if (!dragging) prog.value = Math.min(t, D) + offset(active);
          if (round || square) {
            var q = 100 * prog.value / prog.max;
            prog.style.background = "linear-gradient(to right, " + (round ? "var(--ds-fg,#333) " : "var(--ds-accent) ") + q + "%, color-mix(in srgb,var(--ds-fg,#333) " + (round ? 18 : 35) + "%,transparent) " + q + "%)";
          }
        }
      };
      var tick = function () { show(); if (running()) raf = requestAnimationFrame(tick); };

      a.forEach(function (x, i) {
        x.addEventListener("play", function () {
          clear();
          active = i;
          // (only the sound being played is away from its start)
          a.forEach(function (o) { if (o !== x && o.currentTime > 0) o.currentTime = 0; });
          cancelAnimationFrame(raf);
          tick();
        });
        x.addEventListener("pause", show);
        x.addEventListener("seeked", show);
        x.addEventListener("loadedmetadata", show);
        x.addEventListener("ended", function () {
          if (i < n - 1) next(i + 1);
          else if (loop) next(0);
          else active = 0;
          cancelAnimationFrame(raf);
          tick();
        });
      });
      // (sounds of other players stop the sequence)
      document.addEventListener("play", function (e) {
        if (a.indexOf(e.target) < 0 && e.target.tagName === "AUDIO" && timer !== null) { clear(); show(); }
      }, true);

      // controls of all sounds: the settings of the hidden controls follow those of the controls shown
      [".ds-speed input", ".ds-zoom input", ".ds-vol input"].forEach(function (sel) {
        var from = bar.querySelector(sel);
        if (from) from.addEventListener("input", function () {
          hidden.forEach(function (b) { var to = b.querySelector(sel); if (to) { to.value = from.value; to.dispatchEvent(new Event("input")); } });
        });
      });
      [".ds-pitch", ".ds-vol button", ".ds-dl"].forEach(function (sel) {
        var from = bar.querySelector(sel);
        if (from) from.addEventListener("click", function () {
          hidden.forEach(function (b) { var to = b.querySelector(sel); if (to) to.click(); });
        });
      });

      var lp = bar.querySelector(".ds-loop"), setLoop = function () {
        if (!lp) return;
        lp.setAttribute("aria-pressed", loop);
        lp.title = loop ? "playing repeatedly (click to stop looping)" : "click to play repeatedly";
        paintBtn(lp, loop, color);
      };
      setLoop();
      var fs = bar.querySelector(".ds-fs");
      // (before the listeners of the player, which control a single sound)
      bar.addEventListener("click", function (e) {
        if (e.target.closest(".ds-play")) {
          e.stopPropagation();
          if (running()) { clear(); a[active].pause(); } else start(active);
          show();
        } else if (lp && e.target.closest(".ds-loop")) {
          e.stopPropagation();
          loop = !loop;
          setLoop();
        } else if (fs && e.target.closest(".ds-fs")) {
          e.stopPropagation();
          if (document.fullscreenElement === box) document.exitFullscreen(); else box.requestFullscreen().catch(function () {});
        }
      }, true);
      if (fs) document.addEventListener("fullscreenchange", function () {
        var on = document.fullscreenElement === box;
        fs.textContent = on ? "\u2715 exit full screen" : "\u26F6 full screen";
        fs.title = on ? "exit full screen" : "show in full screen";
        if (on) box.style.setProperty("--ds-bg", p[n - 1].style.getPropertyValue("--ds-bg") || "#fff");
        // widest size at which all figures and the buttons fit in the screen
        var ratio = 0;
        p.forEach(function (e) { var img = e.querySelector(".ds-wrap img"); if (img && img.naturalWidth) ratio += img.naturalHeight / img.naturalWidth; });
        var w = on && ratio ? Math.max(200, (window.innerHeight - bar.offsetHeight - 80) / ratio) + "px" : "";
        p.forEach(function (e) { e.querySelector(".ds-wrap").style.maxWidth = w; });
        bar.style.maxWidth = w;
        window.dispatchEvent(new Event("resize"));
      });
      show();
    };

    window.dynaSpecPlayers = function () {
      document.querySelectorAll(".dynaspec-player:not([data-ready])").forEach(function (div) {
        div.setAttribute("data-ready", "");
        initPlayer(div);
      });
      document.querySelectorAll(".dynaspec-paged:not([data-ready])").forEach(function (div) {
        div.setAttribute("data-ready", "");
        initPaged(div);
      });
      document.querySelectorAll(".dynaspec-scroll:not([data-ready])").forEach(function (div) {
        div.setAttribute("data-ready", "");
        initScrolling(div);
      });
      document.querySelectorAll(".dynaspec-compare:not([data-ready])").forEach(function (div) {
        div.setAttribute("data-ready", "");
        initCompare(div);
      });
    };
  }
  window.dynaSpecPlayers();
})();
