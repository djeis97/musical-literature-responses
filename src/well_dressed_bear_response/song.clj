(ns well-dressed-bear-response.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [overtone.inst.drum :as drums]
            [overtone.inst.piano :as piano]
            [overtone.inst.synth :as synth]))

(zero-conf-on)
(def metro (metronome 110))
(def controls (atom {"/1/fader1" 0.1 "/1/fader2" 0.2 "/1/fader3" 0.2 "/1/fader4" 0.5}))

(def server (osc-server 44100 "osc-clj"))
(osc-listen server (fn [e]
                     (swap! controls assoc (:path e) (first (:args e))))
            :osc-controls)

(defn attach-bus-controls [bus update-fn & rest]
  {:pre [(bus? bus) (ifn? update-fn)]}
  (add-watch controls [bus ::bus-controls]
             (fn [[bus _] _c old new]
               (let [o (apply update-fn old rest)
                     n (apply update-fn new rest)]
                 (when (not= o n)
                   (control-bus-set! bus n)))))
  (control-bus-set! bus (update-fn @controls)))

(defn detach-bus-controls [bus]
  (remove-watch controls [bus ::bus-controls]))

(defn attach-metronome-controls [metronome update-fn & rest]
  {:pre [(ifn? update-fn)]}
  (add-watch controls [metronome ::metronome-controls]
             (fn [[bus _] _c old new]
               (let [o (apply update-fn old rest)
                     n (apply update-fn new rest)]
                 (when (not= o n)
                   (metronome :bpm n))))))

(defn detach-metronome-controls [metronome]
  (remove-watch controls [metronome ::metronome-controls]))

(defn control [n]
  (get @controls n 0.0))

(definst beep [freq 440 amp 0.1 off1 0.2 off2 0.3]
  (lpf (* amp [(+ (saw freq)
                  (saw (+ freq off1)))
               (+ (saw freq)
                  (saw (+ freq off2)))]) 2000))
(definst piano [freq 440 volume 1.0 seconds 1]
  (let [snd (mda-piano :freq freq
                       :decay (* 0.6 seconds))]
    (detect-silence snd 0.005 :action FREE)
    (* volume snd)))

(defn cs80player [& {:keys [note]}]
  (synth/cs80lead (midi->hz note) 1.0
                  :amp (* 2 (control "/1/fader1"))
                  :att (* 1 (control 21))
                  :decay (* 8 (control 22))
                  :sus (* 1 (control 23))
                  :rel (* 8 (control 24))
                  :dtune (* 0.004 (control 25))
                  :vibrate (* 4 (control 26))
                  :vibdepth (* 0.05 (control 27))
                  ))



(definst warbly-sin [amp 0.3 hi-freq 880 hi-detune 1.0 lo-detune 1.005]
  (* amp (+ (sin-osc [hi-freq (* hi-freq hi-detune)])
            (sin-osc [(* hi-freq 0.25) (* hi-freq lo-detune 0.25)]))))

(def main-sin (warbly-sin :amp 0.0 :hi-freq (midi->hz 72)))
(def main-sin-amp-bus (control-bus))
(def detune-bus (control-bus))
(attach-bus-controls main-sin-amp-bus #(* 0.25 (% "/1/fader1") (% "/1/toggle1")))
(attach-bus-controls detune-bus #(+ 1 (* 0.02 (% "/1/fader2" 0.0) (% "/1/toggle2" 0.0))))
(node-map-controls main-sin [:amp main-sin-amp-bus])
(node-map-controls main-sin [:lo-detune detune-bus])

(defn loop-apply [now func & rest]
  (let [[len & rest] (apply func now rest)]
    (apply-by (metro (+ now len)) #'loop-apply (+ now len) func rest)))

(def notes (into {} (mapcat
                     #(mapcat (fn [l n]
                                [[(keyword (str l %)) (+ (* 12 %) n)]
                                 [(keyword (str l % "#")) (inc (+ (* 12 %) n))]
                                 [(keyword (str l % "b")) (dec (+ (* 12 %) n))]])
                              ["C" "D" "E" "F" "G" "A" "B"]
                              [0 2 4 5 7 9 11])
                     (take 8 (iterate inc 0)))))

(defn main-scale [root]
  (->> (phrase (repeat 1)
               (map notes [:C0 :E0b :G0 :G0b :A0 :C1#]))
       (where :pitch (comp temperament/equal scale/chromatic root))
       (wherever (comp not :vol), :vol (is 1.0))
       (all :part :main-melody)))

(def mel-vol-bus (control-bus "Melody Volume Bus"))
(attach-bus-controls mel-vol-bus #(* (% "/1/fader3" 0.0) (% "/1/toggle3" 0.0)))
(defmethod live/play-note :main-melody [{pitch :pitch duration :duration}]
  (let [note (piano :freq pitch :seconds duration)]
    (node-map-controls note [:volume mel-vol-bus])
    note))

(defn metro-duration [metro duration]
  (- (metro duration) (metro 0)))

(attach-metronome-controls metro #(* 200 (% "/1/fader5")))

(defn play-main-scale
  ([now] (play-main-scale now (main-scale scale/C)))
  ([now notes]
   (let [player (fn [{time :time :as note}]
                  (at (metro now)
                      (live/play-note (dissoc note :time))))
         len (:duration (first notes))]
     (player (first notes))
     (if (seq (rest notes))
       [len (rest notes)]
       [len]))))

(defn play-harmony
  ([now] (play-main-scale now (main-scale (comp scale/low scale/c))))
  ([now notes]
   (let [player (fn [{time :time :as note}]
                  (at (metro now)
                      (live/play-note (dissoc note :time))))
         len (:duration (first notes))]
     (player (first notes))
     (if (seq (rest notes))
       [len (rest notes)]
       [len]))))

(def heart-vol-bus (control-bus "Heartbeat Volume Bus"))
(attach-bus-controls heart-vol-bus #(* 1.5 (% "/1/fader4" 0.0) (% "/1/toggle4" 0.0)))
(defmethod live/play-note :heartbeat [{pitch :pitch}]
  (let [note (drums/kick2 :freq pitch)]
    (node-map-controls note [:amp heart-vol-bus])
    note))

(defn heartbeat [root]
  (->> (phrase [1/2 3/2]
               (map notes (repeat :C2)))
       (where :pitch (comp temperament/equal scale/chromatic root))
       (all :part :heartbeat)))

(defn play-heartbeat
  ([now] (play-heartbeat now (heartbeat identity)))
  ([now notes]
   (let [player (fn [{time :time :as note}]
                  (at (metro now)
                      (live/play-note (dissoc note :time))))
         len (:duration (first notes))]
     (player (first notes))
     (if (seq (rest notes))
       [len (rest notes)]
       [len]))))

(defn reverse-arpeggio [root]
  (->> (phrase [1/3 1/3 1/3 2]
               (map notes [:C1 :G0 :E0b :C0 :_]))
       (filter :pitch)
       (where :pitch (comp temperament/equal scale/chromatic root))
       (all :part :main-melody)))

(defn play-reverse-arpeggio
  ([now] (play-reverse-arpeggio now (reverse-arpeggio scale/C)))
  ([now notes]
   (let [player (fn [{time :time :as note}]
                  (at (metro now)
                      (live/play-note (dissoc note :time))))
         len (:duration (first notes))]
     (player (first notes))
     (if (seq (rest notes))
       [len (rest notes)]
       [len]))))

(comment
  (loop-apply (metro) #'play-main-scale) 
  (loop-apply (metro) #'play-reverse-arpeggio) 
  (loop-apply (metro) #'play-heartbeat) 
  )
