(ns well-dressed-bear-response.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [overtone.inst.drum :as drums]
            [overtone.inst.piano :as piano]))

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
               (let [o (update-fn old)
                     n (update-fn new)]
                 (when (not= o n)
                   (control-bus-set! bus n))))))

(defn detach-bus-controls [bus]
  (remove-watch controls [bus ::bus-controls]))


(defn control [n]
  (get @controls n 0.0))

(definst beep [freq 440 amp 0.1 off1 0.2 off2 0.3]
  (lpf (* amp [(+ (saw freq)
                  (saw (+ freq off1)))
               (+ (saw freq)
                  (saw (+ freq off2)))]) 2000))

(defn cs80player [& {:keys [note]}]
  (cs80lead (midi->hz note) 1.0
            :amp (* 2 (control "/1/fader1"))
            :att (* 1 (control 21))
            :decay (* 8 (control 22))
            :sus (* 1 (control 23))
            :rel (* 8 (control 24))
            :dtune (* 0.004 (control 25))
            :vibrate (* 4 (control 26))
            :vibdepth (* 0.05 (control 27))
            )
  )



(definst warbly-sin [amp 0.3 hi-freq 880 hi-detune 1.0 lo-detune 1.005]
  (* amp (+ (sin-osc [hi-freq (* hi-freq hi-detune)])
            (sin-osc [(* hi-freq 0.25) (* hi-freq lo-detune 0.25)]))))

(def main-sin (warbly-sin))
(def main-sin-amp-bus (control-bus))
(def detune-bus (control-bus))
(attach-bus-controls main-sin-amp-bus #(* (% "/1/fader1") (% "/1/toggle1")))
(attach-bus-controls detune-bus #(+ 1 (* 0.01 (% "/1/fader2" 0.0) (% "/1/toggle2" 0.0))))
(node-map-controls main-sin [:amp main-sin-amp-bus])
(node-map-controls main-sin [:lo-detune detune-bus])

(def main-melody
  (let [pitches
        [88 89 84 83 :_]
        durations
        [4 2 6 8 8]
        times (reductions + 0 durations)]
    (map vector times pitches durations)))

(def main-melodies
  {:base (let [pitches
               [88 89 84 83 :_]
               durations
               [4 2 6 15 1]
               times (reductions + 0 durations)]
           (map vector times pitches durations))
   :aug1 (let [pitches
               [88 89 84 88 83 :_]
               durations
               [4 2 6 1/4 59/4 1]
               times (reductions + 0 durations)]
           (map vector times pitches durations))
   :aug2 (let [pitches
               [88 89 84 83 81 :_]
               durations
               [4 2 6 4 11 1]
               times (reductions + 0 durations)]
           (map vector times pitches durations))})

(def main-markov-model
  {:base [:base :base :aug1 :aug2]
   :aug1 [:base]
   :aug2 [:base]})

(defn loop-apply [now func & rest]
  (let [[len & rest] (apply func now rest)]
    (apply-by (metro (+ now len)) #'loop-apply (+ now len) func rest)))

(defn play-main [now mel-ref mel-id]
  (let [play-note (fn [[beat midi dur]]
                    (if (not (= midi :_))
                      (let [synth (at (metro (+ now beat))
                                      (let [note (midi->hz midi)]
                                        (cs80lead note
                                                  :amp (* 2.0 (control "/1/fader3"))
                                                  :att (* 1.0 dur)
                                                  :decay 1.0
                                                  :sus 0.05
                                                  :rel 0.2)))]
                        (at (metro (+ now beat dur))
                            (ctl synth :gate -5.0)))))
        last (last (mel-id @mel-ref))
        len (+ (last 0) (last 2))]
    (dorun (map play-note (mel-id @mel-ref)))
    [len mel-ref (rand-nth (mel-id main-markov-model))]))
