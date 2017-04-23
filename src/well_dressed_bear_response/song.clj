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
