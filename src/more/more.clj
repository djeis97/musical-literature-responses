(ns more.more
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

(def server (osc-server 44101 "osc-clj"))
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

(attach-metronome-controls metro #(* 200 (% "/1/fader5")))

(defn control [n]
  (get @controls n 0.0))

(def notes (into {} (mapcat
                     #(mapcat (fn [l n]
                                [[(keyword (str l %)) (+ (* 12 %) n)]
                                 [(keyword (str l % "#")) (inc (+ (* 12 %) n))]
                                 [(keyword (str l % "b")) (dec (+ (* 12 %) n))]])
                              ["C" "D" "E" "F" "G" "A" "B"]
                              [0 2 4 5 7 9 11])
                     (take 8 (iterate inc 0)))))


(definst piano [freq 440 volume 1.0 seconds 1]
  (let [snd (mda-piano :freq freq
                       :decay (* 0.6 seconds))]
    (detect-silence snd 0.005 :action FREE)
    (* volume snd)))

(definst clap
  [low {:default 7500 :min 100 :max 10000 :step 1}
   hi  {:default 1500 :min 100 :max 10000 :step 1}
   amp {:default 0.3 :min 0.001 :max 1 :step 0.01}
   decay {:default 0.6 :min 0.1 :max 0.8 :step 0.001}]
  (let [noise      (bpf (lpf (white-noise) low) hi)
        clap-env   (line 1 0 decay :action FREE)
        noise-envs (map #(envelope [0 0 1 0] [(* % 0.01) 0 0.04]) (range 8))
        claps      (apply + (* noise (map env-gen noise-envs)))]
    (* amp claps clap-env)))

(def bass-vol-bus (control-bus "Bass Volume"))
(attach-bus-controls bass-vol-bus #(* 2.0 (% "/1/fader1") (% "/1/toggle1")))
(defmethod live/play-note :bassline [{pitch :pitch}]
  (let [node (synth/ks1 :note (hz->midi pitch) :amp 0.9)]
    (node-map-controls node [:amp bass-vol-bus])))

(def melody-vol-bus (control-bus "Melody Volume"))
(attach-bus-controls melody-vol-bus #(* 1.0 (% "/1/fader2") (% "/1/toggle2")))
(defmethod live/play-note :melody [{pitch :pitch}]
  (let [node (piano pitch)]
    (node-map-controls node [:volume melody-vol-bus])))

(def clap-vol-bus (control-bus "Clap Volume"))
(attach-bus-controls clap-vol-bus #(* 0.5 (% "/1/fader3") (% "/1/toggle3")))
(defmethod live/play-note :clap [_]
  (let [node (clap)]
    (node-map-controls node [:amp clap-vol-bus])))
(def heart-vol-bus (control-bus "Heartbeat Volume Bus"))
(attach-bus-controls heart-vol-bus #(* 1.5 (% "/1/fader4" 0.0) (% "/1/toggle4" 0.0)))
(defmethod live/play-note :heartbeat [{pitch :pitch}]
  (let [note (drums/kick2 :freq pitch)]
    (node-map-controls note [:amp heart-vol-bus])
    note))


(defn bassline [root]
  (->> (phrase (cycle [1 1 1/2 1/2 1])
               [0 0 0 0 :_ 5 5 5 5 :_])
       (where :pitch root)
       (all :part :bassline)))

(defn melody [root]
  (->> (phrase (cycle [3/4 1/4 1/2 1/2])
               [0 7 5 3 2 4 3 2 0 7 5 3 2 3 4 2])
       (where :pitch root)
       (all :part :melody)))

(defn clap-line []
  (->> (phrase (cycle [1/3 1/3 1/3 1/2 1/2])
               (repeat 0))
       (all :part :clap)
       (take (* 5 4))))

(defn heartbeat []
  (->> (phrase (cycle [2 1/2 3/2])
               (repeat -24))
       (all :part :heartbeat)
       (take (* 3/2 4))))


(let [cur-bpm (metro :bpm)]
  (def piece
    (->> nil
         (then (->> nil
                    (with (melody identity))
                    (with (bassline (comp scale/lower scale/lower scale/lower)))
                    (with (clap-line))
                    (with (heartbeat))))
         (where :pitch (comp temperament/equal scale/A scale/aeolian))
         (where :time (bpm cur-bpm))
         (where :duration (bpm cur-bpm)))))

(comment
  (live/jam #'piece) 
  )
