(ns poem-response.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [overtone.inst.drum :as drums]
            [overtone.inst.piano :as piano]))

(def back-triad {:i 0 :iii -2 :v -4})

; Instruments
(definst bass [freq 110 volume 1.0]
  (-> (saw freq)
      (* (env-gen (perc 0.1 0.2) :action FREE))
      (* volume)))

(definst organ [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (* (env-gen (adsr 0.01 0.8 0.1) (line:kr 1 0 dur) :action FREE))
      (* 1/4 volume)))

(definst growing [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (* (env-gen (adsr 1.0 0.4 0.1) (line:kr 1 0 dur) :action FREE))
      (* 1/4 volume)))

(definst piano [freq 440 volume 1 seconds 1]
  (let [snd (mda-piano {:freq freq
                        :vel volume
                        :decay (* 0.6 seconds)})]
    (detect-silence snd 0.005 :action FREE)
    (* 1 snd)))

; Arrangement
(defmethod live/play-note :bass [{pitch :pitch vol :vol}] (drums/kick4 pitch 0.5))
(defmethod live/play-note :accompaniment [{pitch :pitch seconds :duration vol :vol}]
  (organ pitch seconds vol))
(defmethod live/play-note :growing-accompaniment [{pitch :pitch seconds :duration vol :vol}]
  (growing pitch seconds vol))
(defmethod live/play-note :melody [{pitch :pitch seconds :duration vol :vol}]
  (piano pitch (* 80 vol) seconds))

; Composition
(def progression [0 0 3 0 4 0])

(def notes [7 9 11
            8 6 4
            5 7 9
            6 4 2
            3 5 7
            4 2 0
            1 3 5
            4 6 8])

(def accompaniment1 [7 9 11 14
                     13 11 8 6
                     5 7 9 12
                     11 9 6 4
                     3 5 7 10
                     9 7 4 2
                     1 3 5 8
                     4 6 8 10])
(def accompaniment2 [7 6 4
                     5 4 2
                     3 2 0
                     1 2
                     4])

(defn melody [root]
  (->> (phrase (repeat 1) notes)
       (where :pitch (scale/from root))
       (all :part :melody)))

(defn acc1 [root]
  (->> (phrase (cycle [1/2 1/2 1 1]) accompaniment1)
       (where :pitch (scale/from root))
       (all :part :melody)))

(defn acc2 [root]
  (->> (phrase (cycle [5/2 1/2 3]) (map #(chord/root {:i 0 :iii 2} %) accompaniment2))
       (where :pitch (scale/from root))
       (all :part :accompaniment)))

(defn acc3 [root]
  (->> (phrase (cycle [5/2 1/6 1/6 1/6])
               (map #(chord/root {:i 0 :iii 2} %) [9 7 9 7
                                                   4 6 4 6
                                                   7 5 7 5
                                                   2 4 2 4
                                                   5 3 5 3
                                                   0 2 0 2
                                                   3 1 3 1
                                                   6 4 6 4]))
       (where :pitch (scale/from root))
       (all :part :growing-accompaniment)))

(defn bass-line [root]
  (->> (phrase [1/3 1/3 1/3 1 1] [2 0 0 2 0])
       (where :pitch (scale/from root))
       (where :pitch (comp scale/lower scale/lower))
       (all :part :bass)))

; Track
(def track
  (->> (melody 0)
       (then (->> (melody 0)
                  (with (all :vol 0.6 (acc3 7)))))
       (then (->> (melody 0)
                  (with (all :vol 0.95 (acc1 7)))))
       (then (->> (melody 0)
                  (with (all :vol 0.7 (acc2 7)))))
       (then (->> (melody 0)
                  (with (all :vol 0.3 (acc2 7)))
                  (with (all :vol 0.95 (acc1 7)))))
       (then (->> (melody 0)
                  (with (all :vol 0.6 (acc3 7)))
                  (with (all :vol 0.9 (acc1 7)))
                  (with (all :vol 0.7 (acc2 -7)))))
       (then (melody -7))
       (then (->> (melody -7)
                  (with (all :vol 0.6 (acc3 0)))))
       (then (->> (melody -7)
                  (with (all :vol 0.6 (acc3 0)))
                  (with (all :vol 0.8 (acc1 7)))))
       (then (melody 7))
       (then (->> (melody 7)
                  (with (all :vol 0.6 (acc3 0)))))
       (then (->> (melody 7)
                  (with (all :vol 0.6 (acc3 0)))
                  (with (all :vol 0.8 (acc1 0)))))
       (then (->> (melody 7)
                  (with (all :vol 0.6 (acc3 0)))
                  (with (all :vol 0.8 (acc1 0)))
                  (with (all :vol 0.6 (acc2 -7)))))
       (then (->> (melody 0)
                  (with (all :vol 0.6 (acc3 0)))
                  (with (all :vol 0.8 (acc1 0)))
                  (with (all :vol 0.6 (acc2 -7)))))
       (then (melody 0))
       (where :pitch (comp temperament/equal scale/A scale/minor))
       (where :time (bpm 100))
       (where :duration (bpm 100))
       (wherever (comp not :vol), :vol (is 1.0))))

(defn -main []
  (live/play track))

(comment
                                        ; Loop the track, allowing live editing.
  (live/jam (var track)) 
  )
