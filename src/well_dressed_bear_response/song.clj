(ns well-dressed-bear-response.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [overtone.inst.drum :as drums]
            [overtone.inst.piano :as piano]))

(definst warbly-sin [amp 0.3 hi-freq 880 hi-detune 1.0 lo-detune 1.005]
  (* amp (+ (sin-osc [hi-freq (* hi-freq hi-detune)])
            (sin-osc [(* hi-freq 0.25) (* hi-freq lo-detune 0.25)]))))

