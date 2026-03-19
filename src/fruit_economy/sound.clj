(ns fruit-economy.sound
  #_(:require [overtone.studio.inst :refer [definst saw]]
              [overtone.live :as live])
  (:import [javax.sound.midi MidiSystem]))

#_(definst foo [] (saw 220))

(def synth (MidiSystem/getSynthesizer))

(def instrument (-> synth
                  (.getDefaultSoundbank)
                  (.getInstruments)))

(def channel (-> synth
               (.getChannels)))

;; [Phone Tones in Musical Notation](https://news.ycombinator.com/item?id=30676957)
;; [Music theory for nerds](https://news.ycombinator.com/item?id=30358903)
  ;; original - https://news.ycombinator.com/item?id=12528144
;; https://hn.algolia.com/?q=music+theory

;; https://stackoverflow.com/questions/16462854/midi-beginner-need-to-play-one-note
;; https://stackoverflow.com/tags/javasound/info
;; https://www.cs.cmu.edu/~music/cmsip/readings/MIDI%20tutorial%20for%20programmers.html
(comment
  (doto synth
    (.open)
    (.loadInstrument (first instrument)))

  (.noteOn (first channel) 60 100)
  (.noteOff (first channel) 60))


;;(live/demo (live/sin-osc 440.0 0.0 1.0 0.0))
;
;(live/definst keyboard [volume 1.0 freq 440]
;  (let [src (live/sin-osc freq)
;        env (live/env-gen (live/perc 0.001 0.3) :action live/FREE)]
;    (* volume 1 src env)))
;
;(defn play-keyboard [ notename]
;  (keyboard :freq (* (live/midi->hz (live/note notename)) 4)))
;
;
;;(play-keyboard :c4)
