(ns fruit-economy.jcomponent.core
  (:require [io.github.humbleui.core :refer [deftype+]]
            [io.github.humbleui.protocols :refer [IComponent -measure -draw -event]])
  (:import [javax.swing JLabel JComboBox JCheckBox JRadioButton JSlider JTextField JTextArea JProgressBar JPanel]
           [io.github.humbleui.skija Canvas]
           [io.github.humbleui.types IPoint]
           [io.github.folcon.skija SkiaGraphics]
           [java.util Vector]
           [clojure.lang PersistentVector]))


(deftype+ Label [width height text ^:mut j-comp]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (let [j-comp (if j-comp
                   j-comp
                   (let [j-label (doto (JLabel. ^String text)
                                   (.setBounds 0 0 width height)
                                   (.setSize width height))]
                     (set! j-comp j-label)
                     j-comp))]
      (when j-comp
        (let [canvas ^Canvas canvas
              layer  (.save canvas)]
          (try
            (.paint ^JLabel j-comp (SkiaGraphics. canvas))
            (finally
              (.restoreToCount canvas layer)))))))
  (-event [_ event]))

(defn label
  [width height ^String text]
  (->Label width height text nil))

(deftype+ ComboBox [width height coll ^:mut j-comp]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (let [j-comp (if j-comp
                   j-comp
                   (let [j-combo-box (doto (JComboBox. (Vector. ^PersistentVector (into [] coll)))
                                       (.setBounds 0 0 width height)
                                       (.setSize width height))]
                     (set! j-comp j-combo-box)
                     j-comp))]
      (when j-comp
        (let [canvas ^Canvas canvas
              layer  (.save canvas)]
          (try
            (.paint ^JComboBox j-comp (SkiaGraphics. canvas))
            (finally
              (.restoreToCount canvas layer)))))))
  (-event [_ event]))

(defn combo-box
  [width height coll]
  (->ComboBox width height coll nil))

(deftype+ CheckBox [width height text default ^:mut j-comp]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (let [j-comp (if j-comp
                   j-comp
                   (let [j-check-box (doto (JCheckBox. ^String text (boolean default))
                                       (.setBounds 0 0 width height)
                                       (.setSize width height))]
                     (set! j-comp j-check-box)
                     j-comp))]
      (when j-comp
        (let [canvas ^Canvas canvas
              layer  (.save canvas)]
          (try
            (.paint ^JCheckBox j-comp (SkiaGraphics. canvas))
            (finally
              (.restoreToCount canvas layer)))))))
  (-event [_ event]))

(defn check-box
  [width height text default]
  (->CheckBox width height text default nil))

(deftype+ RadioButton [width height text default ^:mut j-comp]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (let [j-comp (if j-comp
                   j-comp
                   (let [j-radio-button (doto (JRadioButton. ^String text (boolean default))
                                          (.setBounds 0 0 width height)
                                          (.setSize width height))]
                     (set! j-comp j-radio-button)
                     j-comp))]
      (when j-comp
        (let [canvas ^Canvas canvas
              layer  (.save canvas)]
          (try
            (.paint ^JRadioButton j-comp (SkiaGraphics. canvas))
            (finally
              (.restoreToCount canvas layer)))))))
  (-event [_ event]))

(defn radio-button
  [width height text default]
  (->RadioButton width height text default nil))

(deftype+ Slider [width height min max value ^:mut j-comp]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (let [j-comp (if j-comp
                   j-comp
                   (let [j-slider (doto (JSlider. min max value)
                                    (.setBounds 0 0 width height)
                                    (.setSize width height))]
                     (set! j-comp j-slider)
                     j-comp))]
      (when j-comp
        (let [canvas ^Canvas canvas
              layer  (.save canvas)]
          (try
            (.paint ^JSlider j-comp (SkiaGraphics. canvas))
            (finally
              (.restoreToCount canvas layer)))))))
  (-event [_ event]))

(defn slider
  [width height min max value]
  (->Slider width height min max value nil))

(deftype+ TextField [width height text ^:mut j-comp]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (let [j-comp (if j-comp
                   j-comp
                   (let [j-text-field (doto (JTextField. ^String text)
                                        (.setBounds 0 0 width height)
                                        (.setSize width height))]
                     (set! j-comp j-text-field)
                     j-comp))]
      (when j-comp
        (let [canvas ^Canvas canvas
              layer  (.save canvas)]
          (try
            (.paint ^JTextField j-comp (SkiaGraphics. canvas))
            (finally
              (.restoreToCount canvas layer)))))))
  (-event [_ event]))

(defn text-field
  [width height text]
  (->TextField width height text nil))

(deftype+ TextArea [width height text ^:mut j-comp]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (let [j-comp (if j-comp
                   j-comp
                   (let [j-text-area (doto (JTextArea. ^String text)
                                       (.setBounds 0 0 width height)
                                       (.setSize width height))]
                     (set! j-comp j-text-area)
                     j-comp))]
      (when j-comp
        (let [canvas ^Canvas canvas
              layer  (.save canvas)]
          (try
            (.paint ^JTextArea j-comp (SkiaGraphics. canvas))
            (finally
              (.restoreToCount canvas layer)))))))
  (-event [_ event]))

(defn text-area
  [width height text]
  (->TextArea width height text nil))

(deftype+ ProgressBar [width height value ^:mut j-comp]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (let [j-comp (if j-comp
                   j-comp
                   (let [j-text-area (doto (JProgressBar.)
                                       (.setValue value)
                                       (.setBounds 0 0 width height)
                                       (.setSize width height))]
                     (set! j-comp j-text-area)
                     j-comp))]
      (when j-comp
        (let [canvas ^Canvas canvas
              layer  (.save canvas)]
          (try
            (.paint ^JProgressBar j-comp (SkiaGraphics. canvas))
            (finally
              (.restoreToCount canvas layer)))))))
  (-event [_ event]))

(defn progress-bar
  [width height value]
  (->ProgressBar width height value nil))
