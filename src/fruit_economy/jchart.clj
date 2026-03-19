(ns fruit-economy.jchart
  (:require [io.github.humbleui.ui :as ui]
            [fruit-economy.humble-ui :as custom-ui]
            [fruit-economy.jcomponent.core :as jcomponent])
  (:import [org.jfree.chart ChartFactory ChartPanel]
           [org.jfree.data.category DefaultCategoryDataset]
           [io.github.folcon.skija SkiaGraphics Chart]
           [javax.swing JTextArea JPanel]
           [io.github.humbleui.skija Paint Canvas PaintMode Typeface Font]
           [java.awt Dimension]
           [org.jfree.chart.plot CategoryPlot]))


(defn ->dataset
  "Pass data of the form:
  {\"JFreeSVG\" {\"Warm-up\" 7445 \"Test\" 4297} \"Batik\"  {\"Warm-up\" 24448 \"Test\" 21022}}
  The first key is x, the second is y, with the last being the value."
  [data]
  (let [dataset (DefaultCategoryDataset.)]
    (doseq [[x ys] data
            [y v] ys]
      (.addValue dataset v x y))
    dataset))

(defn ->chart-panel [chart]
  (ChartPanel. chart false))

(defn bar-chart [dataset]
  (let [chart (ChartFactory/createBarChart "title" "categoryAxisLabel" "valueAxisLabel" dataset)
        plot (.getPlot chart)
        renderer (.getRenderer ^CategoryPlot plot)
        chart-panel (->chart-panel chart)]
    (.setFillZoomRectangle chart-panel true)
    (.setMouseWheelEnabled chart-panel true)
    (.setPreferredSize chart-panel (Dimension. 100 100))
    (.setVisible chart-panel true)
    chart-panel))

(comment
  (def bc (bar-chart (->dataset {"JFreeSVG" {"Warm-up" 7445 "Test" 4297} "Batik"  {"Warm-up" 24448 "Test" 21022}})))

  (clojure.reflect/reflect bc))


;;; JSWING TEST
(def jtext-area (doto (JTextArea. "JTexty")
                  (.setBounds 0 0 200 50)))

(def bc (bar-chart (->dataset {"JFreeSVG" {"Warm-up" 7445 "Test" 4297} "Batik"  {"Warm-up" 24448 "Test" 21022}})))
(def panel (doto
             (JPanel.)
             ;(.add ^JTextArea jtext-area)
             (.add ^ChartPanel bc)
             (.setSize 100 100)
             (.setLayout nil)
             (.setVisible true)))

(defn draw-impl-- [^Canvas canvas _window-width _window-height]
  #_(let [border (doto (Paint.)
                   (.setColor (unchecked-int 0xFF000000))
                   (.setMode PaintMode/STROKE)
                   (.setStrokeWidth (* 1 2)))]
      (.drawCircle canvas 5 5 10 border)
      (.paint ^JPanel panel (SkiaGraphics. canvas)))
  (.paint ^JPanel (Chart/bar) #_(Chart/panel) (SkiaGraphics. canvas)))

(def jswing-test
  (ui/dynamic ctx [{:keys [font-default scale] :as ctx} ctx]
    (let []
      (ui/column
        (ui/hoverable
          (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
            (ui/column
              (ui/label (str "Test " hovered?) {:font font-default :paint (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))})
              (jcomponent/label 1000 100 (str "Test " hovered?))
              (jcomponent/combo-box 1000 100 [(str "Test " hovered?)])
              #_(jcomponent/check-box 1000 100 (str "Test " hovered?) hovered?)
              #_(jcomponent/radio-button 1000 100 (str "Test " hovered?) hovered?)
              (jcomponent/slider 1000 100 0 20 6)
              (jcomponent/text-field 1000 100 (str "Test " hovered?))
              (jcomponent/text-area 1000 100 (str "Test " hovered?))
              (jcomponent/progress-bar 1000 100 (if hovered? 25 75)))))
        (ui/hoverable
          (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
            (ui/canvas
              {:on-paint #'draw-impl--})))))))
;;; JSWING TEST END
