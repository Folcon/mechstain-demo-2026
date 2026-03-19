(ns fruit-economy.jcomponent.chart
  (:require [io.github.humbleui.core :refer [deftype+]]
            [io.github.humbleui.protocols :refer [IComponent -measure -draw -event]]
            [io.github.humbleui.ui :as ui])
  (:import [java.awt Dimension]
           [io.github.humbleui.types IPoint IRect]
           [org.jfree.chart ChartPanel ChartFactory JFreeChart]
           [io.github.folcon.skija SkiaGraphics]
           [io.github.humbleui.skija Canvas Surface Image]
           [org.jfree.chart.title DateTitle]
           [org.jfree.data.category DefaultCategoryDataset]
           [org.jfree.chart.plot PlotOrientation XYPlot]
           [java.lang AutoCloseable]
           [org.jfree.data.xy VectorSeries VectorSeriesCollection]
           [org.jfree.chart.axis NumberAxis]
           [org.jfree.chart.renderer.xy VectorRenderer]))


#_
(deftype+ Chart [dataset opts on-event ^:mut j-comp]
  IComponent
  (-measure [_ ctx cs]
    cs
    #_(IPoint. (:width cs) (:height cs)))

  (-draw [_ ctx ^IRect rect ^Canvas canvas]
    (let [{:keys [^int x ^int y width height]} rect
          _ (println :width width :height height)
          j-comp (if j-comp
                   j-comp
                   (let [date-title (DateTitle.) ; Set current date as the subtitle of the chart
                         chart (doto (ChartFactory/createStackedBarChart "Title" "" "" dataset PlotOrientation/VERTICAL true true false)
                                 (.addSubtitle date-title))
                         chart-panel (doto (ChartPanel. chart)
                                       (.setBounds 0 0 width height)
                                       (.setPreferredSize (Dimension. width height)))]
                     (println :building-j-comp)
                     (set! j-comp chart-panel)
                     j-comp))]
      (when j-comp
        (let [layer (.save canvas)]
          (try
            ;(.clipRect canvas (.toRect rect))
            (let [sk-canvas (doto (SkiaGraphics. canvas)
                              (.translate x y))]
              (.paint ^ChartPanel j-comp sk-canvas))
            (finally
              (.restoreToCount canvas layer)))))))

  (-event [_ event]
    (when on-event
      (on-event event))))

(deftype+ Chart [chart-fn on-event ^:mut ^Image image]
  IComponent
  (-measure [_ ctx cs]
    cs)

  (-draw [_ ctx ^IRect rect ^Canvas canvas]
    (let [{:keys [^int x ^int y width height]} rect]
      (when (or (nil? image)
              (not= (.getWidth image) (:width rect))
              (not= (.getHeight image) (:height rect)))
        (when image
          (.close image))

        (set! image
          (with-open [surface (Surface/makeRasterN32Premul width height)]
            (println :update-chart)
            (let [image-canvas (.getCanvas surface)
                  j-comp (chart-fn width height)
                  sk-canvas (SkiaGraphics. image-canvas)]
              (.paint ^ChartPanel j-comp sk-canvas)
              (.makeImageSnapshot surface)))))

      (.drawImageRect canvas image (.toRect rect))))

  (-event [_ event])

  AutoCloseable
  (close [_]))

#_
(deftype+ Chart [width height dataset opts ^:mut j-comp ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx rect ^Canvas canvas]
    (println :chart (:x rect) (:y rect))
    (set! child-rect rect)
    (let [j-comp (if j-comp
                   j-comp
                   (let [date-title (DateTitle.) ; Set current date as the subtitle of the chart
                         chart (doto (ChartFactory/createStackedBarChart "Title" "" "" dataset PlotOrientation/VERTICAL true true false)
                                 (.addSubtitle date-title))
                         chart-panel (doto (ChartPanel. chart)
                                       (.setBounds 0 0 width height)
                                       (.setPreferredSize (Dimension. width height)))]
                     (set! j-comp chart-panel)
                     j-comp))]
      (when j-comp
        (let [canvas ^Canvas canvas
              layer  (.save canvas)]
          (try
            (.translate canvas (:x rect) (:y rect))
            (.paint ^ChartPanel j-comp (SkiaGraphics. canvas))
            (finally
              (.restoreToCount canvas layer)))))))
  (-event [_ event]))

(defn chart [chart-fn]
  (->Chart chart-fn nil nil))

(defn make-chart-fn [dataset]
  (fn [width height]
   (let [date-title (DateTitle.) ; Set current date as the subtitle of the chart
         chart (doto (ChartFactory/createStackedBarChart "Title" "" "" dataset PlotOrientation/VERTICAL true true false)
                 (.addSubtitle date-title))
         chart-panel (doto (ChartPanel. chart)
                       (.setBounds 0 0 width height)
                       (.setPreferredSize (Dimension. width height)))]
     chart-panel)))

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

(defn dr [[SD PV]]
  (cond
    (and (> SD 0) (> PV 0))
    (max (* PV 1.2) 0.01)

    (> SD 0)
    (max (* PV -0.5) 0.01)

    (and (< SD 0) (< PV 0))
    (max (* PV 1.2) -0.01)

    (< SD 0)
    (max (* PV -0.5) -0.01)

    :else 0))

(defn make-chart-fn2 []
  (let [data (reduce
               (fn [vs [r f x y]]
                 (doto vs (.add r f x y)))
               (VectorSeries. "Derivative")
               (let [scale 0.2]
                 (for [sd (range 0 5 0.2)
                       pv (range 0 5 0.2)
                       :let [a (dr [sd sd])
                             b pv
                             m (Math/sqrt (+ (* a a) (* b b)))
                             s (if (< m 1E-6) 1E6 m)
                             x (* scale (/ a s))
                             y (* scale (/ b s))]]
                   [sd pv x y])))]
    (fn [width height]
      (let [chart (JFreeChart.
                    "Direction Field"
                    (XYPlot.
                      (doto (VectorSeriesCollection.) (.addSeries data))
                      (NumberAxis. "Rabbits")
                      (NumberAxis. "Foxes")
                      (VectorRenderer.)))
            chart-panel (doto (ChartPanel. chart)
                          (.setBounds 0 0 width height)
                          (.setPreferredSize (Dimension. width height)))]
        chart-panel))))

