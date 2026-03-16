(ns fruit-economy.humble-ui
  (:require [clojure.stacktrace :as stacktrace]
            [io.github.humbleui.core :as core :refer [deftype+]]
            [io.github.humbleui.protocols :refer [IComponent -measure -draw -event]]
            [io.github.humbleui.ui :as ui]
            [dali.io :refer [render-svg-string]]
            [dali.layout.stack]
            [dali.layout.align]
            [io.github.humbleui.paint :as paint])
  (:import [io.github.humbleui.skija Canvas Data ClipMode Surface]
           [io.github.humbleui.types IPoint IRect Rect]
           [io.github.humbleui.skija.svg SVGDOM]
           [java.lang AutoCloseable]))

;; Should be in io.github.humbleui.ui
(def *broken (atom false))

(defrecord UICanvas [width height on-paint on-event]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs canvas]
    (when on-paint
      (let [canvas ^Canvas canvas
            {:keys [width height]} cs
            layer  (.save canvas)
            rect  (Rect/makeXYWH 0 0 width height)]
        (try
          (.clipRect canvas rect ClipMode/INTERSECT true)
          (try
            (when-not @*broken
              (on-paint canvas width height))
            (catch Exception e
              (reset! *broken true)
              (stacktrace/print-stack-trace (stacktrace/root-cause e))))
          (finally
            (.restoreToCount canvas layer))))))
  (-event [_ event]
    (when on-event
      (try
        (when-not @*broken
          (on-event event))
        (catch Exception e
          (reset! *broken true)
          (stacktrace/print-stack-trace (stacktrace/root-cause e)))))))

(defn ui-canvas
  "(ui-canvas 400 300 {:on-paint #'on-paint-impl
                       :on-event #'on-event-impl})"
  [width height {:keys [on-paint on-event]}]
  (UICanvas. width height on-paint on-event))

(defn svg-canvas
  ([svg-str] (svg-canvas svg-str nil))
  ([svg-str opts]
   (let [dom (with-open [data (Data/makeFromBytes (.getBytes svg-str))]
               (SVGDOM. data))
         scaling (ui/svg-opts->scaling opts)]
     (ui/->SVG dom scaling nil))))

(defn svg
  ([doc] (svg doc nil))
  ([doc opts]
   (svg-canvas (render-svg-string doc) opts)))
;; END Should be in io.github.humbleui.ui

(def render-buffer (atom {}))

(defn make-named-buffer [name requested-width requested-height]
  (let [{:keys [width height]} (get @render-buffer name)]
    (when-not (and (= width requested-width) (= height requested-height))
      (let [surface (Surface/makeRasterN32Premul requested-width requested-height)
            canvas (.getCanvas surface)]
        (swap! render-buffer assoc name {:width requested-width
                                         :height requested-height
                                         :surface surface
                                         :canvas canvas
                                         :image nil})))))

(defn get-named-canvas [name]
  (get-in @render-buffer [name :canvas]))

(defn update-named-buffer [name]
  (swap! render-buffer assoc-in [name :image] (.makeImageSnapshot (get-in @render-buffer [name :surface]))))

(defn draw-named [name canvas]
  (when-let [image (get-in @render-buffer [name :image])]
    (.drawImage canvas image 0 0)))

(comment
  (do
    (require '[clojure.java.io :as io])
    (import '[io.github.humbleui.skija Surface Paint]))

  ;; Testing
  (let [svg-str "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"620\" height=\"472\"><text font-family=\"DejaVu Sans\" stroke-width=\"4\" x=\"310\" y=\"174.47\" font-size=\"180\">TEST</text></svg>"
        data-bytes (.getBytes svg-str)
        data (Data/makeFromBytes data-bytes)
        svg-dom (SVGDOM. data)
        front-buffer (Surface/makeRasterN32Premul 640 360)
        back-buffer (Surface/makeRasterN32Premul 640 360)
        front-canvas (.getCanvas front-buffer)
        back-canvas (.getCanvas back-buffer)

        render-svg? true]
    ;; clear canvas
    (doto back-canvas
      (.clear (unchecked-int 0xFFFFFFFF)))

    (if render-svg?
      ;; render svg
      (.render svg-dom back-canvas)

      ;; draw stuff
      (.drawCircle back-canvas 320 217 16 (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))))

    ;; copy across
    (.drawImage front-canvas (.makeImageSnapshot back-buffer) 0 0)

    (io/copy
      (-> front-buffer
        (.makeImageSnapshot)
        (.encodeToData)
        (.getBytes))
      (io/file "resources/mock-screen.png"))))

;; Should be in io.github.humbleui.ui
(defn fragment [& children]
  (list children))

(def <> #'fragment)

#_#_#_#_
(deftype+ CustomUI [width height on-paint on-event ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (set! child-rect (IRect/makeXYWH 0 0 (:width cs) (:height cs)))
    (when on-paint
      (let [canvas ^Canvas canvas
            {:keys [width height]} cs
            layer  (.save canvas)
            rect  (Rect/makeXYWH 0 0 width height)]
        (try
          (on-paint canvas width height)
          (finally
            (.restoreToCount canvas layer))))))
  (-event [_ event]
    (when on-event
      (on-event event))))

(defn custom-ui
  "(custom-ui 400 300 {:on-paint #'on-paint-impl
                       :on-event #'on-event-impl})"
  [width height {:keys [on-paint on-event]}]
  (->CustomUI width height on-paint on-event nil))

(deftype+ WithBounds [key child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
    (let [width  (-> (:width cs) (/ (:scale ctx)))
          height (-> (:height cs) (/ (:scale ctx)))
          point (IPoint. width height)
          val (get ctx key)
          _ (println :val val :point point)]
      (if-not val
        (-measure child (assoc ctx key point) cs)
        (-measure child ctx cs))))

  (-draw [_ ctx cs ^Canvas canvas]
    (set! child-rect (IRect/makeXYWH 0 0 (:width cs) (:height cs)))
    (let [width  (-> (:width cs) (/ (:scale ctx)))
          height (-> (:height cs) (/ (:scale ctx)))]
      (-draw child (assoc ctx key (IPoint. width height)) cs canvas)))

  (-event [_ event]
    (io.github.humbleui.ui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
    (io.github.humbleui.ui/child-close child)))

(defn with-bounds [key child]
  (->WithBounds key child nil))

(core/deftype+ Clickable [on-click child ^:mut child-rect ^:mut hovered? ^:mut pressed?]
  IComponent
  (-measure [_ ctx cs]
    (core/measure child ctx cs))

  (-draw [_ ctx rect canvas]
    (set! child-rect rect)
    (let [ctx' (cond-> ctx
                 hovered?                (assoc :hui/hovered? true)
                 (and pressed? hovered?) (assoc :hui/active? true))]
      (core/draw-child child ctx' child-rect canvas)))

  (-event [_ event]
    (core/eager-or
      (when (and child-rect (= :mouse-move (:event event)))
        (let [hovered?' (.contains ^IRect child-rect (IPoint. (:x event) (:y event)))]
          (when (not= hovered? hovered?')
            (set! hovered? hovered?')
            true)))
      (if (= :mouse-button (:event event))
        (or
          (core/event-child child event)
          (let [pressed?' (if (:pressed? event)
                            hovered?
                            (do
                              (when (and pressed? hovered? on-click)
                                (on-click))
                              false))]
            (when (not= pressed? pressed?')
              (set! pressed? pressed?')
              true)))
        (core/event-child child event))))

  AutoCloseable
  (close [_]
    (core/child-close child)))

(core/deftype+ VScroll [child ^:mut offset ^:mut self-rect ^:mut child-size ^:mut hovered?]
  IComponent
  (-measure [_ ctx cs]
    (let [child-cs (assoc cs :height Integer/MAX_VALUE)]
      (set! child-size (-measure child ctx child-cs))
      (IPoint. (:width child-size) (:height cs))))

  (-draw [_ ctx ^IRect rect ^Canvas canvas]
    (when (nil? child-size)
      (set! child-size (-measure child ctx (IPoint. (:width rect) Integer/MAX_VALUE))))
    (set! self-rect rect)
    (set! offset (core/clamp offset (- (:height rect) (:height child-size)) 0))
    (let [layer      (.save canvas)
          child-rect (-> rect
                       (update :y + offset)
                       (assoc :height Integer/MAX_VALUE))]
      (try
        (.clipRect canvas (.toRect rect))
        (core/draw child ctx child-rect canvas)
        (finally
          (.restoreToCount canvas layer)))))

  (-event [_ event]
    (when (and self-rect (= :mouse-move (:event event)))
      (let [hovered?' (.contains ^IRect self-rect (IPoint. (:x event) (:y event)))]
        (when (not= hovered? hovered?')
          (set! hovered? hovered?'))))
    (if (= :mouse-scroll (:event event))
      (or
        (core/event-child child event)
        (when hovered?
          (let [offset' (-> offset
                          (+ (:delta-y event))
                          (core/clamp (- (:height self-rect) (:height child-size)) 0))]
            (when (not= offset offset')
              (set! offset offset')
              true))))
      (core/event-child child event))))

(defn vscrollbar [child]
  (when-not (instance? VScroll child)
    (throw (ex-info (str "Expected VScroll, got: " (type child)) {:child child})))
  (io.github.humbleui.ui/->VScrollbar child (io.github.humbleui.paint/fill 0x10000000) (io.github.humbleui.paint/fill 0x60000000) nil))

(core/deftype+ Hoverable [child ^:mut child-rect ^:mut hovered?]
  IComponent
  (-measure [_ ctx cs]
    (core/measure child ctx cs))

  (-draw [_ ctx rect canvas]
    (set! child-rect rect)
    (let [ctx' (cond-> ctx hovered? (assoc :hui/hovered? true))]
      (core/draw-child child ctx' child-rect canvas)))

  (-event [_ event]
    (core/eager-or
      (core/event-child child event)
      (when (and child-rect (= :mouse-move (:event event)))
        (let [hovered?' (.contains ^IRect child-rect (IPoint. (:x event) (:y event)))]
          (when (not= hovered? hovered?')
            (set! hovered? hovered?')
            true)))))

  AutoCloseable
  (close [_]
    (core/child-close child)))

;;(alter-var-root #'io.github.humbleui.ui/->Clickable (fn [_] ->Clickable))
;;(alter-var-root #'io.github.humbleui.ui/->Hoverable (fn [_] ->Hoverable))
;;(alter-var-root #'io.github.humbleui.ui/->VScroll (fn [_] ->VScroll))
;;(alter-var-root #'io.github.humbleui.ui/vscrollbar (fn [_] vscrollbar))
;; END Should be in io.github.humbleui.ui
