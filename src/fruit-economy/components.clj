(ns fruit-economy.components
  (:require [io.github.humbleui.ui :as ui])
  (:import [io.github.humbleui.skija Canvas Paint PaintMode]))


(defn draw-radio-toggle-impl [active? fill scale]
  (fn [^Canvas canvas _window-width _window-height]
    (let [border (doto (Paint.)
                   (.setColor (unchecked-int 0xFF000000))
                   (.setMode PaintMode/STROKE)
                   (.setStrokeWidth (* 1 scale)))]
      (.drawCircle canvas 5 5 10 border)
      (if active?
        (.drawCircle canvas 5 5 6 fill)))))

(defn radio-button [state value selected-path child]
  (ui/clickable
    #(swap! state assoc-in selected-path value)
    (ui/dynamic ctx [{:keys [fill-black scale]} ctx
                     selected (get-in @state selected-path)]
      (ui/column
        (ui/row
          (ui/canvas
            {:on-paint (#'draw-radio-toggle-impl (= selected value) fill-black scale)}))
        (ui/gap 5 0)
        child))))

(defn atom-checkbox [*checked text]
  (ui/clickable
    #(swap! *checked not)
    (ui/dynamic ctx [checked @*checked
                     {:keys [font-ui fill-text leading scale]} ctx]
      (let [border (doto (Paint.)
                     (.setColor (unchecked-int 0xFF000000))
                     (.setMode PaintMode/STROKE)
                     (.setStrokeWidth (* 1 scale)))]
        (ui/row
          (ui/fill border
            (if checked
              (ui/padding 1 1
                (ui/fill fill-text
                  (ui/gap (- leading 2) (- leading 2))))
              (ui/gap leading leading)))
          (ui/gap 5 0)
          (ui/label text {:font font-ui :paint fill-text}))))))
