(ns inkstain.core
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]

   [inkstain.config :as config]
   [inkstain.input :as input]
   [inkstain.state :as state]
   [inkstain.game :as game]

   [io.github.humbleui.app :as app]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.util :as util]
   [io.github.humbleui.window :as window]))



(defn load-state []
  (let [file (io/file ".state")]
    (when (.exists file)
      (edn/read-string (slurp file)))))

(defn save-state [m]
  (let [file   (io/file ".state")
        state  (or (load-state) {})
        state' (merge state m)]
    (spit file (pr-str state'))))

(defonce *app
  (atom nil))

(reset! *app game/game-root)

(defn maybe-save-window-rect [window event]
  (when (#{:window-move :window-resize} (:event event))
    (let [rect (window/window-rect window)
          {:keys [id scale work-area]} (window/screen window)
          x (-> rect :x (- (:x work-area)) (/ scale) int)
          y (-> rect :y (- (:y work-area)) (/ scale) int)
          w (-> rect :width (/ scale) int)
          h (-> rect :height (/ scale) int)]
      (save-state {:screen-id id, :x x, :y y, :width w, :height h}))))

(defn restore-window-rect []
  (util/when-some+ [{:keys [screen-id x y width height]} (load-state)]
    (when-some [screen (util/find-by :id screen-id (app/screens))]
      (let [{:keys [scale work-area]} screen
            right  (-> (:right work-area) (/ scale) int)
            bottom (-> (:bottom work-area) (/ scale) int)
            x      (-> x (min (- right 500)) (max 0))
            y      (-> y (min (- bottom 500)) (max 0))
            width  (min (- right x) width)
            height (min (- bottom y) height)]
        {:screen screen-id, :x x, :y y, :width width, :height height}))))

(defn -main [& args]
  (ui/start-app!
    (deliver state/*window
      (ui/window
        (merge
          {:title    (str config/version-string "👋")
           :screen   (:id (first (app/screens)))
           ;; TODO: Doesn't look like it's picking up the new size, check!
           :width    (first config/init-window-size)
           :height   (second config/init-window-size)
           :x        :center
           :y        :center
           :on-paint #'input/on-frame!
           :on-event #'maybe-save-window-rect
           :exit-on-close? (if config/debug? false true)}
          (restore-window-rect))
        *app))
    (input/set-press-and-hold false))
  @state/*window)
