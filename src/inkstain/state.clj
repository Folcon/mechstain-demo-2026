(ns inkstain.state)



(defonce *next-id (atom 0))
(defn next-id []
  (dec (swap! *next-id inc)))

(defonce *window (promise))
(defonce *state (atom {}))
(defonce *ui-tree (atom nil))

(defonce *debug-state
  (atom {:paused? false
         :error nil}))

(defonce *texture-cache (atom {}))


;; :iso or :ortho
(defonce *selected-camera (atom {:type :ortho}))

(defonce *events (atom []))
(defonce *keys-held (atom #{}))
(defonce *rotation (atom 0.0))
(defonce *stats (atom {}))

(defonce *chat-visible (atom false))
(defonce *chat-history (atom ["System: Welcome to the game!"]))
(defonce *scroll-to-bottom (atom true))
(defonce *last-message-time (atom (System/currentTimeMillis)))

(defonce *key-bindings (atom nil))
