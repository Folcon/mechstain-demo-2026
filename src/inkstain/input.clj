(ns inkstain.input
  (:import [com.studiohartman.jamepad ControllerManager ControllerState]))



(def controller-manager
  (doto (ControllerManager.)
    (.initSDLGamepad)))

(defn get-state [index]
  (.getState ^ControllerManager controller-manager index))

(defn connected? [controller-state]
  (.-isConnected ^ControllerState controller-state))

(defn left-stick-x [controller-state]
  (.leftStickX ^ControllerState controller-state))

(defn left-stick-y [controller-state]
  (.leftStickY ^ControllerState controller-state))

(defn right-stick-x [controller-state]
  (.rightStickX ^ControllerState controller-state))

(defn right-stick-y [controller-state]
  (.rightStickY ^ControllerState controller-state))
