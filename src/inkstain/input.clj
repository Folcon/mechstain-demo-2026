(ns inkstain.input
  (:require [io.github.humbleui.window :as window])
  (:import [com.studiohartman.jamepad ControllerManager ControllerState]))



;; must be on the same thread
(def controller-manager (atom nil))

(defn ensure-init! []
  (when-not @controller-manager
    (let [mgr (ControllerManager.)]
      (.initSDLGamepad mgr)
      (reset! controller-manager mgr))))

(defn get-state [index]
  (ensure-init!)
  (let [mgr ^ControllerManager @controller-manager]
    (.update mgr)  ;; pump SDL events, detects connect/disconnect
    (.getState mgr index)))

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

(defn dpad-up [controller-state]
  (.-dpadUpJustPressed ^ControllerState controller-state))

(defn dpad-down [controller-state]
  (.-dpadDownJustPressed ^ControllerState controller-state))

(defn dpad-left [controller-state]
  (.-dpadLeftJustPressed ^ControllerState controller-state))

(defn dpad-right [controller-state]
  (.-dpadRightJustPressed ^ControllerState controller-state))

(def set-press-and-hold window/set-press-and-hold)
