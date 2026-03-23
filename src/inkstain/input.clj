(ns inkstain.input
  (:require [io.github.humbleui.window :as window]
            [inkstain.state :as state])
  (:import [com.studiohartman.jamepad ControllerManager ControllerState]))



;; TODO: Figure out how to refactor this to be more elegant, this feels so crap right now
;; must be on the same thread
(def controller-manager (atom nil))

(def *current-controller (atom nil))

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

(defn controller-type [controller-state]
  (.-controllerType ^ControllerState controller-state))

(defn left-stick-x [controller-state]
  (.-leftStickX ^ControllerState controller-state))

(defn left-stick-y [controller-state]
  (.-leftStickY ^ControllerState controller-state))

(defn right-stick-x [controller-state]
  (.-rightStickX ^ControllerState controller-state))

(defn right-stick-y [controller-state]
  (.-rightStickY ^ControllerState controller-state))

(defn left-trigger [controller-state]
  (.-leftTrigger ^ControllerState controller-state))

(defn right-trigger [controller-state]
  (.-rightTrigger ^ControllerState controller-state))

(defn left-stick-click [controller-state]
  (.-leftStickJustClicked ^ControllerState controller-state))

(defn right-stick-click [controller-state]
  (.-rightStickJustClicked ^ControllerState controller-state))

(defn dpad-up [controller-state]
  (.-dpadUpJustPressed ^ControllerState controller-state))

(defn dpad-down [controller-state]
  (.-dpadDownJustPressed ^ControllerState controller-state))

(defn dpad-left [controller-state]
  (.-dpadLeftJustPressed ^ControllerState controller-state))

(defn dpad-right [controller-state]
  (.-dpadRightJustPressed ^ControllerState controller-state))

(defn a-button [controller-state]
  (.-aJustPressed ^ControllerState controller-state))

(defn b-button [controller-state]
  (.-bJustPressed ^ControllerState controller-state))

(defn x-button [controller-state]
  (.-xJustPressed ^ControllerState controller-state))

(defn y-button [controller-state]
  (.-yJustPressed ^ControllerState controller-state))

(defn left-bumper-button [controller-state]
  (.-lbJustPressed ^ControllerState controller-state))

(defn right-bumper-button [controller-state]
  (.-rbJustPressed ^ControllerState controller-state))

(defn start-button [controller-state]
  (.-startJustPressed ^ControllerState controller-state))

(defn back-button [controller-state]
  (.-backJustPressed ^ControllerState controller-state))

(defn guide-button [controller-state]
  (.-guideJustPressed ^ControllerState controller-state))


(defn dpad-up-held [controller-state]
  (.-dpadUp ^ControllerState controller-state))

(defn dpad-down-held [controller-state]
  (.-dpadDown ^ControllerState controller-state))

(defn dpad-left-held [controller-state]
  (.-dpadLeft ^ControllerState controller-state))

(defn dpad-right-held [controller-state]
  (.-dpadRight ^ControllerState controller-state))

(defn a-button-held [controller-state]
  (.-a ^ControllerState controller-state))

(defn b-button-held [controller-state]
  (.-b ^ControllerState controller-state))

(defn x-button-held [controller-state]
  (.-x ^ControllerState controller-state))

(defn y-button-held [controller-state]
  (.-y ^ControllerState controller-state))

(defn left-bumper-button-held [controller-state]
  (.-lb ^ControllerState controller-state))

(defn right-bumper-button-held [controller-state]
  (.-rb ^ControllerState controller-state))

(defn start-button-held [controller-state]
  (.-start ^ControllerState controller-state))

(defn back-button-held [controller-state]
  (.-back ^ControllerState controller-state))

(defn guide-button-held [controller-state]
  (.-guide ^ControllerState controller-state))

(defn left-stick-click-held [controller-state]
  (.-leftStickClick ^ControllerState controller-state))

(defn right-stick-click-held [controller-state]
  (.-rightStickClick ^ControllerState controller-state))

(defn left-stick-angle [controller-state]
  (.-leftStickAngle ^ControllerState controller-state))

(defn left-stick-magnitude [controller-state]
  (.-leftStickMagnitude ^ControllerState controller-state))

(defn right-stick-angle [controller-state]
  (.-rightStickAngle ^ControllerState controller-state))

(defn right-stick-magnitude [controller-state]
  (.-rightStickMagnitude ^ControllerState controller-state))

(defn misc1 [controller-state]
  (.-misc1JustPressed ^ControllerState controller-state))

(defn misc1-held [controller-state]
  (.-misc1 ^ControllerState controller-state))

(defn paddle1 [controller-state]
  (.-paddle1JustPressed ^ControllerState controller-state))

(defn paddle2 [controller-state]
  (.-paddle2JustPressed ^ControllerState controller-state))

(defn paddle2-held [controller-state]
  (.-paddle2 ^ControllerState controller-state))

(defn paddle3 [controller-state]
  (.-paddle3JustPressed ^ControllerState controller-state))

(defn paddle3-held [controller-state]
  (.-paddle3 ^ControllerState controller-state))

(defn paddle4 [controller-state]
  (.-paddle4JustPressed ^ControllerState controller-state))

(defn paddle4-held [controller-state]
  (.-paddle4 ^ControllerState controller-state))

(defn touchpad-button [controller-state]
  (.-touchpadButtonJustPressed ^ControllerState controller-state))

(defn touchpad-button-held [controller-state]
  (.-touchpadButton ^ControllerState controller-state))


(def *handlers (atom {}))

;; TODO: make this smart enough to do key reassignment
(defn register-handlers! [focus-id handler-map]
  (swap! *handlers update focus-id merge handler-map))

(defn pop-focus [stack]
  (if (> (count stack) 1)
    (pop stack)
    stack))

(defn push-focus! [focus-id]
  (swap! state/*screen conj focus-id))

(defn pop-focus! []
  (swap! state/*screen pop-focus))

(defn clear-focus! []
  (reset! state/*screen [:menu]))

(defn swap-focus! [focus-id]
  (swap! state/*screen (comp #(conj % focus-id) pop-focus)))

(comment
  (do (pop-focus!)
    (identity @state/*screen))

  (do (swap-focus! :playing)
    (identity @state/*screen)))

(defn on-frame! [window _canvas]
  (let [^ControllerState cs (get-state 0)
        controller (when (and cs (connected? cs)) cs)]
    (reset! *current-controller controller)
    (when controller
      (let [focus (peek @state/*screen)
            handlers (get @*handlers focus)]
        (doseq [[input-key handler] handlers
                :let [pressed? (case input-key
                                 :a-button   (a-button controller)
                                 :b-button   (b-button controller)
                                 :x-button   (x-button controller)
                                 :y-button   (y-button controller)
                                 :start      (start-button controller)
                                 :dpad-up    (dpad-up controller)
                                 :dpad-down  (dpad-down controller)
                                 :dpad-left  (dpad-left controller)
                                 :dpad-right (dpad-right controller)
                                 nil)]
                :when pressed?]
          (handler @state/*state)))
      (when cs
        (window/request-frame window)))))

(def set-press-and-hold window/set-press-and-hold)
