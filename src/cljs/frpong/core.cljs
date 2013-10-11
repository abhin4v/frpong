(ns frpong.core
  (:require [frpong.helpers :as h]
            [cljs.core.async :as async
             :refer [<! >! chan put! close! sliding-buffer dropping-buffer timeout]]
            [domina :as dom :refer [log]]
            [domina.events :as ev])
  (:require-macros [cljs.core.async.macros :as m :refer [go]]
                   [frpong.core :refer (go-loop)]))
;;
;;                                Signal Diagram
;;
;;                                                     +---------------------+
;;                                                     |   +-------------+   |
;;                                                     |   |             |   |
;;                                                     v   v             |   |
;;                                                 +----------+ vel-chan |   |
;;                                             +-->|c-detector+----------+   |
;;                                             |   +----------+          |   |
;;                                             |       +-----------------+   |
;;                                             |       |   +-----------------+
;;                                             |       |   |                 |
;;                                             |       v   v                 |
;;  +---------+ frame-chan  +------+ tick-chan |   +----------+   pos-chan   |
;;  |frame-gen+------------>|ticker+-----------+-->|positioner+--------------+
;;  +---------+             +------+               +----------+              |
;;                                                     +---------------------+
;;                                                     |
;;                                                     v
;;                                                 +----------+
;;                                                 | renderer |
;;                                                 +----------+

(defn abs [x] (.abs js/Math x))

(defn tick-chan [frames]
  (let [c (chan)]
    (go
      (loop [prev (<! frames)]
        (let [t (<! frames)]
          (when (< t (* 10 prev))
            (>! c t))
            (recur t))))
    c))

(defn ball-positioner [ticks vel pos-in pos-out]
  (go-loop
    (let [tick (<! ticks)
          [vel-x vel-y] (<! vel)
          [x y] (<! pos-in)
          pos-next [(+ x (* vel-x tick)) (+ y (* vel-y tick))]]
      (>! pos-out pos-next))))

(defn collision-detector
  [{:keys [width height padding paddle-size paddle-width]}
   ticks game-state pos pl-pos pr-pos vel-in vel-out]
  (let [adjust-v (fn [state v] [(condp = state
                                  :collision-left (abs v)
                                  :collision-right (- (abs v))
                                  :moving v
                                  :gameover 0)
                                state])
        detect-y-collision (fn [y] (cond
                                     (< y padding) :collision-left
                                     (> y (- height padding)) :collision-right
                                     :else :moving))
        detect-x-collision (fn [x y pl-pos pr-pos]
                             (cond
                               (< x (+ paddle-width padding))
                                 (if (and (> y (+ pl-pos padding))
                                       (< y (- (+ pl-pos paddle-size) padding)))
                                   :collision-left
                                   :gameover)
                               (> x (- width (+ paddle-width padding)))
                                 (if (and (> y (+ pr-pos padding))
                                    (< y (- (+ pr-pos paddle-size) padding)))
                                   :collision-right
                                   :gameover)
                               :else :moving))]
    (go-loop
      (let [tick (<! ticks)
            [vel-x vel-y] (<! vel-in)
            [x y] (<! pos)
            pl-pos (<! pl-pos)
            pr-pos (<! pr-pos)
            [xn yn] [(+ x (* vel-x tick)) (+ y (* vel-y tick))]
            [vel-xn bs-x] (adjust-v (detect-x-collision xn yn pl-pos pr-pos) vel-x)
            [vel-yn bs-y] (adjust-v (detect-y-collision yn) vel-y)]
        (>! vel-out [vel-xn vel-yn])
        (>! game-state
          (cond
            (= bs-x :gameover) :gameover
            (or (= bs-x :collision-left) (= bs-x :collision-right)
              (= bs-y :collision-left) (= bs-y :collision-right)) :collision 
            :else :moving))))))

(defn paddle-positioner [keycodes max-y movement pos-in pos-out]
  (let [keys (h/key-chan keycodes)]
    (go-loop
      (let [pos (<! pos-in)]
        (>! pos-out
          (condp = (<! keys)
            :up (max (- pos movement) 0)
            :down (min (+ pos movement) max-y)))))))

(defn renderer [game-state pos vel pl-pos pr-pos]
  (let [pos-el (dom/by-id "pos")
        vel-el (dom/by-id "vel")
        ball-el (dom/by-id "ball")
        state-el (dom/by-id "state")
        lpaddle-el (dom/by-id "lpaddle")
        rpaddle-el (dom/by-id "rpaddle")]
  (go-loop
    (let [[x y] (<! pos)
          gs (<! game-state)]
      (dom/set-text! pos-el (map int [x y]))
      (dom/set-text! vel-el (<! vel))
      (dom/set-text! state-el (name gs))
      (doto ball-el
        (dom/set-attr! "cx" x)
        (dom/set-attr! "cy" y))))
  (go-loop
    (dom/set-attr! lpaddle-el "y" (<! pl-pos)))
  (go-loop
    (dom/set-attr! rpaddle-el "y" (<! pr-pos)))))

(defn ticker [frames stop-frames ticks game-state]
  (let [ticks-in (tick-chan (h/diff-chan frames))]
    (go-loop
      (if (not= :gameover (<! game-state))
        (>! ticks (<! ticks-in))
        (stop-frames)))))

(defn game-setup [{:keys [width height padding paddle-size] :as layout} paddle-movement
                  frames stop-frames game-state pos vel pl-pos pr-pos]
  (let [max-y (- height paddle-size)
        ticks (chan)
        [tick-pos tick-collsion tick-pl tick-pr] (h/multiplex ticks 4)
        [pos-in pos-collision pos-render] (h/multiplex pos 3)
        [vel-pos vel-collision vel-render] (h/multiplex vel 3)
        [pl-pos-in pl-pos-collision pl-pos-render] (h/multiplex pl-pos 3)
        [pr-pos-in pr-pos-collision pr-pos-render] (h/multiplex pr-pos 3)
        [game-state-ticker game-state-render] (h/dup-chan game-state)]
    (ticker frames stop-frames ticks game-state-ticker)
    (ball-positioner tick-pos vel-pos pos-in pos)
    (collision-detector layout tick-collsion game-state pos-collision
      (h/sustain pl-pos-collision tick-pl)
      (h/sustain pr-pos-collision tick-pr)
      vel-collision vel)
    (paddle-positioner {83 :down 87 :up} max-y paddle-movement pl-pos-in pl-pos)
    (paddle-positioner {38 :up 40 :down} max-y paddle-movement pr-pos-in pr-pos)
    [game-state-render pos-render vel-render pl-pos-render pr-pos-render]))

(defn game-init [{:keys [height paddle-size] :as layout}
                 {:keys [init-pos init-vel paddle-movement]} frames stop-frames]
  (let [init-paddle-pos (/ (- height paddle-size) 2)
        pos (chan 1)
        vel (chan 1)
        pl-pos (chan 1)
        pr-pos (chan 1)
        game-state (chan 1)]
    (put! pos init-pos)
    (put! vel init-vel)
    (put! pl-pos init-paddle-pos)
    (put! pr-pos init-paddle-pos)
    (put! game-state :moving)

    (apply renderer
      (game-setup layout paddle-movement frames stop-frames game-state pos vel pl-pos pr-pos))))

(defn ^:export init []
  (let [[frames stop-frames] (h/frame-chan)
        [frames-fps frames-count frames-game] (h/multiplex frames 3)

        fps (h/map-chan #(/ 1000 %) (h/diff-chan frames-fps))
        frames-count (h/counting-chan frames-count)

        layout {:width 800
                :height 400
                :padding 5
                :paddle-size 100
                :paddle-width 10
                :ball-radius 5}
        init-vals {:init-pos [50 100]
                   :init-vel [0.3 0.33]
                   :paddle-movement 20}

        fps-el (dom/by-id "fps")
        frame-el (dom/by-id "frame")]
    (doto (dom/by-id "canvas")
      (dom/set-style! "width" (str (:width layout) "px"))
      (dom/set-style! "height" (str (:height layout) "px")))
    (doto (dom/by-id "ball")
      (dom/set-attr! "r" (:ball-radius layout))
      (dom/set-attr! "cx" (first (:init-pos init-vals)))
      (dom/set-attr! "cy" (second (:init-pos init-vals))))
    (doseq [id ["lpaddle" "rpaddle"]]
      (doto (dom/by-id id)
        (dom/set-attr! "width" (:paddle-width layout))
        (dom/set-attr! "height" (:paddle-size layout))
        (dom/set-attr! "y" (/ (- (:height layout) (:paddle-size layout)) 2))))
    (dom/set-attr! (dom/by-id "lpaddle") "x" 0)
    (dom/set-attr! (dom/by-id "rpaddle") "x" (- (:width layout) (:paddle-width layout)))

    (go-loop
      (dom/set-text! fps-el (<! fps))
      (dom/set-text! frame-el (<! frames-count)))
    (game-init layout init-vals frames-game stop-frames)))
