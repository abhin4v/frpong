(ns frpong.core
  (:require [frpong.helpers :as h :refer (mult tap)]
            [cljs.core.async :refer [<! >! chan put! close! sliding-buffer]]
            [domina :as dom :refer [log]])
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
        (if-let [t (<! frames)]
          (do (when (< t (* 10 prev)) (>! c t))
            (recur t))
          (close! c))))
    c))

(defn next-pos [[x y] [vel-x vel-y] tick]
  [(+ x (* vel-x tick)) (+ y (* vel-y tick))])

(defn ^:export frpong []
  (let [width           800
        height          400
        padding         5
        paddle-size     100
        paddle-width    10
        ball-radius     5
        init-pos        [50 100]
        init-vel        [0.2 0.23]
        paddle-step     20
        max-paddle-y    (- height paddle-size)
        ef-paddle-width (+ paddle-width padding)

        [frames stop-frames] (h/frame-chan)]

    (defn ticker [frames game-state ticks]
      (let [ticks-in (tick-chan (h/diff-chan frames))]
        (go (loop []
              (let [gs (<! game-state)]
                (do (>! ticks (<! ticks-in))
                  (if (= :gameover gs)
                    (stop-frames)
                    (recur))))))))

    (defn ball-positioner [ticks vel pos-in pos-out]
      (go-loop
        (let [tick     (<! ticks)
              pos-next (next-pos (<! pos-in) (<! vel) tick)]
          (>! pos-out pos-next))))

    (defn paddle-positioner [keycodes pos-in pos-out]
      (let [keys (h/key-chan keycodes)]
        (go-loop
          (let [pos (<! pos-in)]
            (>! pos-out
              (condp = (<! keys)
                :up   (max (- pos paddle-step) 0)
                :down (min (+ pos paddle-step) max-paddle-y)))))))

    (defn collision-detector [ticks pos vel-in pl-pos pr-pos game-state vel-out]
      (defn in-y-range? [y paddle-y]
        (and (> y (+ paddle-y padding)) (< y (- (+ paddle-y paddle-size) padding))))

      (defn detect-x-collision [x y lpaddle-y rpaddle-y]
        (cond
          (< x ef-paddle-width)
            (if (in-y-range? y lpaddle-y) :collision-left  :gameover)
          (> x (- width ef-paddle-width))
            (if (in-y-range? y rpaddle-y) :collision-right :gameover)
          :else :moving))

      (defn detect-y-collision [y]
        (cond
          (< y padding)            :collision-left
          (> y (- height padding)) :collision-right
          :else                    :moving))

      (defn collision? [x-state y-state]
        (or (= x-state :collision-left) (= x-state :collision-right)
                (= y-state :collision-left) (= y-state :collision-right)))

      (defn adjust-vel [state v]
        (condp = state
          :collision-left  (abs v)
          :collision-right (- (abs v))
          :moving          v
          :gameover        0))

      (go-loop
        (let [tick          (<! ticks)
              [vel-x vel-y] (<! vel-in)
              [x y]         (<! pos)
              lpaddle-y     (<! pl-pos)
              rpaddle-y     (<! pr-pos)
              [xn yn]       (next-pos [x y] [vel-x vel-y] tick)
              x-state       (detect-x-collision xn yn lpaddle-y rpaddle-y)
              vel-xn        (adjust-vel x-state vel-x)
              y-state       (detect-y-collision yn)
              vel-yn        (adjust-vel y-state vel-y)]
          (>! vel-out [vel-xn vel-yn])
          (>! game-state
            (cond
              (= x-state :gameover)        :gameover
              (collision? x-state y-state) :collision
              :else                        :moving)))))

    (defn renderer [ticks game-state pos pl-pos pr-pos]
      (let [ball-el    (dom/by-id "ball")
            state-el   (dom/by-id "state")
            lpaddle-el (dom/by-id "lpaddle")
            rpaddle-el (dom/by-id "rpaddle")
            fps-el     (dom/by-id "fps")]
      (go-loop
        (let [fps   (/ 1000 (<! ticks))
              [x y] (<! pos)
              gs    (<! game-state)]
          (dom/set-text! fps-el fps)
          (dom/set-text! state-el (name gs))
          (doto ball-el
            (dom/set-attr! "cx" x)
            (dom/set-attr! "cy" y))))
      (go-loop
        (dom/set-attr! lpaddle-el "y" (<! pl-pos)))
      (go-loop
        (dom/set-attr! rpaddle-el "y" (<! pr-pos)))))

    (defn setup-components [frames game-state pos vel pl-pos pr-pos]
      (let [ticks        (chan)
            ticks-m      (mult ticks)
            pos-m        (mult pos)
            vel-m        (mult vel)
            pl-pos-m     (mult pl-pos)
            pr-pos-m     (mult pr-pos)
            game-state-m (mult game-state)]
        (ticker frames (tap game-state-m) ticks)

        (ball-positioner (tap ticks-m) (tap vel-m) (tap pos-m) pos)
        (paddle-positioner {83 :down 87 :up} (tap pl-pos-m) pl-pos)
        (paddle-positioner {38 :up 40 :down} (tap pr-pos-m) pr-pos)

        (collision-detector (tap ticks-m) (tap pos-m) (tap vel-m)
          (h/sustain (tap pl-pos-m) (tap ticks-m (chan (sliding-buffer 1000))))
          (h/sustain (tap pr-pos-m) (tap ticks-m (chan (sliding-buffer 1000))))
          game-state vel)

        (renderer (tap ticks-m) (tap game-state-m) (tap pos-m) (tap pl-pos-m) (tap pr-pos-m))))

    (defn layout-game []
      (doto (dom/by-id "canvas")
          (dom/set-style! "width" (str width "px"))
          (dom/set-style! "height" (str height "px")))
      (doto (dom/by-id "ball")
        (dom/set-attr! "r" ball-radius)
        (dom/set-attr! "cx" (first init-pos))
        (dom/set-attr! "cy" (second init-pos)))
      (doseq [id ["lpaddle" "rpaddle"]]
        (doto (dom/by-id id)
          (dom/set-attr! "width" paddle-width)
          (dom/set-attr! "height" paddle-size)
          (dom/set-attr! "y" (/ (- height paddle-size) 2))))
      (dom/set-attr! (dom/by-id "lpaddle") "x" 0)
      (dom/set-attr! (dom/by-id "rpaddle") "x" (- width paddle-width)))

   (defn start-game []
    (let [init-paddle-pos (/ (- height paddle-size) 2) ;; initial paddle position
          pos             (chan 1)                     ;; ball position signal
          vel             (chan 1)                     ;; ball velocity signal
          pl-pos          (chan 1)                     ;; paddle left position signal
          pr-pos          (chan 1)                     ;; paddle right position signal
          game-state      (chan 1)]                    ;; game state signal
      (layout-game)
      (setup-components frames game-state pos vel pl-pos pr-pos)

      ;; start the game by setting the initial values of the signals
      (put! pos init-pos)
      (put! vel init-vel)
      (put! pl-pos init-paddle-pos)
      (put! pr-pos init-paddle-pos)
      (put! game-state :moving)))

    (start-game)))
