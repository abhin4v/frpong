(ns frpong.core
  (:require [frpong.helpers :refer (mult tap diff-chan key-chan frame-chan sustain)]
            [cljs.core.async :refer [<! >! chan put! close! sliding-buffer]]
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

(defn rand [] (.random js/Math))

(defn abs [x] (.abs js/Math x))

(def PI 3.141592653589793)

(defn deg->rad [deg] (* (/ deg 180) PI))

(defn cos [x] (.cos js/Math x))
(defn sin [x] (.sin js/Math x))

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
  (let [body-el         (.-body js/document)
        width           (- (.-scrollWidth body-el) 20)
        height          (- (.-scrollHeight body-el) 125)
        padding         5
        paddle-size     100
        paddle-width    10
        ball-radius     8
        ball-speed      0.33
        init-pos        [(/ width 2) (/ height 2)]
        init-vel        (let [sgn #(if (< % 0.5) -1 1)
                              deg (+ 35 (* 20 (rand)))
                              rad (deg->rad deg)]
                          (map #(* ball-speed %) 
                            [(* (sgn (rand)) (sin rad)) (* (sgn (rand)) (cos rad))]))
        paddle-step     20
        max-paddle-y    (- height paddle-size)
        ef-paddle-width (+ paddle-width padding)
        init-paddle-pos (/ (- height paddle-size) 2)]

    (defn layout-game
      "Lays out the game screen."
      []
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

    (defn start-game
      "Sets up the game by creating the signals and setting up the components and starts the game."
      []
      (let [frames     (frame-chan)  ;; frames signal
            pos        (chan 1)      ;; ball position signal
            vel        (chan 1)      ;; ball velocity signal
            pl-pos     (chan 1)      ;; paddle left position signal
            pr-pos     (chan 1)      ;; paddle right position signal
            game-state (chan 1)]     ;; game state signal, the state of the game and the current score
        (setup-components frames game-state pos vel pl-pos pr-pos)

        ;; start the game by setting the initial values of the signals
        (put! pos init-pos)
        (put! vel init-vel)
        (put! pl-pos init-paddle-pos)
        (put! pr-pos init-paddle-pos)
        (put! game-state [:moving 0])))

    (defn setup-components
      "Multiplexes the signals and sets up the components by connecting them using the signals 
       tapped from the multiplexers.
       The signals are taken as parameters."
      [frames game-state pos vel pl-pos pr-pos]
      (let [ticks        (chan)             ;; ticks signal
            ticks-m      (mult ticks)       ;; multiplexers for all signals
            pos-m        (mult pos)
            vel-m        (mult vel)
            pl-pos-m     (mult pl-pos)
            pr-pos-m     (mult pr-pos)
            game-state-m (mult game-state)

            ;; paddle position signals are not at the same rate as the rest so they need to be
            ;; sustained at the ticks rate
            pl-pos-sust  (sustain (tap pl-pos-m) (tap ticks-m (chan (sliding-buffer 1000))))
            pr-pos-sust  (sustain (tap pr-pos-m) (tap ticks-m (chan (sliding-buffer 1000))))]
        ;; set up the components
        (ticker frames (tap game-state-m) ticks)

        (ball-positioner (tap ticks-m) (tap vel-m) (tap pos-m) pos)
        (paddle-positioner {83 :down 87 :up} (tap pl-pos-m) pl-pos)
        (paddle-positioner {38 :up 40 :down} (tap pr-pos-m) pr-pos)

        (collision-detector (tap ticks-m) (tap pos-m) (tap vel-m) 
          pl-pos-sust pr-pos-sust (tap game-state-m) game-state vel)

        (renderer (tap ticks-m) (tap game-state-m) (tap pos-m) (tap pl-pos-m) (tap pr-pos-m))))

    (defn ticker 
      "Ticker component.
       Converts `frames` signal to ticks and outputs them to the `ticks` signal
       as long as the `game-state` signal is not :gameover. Once the `game-state` signal is 
       :gameover, stops the `frames` signal hence stopping the entire game.
       Each tick is the number of milliseconds since the last tick was generated."
      [[frames stop-frames] game-state ticks]
      (let [ticks-in (tick-chan (diff-chan frames))]
        (go (loop []
              (let [[state _] (<! game-state)]
                (do (>! ticks (<! ticks-in))
                  (if (= :gameover state)
                    (stop-frames)
                    (recur))))))))

    (defn ball-positioner
      "Ball Positioner component.
       Calculates the next ball position using the current ball position (from the `pos-in` signal)
       and the current tick (from the `ticks` signal) and outputs it to the `pos-out` signal."
      [ticks vel pos-in pos-out]
      (go-loop
        (let [tick     (<! ticks)
              pos-next (next-pos (<! pos-in) (<! vel) tick)]
          (>! pos-out pos-next))))

    (defn paddle-positioner
      "Paddle Positioner component.
       Captures the keydown signal for the provides keycodes and calculates the next paddle
       position using the current paddle position (from the `pos-in` signal) and keydown signal
       and outputs it to the `pos-out` signal."
      [keycodes pos-in pos-out]
      (let [keys (key-chan keycodes)]
        (go-loop
          (let [pos (<! pos-in)]
            (>! pos-out
              (condp = (<! keys)
                :up   (max (- pos paddle-step) 0)
                :down (min (+ pos paddle-step) max-paddle-y)))))))

    (defn collision-detector [ticks pos vel-in pl-pos pr-pos game-state-in game-state vel-out]
      "Collision Detector component.
       Detects the collision of the ball with the walls and the paddles and accordingly calculates
       and outputs the next ball velocity and next game state to the `vel-out` and `game-state` 
       signals respectively.
       Reads the current tick, ball position, ball velocity and left and right paddle positions from
       the `ticks`, `pos`, `vel-in`, `pl-pos` and `pr-pos` signals respectively."
      (defn in-y-range? [y paddle-y]
        (and (> y (- paddle-y padding)) (< y (+ paddle-y paddle-size padding))))

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

      (defn collision-state? [state]
        (or (= state :collision-left) (= state :collision-right)))

      (defn adjust-vel [state v]
        (condp = state
          :collision-left  (abs v)
          :collision-right (- (abs v))
          :moving          v
          :gameover        0))

      (defn perturb [v] (* v (+ 1 (/ (- (rand) 0.5) 25))))

      (go-loop
        (let [;; get all current values
              tick            (<! ticks)
              [vel-x vel-y]   (<! vel-in)
              [x y]           (<! pos)
              lpaddle-y       (<! pl-pos)
              rpaddle-y       (<! pr-pos)
              [xn yn]         (next-pos [x y] [vel-x vel-y] tick)
              [_ score]       (<! game-state-in)
              
              ;; calculate next position and detect collision
              x-state         (detect-x-collision xn yn lpaddle-y rpaddle-y)
              vel-xn          (adjust-vel x-state vel-x)
              y-state         (detect-y-collision yn)
              vel-yn          (adjust-vel y-state vel-y)
              x-collision     (collision-state? x-state)
              y-collision     (collision-state? y-state)

              ;; calculate next game state
              state-n         (cond
                                (= x-state :gameover)        :gameover
                                (or x-collision y-collision) :collision
                                :else                        :moving)
              score-n         (if x-collision (inc score) score)

              ;; add a small random perturbation to the ball velocity on collision with paddles
              [vel-xn vel-yn] (if x-collision
                                (map perturb [vel-xn vel-yn])
                                [vel-xn vel-yn])]
          (>! vel-out [vel-xn vel-yn])
          (>! game-state [state-n score-n]))))

    (defn renderer
      "Renderer component.
       Renders the ball and paddle positions on the browser. Also shows the game state and stats.
       Reads the current values from the signals supplied as parameters."
      [ticks game-state pos pl-pos pr-pos]
      (let [ball-el    (dom/by-id "ball")
            state-el   (dom/by-id "state")
            score-el   (dom/by-id "score")
            lpaddle-el (dom/by-id "lpaddle")
            rpaddle-el (dom/by-id "rpaddle")
            fps-el     (dom/by-id "fps")]
      (go (loop [fps-p nil state-p nil score-p nil]
            (let [fps           (int (/ 1000 (<! ticks)))
                  [x y]         (<! pos)
                  [state score] (<! game-state)
                  state         (condp = state
                                  :moving "Playing"
                                  :collision "Playing"
                                  :gameover "Game Over")]
              (when-not (= fps fps-p)
                (dom/set-text! fps-el fps))
              (when-not (= state state-p)
                (dom/set-text! state-el state))
              (when-not (= score score-p)
                (dom/set-text! score-el score))
              (doto ball-el
                (dom/set-attr! "cx" x)
                (dom/set-attr! "cy" y))
              (recur fps state score))))
      (go-loop
        (dom/set-attr! lpaddle-el "y" (<! pl-pos)))
      (go-loop
        (dom/set-attr! rpaddle-el "y" (<! pr-pos)))))

    ;; Everything is ready now. Layout the game and start it on any keypress!
    (layout-game)
    (ev/listen-once! :keypress start-game)))
