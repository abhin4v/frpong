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

(defn abs [x] (.abs js/Math x))
(defn sqrt [x] (.sqrt js/Math x))
(defn sq [x] (* x x))

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

(def *width*            (- (.-scrollWidth (.-body js/document)) 20))
(def *height*           (- (.-scrollHeight (.-body js/document)) 125))
(def *padding*          5)
(def *paddle-size*      100)
(def *paddle-width*     10)
(def *ball-radius*      8)
(def *ball-speed*       0.5)
(def *center*           [(/ *width* 2 ) (/ *height* 2)])
(def *paddle-step*      20)
(def *max-paddle-y*     (- *height* *paddle-size*))
(def *ef-paddle-width*  (+ *paddle-width* *padding*))
(def *init-paddle-pos*  (/ (- *height* *paddle-size*) 2))
(def *init-vel-deg-lim* [35 55])
(def *perturb-factor*   0.02)
(def *G*                0.01)

(defn layout-game
  "Lays out the game screen."
  []
  (doto (dom/by-id "canvas")
      (dom/set-style! "width" (str *width* "px"))
      (dom/set-style! "height" (str *height* "px")))
  (doto (dom/by-id "ball")
    (dom/set-attr! "r" *ball-radius*)
    (dom/set-attr! "cx" (first *center*))
    (dom/set-attr! "cy" (second *center*)))
  (doseq [id ["lpaddle" "rpaddle"]]
    (doto (dom/by-id id)
      (dom/set-attr! "width" *paddle-width*)
      (dom/set-attr! "height" *paddle-size*)
      (dom/set-attr! "y" (/ (- *height* *paddle-size*) 2))))
  (dom/set-attr! (dom/by-id "lpaddle") "x" 0)
  (dom/set-attr! (dom/by-id "rpaddle") "x" (- *width* *paddle-width*)))

(defn initial-velocity []
  (let [[l h] *init-vel-deg-lim*
        sgn   #(if (< % 0.5) -1 1)
        deg   (+ l (* (- h l) (rand)))
        rad   (deg->rad deg)]
    (map #(* *ball-speed* %) 
      [(* (sgn (rand)) (sin rad)) (* (sgn (rand)) (cos rad))])))

(defn start-game
  "Sets up the game by creating the signals and setting up the components and starts the game."
  []
  (let [frames     (frame-chan) ;; frames signal
        pos        (chan 1)     ;; ball position signal
        vel        (chan 1)     ;; ball velocity signal
        acc        (chan 1)
        pl-pos     (chan 1)     ;; paddle left position signal
        pr-pos     (chan 1)     ;; paddle right position signal
        game-state (chan 1)     ;; game state signal, the state of the game and the current score
        init-vel   (initial-velocity)]
    (setup-components frames game-state pos vel acc pl-pos pr-pos)

    ;; start the game by setting the initial values of the signals
    (put! pos *center*)
    (put! vel init-vel)
    (put! pl-pos *init-paddle-pos*)
    (put! pr-pos *init-paddle-pos*)
    (put! game-state [:moving 0])))

(defn start-on-space []
  (ev/listen-once! :keypress #(if (= (:keyCode %) 32) (start-game) (start-on-space))))

(defn setup-components
  "Creates mult(iple)s of the signals and sets up the components by connecting them using 
   the signals tapped from the mults.
   The signals are taken as parameters."
  [frames game-state pos vel acc pl-pos pr-pos]
  (let [ticks        (chan)             ;; ticks signal
        ticks-m      (mult ticks)       ;; mult(iple)s for all signals
        pos-m        (mult pos)
        vel-m        (mult vel)
        acc-m        (mult acc)
        pl-pos-m     (mult pl-pos)
        pr-pos-m     (mult pr-pos)
        game-state-m (mult game-state)

        ;; paddle position signals are not at the same rate as the rest so they need to be
        ;; sustained at the ticks rate
        pl-pos-sust  (sustain (tap pl-pos-m) (tap ticks-m (chan (sliding-buffer 1000))))
        pr-pos-sust  (sustain (tap pr-pos-m) (tap ticks-m (chan (sliding-buffer 1000))))]
    ;; set up the components by tapping into mults
    (ticker frames (tap game-state-m) ticks)

    (gravitation (tap pos-m) acc)
    (ball-positioner (tap ticks-m) (tap pos-m) (tap vel-m) (tap acc-m) pos)
    (paddle-positioner {83 :down 87 :up} (tap pl-pos-m) pl-pos)
    (paddle-positioner {38 :up 40 :down} (tap pr-pos-m) pr-pos)

    (collision-detector (tap ticks-m) (tap pos-m) (tap vel-m) (tap acc-m)
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
              (if-not (= :gameover state)
                (recur)
                (stop-frames))))))))

(defn gravity-acc [[x y]]
  (let [[cx cy]  *center*
        x-dist   (- cx x)
        y-dist   (- cy y)
        distance (sqrt (+ (sq x-dist) (sq y-dist)))
        bearing  [(/ x-dist distance) (/ y-dist distance)]]
    (if-not (= distance 0)
      (map #(* *G* % (/ 1 distance)) bearing)
      [0 0])))

(defn gravitation
  "Gravitation component.
   Calculates acceleration due to gravitation using the pos from the `pos-in` signal and outputs
   it to the `acc` signal."
  [pos-in acc]
  (go-loop
    (>! acc (gravity-acc (<! pos-in)))))

(defn next-pos [[x y] [vel-x vel-y] [acc-x acc-y] tick]
  [(+ x (* vel-x tick) (* acc-x (sq tick))) (+ y (* vel-y tick) (* acc-y (sq tick)))])

(defn ball-positioner
  "Ball Positioner component.
   Calculates the next ball position using the current ball position, velocity and acceleration
   (from the `pos-in`, `vel` and `acc` signals respectively) and the current tick (from the
   `ticks` signal) and outputs it to the `pos-out` signal."
  [ticks pos-in vel acc pos-out]
  (go-loop
    (let [tick     (<! ticks)
          pos-next (next-pos (<! pos-in) (<! vel) (<! acc) tick)]
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
            :up   (max (- pos *paddle-step*) 0)
            :down (min (+ pos *paddle-step*) *max-paddle-y*)))))))

(defn in-y-range? [y paddle-y]
    (and (> y (- paddle-y *padding*)) (< y (+ paddle-y *paddle-size* *padding*))))

(defn detect-x-collision [x y lpaddle-y rpaddle-y]
  (cond
    (< x *ef-paddle-width*)
      (if (in-y-range? y lpaddle-y) :collision-left  :gameover)
    (> x (- *width* *ef-paddle-width*))
      (if (in-y-range? y rpaddle-y) :collision-right :gameover)
    :else :moving))

(defn detect-y-collision [y]
  (cond
    (< y *padding*)              :collision-left
    (> y (- *height* *padding*)) :collision-right
    :else                        :moving))

(defn collision? [state]
  (or (= state :collision-left) (= state :collision-right)))

(defn adjust-vel [state vel]
  (condp = state
    :collision-left  (abs vel)
    :collision-right (- (abs vel))
    :moving          vel
    :gameover        0))

(defn perturb [v] (* v (+ 1 (/ (- (rand) 0.5) (/ 0.5 *perturb-factor*)))))

(defn collision-detector [ticks pos vel-in acc pl-pos pr-pos game-state-in game-state vel-out]
  "Collision Detector component.
   Detects the collision of the ball with the walls and the paddles and accordingly calculates
   and outputs the next ball velocity and next game state to the `vel-out` and `game-state` 
   signals respectively.
   Reads the current tick, ball position, ball velocity, ball acceleration, left and right paddle 
   positions and game state from the `ticks`, `pos`, `vel-in`, `acc`, `pl-pos`, `pr-pos` 
   and `game-state` signals respectively."

  (go-loop
    (let [;; get all current values
          tick            (<! ticks)
          [vel-x vel-y]   (<! vel-in)
          [x y]           (<! pos)
          [gx gy]         (<! acc)
          lpaddle-y       (<! pl-pos)
          rpaddle-y       (<! pr-pos)
          [_ score]       (<! game-state-in)
          
          ;; calculate next position and detect collision
          [xn yn]         (next-pos [x y] [vel-x vel-y] [gx gy] tick)
          x-state         (detect-x-collision xn yn lpaddle-y rpaddle-y)
          y-state         (detect-y-collision yn)
          x-collision     (collision? x-state)
          y-collision     (collision? y-state)
          
          ;; calculate next velocity and game state
          vel-xn          (min *ball-speed* (+ (adjust-vel x-state vel-x) (* gx tick)))
          vel-yn          (min *ball-speed* (+ (adjust-vel y-state vel-y) (* gy tick)))
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
              state-text    (condp = state
                              :moving "Playing"
                              :collision "Playing"
                              :gameover "Game Over")]
          (doto ball-el
            (dom/set-attr! "cx" x)
            (dom/set-attr! "cy" y))
          (when-not (= fps fps-p)
            (dom/set-text! fps-el fps))
          (when-not (= state state-p)
            (dom/set-text! state-el state-text))
          (when-not (= score score-p)
            (dom/set-text! score-el score))
          (when (= state :gameover)
            (do (dom/set-text! state-el "press <space> to restart")
              (start-on-space)))
          (recur fps state-text score))))
  (go-loop
    (dom/set-attr! lpaddle-el "y" (<! pl-pos)))
  (go-loop
    (dom/set-attr! rpaddle-el "y" (<! pr-pos)))))

;; Everything is ready now. Layout the game and start it on pressing <space>!
(defn ^:export frpong []
  (layout-game)
  (start-on-space))
