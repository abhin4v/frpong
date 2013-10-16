(ns frpong.core
  (:require [frpong.signals :refer (signal keyboard ticks dom-events mult tap)]
            [domina :as dom :refer [log]]
            [domina.events :as ev])
  (:require-macros [cljs.core.async.macros :as m :refer [go]]
                   [frpong.core :refer (go-loop rd wt)]))
;;
;;                                       Signal Diagram
;;
;;                                                +-d-----------+--------------------------+
;;                                                v             |                          |
;;           keyboard +-e-> sampler +---k---> paddle-postnr +-d-+   +-> gravitation +--+   |
;;                            ^                                 |   p                  a   |
;;                            t                                 |   |                  |   |
;;                            |                                 |   |                  |   |
;;                            +-------+          +-p----------+-|---+                  |   d
;;                                    |          |  +-a-------|-|------+---------------+   |
;;                                    |          v  v         | |      |                   v
;;            browser +-b--> ticker +-+--t--> ball-postnr +-p-+-|------|------------p-> renderer
;;                              ^     |             ^         | |      |                  ^  ^
;;  Signals                     |     |       +-----|---------+ |      |                  |  |
;;  -------                     s     |       |     l     +--d--+      |                  s  t
;;  b: browser ticks            |     |       |   +-+-----|-------+    |                  |  |
;;  t: game ticks               |     |       p   l   +-a-|-------|----+                  |  |
;;  e: keyboard events          |     |       v   v   v   v  +-l--+                       |  |
;;  k: keydowns (at rate of t)  |     +---t-> collision-detr                              |  |
;;  p: ball position            |     |             ^        +-s--+-----------------------+  |
;;  l: ball velocity            |     |             s             |                          |
;;  a: ball acceleration        +-----|-------------+-------------+                          |
;;  d: paddle positions               |                                                      |
;;  s: game state                     +------------------------------------------------------+
;;
;;  All signals except the signal e are at the rate of the signal f. The signal e is at the rate 
;;  at which the keyboard issues events.

(defn abs [x] (.abs js/Math x))
(defn sqrt [x] (.sqrt js/Math x))
(defn sq [x] (* x x))

(def PI 3.141592653589793)

(defn deg->rad [deg] (* (/ deg 180) PI))

(defn cos [x] (.cos js/Math x))
(defn sin [x] (.sin js/Math x))

;; Global settings
(def *width*            (- (.-scrollWidth (.-body js/document)) 20))
(def *height*           (- (.-scrollHeight (.-body js/document)) 130))
(def *center*           [(/ *width* 2 ) (/ *height* 2)])
(def *padding*          5)
(def *paddle-size*      100)

(def *ball-radius*      8)
(def *ball-speed*       0.6)
(def *init-vel-deg-lim* [35 55])
(def *perturb-factor*   0.05)

(def *init-mass-radius* 0)

(def *paddle-width*     10)
(def *paddle-step*      8)
(def *max-paddle-y*     (- *height* *paddle-size*))
(def *ef-paddle-width*  (+ *paddle-width* *padding*))
(def *init-paddle-pos*  (/ (- *height* *paddle-size*) 2))

(def *gravity*          (atom 0.005))
(def *gravity-step*     0.005)

(defn mass-radius []
  (+ *init-mass-radius* (* (deref *gravity*) 1000)))

(defn setup-gravity-controls
  "Sets up keyboard controls for changing gravity."
  []
  (let [keydowns   (first (dom-events :keydown))
        actions    { 37 #(- % *gravity-step*) 39 #(+ % *gravity-step*) }
        mass-el    (dom/by-id "mass")]
    (go-loop
      (let [k (:keyCode (rd keydowns))]
        (when (contains? actions k)
          (do (swap! *gravity* #(max 0 (min 0.1 ((actions k) %))))
            (dom/set-attr! mass-el "r" (mass-radius))))))))

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
  (doto (dom/by-id "mass")
    (dom/set-attr! "r" (mass-radius))
    (dom/set-attr! "cx" (first *center*))
    (dom/set-attr! "cy" (second *center*)))
  (doto (dom/by-id "score")
    (dom/set-attr! "x" (first *center*))
    (dom/set-attr! "y" (- *height* 50)))
  (doseq [id ["lpaddle" "rpaddle"]]
    (doto (dom/by-id id)
      (dom/set-attr! "width" *paddle-width*)
      (dom/set-attr! "height" *paddle-size*)
      (dom/set-attr! "y" (/ (- *height* *paddle-size*) 2))))
  (dom/set-attr! (dom/by-id "lpaddle") "x" 0)
  (dom/set-attr! (dom/by-id "rpaddle") "x" (- *width* *paddle-width*)))

(defn initial-velocity
  "Calculates a random initial ball velocity, randomly in any four quadrants, between
   the limits of degrees specified by *init-vel-deg-lim*."
  []
  (let [[l h] *init-vel-deg-lim*
        sgn   #(if (< % 0.5) -1 1)
        deg   (+ l (* (- h l) (rand)))
        rad   (deg->rad deg)]
    (map #(* *ball-speed* %)
      [(* (sgn (rand)) (sin rad)) (* (sgn (rand)) (cos rad))])))

(defn start-game
  "Sets up the game by creating the signals and setting up the components and starts the game."
  []
  (let [br-ticks   (ticks)               ;; ticks signal from the browser
        pos        (signal)              ;; ball position signal
        vel        (signal)              ;; ball velocity signal
        acc        (signal)              ;; ball acceleration signal
        pd-pos     (signal)              ;; paddles position signal
        game-state (signal)              ;; game state signal, the state of the game and the current score
        init-vel   (initial-velocity)]
    (setup-components br-ticks game-state pos vel acc pd-pos)

    ;; start the game by setting the initial values of the signals
    (go
      (wt pos *center*)
      (wt vel init-vel)
      (wt pd-pos [*init-paddle-pos* *init-paddle-pos*])
      (wt game-state [:moving 0]))))

(defn start-on-space []
  (ev/listen-once! :keypress #(if (= (:keyCode %) 32) (start-game) (start-on-space))))

(defn setup-components
  "Creates mult(iple)s of the signals and sets up the components by connecting them using
   the signals tapped from the mults.
   The signals and their stop functions are taken as parameters."
  [[br-ticks stop-ticks] game-state pos vel acc pd-pos]
  (let [ticks                    (signal)           ;; game ticks signal
        ticks-m                  (mult ticks)       ;; mult(iple)s for all signals
        pos-m                    (mult pos)
        vel-m                    (mult vel)
        acc-m                    (mult acc)
        pd-pos-m                 (mult pd-pos)
        game-state-m             (mult game-state)

        ;; keyboard signal for w, s, up and down keys
        [keyboard stop-keyboard] (keyboard (tap ticks-m) {83 :s 87 :w 38 :up 40 :down})

        ;; calling this will stop the ticks and the keyboard signals and hence stop the game
        stop-game                 #(do (stop-ticks) (stop-keyboard))]
    ;; set up the components by tapping into mults
    (ticker br-ticks stop-game (tap game-state-m) ticks)

    (gravitation (tap pos-m) acc)
    (ball-positioner (tap ticks-m) (tap pos-m) (tap vel-m) (tap acc-m) pos)
    (paddle-positioner keyboard (tap pd-pos-m) pd-pos)

    (collision-detector (tap ticks-m) (tap pos-m) (tap vel-m) (tap acc-m)
      (tap pd-pos-m) (tap game-state-m) game-state vel)

    (renderer (tap ticks-m) (tap game-state-m) (tap pos-m) (tap pd-pos-m))))

(defn ticker
  "Ticker component.
   Reads ticks generated by the browser from the `br-ticks` signal and outputs them to the 
   `game-ticks` signal as long as the `game-state` signal is not :gameover.
   Once the `game-state` signal is :gameover, stops the game by calling the `stop-game` function.
   Each tick is the number of milliseconds since the last tick was generated."
  [br-ticks stop-game game-state game-ticks]
  (go (loop []
        (let [[state _] (rd game-state)]
          (do (wt game-ticks (rd br-ticks))
            (if-not (= :gameover state)
              (recur)
              (stop-game)))))))

(defn gravity-acc
  "Calculates acceleration due to gravitation for the ball caused by the mass placed at the
   center of the board."
  [[x y]]
  (let [grav     (deref *gravity*)
        [cx cy]  *center*
        x-dist   (- cx x)
        y-dist   (- cy y)
        distance (sqrt (+ (sq x-dist) (sq y-dist)))
        bearing  [(/ x-dist distance) (/ y-dist distance)]]
    (if-not (= distance 0)
      (map #(* grav % (/ 1 distance)) bearing)
      [0 0])))

(defn gravitation
  "Gravitation component.
   Calculates acceleration due to gravitation using the pos from the `pos-in` signal and outputs
   it to the `acc` signal."
  [pos-in acc]
  (go-loop
    (wt acc (gravity-acc (rd pos-in)))))

(defn next-pos [[x y] [vel-x vel-y] [acc-x acc-y] tick]
  [(+ x (* vel-x tick) (* acc-x (sq tick))) (+ y (* vel-y tick) (* acc-y (sq tick)))])

(defn ball-positioner
  "Ball Positioner component.
   Calculates the next ball position using the current ball position, velocity and acceleration
   (from the `pos-in`, `vel` and `acc` signals respectively) and the current tick (from the
   `ticks` signal) and outputs it to the `pos-out` signal."
  [ticks pos-in vel acc pos-out]
  (go-loop
    (let [tick     (rd ticks)
          pos-next (next-pos (rd pos-in) (rd vel) (rd acc) tick)]
      (wt pos-out pos-next))))

(defn paddle-positioner
  "Paddle Positioner component.
   Captures the keys signal for the provides keycodes and calculates the next paddle
   positions using the current paddle positions (from the `pos-in` signal) and keys signal
   and outputs it to the `pos-out` signal."
  [keys pos-in pos-out]
  (go-loop
    (let [[lpos rpos] (rd pos-in)
          ks          (rd keys)
          move        (fn [pos up down]
                        (cond
                          (contains? ks up)   (max (- pos *paddle-step*) 0)
                          (contains? ks down) (min (+ pos *paddle-step*) *max-paddle-y*)
                          :else                pos))]
      (wt pos-out [(move lpos :w :s) (move rpos :up :down)]))))

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

(defn perturb [v] (* v (+ 1 (* (rand) *perturb-factor*))))

(defn collision-detector [ticks pos vel-in acc pd-pos game-state-in game-state vel-out]
  "Collision Detector component.
   Detects the collision of the ball with the walls and the paddles and accordingly calculates
   and outputs the next ball velocity and next game state to the `vel-out` and `game-state`
   signals respectively.
   Reads the current tick, ball position, ball velocity, ball acceleration, left and right paddle
   positions and game state from the `ticks`, `pos`, `vel-in`, `acc`, `pd-pos` and `game-state`
   signals respectively."

  (go-loop
    (let [;; read all current values
          tick                  (rd ticks)
          [vel-x vel-y]         (rd vel-in)
          [x y]                 (rd pos)
          [gx gy]               (rd acc)
          [lpaddle-y rpaddle-y] (rd pd-pos)
          [_ score]             (rd game-state-in)

          ;; calculate next position and detect collision
          [xn yn]               (next-pos [x y] [vel-x vel-y] [gx gy] tick)
          x-state               (detect-x-collision xn yn lpaddle-y rpaddle-y)
          y-state               (detect-y-collision yn)
          x-collision           (collision? x-state)
          y-collision           (collision? y-state)

          ;; calculate next velocity and game state
          vel-xn                (min *ball-speed* (+ (adjust-vel x-state vel-x) (* gx tick)))
          vel-yn                (min *ball-speed* (+ (adjust-vel y-state vel-y) (* gy tick)))
          state-n               (cond
                                  (= x-state :gameover)        :gameover
                                  (or x-collision y-collision) :collision
                                  :else                        :moving)
          score-n               (if x-collision (inc score) score)

          ;; add a small random perturbation to the ball velocity on collision with paddles
          [vel-xn vel-yn]       (if x-collision
                                  (map perturb [vel-xn vel-yn])
                                  [vel-xn vel-yn])]
      (wt vel-out [vel-xn vel-yn])
      (wt game-state [state-n score-n]))))

(defn renderer
  "Renderer component.
   Renders the ball and paddle positions on the browser. Also shows the game state and stats.
   Reads the current values from the signals supplied as parameters."
  [ticks game-state pos pd-pos]
  (let [ball-el    (dom/by-id "ball")
        score-el   (dom/by-id "score")
        lpaddle-el (dom/by-id "lpaddle")
        rpaddle-el (dom/by-id "rpaddle")
        fps-el     (dom/by-id "fps")
        msg-el     (dom/by-id "msg")]
    (dom/set-style! ball-el "fill" "orange")
    (dom/set-text! msg-el "")
    (go (loop [fps-p nil score-p nil]
          (let [fps                   (int (/ 1000 (rd ticks)))
                [x y]                 (rd pos)
                [lpaddle-y rpaddle-y] (rd pd-pos)
                [state score]         (rd game-state)]
            (doto ball-el
              (dom/set-attr! "cx" x)
              (dom/set-attr! "cy" y))
            (dom/set-attr! lpaddle-el "y" lpaddle-y)
            (dom/set-attr! rpaddle-el "y" rpaddle-y)
            (when-not (= fps fps-p)
              (dom/set-text! fps-el fps))
            (when-not (= score score-p)
              (dom/set-text! score-el score))
            (when (= state :gameover)
              (do (dom/set-style! ball-el "fill" "red")
                (dom/set-html! msg-el "<span class='imp'>GAME OVER</span><br>press &lt;space&gt; to restart")
                (start-on-space)))
            (recur fps score))))))

;; Everything is ready now. Layout the game and start it on pressing <space>!
(defn ^:export frpong []
  (setup-gravity-controls)
  (layout-game)
  (start-on-space))
