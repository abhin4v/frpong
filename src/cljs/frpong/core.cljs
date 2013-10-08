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
  [{:keys [width height padding]} 
   ticks ball-state pos pl-pos pr-pos vel-in vel-out]
  (let [adjust-v (fn [p v size]
                      (cond
                        (< p padding) [(abs v) :collision]
                        (> p (- size padding)) [(- (abs v)) :collision]
                        :else [v :moving]))]
    (go-loop
      (let [tick (<! ticks)
            [vel-x vel-y] (<! vel-in)
            [x y] (<! pos)
            pl-pos (<! pl-pos)
            pr-pos (<! pr-pos)
            [xn yn] [(+ x (* vel-x tick)) (+ y (* vel-y tick))]
            [vel-xn bs-x] (adjust-v xn vel-x width)
            [vel-yn bs-y] (adjust-v yn vel-y height)]
        (>! vel-out [vel-xn vel-yn])
        (>! ball-state
          (if (or (= bs-x :collision) (= bs-y :collision)) :collision :moving))))))

(defn paddle-positioner [keycodes max-y movement pos-in pos-out]
  (let [keys (h/key-chan keycodes)]
    (go-loop
      (let [dir (<! keys)
            pos (<! pos-in)]
        (>! pos-out
          (condp = dir
            :up (max (- pos movement) 0)
            :down (min (+ pos movement) max-y)))))))

(defn renderer [ball-state pos vel pl-pos pr-pos]
  (let [pos-el (dom/by-id "pos")
        vel-el (dom/by-id "vel")
        ball-el (dom/by-id "ball")
        lpaddle-el (dom/by-id "lpaddle")
        rpaddle-el (dom/by-id "rpaddle")]
  (go-loop
    (let [[x y] (<! pos)]
      (dom/set-text! pos-el (map int [x y]))
      (dom/set-text! vel-el (<! vel))
      (doto ball-el
        (dom/set-attr! "cx" x)
        (dom/set-attr! "cy" y)
        (dom/set-style! "fill" (if (= :collision (<! ball-state)) "#0f0" "#000")))))
  (go-loop
    (dom/set-attr! lpaddle-el "y" (<! pl-pos)))
  (go-loop
    (dom/set-attr! rpaddle-el "y" (<! pr-pos)))))

(defn game-setup [{:keys [width height padding paddle-size] :as layout} paddle-movement
                  frames ball-state pos vel pl-pos pr-pos]
  (let [max-y (- height paddle-size)
        [tick-pos tick-collsion tick-pl tick-pr] (h/multiplex (tick-chan (h/diff-chan frames)) 4)
        [pos-pos pos-collision pos-render] (h/multiplex pos 3)
        [vel-pos vel-collision vel-render] (h/multiplex vel 3)
        [pl-pos-pos pl-pos-collision pl-pos-render] (h/multiplex pl-pos 3)
        [pr-pos-pos pr-pos-collision pr-pos-render] (h/multiplex pr-pos 3)]
    (ball-positioner tick-pos vel-pos pos-pos pos)
    (collision-detector layout tick-collsion ball-state pos-collision
      (h/sustain pl-pos-collision tick-pl)
      (h/sustain pr-pos-collision tick-pr)
      vel-collision vel)
    (paddle-positioner {83 :down 87 :up} max-y paddle-movement pl-pos-pos pl-pos)
    (paddle-positioner {38 :up 40 :down} max-y paddle-movement pr-pos-pos pr-pos)
    [ball-state pos-render vel-render pl-pos-render pr-pos-render]))

(defn game-init [{:keys [height paddle-size] :as layout}
                 {:keys [init-pos init-vel paddle-movement]} frames]
  (let [init-paddle-pos (/ (- height paddle-size) 2)
        pos (chan 1)
        vel (chan 1)
        pl-pos (chan 1)
        pr-pos (chan 1)
        ball-state (chan 1)]
    (put! pos init-pos)
    (put! vel init-vel)
    (put! pl-pos init-paddle-pos)
    (put! pr-pos init-paddle-pos)

    (apply renderer
      (game-setup layout paddle-movement frames ball-state pos vel pl-pos pr-pos))))

(defn ^:export init []
  (let [frames (h/map-chan first (h/frame-chan))
        [frames-fps frames-count frames-game] (h/multiplex frames 3)

        fps (h/map-chan #(/ 1000 %) (h/diff-chan frames-fps))
        frames-count (h/counting-chan frames-count)

        layout {:width 600
                :height 300
                :padding 5
                :paddle-size 80}
        init-vals {:init-pos [5 100]
                   :init-vel [0.2 0.22]
                   :paddle-movement 10}

        fps-el (dom/by-id "fps")
        frame-el (dom/by-id "frame")]
    (doto (dom/by-id "canvas")
      (dom/set-style! "width" (str (:width layout) "px"))
      (dom/set-style! "height" (str (:height layout) "px")))
    
    (go-loop
      (dom/set-text! fps-el (<! fps))
      (dom/set-text! frame-el (<! frames-count)))
    (game-init layout init-vals frames-game)))
