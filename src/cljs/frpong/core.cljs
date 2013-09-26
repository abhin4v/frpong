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

(defn tick-chan [frame-chan]
  (let [c (chan)]
    (go
      (loop [prev (<! frame-chan)]
        (let [t (<! frame-chan)]
          (when (< t (* 10 prev))
            (>! c t))
            (recur t))))
    c))

(defn ball-positioner [tick-chan vel-chan pos-chan-in pos-chan-out]
  (go-loop
    (let [tick (<! tick-chan)
          [vel-x vel-y] (<! vel-chan)
          [x y] (<! pos-chan-in)
          pos-next [(+ x (* vel-x tick)) (+ y (* vel-y tick))]]
      (>! pos-chan-out pos-next))))

(defn collision-detector 
  [{:keys [width height padding]} tick-chan pos-chan vel-chan-in vel-chan-out]
  (go-loop
    (let [adjust-v (fn [p v size]
                      (cond
                        (< p padding) (abs v)
                        (> p (- size padding)) (- (abs v))
                        :else v))
          tick (<! tick-chan)
          [vel-x vel-y] (<! vel-chan-in)
          [x y] (<! pos-chan)
          [xn yn] [(+ x (* vel-x tick)) (+ y (* vel-y tick))]
          vel-xn (adjust-v xn vel-x width)
          vel-yn (adjust-v yn vel-y height)]
      (>! vel-chan-out [vel-xn vel-yn]))))

(defn paddle-positioner [keycodes max-y movement pos-chan-in pos-chan-out]
  (let [key-chan (h/key-chan keycodes)]
    (go-loop
      (let [dir (<! key-chan)
            pos (<! pos-chan-in)]
        (condp = dir
          :up (>! pos-chan-out (max (- pos movement) 0))
          :down (>! pos-chan-out (min (+ pos movement) max-y)))))))

(defn renderer [pos-chan vel-chan pl-pos-chan pr-pos-chan]
  (go-loop
    (let [[x y] (map int (<! pos-chan))
          vel (<! vel-chan)]
      (dom/set-text! (dom/by-id "pos") [x y])
      (dom/set-text! (dom/by-id "vel") vel)
      (doto (dom/by-id "ball")
        (dom/set-attr! "cx" x)
        (dom/set-attr! "cy" y))))
  (go-loop
    (dom/set-attr! (dom/by-id "lpaddle") "y" (<! pl-pos-chan)))
  (go-loop
    (dom/set-attr! (dom/by-id "rpaddle") "y" (<! pr-pos-chan))))

(defn game-setup [{:keys [width height padding paddle-size] :as layout} paddle-movement
                  frame-chan pos-chan vel-chan pl-pos-chan pr-pos-chan]
  (let [max-y (- height paddle-size)
        [tick-chan-pos tick-chan-collsion] (h/dup-chan (tick-chan (h/diff-chan frame-chan)))
        [pos-chan-pos pos-chan-collision pos-chan-render] (h/multiplex pos-chan 3)
        [vel-chan-pos vel-chan-collision vel-chan-render] (h/multiplex vel-chan 3)
        [pl-pos-chan-pos pl-pos-chan-render] (h/dup-chan pl-pos-chan)
        [pr-pos-chan-pos pr-pos-chan-render] (h/dup-chan pr-pos-chan)]
    (ball-positioner tick-chan-pos vel-chan-pos pos-chan-pos pos-chan)
    (collision-detector layout tick-chan-collsion pos-chan-collision vel-chan-collision vel-chan)
    (paddle-positioner {83 :down 87 :up} max-y paddle-movement pl-pos-chan-pos pl-pos-chan)
    (paddle-positioner {38 :up 40 :down} max-y paddle-movement pr-pos-chan-pos pr-pos-chan)
    [pos-chan-render vel-chan-render pl-pos-chan-render pr-pos-chan-render]))

(defn game-init [{:keys [height paddle-size] :as layout}
                 {:keys [init-pos init-vel paddle-movement]} frame-chan]
  (let [paddle-x (/ (- height paddle-size) 2)
        pos-chan (chan 1)
        vel-chan (chan 1)
        pl-pos-chan (chan 1)
        pr-pos-chan (chan 1)]
    (put! pos-chan init-pos)
    (put! vel-chan init-vel)
    (put! pl-pos-chan paddle-x)
    (put! pr-pos-chan paddle-x)

    (apply renderer
      (game-setup layout paddle-movement frame-chan pos-chan vel-chan pl-pos-chan pr-pos-chan))))

(defn ^:export init []
  (let [frame-chan (h/frame-chan)
        [frame-chan-fps frame-chan-count frame-chan-game] (h/multiplex frame-chan 3)

        fps-chan (h/map-chan #(/ 1000 %) (h/diff-chan frame-chan-fps))
        frame-count-chan (h/counting-chan frame-chan-count)

        layout {:width 600
                :height 300
                :padding 5
                :paddle-size 80}
        init-vals {:init-pos [5 100]
                   :init-vel [0.2 0.22]
                   :paddle-movement 10}]
    (doto (dom/by-id "canvas")
      (dom/set-style! "width" (str (:width layout) "px"))
      (dom/set-style! "height" (str (:height layout) "px")))
    
    (go-loop
      (dom/set-text! (dom/by-id "fps") (<! fps-chan))
      (dom/set-text! (dom/by-id "frame") (<! frame-count-chan)))
    (game-init layout init-vals frame-chan-game)))
