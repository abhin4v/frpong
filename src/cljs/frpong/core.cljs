(ns frpong.core
  (:require [frpong.helpers :as h :refer [log]]
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

(defn positioner [tick-chan vel-chan pos-chan-in pos-chan-out]
  (go-loop
    (let [tick (<! tick-chan)
          [vel-x vel-y] (<! vel-chan)
          [x y] (<! pos-chan-in)
          pos-next [(+ x (* vel-x tick)) (+ y (* vel-y tick))]]
      (>! pos-chan-out pos-next))))

(defn collision-detector [width height padding tick-chan pos-chan vel-chan-in vel-chan-out]
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

(defn render-loop [pos-chan vel-chan]
  (go-loop
    (let [[x y] (map int (<! pos-chan))
          vel (<! vel-chan)]
      (dom/set-text! (dom/by-id "pos") [x y])
      (dom/set-text! (dom/by-id "vel") vel)
      (dom/set-attr! (dom/by-id "ball") "cx" x)
      (dom/set-attr! (dom/by-id "ball") "cy" y))))

(defn game-setup [width height padding frame-chan pos-chan vel-chan]
  (let [[tick-chan-pos tick-chan-collsion] (h/dup-chan (tick-chan (h/diff-chan frame-chan)))
        [pos-chan-pos pos-chan-render pos-chan-collision] (h/multiplex pos-chan 3)
        [vel-chan-pos vel-chan-collision vel-chan-render] (h/multiplex vel-chan 3)]
    (positioner tick-chan-pos vel-chan-pos pos-chan-pos pos-chan)
    (collision-detector width height padding
      tick-chan-collsion pos-chan-collision vel-chan-collision vel-chan)
    [pos-chan-render vel-chan-render]))

(defn game-init [width height padding init-pos init-vel frame-chan]
  (let [pos-chan (chan 1)
        vel-chan (chan 1)
        [pos-chan-render vel-chan-render] 
          (game-setup width height padding frame-chan pos-chan vel-chan)]
    (put! pos-chan init-pos)
    (put! vel-chan init-vel)

    (render-loop pos-chan-render vel-chan-render)))

(defn ^:export init []
  (let [frame-chan (h/frame-chan)
        [frame-chan-fps frame-chan-game frame-chan-count] (h/multiplex frame-chan 3)

        fps-chan (h/map-chan #(/ 1000 %) (h/diff-chan frame-chan-fps))
        frame-count-chan (h/counting-chan frame-chan-count)

        width 600
        height 400
        padding 5
        init-pos [5 100]
        init-vel [0.2 0.22]]
    (go-loop
      (dom/set-text! (dom/by-id "fps") (<! fps-chan))
      (dom/set-text! (dom/by-id "frame") (<! frame-count-chan)))
    (game-init width height padding init-pos init-vel frame-chan-game)))
