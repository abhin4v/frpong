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

(defn positioner [tick-chan vel-chan pos-chan-in pos-chan-out]
  (go-loop
    (let [tick (<! tick-chan)
          [vel-x vel-y] (<! vel-chan)
          [x y] (<! pos-chan-in)
          pos-next [(+ x (* vel-x tick)) (+ y (* vel-y tick))]]
      (>! pos-chan-out pos-next))))

(defn collision-detector [width height padding tick-chan pos-chan vel-chan-in vel-chan-out]
  (go-loop
    (let [tick (<! tick-chan)
          [vel-x vel-y] (<! vel-chan-in)
          [x y] (<! pos-chan)
          [xn yn] [(+ x (* vel-x tick)) (+ y (* vel-y tick))]
          vel-xn (if (and (> xn padding) (< xn (- width padding))) vel-x (- vel-x))
          vel-yn (if (and (> yn padding) (< yn (- height padding))) vel-y (- vel-y))]
      (>! vel-chan-out [vel-xn vel-yn]))))

(defn ^:export init []
  (let [frame-chan (h/frame-chan)
        [frame-chan1 frame-chan2] (h/dup-chan frame-chan)
        
        fps-chan (h/map-chan #(/ 1000 %) (h/diff-chan frame-chan2))
        
        width 300
        height 200
        padding 5
        init-pos [5 100]
        init-vel [0.1 0.12]
        
        [tick-chan-pos tick-chan-collsion] (h/dup-chan (h/diff-chan frame-chan1))
        
        pos-chan (chan)
        [pos-chan-pos pos-chan-render pos-chan-collision] (h/multiplex pos-chan 3)

        vel-chan (chan)
        [vel-chan-pos vel-chan-collision] (h/dup-chan vel-chan)]
    (positioner tick-chan-pos vel-chan-pos pos-chan-pos pos-chan)
    (collision-detector width height padding 
      tick-chan-collsion pos-chan-collision vel-chan-collision vel-chan)
    
    (go (>! pos-chan init-pos))
    (go (>! vel-chan init-vel))
    
    (go-loop
      (let [[x y] (map int (<! pos-chan-render))]
        (dom/set-text! (dom/by-id "fps") (<! fps-chan))
        (dom/set-text! (dom/by-id "pos") [x y])
        (dom/set-attr! (dom/by-id "ball") "cx" x)
        (dom/set-attr! (dom/by-id "ball") "cy" y)))))
