(ns frpong.helpers
  (:require [cljs.core.async :as async
             :refer [<! >! chan put! close! sliding-buffer dropping-buffer timeout]]
            [domina :as dom :refer [log]]
            [domina.events :as ev])
  (:require-macros [cljs.core.async.macros :as m :refer [go]]
                   [frpong.core :refer (go-loop)]))

(defn now []
  (.valueOf (js/Date.)))

(defn put-all! [cs x]
  (doseq [c cs]
    (put! c x)))

(defn cconj [v c1]
  (let [c2 (chan)]
    (go
      (>! c2 v)
      (while true
        (>! c2 (<! c1))))
    c2))

(defn multiplex [in cs-or-n]
  (let [cs (if (number? cs-or-n)
             (repeatedly cs-or-n chan)
             cs-or-n)]
    (go (loop []
          (let [x (<! in)]
            (if-not (nil? x)
              (do
                (put-all! cs x)
                (recur))
              :done))))
    cs))

(defn copy-chan
  ([c]
    (first (multiplex c 1)))
  ([out c]
    (first (multiplex c [out]))))

(defn dup-chan [c]
  (multiplex c 2))

(defn map-chan
  ([f source] (map-chan (chan) f source))
  ([c f source]
    (go-loop
      (>! c (f (<! source))))
    c))

(defn filter-chan
  ([f source] (filter-chan (chan) f source))
  ([c f source]
    (go-loop
      (let [v (<! source)]
        (when (f v)
          (>! c v))))
    c))

(defn interval-chan
  ([msecs]
    (interval-chan msecs :leading))
  ([msecs type]
    (interval-chan (chan (dropping-buffer 1)) msecs type))
  ([c msecs type]
    (condp = type
      :leading (go-loop
                 (>! c (now))
                 (<! (timeout msecs)))
      :falling (go-loop
                 (<! (timeout msecs))
                 (>! c (now))))
    c))

(defn throttle
  ([source control]
    (throttle (chan) source control))
  ([c source control]
    (go
      (loop [state ::init last nil]
        (let [[v sc] (alts! [source control])]
          (condp = sc
            source (condp = state
                     ::init (do (>! c v)
                              (recur ::throttling last))
                     ::throttling (recur state v))
            control (if last 
                      (do (>! c last)
                       (recur state nil))
                     (recur ::init last))))))
    c))

(defn debounce
  ([source msecs]
    (debounce (chan) source msecs))
  ([c source msecs]
    (go
      (loop [state ::init cs [source]]
        (let [[_ threshold] cs]
          (let [[v sc] (alts! cs)]
            (condp = sc
              source (condp = state
                       ::init
                         (do (>! c v)
                           (recur ::debouncing
                             (conj cs (timeout msecs))))
                       ::debouncing
                         (recur state
                           (conj (pop cs) (timeout msecs))))
              threshold (recur ::init (pop cs)))))))
    c))

(defn after-last
  ([source msecs]
    (after-last (chan) source msecs))
  ([c source msecs]
    (go
      (loop [cs [source]]
        (let [[_ toc] cs]
          (let [[v sc] (alts! cs :priority true)]
            (recur
              (condp = sc
                source (conj (if toc (pop cs) cs)
                         (timeout msecs))
                toc (do (>! c (now)) (pop cs))))))))
    c))

(defn fan-in
  ([ins] (fan-in (chan) ins))
  ([c ins]
    (go (while true
          (let [[x] (alts! ins)]
            (>! c x))))
    c))

(defn distinct-chan
  ([source] (distinct-chan (chan) source))
  ([c source]
    (go
      (loop [last ::init]
        (let [v (<! source)]
          (when-not (= last v)
            (>! c v))
          (recur v))))
    c))

(defn event-chan [event-type]
  (let [c (chan)]
    (ev/listen! event-type #(put! c %))
    c))

(defn key-chan [keycodes]
  (let [source (event-chan :keydown)
        c (chan)]
    (go-loop
      (let [kc (:keyCode (<! source))]
        (when (contains? keycodes kc)
          (>! c (keycodes kc)))))
    c))

(defn frame-chan []
  (let [c (chan (sliding-buffer 1000))
        step (fn step [ts]  (do (put! c ts) (.requestAnimationFrame js/window step)))]
    (.requestAnimationFrame js/window step)
    c))

(defn counting-chan [source]
  (let [c (chan)]
    (go
      (loop [count 0]
        (<! source)
        (>! c count)
        (recur (inc count))))
    c))

(defn diff-chan [source]
  (let [c (chan)]
    (go
      (let [start (<! source)]
        (loop [start start]
          (let [ts (<! source)]
            (>! c (- ts start))
            (recur ts)))))
    c))

(defn dropping-chan [source n]
  (let [c (chan)]
    (go
      (loop [count 0]
        (if (= count 0)
          (>! c (<! source))
          (<! source))
        (recur (rem (inc count) n))))
    c))
