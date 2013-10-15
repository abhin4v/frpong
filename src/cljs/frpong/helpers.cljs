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

(defn probe [ch probe-name]
  (let [c (chan)]
    (go (loop []
          (if-let [v (<! ch)]
            (do (log (str (now) " " probe-name ": " v))
              (>! c v)
              (recur))
            (close! c))))
    c))

(defn map-chan [f source]
  (let [c (chan)]
    (go (loop []
          (if-let [v (<! source)]
            (do (>! c (f v)) (recur))
            (close! c))))
    c))

(defn filter-chan [f source]
  (let [c (chan)]
    (go (loop []
          (if-let [v (<! source)]
            (do (when (f v) (>! c v)) (recur))
            (close! c))))
    c))

(defn interval-chan
  ([msecs]
    (interval-chan msecs :leading))
  ([msecs type]
    (let [c (chan (dropping-buffer 1))]
      (condp = type
        :leading (go-loop
                   (>! c (now))
                   (<! (timeout msecs)))
        :falling (go-loop
                   (<! (timeout msecs))
                   (>! c (now))))
      c)))

(defn throttle [source control]
  (let [c (chan)]
    (go
      (loop [state ::init last nil]
        (let [[v sc] (alts! [source control])]
          (condp = sc
            source (condp = state
                     ::init (do (>! c v) (recur ::throttling last))
                     ::throttling (recur state v))
            control (if last 
                      (do (>! c last) (recur state nil))
                     (recur ::init last))))))
    c))

(defn sustain [source control]
  (let [c (chan)]
    (go
      (loop [last nil]
        (let [[v ch] (alts! [source control] :priority true)]
          (if (nil? v)
            (close! c)
            (condp = ch
              source (do (>! c v) (recur v))
              control (do (when last (>! c last)) (recur last)))))))
    c))

(defn debounce [source msecs]
  (let [c (chan)]
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

(defn after-last [source msecs]
  (let [c (chan)]
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

(defn fan-in [ins]
  (let [c (chan)]
    (go-loop
      (let [[x] (alts! ins)]
        (>! c x)))
    c))

(defn distinct-chan [source]
  (let [c (chan)]
    (go
      (loop [last ::init]
        (let [v (<! source)]
          (when-not (= last v) (>! c v))
          (recur v))))
    c))

(defn event-chan
  ([event-type]
    (let [c      (chan)
          [lkey] (ev/listen! event-type #(put! c %))]
      [c #(do (ev/unlisten-by-key! lkey) (close! c))]))
  ([node event-type]
    (let [c (chan)
          [lkey] (ev/listen! node event-type #(put! c %))]
      [c #(do (ev/unlisten-by-key! lkey) (close! c))])))

(defn key-chan [keydowns keyups sampler keycodes]
  (let [c   (chan)
        ops { keydowns conj
              keyups   disj }]
    (go (loop [keys #{}]
      (let [[v ch] (alts! [keydowns keyups sampler] :priority true)]
        (if-not (nil? v)
          (if (or (= ch keydowns) (= ch keyups))
            (let [k (:keyCode v)]
              (if (contains? keycodes k)
                (recur ((ops ch) keys (keycodes k)))
                (recur keys)))
            (do (>! c keys) (recur keys)))
          (close! c)))))
    c))

(defn frame-chan []
  (let [fc (chan (sliding-buffer 1000))
        rc (chan (sliding-buffer 10))
        step (fn step [ts] 
               (let [req-id (.requestAnimationFrame js/window step)]
                 (put! fc ts)
                 (put! rc req-id)))
        stop-fn (fn []
                  (go (loop []
                        (if-let [id (<! rc)]
                          (do (.cancelAnimationFrame js/window id) (recur)))))
                  (close! fc)
                  (close! rc))]
    (.requestAnimationFrame js/window step)
    [fc stop-fn]))

(defn counting-chan [source]
  (let [c (chan)]
    (go
      (loop [count 0]
        (if-let [v (<! source)]
          (do (>! c count) (recur (inc count)))
          (close! c))))
    c))

(defn diff-chan [source]
  (let [c (chan)]
    (go
      (let [start (<! source)]
        (loop [start start]
          (if-let [v (<! source)]
            (do (>! c (- v start)) (recur v))
            (close! c)))))
    c))

(defn dropping-chan [source n]
  (let [c (chan)]
    (go
      (loop [count 0]
        (if-let [v (<! source)]
          (do (when (= count 0) (>! c v))
            (recur (rem (inc count) n)))
          (close! c))))
    c))

(defprotocol Mux
  (muxch* [_]))

(defprotocol Mult
  (tap* [m ch close?])
  (untap* [m ch])
  (untap-all* [m]))

(defn mult
  "Creates and returns a mult(iple) of the supplied channel. Channels
  containing copies of the channel can be created with 'tap', and
  detached with 'untap'.

  Each item is distributed to all taps in parallel and synchronously,
  i.e. each tap must accept before the next item is distributed. Use
  buffering/windowing to prevent slow taps from holding up the mult.

  Items received when there are no taps get dropped.

  If a tap put throws an exception, it will be removed from the mult."
  [ch]
  (let [cs (atom {}) ;;ch->close?
        m (reify
            Mux
            (muxch* [_] ch)

            Mult
            (tap* [_ ch close?] (swap! cs assoc ch close?) nil)
            (untap* [_ ch] (swap! cs dissoc ch) nil)
            (untap-all* [_] (reset! cs {}) nil))
        dchan (chan 1)
        dctr (atom nil)
        done #(when (zero? (swap! dctr dec))
                (put! dchan true))]
    (go (loop []
      (let [val (<! ch)]
        (if (nil? val)
          (doseq [[c close?] @cs]
            (when close? (close! c)))
          (let [chs (keys @cs)]
            (reset! dctr (count chs))
            (doseq [c chs]
              (put! c val done))
            ;;wait for all
            (when (seq chs)
              (<! dchan))
            (recur))))))
    m))

(defn tap
  "Copies the mult source onto the supplied channel.

  By default the channel will be closed when the source closes,
  but can be determined by the close? parameter."
  ([mult] (tap mult (chan)))
  ([mult ch] (tap mult ch true))
  ([mult ch close?] (tap* mult ch close?) ch))

(defn untap
  "Disconnects a target channel from a mult"
  [mult ch]
  (untap* mult ch))

(defn untap-all
  "Disconnects all target channels from a mult"
  [mult] (untap-all* mult))