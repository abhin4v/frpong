(ns frpong.core
  (:require [cljs.core.async :as async
             :refer [<! >! chan put!]]
            [domina :as dom]
            [domina.events :as ev])
  (:require-macros [cljs.core.async.macros :as m :refer [go]]))


(defn event-chan [event-type]
  (let [c (chan)]
    (ev/listen! js/document event-type
      (fn [e] (put! c e)))
    c))

(defn ^:export init []
  (let [mm-chan (event-chan :mousemove)]
    (go
      (while true
        (let [e (<! mm-chan)]
          (.log js/console (str (:clientX e) " , " (:clientY e))))))))