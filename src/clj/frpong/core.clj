(ns frpong.core)

(defmacro go-loop [& body]
  `(cljs.core.async.macros/go
     (while true
       ~@body)))

(defmacro rd [& body] `(cljs.core.async/<! ~@body))

(defmacro wt [& body] `(cljs.core.async/>! ~@body))
