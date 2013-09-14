(defproject frpong "0.1.0-SNAPSHOT"
  :description "FRP Pong in ClojureScript using core.async"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [domina "1.0.2-SNAPSHOT"]
                 [org.clojure/clojurescript "0.0-1878"]
                 [org.clojure/core.async "0.1.222.0-83d0c2-alpha"]]

  :plugins [[lein-cljsbuild "0.3.3"]]

  :cljsbuild {:builds
              [{:source-paths ["src/cljs"]
                :compiler {:output-to "resources/public/js/frpong.js"
                           :optimizations :whitespace
                           :pretty-print true}}]})
