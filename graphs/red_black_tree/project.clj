(defproject red_black_tree "0.1.0-SNAPSHOT"
  :description "Implement a red black tree."
  :url "http://lettier.com/"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :main ^:skip-aot red-black-tree.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
