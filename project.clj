(defproject inferno "0.1.0-SNAPSHOT"
  :description "A text-based adventure game built using Clojure."
  :url "https://github.com/yiyanglin98/inferno"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :repl-options {:init-ns inferno.core}
  :main ^:skip-aot inferno.core/main
  )
