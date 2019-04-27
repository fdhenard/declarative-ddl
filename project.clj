(defproject declarative-ddl "0.1.1-SNAPSHOT"
  :description "Database schema declarations with Clojure, plus utilities around that"
  :url "https://github.com/fdhenard/declarative-ddl"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [diff-as-list "2.2.6"]
                 [clojure.java-time "0.3.2"]
                 [conman "0.8.3"]
                 [org.clojure/clojurescript "1.10.520"]
                 [funcool/decimal "1.0.2"]]
  :repl-options {:init-ns declarative-ddl.core}
  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-doo "0.1.10"]
            [lein-figwheel "0.5.18"]]
  :cljsbuild {:builds {;; :main {:source-paths ["src/cljs" "src/cljc"]}
                       :test {:source-paths ["src/cljs" "src/cljc" "test/cljs"]
                              :compiler {:output-to "target/js/test/test.js"
                                         :output-dir "target/js/test/"
                                         :target :nodejs
                                         :main declarative-ddl.runner
                                         :optimizations :none
                                         }}
                       :repl {:source-paths ["src/cljs" "src/cljc"]
                              :figwheel true
                              :compiler {:main "declarative-ddl.core"
                                         :output-to "target/js/repl/main.js"
                                         :output-dir "target/js/repl/"}}}}
  :test-paths ["test/clj"])
