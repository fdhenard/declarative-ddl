(defproject declarative-ddl "0.1.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [diff-as-list "2.2.6"]
                 [clojure.java-time "0.3.2"]
                 ]
  :repl-options {:init-ns declarative-ddl.core})
