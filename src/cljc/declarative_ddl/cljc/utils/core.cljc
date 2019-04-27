(ns declarative-ddl.cljc.utils.core
  (:require #?(:clj [clojure.pprint :as pp]
               :cljs [cljs.pprint :as pp])))

(defn pp [derta]
  (with-out-str (pp/pprint derta)))

(defn log [a-string]
  #?(:clj (println a-string)
     :cljs (.log js/console a-string)))
