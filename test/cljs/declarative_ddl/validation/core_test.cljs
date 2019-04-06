(ns declarative-ddl.validation.core-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [declarative-ddl.validation.core :as val]
            [clojure.spec.alpha :as spec]))

;; (deftest fakey
;;   (is (= 1 1)))

;; (deftest fails
;;   (is (= 1 2)))

(def numeric-field-def {:type :numeric
                        :total-length 5
                        :decimal-places 2})

(def spec-res (val/field-def-to-spec numeric-field-def))

(deftest numeric-too-many-decimal-places
  (let [splain (spec/explain-data spec-res 1.111)
        problem-str (-> splain :cljs.spec.alpha/problems first :pred str)]
    (is (= problem-str "is-decimal-places-below-bounds?"))))

(deftest numeric-ok
  (let [splain (spec/explain-data spec-res 1.11)]
    (is (nil? splain))))

(deftest numeric-too-big
  (let [splain (spec/explain-data spec-res 11111.1)
        problem-str (-> splain :cljs.spec.alpha/problems first :pred str)]
    (is (= "is-total-length-below-bounds?" problem-str))))
